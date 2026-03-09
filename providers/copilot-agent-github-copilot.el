;;; copilot-agent-github-copilot.el --- GitHub Copilot provider -*- lexical-binding: t -*-

;;; Commentary:
;; Provider for GitHub Copilot via a direct GitHub OAuth device flow.
;; No API key or `gh' CLI required — authenticate once with your GitHub
;; account; the token is stored in ~/.emacs-copilot-agent/github_copilot_creds.json.
;;
;; Auth flow (GitHub OAuth 2.0 Device Authorization Grant):
;;   M-x copilot-agent-github-copilot-login
;;   → Opens https://github.com/login/device in browser with a user code.
;;   → Polls until approved, then saves the GitHub OAuth token.
;;   → Short-lived Copilot session tokens (~30 min) are fetched and cached
;;     automatically on each request.
;;
;; Available models: M-x copilot-agent-github-copilot-list-models
;; API: https://api.githubcopilot.com/chat/completions (OpenAI-compatible)
;; Requires: a GitHub account with an active Copilot subscription.

;;; Code:

(require 'json)
(require 'url)
(require 'copilot-agent-api)

;;; ---------- Constants ----------

(defconst copilot-agent-github-copilot--device-url
  "https://github.com/login/device/code"
  "GitHub device-code endpoint.")

(defconst copilot-agent-github-copilot--token-url
  "https://github.com/login/oauth/access_token"
  "GitHub OAuth token endpoint.")

(defconst copilot-agent-github-copilot--session-token-url
  "https://api.github.com/copilot_internal/v2/token"
  "Endpoint to exchange a GitHub token for a short-lived Copilot session token.")

(defconst copilot-agent-github-copilot--api-base
  "https://api.githubcopilot.com"
  "GitHub Copilot API base URL.")

(defconst copilot-agent-github-copilot--client-id
  "Iv1.b507a08c87ecfe98"
  "GitHub OAuth App client ID (same one used by copilot.vim / copilot.lua).")

(defconst copilot-agent-github-copilot--creds-file
  (expand-file-name "~/.emacs-copilot-agent/github_copilot_creds.json")
  "Path where the GitHub OAuth token is persisted.")

;;; ---------- Customisation ----------

(defcustom copilot-agent-github-copilot-default-model "gpt-4o"
  "Default GitHub Copilot model.
Run M-x copilot-agent-github-copilot-list-models to see what is available
for your subscription, then set this to any model ID from that list."
  :type 'string
  :group 'copilot-agent)

;;; ---------- In-memory Copilot session token cache ----------

(defvar copilot-agent-github-copilot--session-token nil
  "Cached Copilot session token string, or nil if not yet fetched.")

(defvar copilot-agent-github-copilot--session-expires 0
  "Unix timestamp (float) at which the cached session token expires.")

;;; ---------- Synchronous HTTP helpers ----------

(defun copilot-agent-github-copilot--post-form (url params)
  "POST URL-encoded PARAMS alist to URL synchronously.
Returns a parsed JSON alist or signals an error."
  (let* ((body (mapconcat (lambda (p)
                            (concat (url-hexify-string (car p))
                                    "="
                                    (url-hexify-string (cdr p))))
                          params "&"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")
            ("Accept"       . "application/json")))
         (url-request-data (encode-coding-string body 'utf-8))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (error "GitHub Copilot: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^\r?$" nil t)
          (json-read))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun copilot-agent-github-copilot--get-json (url github-token)
  "GET URL synchronously using GITHUB-TOKEN for authentication.
Returns a parsed JSON alist or signals an error."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "token " github-token))
            ("Accept"        . "application/json")))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (error "GitHub Copilot: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^\r?$" nil t)
          (json-read))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ---------- Credential Storage ----------

(defun copilot-agent-github-copilot--save-creds (github-token)
  "Persist GITHUB-TOKEN to the credentials file (mode 0600)."
  (make-directory (file-name-directory copilot-agent-github-copilot--creds-file) t)
  (with-temp-file copilot-agent-github-copilot--creds-file
    (insert (json-encode `((github_token . ,github-token)))))
  (set-file-modes copilot-agent-github-copilot--creds-file #o600))

(defun copilot-agent-github-copilot--load-github-token ()
  "Return the stored GitHub OAuth token string, or nil if absent."
  (when (file-exists-p copilot-agent-github-copilot--creds-file)
    (condition-case _
        (cdr (assq 'github_token
                   (json-read-file copilot-agent-github-copilot--creds-file)))
      (error nil))))

;;; ---------- Copilot Session Token ----------

(defun copilot-agent-github-copilot--fetch-session-token (github-token)
  "Exchange GITHUB-TOKEN for a short-lived Copilot session token.
Caches the result in memory. Returns the token string."
  (let* ((result  (copilot-agent-github-copilot--get-json
                   copilot-agent-github-copilot--session-token-url
                   github-token))
         (token   (cdr (assq 'token      result)))
         (expires (cdr (assq 'expires_at result))))
    (unless token
      (error (concat "GitHub Copilot: failed to obtain session token.  "
                     "Check your Copilot subscription is active.")))
    (setq copilot-agent-github-copilot--session-token   token
          copilot-agent-github-copilot--session-expires (or expires 0))
    token))

(defun copilot-agent-github-copilot--valid-session-token ()
  "Return a valid Copilot session token, refreshing automatically if expired."
  (let ((github-token (copilot-agent-github-copilot--load-github-token)))
    (unless github-token
      (error (concat "No GitHub credentials found.  "
                     "Run M-x copilot-agent-github-copilot-login first.")))
    ;; Refresh if the cached token expires within 60 s
    (if (> (- copilot-agent-github-copilot--session-expires (float-time)) 60)
        copilot-agent-github-copilot--session-token
      (copilot-agent-github-copilot--fetch-session-token github-token))))

;;; ---------- Device OAuth Login ----------

;;;###autoload
(defun copilot-agent-github-copilot-login ()
  "Authenticate with GitHub using the OAuth device-code flow.
Opens https://github.com/login/device in a browser; enter the displayed
one-time code to approve access.  The GitHub OAuth token is saved to
~/.emacs-copilot-agent/github_copilot_creds.json."
  (interactive)
  ;; Step 1: request a device code
  (let* ((device      (copilot-agent-github-copilot--post-form
                       copilot-agent-github-copilot--device-url
                       `(("client_id" . ,copilot-agent-github-copilot--client-id)
                         ("scope"     . "read:user"))))
         (device-code (cdr (assq 'device_code     device)))
         (user-code   (cdr (assq 'user_code        device)))
         (verify-url  (cdr (assq 'verification_uri device)))
         (exp-secs    (or  (cdr (assq 'expires_in  device)) 300))
         (interval-ms (* (or (cdr (assq 'interval  device)) 5) 1000)))
    (unless (and device-code user-code verify-url)
      (error "GitHub Copilot device auth failed: %s" (json-encode device)))
    ;; Step 2: prompt user and open browser
    (kill-new user-code)
    (message "GitHub Copilot login:\n  Opening: %s\n  Enter code: %s  (copied to clipboard)\n  Waiting…"
             verify-url user-code)
    (browse-url verify-url)
    ;; Step 3: poll for the access token
    (let ((deadline (+ (float-time) exp-secs))
          (token nil))
      (while (and (< (float-time) deadline) (not token))
        (sit-for (/ interval-ms 1000.0))
        (condition-case _
            (let* ((result (copilot-agent-github-copilot--post-form
                            copilot-agent-github-copilot--token-url
                            `(("grant_type"  . "urn:ietf:params:oauth:grant-type:device_code")
                              ("client_id"   . ,copilot-agent-github-copilot--client-id)
                              ("device_code" . ,device-code))))
                   (err    (cdr (assq 'error        result)))
                   (access (cdr (assq 'access_token result))))
              (cond
               ((equal err "authorization_pending") nil)   ; keep waiting
               ((equal err "slow_down")
                (setq interval-ms (min (* interval-ms 2) 10000)))
               (access
                (copilot-agent-github-copilot--save-creds access)
                (setq token access))
               (err (error "GitHub OAuth error: %s" err))))
          (error nil)))        ; ignore transient HTTP errors during polling
      (if token
          (message "GitHub Copilot login successful!  Credentials saved to %s"
                   copilot-agent-github-copilot--creds-file)
        (error "GitHub Copilot login timed out.  Run M-x copilot-agent-github-copilot-login again.")))))

;;; ---------- List Available Models ----------

;;;###autoload
(defun copilot-agent-github-copilot-list-models ()
  "Fetch and display the models available for your Copilot subscription."
  (interactive)
  (let* ((session-token (copilot-agent-github-copilot--valid-session-token))
         (url           (concat copilot-agent-github-copilot--api-base "/models"))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization"          . ,(concat "Bearer " session-token))
            ("Copilot-Integration-Id" . "vscode-chat")
            ("Editor-Version"         . "vscode/1.85.0")
            ("Accept"                 . "application/json")))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf (error "GitHub Copilot: no response from /models"))
    (let* ((data   (unwind-protect
                       (with-current-buffer buf
                         (goto-char (point-min))
                         (re-search-forward "^\r?$" nil t)
                         (json-read))
                     (kill-buffer buf)))
           (models (cdr (assq 'data data))))
      (if (and models (> (length models) 0))
          (with-output-to-temp-buffer "*Copilot Models*"
            (princ "Available GitHub Copilot models:\n")
            (princ "(set with: (setq copilot-agent-github-copilot-default-model \"MODEL-ID\"))\n\n")
            (seq-doseq (m models)
              (let* ((id   (cdr (assq 'id           m)))
                     (name (cdr (assq 'name         m)))
                     (caps (cdr (assq 'capabilities m)))
                     (tc   (and caps (cdr (assq 'supports_tool_calls caps)))))
                (princ (format "  %-45s %s%s\n"
                               id
                               (or name "")
                               (if (eq tc t) "  [tool-calls]" ""))))))
        (message "No models returned — check your Copilot subscription.")))))

;;; ---------- Tool Schema Formatting ----------
;; GitHub Copilot uses the OpenAI function-calling format.

(defun copilot-agent-github-copilot--format-tools (tool-schema)
  "Convert universal TOOL-SCHEMA to OpenAI function-calling format."
  (mapcar (lambda (tool)
            `((type     . "function")
              (function . ((name        . ,(cdr (assq 'name        tool)))
                           (description . ,(cdr (assq 'description tool)))
                           (parameters  . ,(cdr (assq 'parameters  tool)))))))
          tool-schema))

;;; ---------- Message Conversion ----------

(defun copilot-agent-github-copilot--convert-message (msg)
  "Convert a session message MSG to OpenAI wire format."
  (let ((role    (cdr (assq 'role         msg)))
        (content (cdr (assq 'content      msg)))
        (tc-id   (cdr (assq 'tool_call_id msg))))
    (cond
     ;; Tool result — already in OpenAI format
     (tc-id msg)
     ;; Assistant message with tool calls stored in raw-content shape
     ((and (listp content) (assq 'tool_calls content))
      `((role       . ,(if (equal role "assistant") "assistant" role))
        (content    . ,(cdr (assq 'content    content)))
        (tool_calls . ,(cdr (assq 'tool_calls content)))))
     ;; Plain text
     (t
      `((role    . ,role)
        (content . ,(cond ((stringp content) content)
                          ((null content)    "")
                          (t (format "%s" content)))))))))

;;; ---------- Request Building ----------

(defun copilot-agent-github-copilot--build-request (session)
  "Build a JSON request body string for SESSION."
  (let* ((model   (or (plist-get session :model)
                      copilot-agent-github-copilot-default-model))
         (msgs    (plist-get session :messages))
         (tools   (plist-get session :tools))
         (system  (plist-get session :system-prompt))
         (max-tok (or (plist-get session :max-tokens) 8192))
         (all-msgs (if system
                       (cons `((role . "system") (content . ,system)) msgs)
                     msgs))
         (req `((model      . ,model)
                (messages   . ,(vconcat
                                (mapcar #'copilot-agent-github-copilot--convert-message
                                        all-msgs)))
                (max_tokens . ,max-tok)
                (stream     . :json-false))))
    (when tools
      (setq req (append req
                        `((tools . ,(vconcat
                                     (copilot-agent-github-copilot--format-tools tools)))))))
    (json-encode req)))

;;; ---------- Response Parsing ----------

(defun copilot-agent-github-copilot--parse-response (raw-json)
  "Parse RAW-JSON from the GitHub Copilot API into a normalised response plist.
Returns plist with :text :tool-calls :stop-reason :raw-content or :error."
  (condition-case err
      (let* ((data    (json-read-from-string raw-json))
             (err-obj (cdr (assq 'error data))))
        (when err-obj
          (cl-return-from copilot-agent-github-copilot--parse-response
            (list :error (format "%s: %s"
                                 (or (cdr (assq 'code    err-obj)) "error")
                                 (cdr (assq 'message err-obj))))))
        (let* ((choices (cdr (assq 'choices data)))
               (choice  (and (vectorp choices) (> (length choices) 0) (aref choices 0)))
               (msg     (and choice (cdr (assq 'message       choice))))
               (finish  (and choice (cdr (assq 'finish_reason choice))))
               (content (and msg (cdr (assq 'content    msg))))
               (tc-raw  (and msg (cdr (assq 'tool_calls msg))))
               tool-calls)
          (unless choice
            (cl-return-from copilot-agent-github-copilot--parse-response
              (list :error "GitHub Copilot response contained no choices")))
          (when (and tc-raw (vectorp tc-raw))
            (seq-doseq (tc tc-raw)
              (let* ((id       (cdr (assq 'id tc)))
                     (fn       (cdr (assq 'function tc)))
                     (name     (cdr (assq 'name fn)))
                     (args-str (cdr (assq 'arguments fn)))
                     (args     (condition-case _
                                   (json-read-from-string (or args-str "{}"))
                                 (error nil))))
                (push (list :id id :name name :input args) tool-calls))))
          (let* ((ordered (nreverse tool-calls))
                 (raw-content
                  (if ordered
                      `((content    . ,content)
                        (tool_calls . ,(vconcat
                                        (mapcar
                                         (lambda (tc)
                                           `((id   . ,(plist-get tc :id))
                                             (type . "function")
                                             (function
                                              . ((name      . ,(plist-get tc :name))
                                                 (arguments . ,(json-encode
                                                                (plist-get tc :input)))))))
                                         ordered))))
                    (or content ""))))
            (list :text        (unless (equal content :null) content)
                  :tool-calls  ordered
                  :stop-reason finish
                  :raw-content raw-content))))
    (error (list :error (format "Parse error: %s" (error-message-string err))))))

;;; ---------- Provider Send Function ----------

(defun copilot-agent-github-copilot-send (session callback)
  "Send SESSION to GitHub Copilot asynchronously.
CALLBACK is called as (RESPONSE-PLIST NIL) or (NIL ERROR-STRING)."
  (let* ((token (condition-case e
                    (copilot-agent-github-copilot--valid-session-token)
                  (error (funcall callback nil (error-message-string e)) nil)))
         (url   (concat copilot-agent-github-copilot--api-base "/chat/completions"))
         (body  (copilot-agent-github-copilot--build-request session)))
    (when token
      (copilot-agent-api--curl-post
       url
       (list (concat "Authorization: Bearer " token)
             "Copilot-Integration-Id: vscode-chat"
             "Editor-Version: vscode/1.85.0"
             "openai-intent: conversation-panel")
       body
       (lambda (raw-output http-error)
         (if http-error
             (funcall callback nil http-error)
           (let ((parsed (copilot-agent-github-copilot--parse-response raw-output)))
             (if (plist-get parsed :error)
                 (funcall callback nil (plist-get parsed :error))
               (funcall callback parsed nil)))))))))

;;; ---------- Tool Result Message Builder ----------

(defun copilot-agent-github-copilot-make-tool-result-message (tool-results)
  "Return a list of OpenAI tool-role messages, one per entry in TOOL-RESULTS."
  (mapcar (lambda (r)
            `((role         . "tool")
              (tool_call_id . ,(plist-get r :tool-use-id))
              (content      . ,(plist-get r :content))))
          tool-results))

;;; ---------- Provider Registration ----------

(with-eval-after-load 'copilot-agent-api
  (copilot-agent-api-register-provider
   'github-copilot
   (list :display-name        "GitHub Copilot"
         :default-model       copilot-agent-github-copilot-default-model
         :default-model-fn    (lambda () copilot-agent-github-copilot-default-model)
         :send-fn             #'copilot-agent-github-copilot-send
         :make-tool-result-fn #'copilot-agent-github-copilot-make-tool-result-message
         :format-tools-fn     #'copilot-agent-github-copilot--format-tools)))

(provide 'copilot-agent-github-copilot)
;;; copilot-agent-github-copilot.el ends here

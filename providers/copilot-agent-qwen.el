;;; copilot-agent-qwen.el --- Qwen portal provider (free OAuth tier) -*- lexical-binding: t -*-

;; Package-Lint-Main-File: "../copilot-agent.el"

;;; Commentary:
;; Provider for Alibaba's Qwen models via the free-tier portal OAuth flow.
;; No API key required — authenticate once with your Qwen account and tokens
;; are stored in ~/.emacs-copilot-agent/qwen_oauth_creds.json.
;;
;; Auth flow (OAuth 2.0 Device Authorization Grant + PKCE):
;;   M-x copilot-agent-qwen-login
;;   → Opens https://chat.qwen.ai in browser, shows a user code to approve.
;;   → Polls until approved, then saves access + refresh tokens.
;;   → Tokens auto-refresh on each session; re-run login if refresh expires.
;;
;; API: https://portal.qwen.ai/v1  (OpenAI-compatible /chat/completions)
;; Models: "coder-model" (Qwen Coder) and "vision-model" (Qwen Vision)
;; Free tier: 2 000 requests/day.

;;; Code:

(require 'json)
(require 'url)
(require 'copilot-agent-api)

;;; ---------- Constants ----------

(defconst copilot-agent-qwen--oauth-base    "https://chat.qwen.ai")
(defconst copilot-agent-qwen--device-url    (concat copilot-agent-qwen--oauth-base "/api/v1/oauth2/device/code"))
(defconst copilot-agent-qwen--token-url     (concat copilot-agent-qwen--oauth-base "/api/v1/oauth2/token"))
(defconst copilot-agent-qwen--client-id     "f0304373b74a44d2b584a3fb70ca9e56")
(defconst copilot-agent-qwen--scope         "openid profile email model.completion")
(defconst copilot-agent-qwen--api-base      "https://portal.qwen.ai/v1")
(defconst copilot-agent-qwen--creds-file
  (expand-file-name "~/.emacs-copilot-agent/qwen_oauth_creds.json")
  "Path where Qwen OAuth tokens are stored.")

;;; ---------- Customisation ----------

(defcustom copilot-agent-qwen-default-model "coder-model"
  "Default Qwen portal model.  Either \"coder-model\" or \"vision-model\"."
  :type '(choice (const "coder-model") (const "vision-model") string)
  :group 'copilot-agent)

;;; ---------- PKCE Utilities ----------

(defun copilot-agent-qwen--base64url (s)
  "Base64url-encode unibyte string S without padding."
  (replace-regexp-in-string
   "=" ""
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string "+" "-" (base64-encode-string s t)))))

(defun copilot-agent-qwen--random-bytes (n)
  "Return a unibyte string of N cryptographically random bytes.
Uses `gnutls-random' when available (GnuTLS linked Emacs), otherwise
falls back to Emacs `random' which is not cryptographic but still
produces a unique-per-session verifier for the PKCE device flow."
  (if (fboundp 'gnutls-random)
      (gnutls-random n)
    (apply #'unibyte-string (mapcar (lambda (_) (random 256)) (make-list n nil)))))

(defun copilot-agent-qwen--pkce ()
  "Return (VERIFIER . CHALLENGE) for PKCE-S256.
VERIFIER is a base64url string of 32 random bytes.
CHALLENGE is the base64url-encoded SHA-256 hash of VERIFIER."
  (let* ((raw      (copilot-agent-qwen--random-bytes 32))
         (verifier  (copilot-agent-qwen--base64url raw))
         (hash      (secure-hash 'sha256 verifier nil nil t))  ; binary bytes
         (challenge (copilot-agent-qwen--base64url hash)))
    (cons verifier challenge)))

;;; ---------- Synchronous OAuth HTTP ----------

(defun copilot-agent-qwen--post-form (url params)
  "POST URL-encoded PARAMS alist to URL synchronously.
Returns parsed JSON alist or signals an error."
  (let* ((body (mapconcat
                (lambda (p)
                  (concat (url-hexify-string (car p)) "=" (url-hexify-string (cdr p))))
                params "&"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")
            ("Accept"       . "application/json")))
         (url-request-data (encode-coding-string body 'utf-8))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (error "Qwen: network error — no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^\r?$" nil t)  ; skip HTTP headers
          (json-read))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ---------- Credential Storage ----------

(defun copilot-agent-qwen--save-creds (access refresh expires)
  "Write ACCESS, REFRESH tokens and EXPIRES (ms) to the credentials file."
  (make-directory (file-name-directory copilot-agent-qwen--creds-file) t)
  (with-temp-file copilot-agent-qwen--creds-file
    (insert (json-encode `((access_token  . ,access)
                           (refresh_token . ,refresh)
                           (expires       . ,expires)))))
  (set-file-modes copilot-agent-qwen--creds-file #o600))

(defun copilot-agent-qwen--load-creds ()
  "Load credentials from the credentials file.
Returns alist with keys access_token, refresh_token, expires (ms), or nil."
  (when (file-exists-p copilot-agent-qwen--creds-file)
    (condition-case _
        (json-read-file copilot-agent-qwen--creds-file)
      (error nil))))

;;; ---------- Token Refresh ----------

(defun copilot-agent-qwen--refresh-token (refresh-token)
  "Exchange REFRESH-TOKEN for a new access token.
Returns updated creds alist or signals an error."
  (let* ((result (copilot-agent-qwen--post-form
                  copilot-agent-qwen--token-url
                  `(("grant_type"    . "refresh_token")
                    ("client_id"     . ,copilot-agent-qwen--client-id)
                    ("refresh_token" . ,refresh-token))))
         (access    (cdr (assq 'access_token  result)))
         (new-ref   (or (cdr (assq 'refresh_token result)) refresh-token))
         (exp-in    (cdr (assq 'expires_in    result))))
    (unless access
      (error "Qwen token refresh failed — re-run M-x copilot-agent-qwen-login"))
    (let ((expires (+ (truncate (* (float-time) 1000))
                      (* (or exp-in 3600) 1000))))
      (copilot-agent-qwen--save-creds access new-ref expires)
      `((access_token  . ,access)
        (refresh_token . ,new-ref)
        (expires       . ,expires)))))

;;; ---------- Token Acquisition ----------

(defun copilot-agent-qwen--valid-access-token ()
  "Return a valid access token string, refreshing if needed.
Signal an error if no credentials exist (run \\[copilot-agent-qwen-login])."
  (let* ((creds   (copilot-agent-qwen--load-creds))
         (access  (and creds (cdr (assq 'access_token  creds))))
         (refresh (and creds (cdr (assq 'refresh_token creds))))
         (expires (and creds (cdr (assq 'expires       creds))))
         (now-ms  (truncate (* (float-time) 1000))))
    (unless access
      (error "No Qwen credentials found -- run M-x copilot-agent-qwen-login first"))
    ;; Refresh if token expires within 60 s
    (if (and expires (> (- expires now-ms) 60000))
        access
      (unless refresh
        (error "Qwen refresh token missing — re-run M-x copilot-agent-qwen-login"))
      (cdr (assq 'access_token (copilot-agent-qwen--refresh-token refresh))))))

;;; ---------- Device OAuth Login ----------

;;;###autoload
(defun copilot-agent-qwen-login ()
  "Authenticate with Qwen using the device-code OAuth flow (free tier).
Opens a browser URL; enter the displayed code to approve access.
Tokens are saved to ~/.emacs-copilot-agent/qwen_oauth_creds.json."
  (interactive)
  (let* ((pkce      (copilot-agent-qwen--pkce))
         (verifier  (car pkce))
         (challenge (cdr pkce))
         ;; Step 1: request device code
         (device (copilot-agent-qwen--post-form
                  copilot-agent-qwen--device-url
                  `(("client_id"             . ,copilot-agent-qwen--client-id)
                    ("scope"                 . ,copilot-agent-qwen--scope)
                    ("code_challenge"        . ,challenge)
                    ("code_challenge_method" . "S256"))))
         (device-code  (cdr (assq 'device_code  device)))
         (user-code    (cdr (assq 'user_code    device)))
         (verify-url   (or (cdr (assq 'verification_uri_complete device))
                           (cdr (assq 'verification_uri          device))))
         (exp-secs     (or (cdr (assq 'expires_in device)) 300))
         (interval-ms  (* (or (cdr (assq 'interval device)) 2) 1000)))
    (unless (and device-code user-code verify-url)
      (error "Qwen device authorization failed: %s" (json-encode device)))
    ;; Step 2: tell user and open browser
    (message "Qwen login: opening %s\nIf prompted, enter code: %s"
             verify-url user-code)
    (browse-url verify-url)
    ;; Step 3: poll for token
    (let ((deadline (+ (float-time) exp-secs))
          (token nil))
      (while (and (< (float-time) deadline) (not token))
        (sit-for (/ interval-ms 1000.0))
        (condition-case _
            (let* ((result (copilot-agent-qwen--post-form
                            copilot-agent-qwen--token-url
                            `(("grant_type"   . "urn:ietf:params:oauth:grant-type:device_code")
                              ("client_id"    . ,copilot-agent-qwen--client-id)
                              ("device_code"  . ,device-code)
                              ("code_verifier". ,verifier))))
                   (err    (cdr (assq 'error result)))
                   (access (cdr (assq 'access_token  result)))
                   (ref    (cdr (assq 'refresh_token result)))
                   (exp-in (cdr (assq 'expires_in    result))))
              (cond
               ((equal err "authorization_pending") nil)  ; keep waiting
               ((equal err "slow_down")
                (setq interval-ms (min (* interval-ms 2) 10000)))
               (access
                (let ((expires (+ (truncate (* (float-time) 1000))
                                  (* (or exp-in 3600) 1000))))
                  (copilot-agent-qwen--save-creds access ref expires)
                  (setq token access)))
               (err (error "Qwen OAuth error: %s" err))))
          (error nil)))  ; ignore transient HTTP errors during polling
      (if token
          (message "Qwen login successful!  Credentials saved to %s"
                   copilot-agent-qwen--creds-file)
        (error "Qwen login timed out -- run M-x copilot-agent-qwen-login again")))))

;;; ---------- Tool Schema Formatting ----------

(defun copilot-agent-qwen--format-tools (tool-schema)
  "Convert universal TOOL-SCHEMA to OpenAI function-calling format."
  (mapcar (lambda (tool)
            `((type     . "function")
              (function . ((name        . ,(cdr (assq 'name        tool)))
                           (description . ,(cdr (assq 'description tool)))
                           (parameters  . ,(cdr (assq 'parameters  tool)))))))
          tool-schema))

;;; ---------- Message Conversion ----------

(defun copilot-agent-qwen--convert-message (msg)
  "Convert a session message MSG to OpenAI wire format.
Handles three cases:
  1. Tool result: already in OpenAI format (has tool_call_id) — pass through.
  2. Assistant with tool calls: raw-content is an alist with tool_calls key.
  3. Plain text user/assistant/system: wrap content as string."
  (let ((role    (cdr (assq 'role         msg)))
        (content (cdr (assq 'content      msg)))
        (tc-id   (cdr (assq 'tool_call_id msg))))
    (cond
     ;; Tool result message — already in OpenAI format
     (tc-id msg)
     ;; Assistant message whose raw-content encodes OpenAI tool_calls
     ((and (listp content) (assq 'tool_calls content))
      `((role       . ,(if (equal role "assistant") "assistant" role))
        (content    . ,(cdr (assq 'content    content)))
        (tool_calls . ,(cdr (assq 'tool_calls content)))))
     ;; Plain text content (string or simple value)
     (t
      `((role    . ,role)
        (content . ,(cond ((stringp content) content)
                          ((null content)    "")
                          (t (format "%s" content)))))))))

;;; ---------- Request Building ----------

(defun copilot-agent-qwen--build-request (session)
  "Build JSON request body string for SESSION."
  (let* ((model   (or (plist-get session :model)
                      copilot-agent-qwen-default-model))
         (msgs    (plist-get session :messages))
         (tools   (plist-get session :tools))
         (system  (plist-get session :system-prompt))
         (max-tok (or (plist-get session :max-tokens) 8192))
         ;; Prepend system message if present
         (all-msgs (if system
                       (cons `((role . "system") (content . ,system)) msgs)
                     msgs))
         (req `((model      . ,model)
                (messages   . ,(vconcat (mapcar #'copilot-agent-qwen--convert-message
                                                all-msgs)))
                (max_tokens . ,max-tok)
                (stream     . :json-false))))
    (when tools
      (setq req (append req
                        `((tools . ,(vconcat (copilot-agent-qwen--format-tools tools)))))))
    (json-encode req)))

;;; ---------- Response Parsing ----------

(defun copilot-agent-qwen--parse-response (raw-json)
  "Parse RAW-JSON from Qwen/OpenAI API into a normalised response plist.
Returns plist with :text :tool-calls :stop-reason :raw-content :error."
  (condition-case err
      (let* ((data    (json-read-from-string raw-json))
             (err-obj (cdr (assq 'error data))))
        (when err-obj
          (cl-return-from copilot-agent-qwen--parse-response
            (list :error (format "%s: %s"
                                 (or (cdr (assq 'code err-obj)) "error")
                                 (cdr (assq 'message err-obj))))))
        (let* ((choices (cdr (assq 'choices data)))
               (choice  (and (vectorp choices) (> (length choices) 0) (aref choices 0)))
               (message (and choice (cdr (assq 'message choice))))
               (finish  (and choice (cdr (assq 'finish_reason choice))))
               (content (and message (cdr (assq 'content message))))
               (tc-raw  (and message (cdr (assq 'tool_calls message))))
               tool-calls)
          (unless choice
            (cl-return-from copilot-agent-qwen--parse-response
              (list :error "Qwen response contained no choices")))
          ;; Parse tool calls — arguments arrive as JSON strings, must decode
          (when (and tc-raw (vectorp tc-raw))
            (seq-doseq (tc tc-raw)
              (let* ((id   (cdr (assq 'id tc)))
                     (fn   (cdr (assq 'function tc)))
                     (name (cdr (assq 'name fn)))
                     (args-str (cdr (assq 'arguments fn)))
                     (args (condition-case _
                               (json-read-from-string (or args-str "{}"))
                             (error nil))))
                (push (list :id id :name name :input args) tool-calls))))
          (let* ((ordered (nreverse tool-calls))
                 ;; raw-content encodes the OpenAI assistant message shape so
                 ;; convert-message can reconstruct it faithfully for history.
                 (raw-content
                  (if ordered
                      `((content    . ,content)
                        (tool_calls . ,(vconcat
                                        (mapcar (lambda (tc)
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

(defun copilot-agent-qwen-send (session callback)
  "Send SESSION to Qwen portal asynchronously.
CALLBACK is called as (RESPONSE-PLIST NIL) or (NIL ERROR-STRING)."
  (let* ((token (condition-case e
                    (copilot-agent-qwen--valid-access-token)
                  (error (funcall callback nil (error-message-string e)) nil)))
         (url   (concat copilot-agent-qwen--api-base "/chat/completions"))
         (body  (copilot-agent-qwen--build-request session)))
    (when token
      (copilot-agent-api--curl-post
       url
       (list (concat "Authorization: Bearer " token))
       body
       (lambda (raw-output http-error)
         (if http-error
             (funcall callback nil http-error)
           (let ((parsed (copilot-agent-qwen--parse-response raw-output)))
             (if (plist-get parsed :error)
                 (funcall callback nil (plist-get parsed :error))
               (funcall callback parsed nil)))))))))

;;; ---------- Tool Result Message Builder ----------

(defun copilot-agent-qwen-make-tool-result-message (tool-results)
  "Return a LIST of OpenAI tool-role messages, one per result in TOOL-RESULTS.
OpenAI-compatible APIs require one {\"role\":\"tool\"} message per tool call."
  (mapcar (lambda (r)
            `((role         . "tool")
              (tool_call_id . ,(plist-get r :tool-use-id))
              (content      . ,(plist-get r :content))))
          tool-results))

;;; ---------- Model List ----------

(defun copilot-agent-qwen--list-models ()
  "Return the list of known Qwen portal model IDs."
  '("coder-model" "vision-model"))

(defun copilot-agent-qwen--set-model (model)
  "Set the active Qwen model to MODEL."
  (setq copilot-agent-qwen-default-model model))

;;; ---------- Provider Registration ----------

(copilot-agent-api-register-provider
 'qwen
 (list :display-name        "Alibaba Qwen (free portal)"
       :default-model       copilot-agent-qwen-default-model
       :default-model-fn    (lambda () copilot-agent-qwen-default-model)
       :send-fn             #'copilot-agent-qwen-send
       :make-tool-result-fn #'copilot-agent-qwen-make-tool-result-message
       :format-tools-fn     #'copilot-agent-qwen--format-tools
       :list-models-fn      #'copilot-agent-qwen--list-models
       :set-model-fn        #'copilot-agent-qwen--set-model))

(provide 'copilot-agent-qwen)
;;; copilot-agent-qwen.el ends here

;;; copilot-agent-gemini.el --- Google Gemini provider -*- lexical-binding: t -*-

;;; Commentary:
;; Implements the copilot-agent provider protocol for Google Gemini.
;;
;; Two authentication modes, selected by `copilot-agent-gemini-auth-mode':
;;
;;   api-key (default) — API key from auth-source:
;;     machine generativelanguage.googleapis.com login apikey password YOUR_KEY
;;
;;   cli — OAuth via the installed Gemini CLI (no API key needed):
;;     1. Install Gemini CLI:  brew install gemini-cli  (or npm -g @google/gemini-cli)
;;     2. M-x copilot-agent-gemini-login
;;     Tokens are saved to ~/.emacs-copilot-agent/gemini_oauth_creds.json.
;;     The client_id / client_secret are read from the installed Gemini CLI's
;;     own oauth2.js bundle, so no separate Google Cloud project is needed.
;;
;; Gemini API differences from Anthropic handled here:
;;   - Role names: "user" / "model"  (not "user" / "assistant")
;;   - Content format: {role, parts:[{text}]}  (not {role, content})
;;   - Tool calls:  parts:[{functionCall:{name,args}}]
;;   - Tool results: parts:[{functionResponse:{name,response:{result}}}]
;;   - Tool schema types: UPPERCASE  ("STRING" not "string")
;;   - api-key mode: key passed as query param; cli mode: Authorization: Bearer

;;; Code:

(require 'auth-source)
(require 'json)
(require 'url)
(require 'copilot-agent-api nil t)

;;; ---------- Configuration ----------

(defcustom copilot-agent-gemini-default-model "gemini-2.0-flash"
  "Default Gemini model to use."
  :type 'string
  :group 'copilot-agent)

(defcustom copilot-agent-gemini-auth-mode 'api-key
  "Authentication mode for the Gemini provider.
`api-key' — use an API key from auth-source (existing behaviour).
`cli'      — use OAuth via the installed Gemini CLI; run
             M-x copilot-agent-gemini-login once to authenticate."
  :type '(choice (const :tag "API key (auth-source)" api-key)
                 (const :tag "Gemini CLI OAuth"       cli))
  :group 'copilot-agent)

(defconst copilot-agent-gemini--base-url
  "https://generativelanguage.googleapis.com/v1beta/models"
  "Gemini API base URL (used by both auth modes).")

;;; ---------- API-key Auth ----------

(defun copilot-agent-gemini--api-key ()
  "Retrieve the Gemini API key from auth-source.
Add to ~/.authinfo:
  machine generativelanguage.googleapis.com login apikey password YOUR_KEY"
  (or (auth-source-pick-first-password
       :host "generativelanguage.googleapis.com")
      (error (concat "Gemini API key not found.  "
                     "Add to ~/.authinfo:\n"
                     "  machine generativelanguage.googleapis.com "
                     "login apikey password YOUR_KEY"))))

(defun copilot-agent-gemini--endpoint (model)
  "Return the generateContent endpoint URL for MODEL."
  (format "%s/%s:generateContent" copilot-agent-gemini--base-url model))

;;; ---------- CLI Auth — PKCE ----------

(defun copilot-agent-gemini--base64url (s)
  "Base64url-encode unibyte string S without padding."
  (replace-regexp-in-string
   "=" ""
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string "+" "-" (base64-encode-string s t)))))

(defun copilot-agent-gemini--random-bytes (n)
  "Return N cryptographically random bytes as a unibyte string."
  (if (fboundp 'gnutls-random)
      (gnutls-random n)
    (apply #'unibyte-string (mapcar (lambda (_) (random 256)) (make-list n nil)))))

(defun copilot-agent-gemini--pkce ()
  "Return (VERIFIER . CHALLENGE) for PKCE-S256."
  (let* ((raw      (copilot-agent-gemini--random-bytes 32))
         (verifier  (copilot-agent-gemini--base64url raw))
         (hash      (secure-hash 'sha256 verifier nil nil t))
         (challenge (copilot-agent-gemini--base64url hash)))
    (cons verifier challenge)))

;;; ---------- CLI Auth — Synchronous HTTP Form POST ----------

(defun copilot-agent-gemini--post-form (url params)
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
      (error "Gemini: network error — no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^\r?$" nil t)
          (json-read))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ---------- CLI Auth — Find Gemini CLI credentials ----------

(defun copilot-agent-gemini--find-cli-oauth2-js ()
  "Return path to the Gemini CLI's oauth2.js file, or nil if not found.
Resolves the real path of the `gemini' binary and walks up the directory
tree to find node_modules/@google/gemini-cli-core/dist/src/code_assist/oauth2.js."
  (let* ((bin  (executable-find "gemini"))
         (real (and bin (file-truename bin)))
         (dir  (and real (file-name-directory real)))
         found)
    (when dir
      (let ((current dir))
        (dotimes (_ 10)
          (unless found
            (let ((candidate
                   (expand-file-name
                    "node_modules/@google/gemini-cli-core/dist/src/code_assist/oauth2.js"
                    current)))
              (if (file-exists-p candidate)
                  (setq found candidate)
                (setq current
                      (file-name-directory (directory-file-name current)))))))))
    found))

(defun copilot-agent-gemini--cli-oauth-creds ()
  "Return (CLIENT-ID . CLIENT-SECRET) extracted from the installed Gemini CLI.
Signals an error if the Gemini CLI is not found or the credentials cannot
be parsed."
  (let ((oauth2-js (copilot-agent-gemini--find-cli-oauth2-js)))
    (unless oauth2-js
      (error (concat "Gemini CLI not found.  Install with:\n"
                     "  brew install gemini-cli\n"
                     "or: npm install -g @google/gemini-cli")))
    (let* ((content  (with-temp-buffer
                       (insert-file-contents oauth2-js)
                       (buffer-string)))
           (id-match  (and (string-match
                            (concat "\\([0-9]\\{10,\\}-[a-z0-9]+"
                                    "\\.apps\\.googleusercontent\\.com\\)")
                            content)
                           (match-string 1 content)))
           (sec-match (and (string-match "\\(GOCSPX-[A-Za-z0-9_-]+\\)" content)
                           (match-string 1 content))))
      (unless (and id-match sec-match)
        (error "Could not extract OAuth credentials from Gemini CLI's oauth2.js"))
      (cons id-match sec-match))))

;;; ---------- CLI Auth — Local Callback Server ----------

(defvar copilot-agent-gemini--oauth-code nil
  "Authorization code received from the OAuth callback.")

(defvar copilot-agent-gemini--oauth-server nil
  "TCP server process listening for the OAuth redirect.")

(defun copilot-agent-gemini--oauth-filter (proc string)
  "Handle incoming HTTP data on PROC; extract OAuth code from STRING."
  (when (string-match "[?&]code=\\([^& \t\r\n]+\\)" string)
    (setq copilot-agent-gemini--oauth-code (match-string 1 string))
    (process-send-string
     proc
     (concat "HTTP/1.1 200 OK\r\n"
             "Content-Type: text/html\r\n"
             "Connection: close\r\n\r\n"
             "<html><body>"
             "<h2>Gemini login successful.</h2>"
             "<p>You may close this tab and return to Emacs.</p>"
             "</body></html>"))
    (run-at-time 0.5 nil
                 (lambda ()
                   (ignore-errors (delete-process proc))
                   (when (process-live-p copilot-agent-gemini--oauth-server)
                     (delete-process copilot-agent-gemini--oauth-server))
                   (setq copilot-agent-gemini--oauth-server nil)))))

(defun copilot-agent-gemini--start-callback-server ()
  "Start a localhost HTTP server to capture the OAuth callback.
Returns the assigned port number."
  (setq copilot-agent-gemini--oauth-code nil)
  (let ((server (make-network-process
                 :name     "copilot-gemini-oauth-cb"
                 :server   t
                 :host     "127.0.0.1"
                 :service  0
                 :family   'ipv4
                 :filter   #'copilot-agent-gemini--oauth-filter
                 :sentinel #'ignore)))
    (setq copilot-agent-gemini--oauth-server server)
    (cadr (process-contact server))))

;;; ---------- CLI Auth — Credential Storage ----------

(defconst copilot-agent-gemini--cli-creds-file
  (expand-file-name "~/.emacs-copilot-agent/gemini_oauth_creds.json")
  "Path where Gemini CLI OAuth tokens are stored.")

(defun copilot-agent-gemini--cli-save-creds (access refresh expires)
  "Save ACCESS, REFRESH tokens and EXPIRES (ms) to the credentials file."
  (make-directory (file-name-directory copilot-agent-gemini--cli-creds-file) t)
  (with-temp-file copilot-agent-gemini--cli-creds-file
    (insert (json-encode `((access_token  . ,access)
                           (refresh_token . ,refresh)
                           (expires       . ,expires)))))
  (set-file-modes copilot-agent-gemini--cli-creds-file #o600))

(defun copilot-agent-gemini--cli-load-creds ()
  "Load credentials from the credentials file.  Returns alist or nil."
  (when (file-exists-p copilot-agent-gemini--cli-creds-file)
    (condition-case _
        (json-read-file copilot-agent-gemini--cli-creds-file)
      (error nil))))

(defun copilot-agent-gemini--cli-refresh-token (client-id client-secret refresh-token)
  "Exchange REFRESH-TOKEN for a new access token using CLIENT-ID and CLIENT-SECRET.
Saves and returns updated credentials alist."
  (let* ((result  (copilot-agent-gemini--post-form
                   "https://oauth2.googleapis.com/token"
                   `(("grant_type"    . "refresh_token")
                     ("client_id"     . ,client-id)
                     ("client_secret" . ,client-secret)
                     ("refresh_token" . ,refresh-token))))
         (access  (cdr (assq 'access_token  result)))
         (new-ref (or (cdr (assq 'refresh_token result)) refresh-token))
         (exp-in  (cdr (assq 'expires_in    result))))
    (unless access
      (error "Gemini token refresh failed — re-run M-x copilot-agent-gemini-login"))
    (let ((expires (+ (truncate (* (float-time) 1000))
                      (* (or exp-in 3600) 1000))))
      (copilot-agent-gemini--cli-save-creds access new-ref expires)
      `((access_token  . ,access)
        (refresh_token . ,new-ref)
        (expires       . ,expires)))))

(defun copilot-agent-gemini--cli-valid-access-token ()
  "Return a valid access token string, refreshing if within 60 s of expiry.
Signals an error if no credentials exist (run M-x copilot-agent-gemini-login)."
  (let* ((creds   (copilot-agent-gemini--cli-load-creds))
         (access  (and creds (cdr (assq 'access_token  creds))))
         (refresh (and creds (cdr (assq 'refresh_token creds))))
         (expires (and creds (cdr (assq 'expires       creds))))
         (now-ms  (truncate (* (float-time) 1000))))
    (unless access
      (error "No Gemini CLI credentials.  Run M-x copilot-agent-gemini-login first."))
    (if (and expires (> (- expires now-ms) 60000))
        access
      (unless refresh
        (error "Gemini refresh token missing — re-run M-x copilot-agent-gemini-login"))
      (let ((cli-creds (copilot-agent-gemini--cli-oauth-creds)))
        (cdr (assq 'access_token
                   (copilot-agent-gemini--cli-refresh-token
                    (car cli-creds) (cdr cli-creds) refresh)))))))

;;; ---------- CLI Auth — Login Command ----------

(defconst copilot-agent-gemini--oauth-auth-url
  "https://accounts.google.com/o/oauth2/v2/auth")
(defconst copilot-agent-gemini--oauth-scopes
  "https://www.googleapis.com/auth/generative-language openid email profile")
(defconst copilot-agent-gemini--oauth-token-url
  "https://oauth2.googleapis.com/token")

;;;###autoload
(defun copilot-agent-gemini-login ()
  "Authenticate with Gemini using the installed Gemini CLI's OAuth credentials.
Reads the client_id and client_secret from the Gemini CLI's bundled oauth2.js,
performs a PKCE browser auth flow, and saves tokens to
~/.emacs-copilot-agent/gemini_oauth_creds.json.

Requires the Gemini CLI to be installed:
  brew install gemini-cli  or  npm install -g @google/gemini-cli"
  (interactive)
  (let* ((cli-creds    (copilot-agent-gemini--cli-oauth-creds))
         (client-id    (car cli-creds))
         (client-secret (cdr cli-creds))
         (pkce          (copilot-agent-gemini--pkce))
         (verifier      (car pkce))
         (challenge     (cdr pkce))
         (port          (copilot-agent-gemini--start-callback-server))
         (redirect-uri  (format "http://127.0.0.1:%d/oauth2callback" port))
         (auth-url
          (concat copilot-agent-gemini--oauth-auth-url
                  "?client_id="             (url-hexify-string client-id)
                  "&redirect_uri="          (url-hexify-string redirect-uri)
                  "&response_type=code"
                  "&scope="                 (url-hexify-string copilot-agent-gemini--oauth-scopes)
                  "&code_challenge="        challenge
                  "&code_challenge_method=S256"
                  "&access_type=offline"
                  "&prompt=consent")))
    (message "Gemini login: opening browser for Google sign-in...")
    (browse-url auth-url)
    ;; Wait up to 120 s for the browser callback
    (let ((deadline (+ (float-time) 120))
          code)
      (while (and (< (float-time) deadline)
                  (not (setq code copilot-agent-gemini--oauth-code)))
        (sit-for 0.5))
      (when (process-live-p copilot-agent-gemini--oauth-server)
        (delete-process copilot-agent-gemini--oauth-server)
        (setq copilot-agent-gemini--oauth-server nil))
      (unless code
        (error "Gemini login timed out.  Run M-x copilot-agent-gemini-login again."))
      ;; Exchange auth code for tokens
      (let* ((result  (copilot-agent-gemini--post-form
                       copilot-agent-gemini--oauth-token-url
                       `(("grant_type"    . "authorization_code")
                         ("client_id"     . ,client-id)
                         ("client_secret" . ,client-secret)
                         ("code"          . ,code)
                         ("redirect_uri"  . ,redirect-uri)
                         ("code_verifier" . ,verifier))))
             (access  (cdr (assq 'access_token  result)))
             (refresh (cdr (assq 'refresh_token result)))
             (exp-in  (cdr (assq 'expires_in    result))))
        (unless access
          (error "Gemini token exchange failed: %s" (json-encode result)))
        (let ((expires (+ (truncate (* (float-time) 1000))
                          (* (or exp-in 3600) 1000))))
          (copilot-agent-gemini--cli-save-creds access refresh expires)
          (message "Gemini login successful!  Credentials saved to %s"
                   copilot-agent-gemini--cli-creds-file))))))

;;; ---------- Tool Schema Conversion ----------

(defun copilot-agent-gemini--upcase-types (params)
  "Recursively convert JSON-Schema type strings in PARAMS to Gemini uppercase.
Only the value of `type' keys is uppercased; other string values are left alone.
e.g. {type: \"string\"} -> {type: \"STRING\"}"
  (cond
   ((vectorp params)
    (vconcat (mapcar #'copilot-agent-gemini--upcase-types (append params nil))))
   ((listp params)
    (mapcar (lambda (pair)
              (cons (car pair)
                    (if (eq (car pair) 'type)
                        (upcase (cdr pair))          ; only uppercase type values
                      (copilot-agent-gemini--upcase-types (cdr pair)))))
            params))
   (t params)))   ; strings and other atoms pass through unchanged

(defun copilot-agent-gemini--format-tools (tool-schema)
  "Convert universal TOOL-SCHEMA list to Gemini `tools' array.
Gemini wraps declarations in a `function_declarations' key and uses
uppercase type names."
  (list
   (list
    (cons 'function_declarations
          (vconcat
           (mapcar
            (lambda (tool)
              (let ((params (copilot-agent-gemini--upcase-types
                             (cdr (assq 'parameters tool)))))
                `((name        . ,(cdr (assq 'name tool)))
                  (description . ,(cdr (assq 'description tool)))
                  (parameters  . ,params))))
            tool-schema))))))

;;; ---------- Message Conversion ----------

(defun copilot-agent-gemini--canonical->gemini-role (role)
  "Convert canonical role string (\"user\"/\"assistant\") to Gemini role."
  (if (equal role "assistant") "model" role))

(defun copilot-agent-gemini--canonical->parts (content)
  "Convert canonical message CONTENT to a Gemini `parts' vector.
CONTENT may be a string (simple text) or a vector of content blocks."
  (cond
   ;; Simple string → single text part
   ((stringp content)
    (vector `((text . ,content))))
   ;; Vector of blocks (tool_use, text, tool_result, …)
   ((or (vectorp content) (listp content))
    (vconcat
     (delq nil
           (mapcar
            (lambda (block)
              (let ((type (cdr (assq 'type block))))
                (cond
                 ((equal type "text")
                  `((text . ,(cdr (assq 'text block)))))
                 ((equal type "tool_use")
                  `((functionCall
                     . ((name . ,(cdr (assq 'name block)))
                        (args . ,(cdr (assq 'input block)))))))
                 ((equal type "tool_result")
                  `((functionResponse
                     . ((name     . ,(cdr (assq 'tool_use_id block)))
                        (response . ((result . ,(cdr (assq 'content block)))))))))
                 (t nil))))
            (if (vectorp content) (append content nil) content)))))
   (t (vector `((text . ,(format "%s" content)))))))

(defun copilot-agent-gemini--native-parts-p (content)
  "Return t if CONTENT is a native Gemini parts vector (not canonical typed blocks).
Native Gemini parts have keys like `text'/`functionCall'/`thought'; canonical
blocks always carry a `type' key.  This lets convert-message pass native parts
through unchanged, preserving `thoughtSignature' fields required by thinking models."
  (and content
       (or (vectorp content) (listp content))
       (let ((first (if (vectorp content)
                        (and (> (length content) 0) (aref content 0))
                      (car content))))
         (and first (listp first) (not (assq 'type first))))))

(defun copilot-agent-gemini--convert-message (msg)
  "Convert a canonical message alist MSG to Gemini wire format.
Three cases for the message content:
  1. MSG has a `parts' key — already Gemini format (tool-result messages).
  2. MSG `content' is native Gemini parts (no canonical `type' key) — pass through
     unchanged so `thoughtSignature' fields are preserved for thinking models.
  3. Otherwise convert canonical typed blocks via `canonical->parts'."
  (let ((role    (cdr (assq 'role msg)))
        (content (cdr (assq 'content msg)))
        (parts   (cdr (assq 'parts msg))))
    `((role  . ,(copilot-agent-gemini--canonical->gemini-role role))
      (parts . ,(cond
                 (parts parts)
                 ((copilot-agent-gemini--native-parts-p content)
                  (if (vectorp content) content (vconcat content)))
                 (t (copilot-agent-gemini--canonical->parts content)))))))

;;; ---------- Request Building ----------

(defun copilot-agent-gemini--build-request (session)
  "Build the JSON request body string for SESSION."
  (let* ((model   (or (plist-get session :model)
                      copilot-agent-gemini-default-model))
         (msgs    (plist-get session :messages))
         (tools   (plist-get session :tools))
         (system  (plist-get session :system-prompt))
         (max-tok (or (plist-get session :max-tokens) 8192))
         (req     `((contents          . ,(vconcat (mapcar
                                                    #'copilot-agent-gemini--convert-message
                                                    msgs)))
                    (generationConfig  . ((maxOutputTokens . ,max-tok))))))
    (when system
      (push `(system_instruction . ((parts . ,(vector `((text . ,system))))))
            req))
    (when tools
      (push `(tools . ,(vconcat (copilot-agent-gemini--format-tools tools)))
            req))
    (json-encode req)))

;;; ---------- Response Parsing ----------

(defun copilot-agent-gemini--parse-response (raw-json)
  "Parse RAW-JSON string from Gemini API into a normalized response plist.
Returns plist with :text :tool-calls :stop-reason :raw-content :error."
  (condition-case err
      (let* ((data       (json-read-from-string raw-json))
             (error-obj  (cdr (assq 'error data)))
             (candidates (cdr (assq 'candidates data))))
        ;; API-level error
        (when error-obj
          (cl-return-from copilot-agent-gemini--parse-response
            (list :error (format "%s: %s"
                                 (cdr (assq 'code error-obj))
                                 (cdr (assq 'message error-obj))))))
        (unless candidates
          (cl-return-from copilot-agent-gemini--parse-response
            (list :error "Gemini response contained no candidates")))
        (let* ((candidate   (aref candidates 0))
               (finish      (cdr (assq 'finishReason candidate)))
               (content     (cdr (assq 'content candidate)))
               (parts       (cdr (assq 'parts content)))
               text tool-calls)
          ;; Walk parts.
          ;; Skip `thought' parts (internal reasoning for thinking models) — they
          ;; must be preserved in raw-content for history but must NOT be shown to
          ;; the user as assistant text.
          (seq-doseq (part (if (vectorp parts) parts (vconcat parts)))
            (cond
             ((cdr (assq 'thought part)) nil)   ; thinking part — skip display
             ((assq 'text part)
              (setq text (concat (or text "") (cdr (assq 'text part)))))
             ((assq 'functionCall part)
              (let* ((fc   (cdr (assq 'functionCall part)))
                     (name (cdr (assq 'name fc)))
                     (args (cdr (assq 'args fc))))
                (push (list :id    name   ; Gemini has no real call IDs; use name so
                            :name  name   ; functionResponse.name matches functionCall.name
                            :input args)
                      tool-calls)))))
          ;; Store the ORIGINAL Gemini parts as raw-content so that thoughtSignature
          ;; fields on functionCall parts are preserved when history is replayed.
          ;; convert-message detects native Gemini parts and passes them through unchanged.
          (let ((ordered-calls (nreverse tool-calls)))
            (list :text        text
                  :tool-calls  ordered-calls
                  :stop-reason finish
                  :raw-content (if (vectorp parts) parts (vconcat parts))))))
    (error (list :error (format "Parse error: %s" (error-message-string err))))))

;;; ---------- Provider Send Function ----------

(defun copilot-agent-gemini-send (session callback)
  "Send SESSION to Gemini API asynchronously.
CALLBACK is called as (RESPONSE-PLIST NIL) or (NIL ERROR-STRING).
Dispatches to API-key or CLI-OAuth mode based on `copilot-agent-gemini-auth-mode'."
  (if (eq copilot-agent-gemini-auth-mode 'cli)
      (copilot-agent-gemini--send-cli session callback)
    (copilot-agent-gemini--send-api-key session callback)))

(defun copilot-agent-gemini--send-api-key (session callback)
  "Send SESSION using an API key (query-param auth)."
  (let* ((api-key (condition-case e
                      (copilot-agent-gemini--api-key)
                    (error (funcall callback nil (error-message-string e)) nil)))
         (model   (or (plist-get session :model) copilot-agent-gemini-default-model))
         (url     (format "%s?key=%s" (copilot-agent-gemini--endpoint model) api-key))
         (body    (copilot-agent-gemini--build-request session)))
    (when api-key
      (copilot-agent-api--curl-post url '() body
       (lambda (raw http-error)
         (if http-error
             (funcall callback nil http-error)
           (let ((parsed (copilot-agent-gemini--parse-response raw)))
             (if (plist-get parsed :error)
                 (funcall callback nil (plist-get parsed :error))
               (funcall callback parsed nil)))))))))

(defun copilot-agent-gemini--send-cli (session callback)
  "Send SESSION using an OAuth Bearer token (CLI auth mode)."
  (let* ((token (condition-case e
                    (copilot-agent-gemini--cli-valid-access-token)
                  (error (funcall callback nil (error-message-string e)) nil)))
         (model (or (plist-get session :model) copilot-agent-gemini-default-model))
         (url   (copilot-agent-gemini--endpoint model))
         (body  (copilot-agent-gemini--build-request session)))
    (when token
      (copilot-agent-api--curl-post
       url
       (list (concat "Authorization: Bearer " token))
       body
       (lambda (raw http-error)
         (if http-error
             (funcall callback nil http-error)
           (let ((parsed (copilot-agent-gemini--parse-response raw)))
             (if (plist-get parsed :error)
                 (funcall callback nil (plist-get parsed :error))
               (funcall callback parsed nil)))))))))

;;; ---------- Tool Result Message Builder ----------

(defun copilot-agent-gemini-make-tool-result-message (tool-results)
  "Build a Gemini user-role message with TOOL-RESULTS.
TOOL-RESULTS is a list of plists: (:tool-use-id ID :content RESULT-STRING)."
  `((role  . "user")
    (parts . ,(vconcat
               (mapcar
                (lambda (r)
                  `((functionResponse
                     . ((name     . ,(plist-get r :tool-use-id))
                        (response . ((result . ,(plist-get r :content))))))))
                tool-results)))))

;;; ---------- Provider Registration ----------

(with-eval-after-load 'copilot-agent-api
  (copilot-agent-api-register-provider
   'gemini
   (list :display-name          "Google Gemini"
         :default-model         copilot-agent-gemini-default-model
         :default-model-fn      (lambda () copilot-agent-gemini-default-model)
         :send-fn               #'copilot-agent-gemini-send
         :make-tool-result-fn   #'copilot-agent-gemini-make-tool-result-message
         :format-tools-fn       #'copilot-agent-gemini--format-tools)))

(provide 'copilot-agent-gemini)
;;; copilot-agent-gemini.el ends here

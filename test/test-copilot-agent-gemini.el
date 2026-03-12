;;; test-copilot-agent-gemini.el --- ERT tests for the Gemini provider -*- lexical-binding: t -*-

;;; Commentary:
;; Regression tests for providers/copilot-agent-gemini.el.
;; Covers: type upscaling, tool schema conversion, message format conversion,
;; request building, response parsing (text, functionCall, error),
;; and tool result message construction.
;;
;; Run: emacs -batch -L .. -L ../providers -l test-copilot-agent-gemini.el \
;;             -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (d (list root (expand-file-name "providers" root)))
    (unless (member d load-path) (push d load-path))))

(require 'auth-source)
(require 'copilot-agent-tools)
(require 'copilot-agent-api)
(require 'copilot-agent-gemini)

;;; ---------- Fixture Data ----------

(defconst gemini-test--text-response
  "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Hello from Gemini!\"}],\
\"role\":\"model\"},\"finishReason\":\"STOP\",\"index\":0}]}"
  "Fixture: a plain text response from Gemini.")

(defconst gemini-test--function-call-response
  "{\"candidates\":[{\"content\":{\"parts\":[\
{\"text\":\"I will run ls.\"},\
{\"functionCall\":{\"name\":\"shell_command\",\"args\":{\"command\":\"ls -la\"}}}\
],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}"
  "Fixture: a response with text and a functionCall part.")

(defconst gemini-test--multi-fn-response
  "{\"candidates\":[{\"content\":{\"parts\":[\
{\"functionCall\":{\"name\":\"read_file\",\"args\":{\"path\":\"/etc/hosts\"}}},\
{\"functionCall\":{\"name\":\"shell_command\",\"args\":{\"command\":\"uptime\"}}}\
],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}"
  "Fixture: a response with two functionCall parts.")

(defconst gemini-test--error-response
  "{\"error\":{\"code\":403,\"message\":\"API key not valid.\",\"status\":\"PERMISSION_DENIED\"}}"
  "Fixture: a Gemini API error response.")

(defconst gemini-test--no-candidates-response
  "{\"promptFeedback\":{\"blockReason\":\"SAFETY\"}}"
  "Fixture: a response with no candidates (blocked by safety filter).")

;;; ---------- Type Upscaling ----------

(ert-deftest gemini/upcase-types-string ()
  "upcase-types uppercases the value of a 'type' key in an alist."
  ;; Bare strings are left alone; only the value under a `type' alist key is uppercased.
  (let ((result (copilot-agent-gemini--upcase-types '((type . "string")))))
    (should (equal (cdr (assq 'type result)) "STRING"))))

(ert-deftest gemini/upcase-types-object ()
  "upcase-types uppercases 'object' when it appears as the value of a 'type' key."
  (let ((result (copilot-agent-gemini--upcase-types '((type . "object")))))
    (should (equal (cdr (assq 'type result)) "OBJECT"))))

(ert-deftest gemini/upcase-types-in-alist ()
  "upcase-types recurses into alists and uppercases type values."
  (let* ((params '((type . "object")
                   (properties . ((command . ((type . "string")))))))
         (result (copilot-agent-gemini--upcase-types params)))
    (should (equal (cdr (assq 'type result)) "OBJECT"))
    (let ((cmd-props (cdr (assq 'command (cdr (assq 'properties result))))))
      (should (equal (cdr (assq 'type cmd-props)) "STRING")))))

(ert-deftest gemini/upcase-types-non-type-keys-unchanged ()
  "upcase-types does not alter non-type keys like description."
  (let* ((params '((description . "a lowercase description")
                   (type . "string")))
         (result (copilot-agent-gemini--upcase-types params)))
    (should (equal (cdr (assq 'description result)) "a lowercase description"))))

;;; ---------- Tool Schema Formatting ----------

(ert-deftest gemini/format-tools-wraps-in-function-declarations ()
  "format-tools returns a list with a function_declarations key."
  (let* ((schema (list (list (cons 'name "shell_command")
                             (cons 'description "Run a shell command")
                             (cons 'parameters '((type . "object"))))))
         (result (copilot-agent-gemini--format-tools schema)))
    (should (= (length result) 1))
    (let ((decls (cdr (assq 'function_declarations (car result)))))
      (should (> (length decls) 0)))))

(ert-deftest gemini/format-tools-type-is-uppercase ()
  "format-tools produces uppercase type names in function declarations."
  (let* ((schema (list (list (cons 'name "read_file")
                             (cons 'description "Read a file")
                             (cons 'parameters
                                   '((type . "object")
                                     (properties
                                      . ((path . ((type . "string"))))))))))
         (result (copilot-agent-gemini--format-tools schema))
         (decl   (aref (cdr (assq 'function_declarations (car result))) 0))
         (params (cdr (assq 'parameters decl))))
    (should (equal (cdr (assq 'type params)) "OBJECT"))))

(ert-deftest gemini/format-tools-full-schema ()
  "format-tools handles the full copilot-agent-tools-schema without error."
  (let* ((result (copilot-agent-gemini--format-tools copilot-agent-tools-schema))
         (decls  (cdr (assq 'function_declarations (car result)))))
    (should (= (length decls) (length copilot-agent-tools-schema)))))

;;; ---------- Role Conversion ----------

(ert-deftest gemini/role-user-stays-user ()
  "canonical->gemini-role maps 'user' to 'user'."
  (should (equal (copilot-agent-gemini--canonical->gemini-role "user") "user")))

(ert-deftest gemini/role-assistant-becomes-model ()
  "canonical->gemini-role maps 'assistant' to 'model'."
  (should (equal (copilot-agent-gemini--canonical->gemini-role "assistant") "model")))

;;; ---------- Content / Parts Conversion ----------

(ert-deftest gemini/parts-from-string ()
  "canonical->parts wraps a plain string in a text part."
  (let ((parts (copilot-agent-gemini--canonical->parts "Hello")))
    (should (vectorp parts))
    (should (= (length parts) 1))
    (should (equal (cdr (assq 'text (aref parts 0))) "Hello"))))

(ert-deftest gemini/parts-from-text-block-vector ()
  "canonical->parts extracts text from a text-type content block."
  (let* ((blocks (vector '((type . "text") (text . "Hi there"))))
         (parts  (copilot-agent-gemini--canonical->parts blocks)))
    (should (vectorp parts))
    (should (equal (cdr (assq 'text (aref parts 0))) "Hi there"))))

(ert-deftest gemini/parts-from-tool-use-block ()
  "canonical->parts converts a tool_use block to a functionCall part."
  (let* ((blocks (vector '((type . "tool_use")
                            (id   . "tu_1")
                            (name . "shell_command")
                            (input . ((command . "ls"))))))
         (parts  (copilot-agent-gemini--canonical->parts blocks))
         (fc     (cdr (assq 'functionCall (aref parts 0)))))
    (should fc)
    (should (equal (cdr (assq 'name fc)) "shell_command"))))

(ert-deftest gemini/parts-from-tool-result-block ()
  "canonical->parts converts a tool_result block to a functionResponse part."
  (let* ((blocks (vector '((type . "tool_result")
                            (tool_use_id . "tu_1")
                            (content . "output text"))))
         (parts  (copilot-agent-gemini--canonical->parts blocks))
         (fr     (cdr (assq 'functionResponse (aref parts 0)))))
    (should fr)
    (should (equal (cdr (assq 'result (cdr (assq 'response fr)))) "output text"))))

;;; ---------- Message Conversion ----------

(ert-deftest gemini/convert-message-user-text ()
  "convert-message produces role=user with a text part for a simple user message."
  (let* ((msg    '((role . "user") (content . "Hello")))
         (result (copilot-agent-gemini--convert-message msg)))
    (should (equal (cdr (assq 'role result)) "user"))
    (let ((parts (cdr (assq 'parts result))))
      (should (vectorp parts))
      (should (equal (cdr (assq 'text (aref parts 0))) "Hello")))))

(ert-deftest gemini/convert-message-assistant-becomes-model ()
  "convert-message sets role to 'model' for assistant messages."
  (let* ((msg    '((role . "assistant") (content . "Hi")))
         (result (copilot-agent-gemini--convert-message msg)))
    (should (equal (cdr (assq 'role result)) "model"))))

;;; ---------- Request Building ----------

(ert-deftest gemini/build-request-contains-contents ()
  "build-request JSON includes a 'contents' array."
  (let* ((session (list :provider 'gemini
                        :model "gemini-2.0-flash"
                        :messages '(((role . "user") (content . "Hello")))
                        :tools nil
                        :system-prompt nil
                        :max-tokens 1024))
         (json    (copilot-agent-gemini--build-request session))
         (parsed  (json-read-from-string json)))
    (should (assq 'contents parsed))
    (should (> (length (cdr (assq 'contents parsed))) 0))))

(ert-deftest gemini/build-request-system-instruction ()
  "build-request includes system_instruction when system-prompt is provided."
  (let* ((session (list :provider 'gemini
                        :model "gemini-2.0-flash"
                        :messages '()
                        :tools nil
                        :system-prompt "You are helpful."
                        :max-tokens 512))
         (json   (copilot-agent-gemini--build-request session))
         (parsed (json-read-from-string json)))
    (should (assq 'system_instruction parsed))))

(ert-deftest gemini/build-request-no-system-omits-key ()
  "build-request omits system_instruction when system-prompt is nil."
  (let* ((session (list :provider 'gemini
                        :model "gemini-2.0-flash"
                        :messages '()
                        :tools nil
                        :system-prompt nil
                        :max-tokens 512))
         (json   (copilot-agent-gemini--build-request session))
         (parsed (json-read-from-string json)))
    (should-not (assq 'system_instruction parsed))))

(ert-deftest gemini/build-request-includes-tools ()
  "build-request includes tools array when tools are provided."
  (let* ((session (list :provider 'gemini
                        :model "gemini-2.0-flash"
                        :messages '()
                        :tools copilot-agent-tools-schema
                        :system-prompt nil
                        :max-tokens 512))
         (json   (copilot-agent-gemini--build-request session))
         (parsed (json-read-from-string json)))
    (should (assq 'tools parsed))))

;;; ---------- Response Parsing ----------

(ert-deftest gemini/parse-text-response ()
  "parse-response extracts text from a plain text response."
  (let ((result (copilot-agent-gemini--parse-response gemini-test--text-response)))
    (should (equal (plist-get result :text) "Hello from Gemini!"))
    (should (null (plist-get result :tool-calls)))
    (should-not (plist-get result :error))))

(ert-deftest gemini/parse-function-call-response ()
  "parse-response extracts tool call from a functionCall response."
  (let* ((result (copilot-agent-gemini--parse-response
                  gemini-test--function-call-response))
         (calls  (plist-get result :tool-calls)))
    (should (equal (plist-get result :text) "I will run ls."))
    (should (= (length calls) 1))
    (let ((call (car calls)))
      (should (equal (plist-get call :name) "shell_command"))
      (should (equal (cdr (assq 'command (plist-get call :input))) "ls -la")))))

(ert-deftest gemini/parse-multiple-function-calls ()
  "parse-response extracts multiple functionCall parts in order."
  (let* ((result (copilot-agent-gemini--parse-response
                  gemini-test--multi-fn-response))
         (calls  (plist-get result :tool-calls)))
    (should (= (length calls) 2))
    (should (equal (plist-get (nth 0 calls) :name) "read_file"))
    (should (equal (plist-get (nth 1 calls) :name) "shell_command"))))

(ert-deftest gemini/parse-error-response ()
  "parse-response returns :error for an API error payload."
  (let ((result (copilot-agent-gemini--parse-response gemini-test--error-response)))
    (should (plist-get result :error))
    (should (string-match-p "API key not valid" (plist-get result :error)))))

(ert-deftest gemini/parse-no-candidates ()
  "parse-response returns :error when candidates array is absent."
  (let ((result (copilot-agent-gemini--parse-response
                 gemini-test--no-candidates-response)))
    (should (plist-get result :error))))

(ert-deftest gemini/parse-malformed-json ()
  "parse-response returns :error for malformed JSON."
  (let ((result (copilot-agent-gemini--parse-response "{bad json")))
    (should (plist-get result :error))))

;;; ---------- Tool Result Message ----------

(ert-deftest gemini/make-tool-result-message-structure ()
  "make-tool-result-message builds a user-role message with functionResponse parts."
  (let* ((results (list (list :tool-use-id "gemini-tool-shell_command"
                              :content "file1\nfile2")))
         (msg     (copilot-agent-gemini-make-tool-result-message results)))
    (should (equal (cdr (assq 'role msg)) "user"))
    (let* ((parts (cdr (assq 'parts msg)))
           (fr    (cdr (assq 'functionResponse (aref parts 0)))))
      (should fr)
      (should (equal (cdr (assq 'name fr)) "gemini-tool-shell_command"))
      (should (equal (cdr (assq 'result (cdr (assq 'response fr))))
                     "file1\nfile2")))))

(ert-deftest gemini/make-tool-result-message-multiple ()
  "make-tool-result-message handles multiple results."
  (let* ((results (list (list :tool-use-id "id1" :content "r1")
                        (list :tool-use-id "id2" :content "r2")))
         (msg   (copilot-agent-gemini-make-tool-result-message results))
         (parts (cdr (assq 'parts msg))))
    (should (= (length parts) 2))))

;;; ---------- Endpoint URL ----------

(ert-deftest gemini/endpoint-contains-model ()
  "gemini--endpoint returns a URL containing the model name."
  (let ((url (copilot-agent-gemini--endpoint "gemini-2.0-flash")))
    (should (string-match-p "gemini-2.0-flash" url))
    (should (string-match-p "generateContent" url))))

;;; ---------- CLI Auth — PKCE ----------

(ert-deftest gemini-cli/pkce-verifier-length ()
  "PKCE verifier is a non-empty base64url string."
  (let ((pkce (copilot-agent-gemini--pkce)))
    (should (stringp (car pkce)))
    (should (> (length (car pkce)) 0))))

(ert-deftest gemini-cli/pkce-challenge-no-padding ()
  "PKCE S256 challenge must not contain base64 padding '='."
  (let ((challenge (cdr (copilot-agent-gemini--pkce))))
    (should-not (string-match-p "=" challenge))))

(ert-deftest gemini-cli/pkce-challenge-no-plus-slash ()
  "PKCE challenge uses base64url alphabet (no '+' or '/')."
  (let ((challenge (cdr (copilot-agent-gemini--pkce))))
    (should-not (string-match-p "[+/]" challenge))))

(ert-deftest gemini-cli/pkce-pairs-differ ()
  "Each PKCE call produces a fresh unique pair."
  (let ((a (copilot-agent-gemini--pkce))
        (b (copilot-agent-gemini--pkce)))
    (should-not (equal (car a) (car b)))))

;;; ---------- CLI Auth — Credential Storage ----------

(ert-deftest gemini-cli/save-and-load-creds ()
  "save-creds writes a JSON file; load-creds reads it back."
  (let* ((tmp-dir  (make-temp-file "gemini-test-creds" t))
         (tmp-file (expand-file-name "gemini_oauth_creds.json" tmp-dir))
         (copilot-agent-gemini--cli-creds-file tmp-file))
    (unwind-protect
        (progn
          (copilot-agent-gemini--cli-save-creds "tok123" "ref456" 9999999)
          (let ((creds (copilot-agent-gemini--cli-load-creds)))
            (should (equal (cdr (assq 'access_token  creds)) "tok123"))
            (should (equal (cdr (assq 'refresh_token creds)) "ref456"))
            (should (equal (cdr (assq 'expires       creds)) 9999999))))
      (delete-directory tmp-dir t))))

(ert-deftest gemini-cli/load-creds-returns-nil-when-missing ()
  "load-creds returns nil when the credentials file does not exist."
  (let ((copilot-agent-gemini--cli-creds-file "/nonexistent/path/creds.json"))
    (should (null (copilot-agent-gemini--cli-load-creds)))))

(ert-deftest gemini-cli/creds-file-mode-600 ()
  "save-creds sets file permissions to 0600."
  (let* ((tmp-dir  (make-temp-file "gemini-test-mode" t))
         (tmp-file (expand-file-name "gemini_oauth_creds.json" tmp-dir))
         (copilot-agent-gemini--cli-creds-file tmp-file))
    (unwind-protect
        (progn
          (copilot-agent-gemini--cli-save-creds "a" "b" 1)
          (should (= (logand (file-modes tmp-file) #o777) #o600)))
      (delete-directory tmp-dir t))))

;;; ---------- CLI Auth — Valid Token ----------

(ert-deftest gemini-cli/valid-token-returns-when-fresh ()
  "valid-access-token returns access token when not expired."
  (let* ((tmp-dir  (make-temp-file "gemini-test-tok" t))
         (tmp-file (expand-file-name "gemini_oauth_creds.json" tmp-dir))
         (copilot-agent-gemini--cli-creds-file tmp-file)
         ;; expires 1 hour from now
         (future-ms (+ (truncate (* (float-time) 1000)) 3600000)))
    (unwind-protect
        (progn
          (copilot-agent-gemini--cli-save-creds "access-ok" "ref" future-ms)
          (should (equal (copilot-agent-gemini--cli-valid-access-token) "access-ok")))
      (delete-directory tmp-dir t))))

(ert-deftest gemini-cli/valid-token-errors-when-no-creds ()
  "valid-access-token signals an error when no credentials exist."
  (let ((copilot-agent-gemini--cli-creds-file "/no/such/file.json"))
    (should-error (copilot-agent-gemini--cli-valid-access-token))))

;;; ---------- CLI Auth — Client Extraction ----------

(ert-deftest gemini-cli/extract-client-id-from-oauth2-js ()
  "cli-oauth-creds extracts client_id from a synthetic oauth2.js file."
  (let* ((tmp-dir  (make-temp-file "gemini-test-oauth2" t))
         (fake-js  (expand-file-name "oauth2.js" tmp-dir)))
    (unwind-protect
        (progn
          (with-temp-file fake-js
            (insert "const OAUTH_CLIENT_ID = '123456789012-abcdefghij.apps.googleusercontent.com';\n")
            (insert "const OAUTH_CLIENT_SECRET = 'GOCSPX-FakeSecretValue123';\n"))
          ;; Stub find-cli-oauth2-js to return our fake file
          (cl-letf (((symbol-function 'copilot-agent-gemini--find-cli-oauth2-js)
                     (lambda () fake-js)))
            (let ((creds (copilot-agent-gemini--cli-oauth-creds)))
              (should (string-match-p "googleusercontent\\.com" (car creds)))
              (should (string-match-p "GOCSPX-" (cdr creds))))))
      (delete-directory tmp-dir t))))

(ert-deftest gemini-cli/extract-errors-when-cli-not-found ()
  "cli-oauth-creds signals an error when find-cli-oauth2-js returns nil."
  (cl-letf (((symbol-function 'copilot-agent-gemini--find-cli-oauth2-js)
             (lambda () nil)))
    (should-error (copilot-agent-gemini--cli-oauth-creds))))

;;; ---------- CLI Auth — Callback Server ----------

(ert-deftest gemini-cli/start-callback-server-returns-port ()
  "start-callback-server starts a TCP server and returns a positive port number."
  (let ((port (copilot-agent-gemini--start-callback-server)))
    (unwind-protect
        (progn
          (should (integerp port))
          (should (> port 0)))
      (when (process-live-p copilot-agent-gemini--oauth-server)
        (delete-process copilot-agent-gemini--oauth-server))
      (setq copilot-agent-gemini--oauth-server nil
            copilot-agent-gemini--oauth-code   nil))))

(ert-deftest gemini-cli/callback-server-no-nowait-incompatibility ()
  "start-callback-server must not pass :nowait to make-network-process (incompatible with :server)."
  ;; If :nowait were present the call would signal an error; verify it succeeds.
  (let (err)
    (condition-case e
        (let ((_port (copilot-agent-gemini--start-callback-server)))
          (when (process-live-p copilot-agent-gemini--oauth-server)
            (delete-process copilot-agent-gemini--oauth-server))
          (setq copilot-agent-gemini--oauth-server nil
                copilot-agent-gemini--oauth-code   nil))
      (error (setq err e)))
    (should-not err)))

(ert-deftest gemini-cli/oauth-filter-extracts-code ()
  "oauth-filter sets copilot-agent-gemini--oauth-code from an HTTP GET request string."
  (setq copilot-agent-gemini--oauth-code nil)
  (cl-letf (((symbol-function 'process-send-string) #'ignore)
            ((symbol-function 'run-at-time)          #'ignore))
    (copilot-agent-gemini--oauth-filter nil
     "GET /oauth2callback?code=TEST_CODE_ABC&state=xyz HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"))
  (should (equal copilot-agent-gemini--oauth-code "TEST_CODE_ABC"))
  (setq copilot-agent-gemini--oauth-code nil))

;;; ---------- CLI Auth — Code Assist Request/Response ----------

(ert-deftest gemini-cli/save-and-load-project-id ()
  "save-creds persists project ID; load-creds reads it back."
  (let* ((tmp-dir  (make-temp-file "gemini-test-proj" t))
         (tmp-file (expand-file-name "gemini_oauth_creds.json" tmp-dir))
         (copilot-agent-gemini--cli-creds-file tmp-file))
    (unwind-protect
        (progn
          (copilot-agent-gemini--cli-save-creds "tok" "ref" 9999 "my-project-123")
          (let ((creds (copilot-agent-gemini--cli-load-creds)))
            (should (equal (cdr (assq 'project creds)) "my-project-123"))))
      (delete-directory tmp-dir t))))

(ert-deftest gemini-cli/send-cli-fetches-project-on-demand ()
  "send-cli fetches and caches the project ID when it is missing from saved creds."
  (let* ((tmp-dir  (make-temp-file "gemini-test-noproj" t))
         (tmp-file (expand-file-name "gemini_oauth_creds.json" tmp-dir))
         (copilot-agent-gemini--cli-creds-file tmp-file)
         fetched)
    (unwind-protect
        (progn
          ;; Save creds WITHOUT a project field (simulates old login)
          (copilot-agent-gemini--cli-save-creds "tok" "ref" 99999999999)
          (cl-letf (((symbol-function 'copilot-agent-gemini--cli-valid-access-token)
                     (lambda () "tok"))
                    ((symbol-function 'copilot-agent-gemini--cli-fetch-project)
                     (lambda (_tok) (setq fetched t) "fetched-project-id"))
                    ((symbol-function 'copilot-agent-api--curl-post)
                     (lambda (_url _hdrs _body _cb) nil)))  ; stop after project fetch
            (copilot-agent-gemini--send-cli
             (list :model "gemini-2.0-flash" :messages '() :tools nil
                   :system-prompt nil :max-tokens 512)
             #'ignore))
          (should fetched)
          ;; Project should now be persisted in the credentials file
          (should (equal (cdr (assq 'project (copilot-agent-gemini--cli-load-creds)))
                         "fetched-project-id")))
      (delete-directory tmp-dir t))))

;;; ---------- CLI Auth — Auth Mode Dispatch ----------

(ert-deftest gemini-cli/send-dispatches-to-api-key-mode ()
  "gemini-send calls --send-api-key when auth-mode is api-key."
  (let ((copilot-agent-gemini-auth-mode 'api-key)
        called)
    (cl-letf (((symbol-function 'copilot-agent-gemini--send-api-key)
               (lambda (_s _cb) (setq called t)))
              ((symbol-function 'copilot-agent-gemini--send-cli)
               (lambda (_s _cb) (error "should not be called"))))
      (copilot-agent-gemini-send '(:model "x") #'ignore)
      (should called))))

(ert-deftest gemini-cli/send-dispatches-to-cli-mode ()
  "gemini-send calls --send-cli when auth-mode is cli."
  (let ((copilot-agent-gemini-auth-mode 'cli)
        called)
    (cl-letf (((symbol-function 'copilot-agent-gemini--send-cli)
               (lambda (_s _cb) (setq called t)))
              ((symbol-function 'copilot-agent-gemini--send-api-key)
               (lambda (_s _cb) (error "should not be called"))))
      (copilot-agent-gemini-send '(:model "x") #'ignore)
      (should called))))

(provide 'test-copilot-agent-gemini)
;;; test-copilot-agent-gemini.el ends here

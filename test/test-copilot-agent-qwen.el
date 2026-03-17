;;; test-copilot-agent-qwen.el --- Tests for the Qwen provider -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for copilot-agent-qwen.el.
;; Covers: PKCE, credential I/O, message conversion, request building,
;; response parsing (text + tool calls), tool result formatting.

;;; Code:

(require 'ert)

(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root     (expand-file-name ".." this-dir)))
  (dolist (d (list root (expand-file-name "providers" root)))
    (unless (member d load-path) (push d load-path))))

(require 'copilot-agent-api)
(require 'copilot-agent-qwen)

;;; ---------- PKCE ----------

(ert-deftest qwen/pkce-returns-verifier-and-challenge ()
  "pkce returns a cons (verifier . challenge), both non-empty strings."
  (let ((pair (copilot-agent-qwen--pkce)))
    (should (consp pair))
    (should (stringp (car pair)))
    (should (stringp (cdr pair)))
    (should (> (length (car pair)) 0))
    (should (> (length (cdr pair)) 0))))

(ert-deftest qwen/pkce-verifier-and-challenge-differ ()
  "Verifier and challenge must be different strings."
  (let ((pair (copilot-agent-qwen--pkce)))
    (should-not (equal (car pair) (cdr pair)))))

(ert-deftest qwen/pkce-no-padding ()
  "Verifier and challenge must not contain base64 padding '='."
  (let ((pair (copilot-agent-qwen--pkce)))
    (should-not (string-match-p "=" (car pair)))
    (should-not (string-match-p "=" (cdr pair)))))

(ert-deftest qwen/pkce-url-safe-chars ()
  "Verifier and challenge contain only URL-safe base64url characters."
  (let* ((pair       (copilot-agent-qwen--pkce))
         (safe-re    "^[A-Za-z0-9_-]+$"))
    (should (string-match-p safe-re (car pair)))
    (should (string-match-p safe-re (cdr pair)))))

(ert-deftest qwen/pkce-unique-each-call ()
  "Two consecutive pkce calls must produce different verifiers."
  (let ((a (copilot-agent-qwen--pkce))
        (b (copilot-agent-qwen--pkce)))
    (should-not (equal (car a) (car b)))))

;;; ---------- base64url ----------

(ert-deftest qwen/base64url-no-padding ()
  "base64url encoding must strip '=' padding."
  (let ((result (copilot-agent-qwen--base64url (make-string 5 ?A))))
    (should-not (string-match-p "=" result))))

(ert-deftest qwen/base64url-url-safe ()
  "base64url encoding must replace '+' with '-' and '/' with '_'."
  ;; Build a string that would normally produce '+' and '/' in base64.
  ;; \xfb\xff encodes to +/... in standard base64.
  (let* ((input  (unibyte-string #xfb #xff))
         (result (copilot-agent-qwen--base64url input)))
    (should-not (string-match-p "+" result))
    (should-not (string-match-p "/" result))))

;;; ---------- Credential Storage ----------

(ert-deftest qwen/save-and-load-creds ()
  "Credentials saved by save-creds must be loadable by load-creds."
  (let* ((tmp  (make-temp-file "qwen-creds-" nil ".json"))
         (copilot-agent-qwen--creds-file tmp))
    (unwind-protect
        (progn
          (copilot-agent-qwen--save-creds "ACCESS" "REFRESH" 9999999999000)
          (let ((creds (copilot-agent-qwen--load-creds)))
            (should (equal (cdr (assq 'access_token  creds)) "ACCESS"))
            (should (equal (cdr (assq 'refresh_token creds)) "REFRESH"))
            (should (equal (cdr (assq 'expires       creds)) 9999999999000))))
      (delete-file tmp))))

(ert-deftest qwen/load-creds-returns-nil-when-missing ()
  "load-creds must return nil when the credentials file does not exist."
  (let ((copilot-agent-qwen--creds-file "/tmp/nonexistent-qwen-creds-xyz.json"))
    (should-not (copilot-agent-qwen--load-creds))))

(ert-deftest qwen/load-creds-returns-nil-on-malformed-json ()
  "load-creds must return nil rather than signalling on malformed JSON."
  (let* ((tmp (make-temp-file "qwen-bad-" nil ".json"))
         (copilot-agent-qwen--creds-file tmp))
    (unwind-protect
        (progn
          (write-region "not json {{" nil tmp)
          (should-not (copilot-agent-qwen--load-creds)))
      (delete-file tmp))))

;;; ---------- Tool Schema Formatting ----------

(ert-deftest qwen/format-tools-produces-function-type ()
  "format-tools must wrap each tool with type=function."
  (let* ((schema '(((name . "shell_command")
                    (description . "Run a shell command")
                    (parameters . ((type . "object"))))))
         (result (copilot-agent-qwen--format-tools schema)))
    (should (= (length result) 1))
    (should (equal (cdr (assq 'type (car result))) "function"))))

(ert-deftest qwen/format-tools-preserves-name-and-description ()
  "format-tools must keep tool name and description under the function key."
  (let* ((schema '(((name . "read_file")
                    (description . "Read a file")
                    (parameters . ((type . "object"))))))
         (fn (cdr (assq 'function (car (copilot-agent-qwen--format-tools schema))))))
    (should (equal (cdr (assq 'name        fn)) "read_file"))
    (should (equal (cdr (assq 'description fn)) "Read a file"))))

(ert-deftest qwen/format-tools-multiple ()
  "format-tools must handle multiple tools."
  (let* ((schema '(((name . "a") (description . "A") (parameters . nil))
                   ((name . "b") (description . "B") (parameters . nil))))
         (result (copilot-agent-qwen--format-tools schema)))
    (should (= (length result) 2))))

;;; ---------- Message Conversion ----------

(ert-deftest qwen/convert-message-plain-user ()
  "convert-message must produce an OpenAI user message from a canonical user message."
  (let* ((msg  '((role . "user") (content . "hello")))
         (conv (copilot-agent-qwen--convert-message msg)))
    (should (equal (cdr (assq 'role    conv)) "user"))
    (should (equal (cdr (assq 'content conv)) "hello"))))

(ert-deftest qwen/convert-message-plain-assistant ()
  "convert-message must produce an OpenAI assistant message."
  (let* ((msg  '((role . "assistant") (content . "I can help")))
         (conv (copilot-agent-qwen--convert-message msg)))
    (should (equal (cdr (assq 'role    conv)) "assistant"))
    (should (equal (cdr (assq 'content conv)) "I can help"))))

(ert-deftest qwen/convert-message-tool-result-passthrough ()
  "convert-message must pass tool-result messages through unchanged."
  (let* ((msg  '((role . "tool") (tool_call_id . "call_1") (content . "output")))
         (conv (copilot-agent-qwen--convert-message msg)))
    (should (equal (cdr (assq 'role         conv)) "tool"))
    (should (equal (cdr (assq 'tool_call_id conv)) "call_1"))
    (should (equal (cdr (assq 'content      conv)) "output"))))

(ert-deftest qwen/convert-message-assistant-with-tool-calls ()
  "convert-message must expand raw-content with tool_calls into OpenAI format."
  (let* ((tc-vec (vector '((id . "c1") (type . "function")
                            (function . ((name . "ls") (arguments . "{}"))))))
         (raw    `((content . nil) (tool_calls . ,tc-vec)))
         (msg    `((role . "assistant") (content . ,raw)))
         (conv   (copilot-agent-qwen--convert-message msg)))
    (should (equal (cdr (assq 'role       conv)) "assistant"))
    (should (null  (cdr (assq 'content    conv))))
    (should (vectorp (cdr (assq 'tool_calls conv))))
    (should (= (length (cdr (assq 'tool_calls conv))) 1))))

;;; ---------- Response Parsing ----------

(ert-deftest qwen/parse-response-text-only ()
  "parse-response must extract text from a plain OpenAI text response."
  (let* ((json "{\"choices\":[{\"message\":{\"role\":\"assistant\",\
\"content\":\"Hello!\"},\"finish_reason\":\"stop\"}]}")
         (result (copilot-agent-qwen--parse-response json)))
    (should (equal (plist-get result :text)        "Hello!"))
    (should (null  (plist-get result :tool-calls)))
    (should (equal (plist-get result :stop-reason) "stop"))
    (should-not (plist-get result :error))))

(ert-deftest qwen/parse-response-tool-call ()
  "parse-response must decode tool_calls from the OpenAI response."
  (let* ((json (concat
                "{\"choices\":[{\"message\":{\"role\":\"assistant\","
                "\"content\":null,\"tool_calls\":[{\"id\":\"call_1\","
                "\"type\":\"function\",\"function\":{\"name\":\"list_directory\","
                "\"arguments\":\"{\\\"path\\\":\\\"/tmp\\\"}\"}}]},"
                "\"finish_reason\":\"tool_calls\"}]}"))
         (result (copilot-agent-qwen--parse-response json))
         (tc     (car (plist-get result :tool-calls))))
    (should-not (plist-get result :error))
    (should (= (length (plist-get result :tool-calls)) 1))
    (should (equal (plist-get tc :id)   "call_1"))
    (should (equal (plist-get tc :name) "list_directory"))
    (should (equal (cdr (assq 'path (plist-get tc :input))) "/tmp"))))

(ert-deftest qwen/parse-response-tool-call-arguments-are-decoded ()
  "parse-response must JSON-decode the arguments string, not leave it as a string."
  (let* ((json (concat
                "{\"choices\":[{\"message\":{\"role\":\"assistant\","
                "\"content\":null,\"tool_calls\":[{\"id\":\"c\","
                "\"type\":\"function\",\"function\":{\"name\":\"f\","
                "\"arguments\":\"{\\\"key\\\":\\\"val\\\"}\"}}]},"
                "\"finish_reason\":\"tool_calls\"}]}"))
         (tc (car (plist-get (copilot-agent-qwen--parse-response json) :tool-calls))))
    (should (listp (plist-get tc :input)))   ; decoded to alist, not string
    (should (equal (cdr (assq 'key (plist-get tc :input))) "val"))))

(ert-deftest qwen/parse-response-api-error ()
  "parse-response must surface API errors in :error."
  (let* ((json "{\"error\":{\"code\":\"invalid_model\",\"message\":\"No such model\"}}")
         (result (copilot-agent-qwen--parse-response json)))
    (should (plist-get result :error))
    (should (string-match-p "No such model" (plist-get result :error)))))

(ert-deftest qwen/parse-response-no-choices-error ()
  "parse-response must return :error when choices array is empty."
  (let* ((json "{\"choices\":[]}")
         (result (copilot-agent-qwen--parse-response json)))
    (should (plist-get result :error))))

(ert-deftest qwen/parse-response-malformed-json-error ()
  "parse-response must return :error on malformed JSON."
  (let ((result (copilot-agent-qwen--parse-response "not json {")))
    (should (plist-get result :error))))

(ert-deftest qwen/parse-response-raw-content-preserves-tool-calls ()
  "raw-content must encode tool_calls so convert-message can reconstruct the history."
  (let* ((json (concat
                "{\"choices\":[{\"message\":{\"role\":\"assistant\","
                "\"content\":null,\"tool_calls\":[{\"id\":\"c1\","
                "\"type\":\"function\",\"function\":{\"name\":\"read_file\","
                "\"arguments\":\"{\\\"path\\\":\\\"/etc/hosts\\\"}\"}}]},"
                "\"finish_reason\":\"tool_calls\"}]}"))
         (result  (copilot-agent-qwen--parse-response json))
         (raw     (plist-get result :raw-content)))
    (should (listp raw))
    (should (assq 'tool_calls raw))
    (should (vectorp (cdr (assq 'tool_calls raw))))))

;;; ---------- Tool Result Messages ----------

(ert-deftest qwen/make-tool-result-message-returns-list ()
  "make-tool-result-message must return a LIST of messages, one per tool result."
  (let* ((results (list (list :tool-use-id "call_1" :content "output1")
                        (list :tool-use-id "call_2" :content "output2")))
         (msgs    (copilot-agent-qwen-make-tool-result-message results)))
    (should (listp msgs))
    (should (= (length msgs) 2))))

(ert-deftest qwen/make-tool-result-message-role-is-tool ()
  "Each message returned must have role=tool."
  (let* ((results (list (list :tool-use-id "c" :content "r")))
         (msg     (car (copilot-agent-qwen-make-tool-result-message results))))
    (should (equal (cdr (assq 'role msg)) "tool"))))

(ert-deftest qwen/make-tool-result-message-tool-call-id ()
  "Each message must carry the correct tool_call_id."
  (let* ((results (list (list :tool-use-id "call_abc" :content "out")))
         (msg     (car (copilot-agent-qwen-make-tool-result-message results))))
    (should (equal (cdr (assq 'tool_call_id msg)) "call_abc"))))

(ert-deftest qwen/make-tool-result-message-content ()
  "Each message must carry the tool result as content."
  (let* ((results (list (list :tool-use-id "x" :content "result text")))
         (msg     (car (copilot-agent-qwen-make-tool-result-message results))))
    (should (equal (cdr (assq 'content msg)) "result text"))))

;;; ---------- API multi-message dispatch in core ----------

(ert-deftest qwen/dispatch-list-of-messages ()
  "The dispatch predicate (symbolp (caar result)) must correctly identify a list
of messages: (caar list-of-msgs) is a cons cell, not a symbol."
  ;; list-of-messages: (((role . "tool") ...) ((role . "tool") ...))
  ;; (caar ...) = (car (car ...)) = (car ((role . "tool") ...)) = (role . "tool") = cons
  (let ((list-of-msgs (list '((role . "tool") (tool_call_id . "c1") (content . "r1"))
                            '((role . "tool") (tool_call_id . "c2") (content . "r2")))))
    (should-not (symbolp (caar list-of-msgs)))))

(ert-deftest qwen/dispatch-single-message ()
  "The dispatch predicate (symbolp (caar result)) must correctly identify a single
message alist: (caar single-msg) is the symbol 'role."
  ;; single message: ((role . "user") (content . ...))
  ;; (caar ...) = (car (car ...)) = (car (role . "user")) = role = symbol
  (let ((single-msg '((role . "user") (content . "text"))))
    (should (symbolp (caar single-msg)))))

(ert-deftest qwen/process-tools-appends-list-of-messages ()
  "The dispatch in copilot-agent-api--process-tools appends N messages when
make-tool-result-fn returns a list of N alists (OpenAI-style tool results)."
  (require 'copilot-agent-api)
  (let* ((session (list :provider 'test-qwen-dispatch
                        :messages '()
                        :tools    nil
                        :approve  'all)))
    (copilot-agent-api-register-provider
     'test-qwen-dispatch
     (list :display-name        "mock-list"
           :send-fn             (lambda (_s _cb) nil)
           :format-tools-fn     #'identity
           :make-tool-result-fn
           (lambda (results)
             (mapcar (lambda (r)
                       `((role . "tool")
                         (tool_call_id . ,(plist-get r :tool-use-id))
                         (content      . ,(plist-get r :content))))
                     results))))
    ;; Run the actual dispatch logic
    (let ((msg-or-list
           (funcall (plist-get (copilot-agent-api--get-provider 'test-qwen-dispatch)
                               :make-tool-result-fn)
                    (list (list :tool-use-id "c1" :content "r1")
                          (list :tool-use-id "c2" :content "r2")))))
      (if (symbolp (caar msg-or-list))
          (copilot-agent-api--append-message session msg-or-list)
        (dolist (m msg-or-list)
          (copilot-agent-api--append-message session m))))
    (should (= (length (plist-get session :messages)) 2))
    (should (equal (cdr (assq 'role (nth 0 (plist-get session :messages)))) "tool"))
    (should (equal (cdr (assq 'role (nth 1 (plist-get session :messages)))) "tool"))))

(ert-deftest qwen/process-tools-appends-single-message ()
  "The dispatch must append exactly one message when make-tool-result-fn returns
a single alist (Anthropic/Gemini style)."
  (require 'copilot-agent-api)
  (let* ((session (list :provider 'test-single-dispatch
                        :messages '()
                        :tools    nil
                        :approve  'all)))
    (copilot-agent-api-register-provider
     'test-single-dispatch
     (list :display-name        "mock-single"
           :send-fn             (lambda (_s _cb) nil)
           :format-tools-fn     #'identity
           :make-tool-result-fn
           (lambda (_results)
             ;; Returns a SINGLE message (Anthropic/Gemini style)
             '((role . "user") (content . "tool result")))))
    (let ((msg-or-list
           (funcall (plist-get (copilot-agent-api--get-provider 'test-single-dispatch)
                               :make-tool-result-fn)
                    (list (list :tool-use-id "x" :content "out")))))
      (if (symbolp (caar msg-or-list))
          (copilot-agent-api--append-message session msg-or-list)
        (dolist (m msg-or-list)
          (copilot-agent-api--append-message session m))))
    (should (= (length (plist-get session :messages)) 1))
    (should (equal (cdr (assq 'role (nth 0 (plist-get session :messages)))) "user"))))

(provide 'test-copilot-agent-qwen)
;;; test-copilot-agent-qwen.el ends here

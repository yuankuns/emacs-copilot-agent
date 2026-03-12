;;; test-copilot-agent-anthropic.el --- ERT tests for the Anthropic provider -*- lexical-binding: t -*-

;;; Commentary:
;; Regression tests for providers/copilot-agent-anthropic.el.
;; Covers: tool schema formatting, request building, response parsing
;; (text, tool_use, errors), and tool result message construction.
;; API calls are not made; all tests use fixture data.
;;
;; Run: emacs -batch -L .. -L ../providers -l test-copilot-agent-anthropic.el \
;;             -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (d (list root (expand-file-name "providers" root)))
    (unless (member d load-path) (push d load-path))))

;; Stub auth-source so tests don't need real credentials
(require 'auth-source)
(require 'copilot-agent-tools)
(require 'copilot-agent-api)
(require 'copilot-agent-anthropic)

;;; ---------- Fixture Data ----------

(defconst anthropic-test--text-response
  "{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\
\"model\":\"claude-sonnet-4-6\",\"stop_reason\":\"end_turn\",\
\"content\":[{\"type\":\"text\",\"text\":\"Hello, world!\"}]}"
  "Fixture: a simple text response from Anthropic.")

(defconst anthropic-test--tool-use-response
  "{\"id\":\"msg_02\",\"type\":\"message\",\"role\":\"assistant\",\
\"model\":\"claude-sonnet-4-6\",\"stop_reason\":\"tool_use\",\
\"content\":[\
  {\"type\":\"text\",\"text\":\"I will run ls.\"},\
  {\"type\":\"tool_use\",\"id\":\"toolu_abc\",\"name\":\"shell_command\",\
   \"input\":{\"command\":\"ls -la\"}}\
]}"
  "Fixture: a response with both text and a tool_use block.")

(defconst anthropic-test--multi-tool-response
  "{\"id\":\"msg_03\",\"type\":\"message\",\"role\":\"assistant\",\
\"model\":\"claude-sonnet-4-6\",\"stop_reason\":\"tool_use\",\
\"content\":[\
  {\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"read_file\",\
   \"input\":{\"path\":\"/etc/hosts\"}},\
  {\"type\":\"tool_use\",\"id\":\"toolu_2\",\"name\":\"shell_command\",\
   \"input\":{\"command\":\"uname -a\"}}\
]}"
  "Fixture: a response with two tool_use blocks.")

(defconst anthropic-test--error-response
  "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\
\"message\":\"Invalid API key\"}}"
  "Fixture: an API error response.")

;;; ---------- Tool Schema Formatting ----------

(ert-deftest anthropic/format-tools-renames-parameters ()
  "format-tools converts `parameters' key to `input_schema'."
  (let* ((schema (list (list (cons 'name "shell_command")
                             (cons 'description "Run a shell command")
                             (cons 'parameters '((type . "object")
                                                 (properties
                                                  . ((command . ((type . "string"))))))))))
         (result (copilot-agent-anthropic--format-tools schema)))
    (should (= (length result) 1))
    (let ((tool (car result)))
      (should (assq 'input_schema tool))
      (should-not (assq 'parameters tool))
      (should (equal (cdr (assq 'name tool)) "shell_command")))))

(ert-deftest anthropic/format-tools-preserves-all-fields ()
  "format-tools keeps name and description intact."
  (let* ((schema (list (list (cons 'name "read_file")
                             (cons 'description "Read a file")
                             (cons 'parameters '((type . "object"))))))
         (tool (car (copilot-agent-anthropic--format-tools schema))))
    (should (equal (cdr (assq 'name tool)) "read_file"))
    (should (equal (cdr (assq 'description tool)) "Read a file"))))

(ert-deftest anthropic/format-tools-full-schema ()
  "format-tools handles the full copilot-agent-tools-schema without error."
  (let ((result (copilot-agent-anthropic--format-tools copilot-agent-tools-schema)))
    (should (= (length result) (length copilot-agent-tools-schema)))
    (dolist (tool result)
      (should (assq 'name tool))
      (should (assq 'input_schema tool))
      (should-not (assq 'parameters tool)))))

;;; ---------- Request Building ----------

(ert-deftest anthropic/build-request-contains-model ()
  "build-request includes the model field."
  (let* ((session (list :provider 'anthropic
                        :model "claude-opus-4-6"
                        :messages '(((role . "user") (content . "hi")))
                        :tools nil
                        :system-prompt nil
                        :max-tokens 1024))
         (json (copilot-agent-anthropic--build-request session))
         (parsed (json-read-from-string json)))
    (should (equal (cdr (assq 'model parsed)) "claude-opus-4-6"))))

(ert-deftest anthropic/build-request-includes-messages ()
  "build-request serialises messages array."
  (let* ((session (list :provider 'anthropic
                        :model "claude-sonnet-4-6"
                        :messages '(((role . "user") (content . "hello")))
                        :tools nil
                        :system-prompt nil
                        :max-tokens 512))
         (json (copilot-agent-anthropic--build-request session))
         (parsed (json-read-from-string json))
         (msgs (cdr (assq 'messages parsed))))
    (should (> (length msgs) 0))))

(ert-deftest anthropic/build-request-system-prompt ()
  "build-request includes system prompt when provided."
  (let* ((session (list :provider 'anthropic
                        :model "claude-sonnet-4-6"
                        :messages '()
                        :tools nil
                        :system-prompt "Be helpful."
                        :max-tokens 512))
         (json (copilot-agent-anthropic--build-request session))
         (parsed (json-read-from-string json)))
    (should (equal (cdr (assq 'system parsed)) "Be helpful."))))

(ert-deftest anthropic/build-request-no-system-omits-key ()
  "build-request omits system key when prompt is nil."
  (let* ((session (list :provider 'anthropic
                        :model "claude-sonnet-4-6"
                        :messages '()
                        :tools nil
                        :system-prompt nil
                        :max-tokens 512))
         (json (copilot-agent-anthropic--build-request session))
         (parsed (json-read-from-string json)))
    (should-not (assq 'system parsed))))

(ert-deftest anthropic/build-request-includes-tools ()
  "build-request serialises tools array when tools are present."
  (let* ((session (list :provider 'anthropic
                        :model "claude-sonnet-4-6"
                        :messages '()
                        :tools copilot-agent-tools-schema
                        :system-prompt nil
                        :max-tokens 512))
         (json (copilot-agent-anthropic--build-request session))
         (parsed (json-read-from-string json)))
    (should (assq 'tools parsed))
    (should (> (length (cdr (assq 'tools parsed))) 0))))

;;; ---------- Response Parsing ----------

(ert-deftest anthropic/parse-text-response ()
  "parse-response extracts text from a plain text response."
  (let ((result (copilot-agent-anthropic--parse-response
                 anthropic-test--text-response)))
    (should (equal (plist-get result :text) "Hello, world!"))
    (should (null (plist-get result :tool-calls)))
    (should (equal (plist-get result :stop-reason) "end_turn"))
    (should-not (plist-get result :error))))

(ert-deftest anthropic/parse-tool-use-response ()
  "parse-response extracts tool call from a tool_use response."
  (let* ((result (copilot-agent-anthropic--parse-response
                  anthropic-test--tool-use-response))
         (calls  (plist-get result :tool-calls)))
    (should (equal (plist-get result :text) "I will run ls."))
    (should (= (length calls) 1))
    (let ((call (car calls)))
      (should (equal (plist-get call :name) "shell_command"))
      (should (equal (plist-get call :id)   "toolu_abc"))
      (should (equal (cdr (assq 'command (plist-get call :input))) "ls -la")))
    (should (equal (plist-get result :stop-reason) "tool_use"))))

(ert-deftest anthropic/parse-multiple-tool-calls ()
  "parse-response extracts multiple tool_use blocks in order."
  (let* ((result (copilot-agent-anthropic--parse-response
                  anthropic-test--multi-tool-response))
         (calls  (plist-get result :tool-calls)))
    (should (= (length calls) 2))
    (should (equal (plist-get (nth 0 calls) :name) "read_file"))
    (should (equal (plist-get (nth 1 calls) :name) "shell_command"))))

(ert-deftest anthropic/parse-error-response ()
  "parse-response returns :error for an API error payload."
  (let ((result (copilot-agent-anthropic--parse-response
                 anthropic-test--error-response)))
    (should (plist-get result :error))
    (should (string-match-p "Invalid API key" (plist-get result :error)))))

(ert-deftest anthropic/parse-malformed-json ()
  "parse-response returns :error for malformed JSON."
  (let ((result (copilot-agent-anthropic--parse-response "not json{")))
    (should (plist-get result :error))))

;;; ---------- Tool Result Message ----------

(ert-deftest anthropic/make-tool-result-message-structure ()
  "make-tool-result-message builds a valid user-role message."
  (let* ((results (list (list :tool-use-id "toolu_abc" :content "file1\nfile2")))
         (msg (copilot-agent-anthropic-make-tool-result-message results)))
    (should (equal (cdr (assq 'role msg)) "user"))
    (let ((content (cdr (assq 'content msg))))
      (should (> (length content) 0))
      (let ((block (aref content 0)))
        (should (equal (cdr (assq 'type block)) "tool_result"))
        (should (equal (cdr (assq 'tool_use_id block)) "toolu_abc"))
        (should (equal (cdr (assq 'content block)) "file1\nfile2"))))))

(ert-deftest anthropic/make-tool-result-message-multiple ()
  "make-tool-result-message handles multiple results."
  (let* ((results (list (list :tool-use-id "id1" :content "r1")
                        (list :tool-use-id "id2" :content "r2")))
         (msg (copilot-agent-anthropic-make-tool-result-message results))
         (content (cdr (assq 'content msg))))
    (should (= (length content) 2))))

(provide 'test-copilot-agent-anthropic)
;;; test-copilot-agent-anthropic.el ends here

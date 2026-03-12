;;; test-copilot-agent-api.el --- ERT tests for copilot-agent-api -*- lexical-binding: t -*-

;;; Commentary:
;; Regression tests for copilot-agent-api.el.
;; Covers: provider registry, session creation, message appending,
;; JSON utilities, tool approval logic, and the agentic loop
;; (using stubbed providers to avoid real HTTP calls).
;;
;; Run: emacs -batch -L .. -l test-copilot-agent-api.el \
;;             -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (unless (member root load-path) (push root load-path)))

(require 'copilot-agent-tools)
(require 'copilot-agent-api)

;;; ---------- Helpers ----------

(defun api-test--make-stub-provider (&optional response)
  "Return a minimal provider plist.
:send-fn immediately calls back with RESPONSE."
  (let ((resp (or response
                  (list :text "stub reply"
                        :tool-calls nil
                        :stop-reason "end_turn"
                        :raw-content (vector '((type . "text") (text . "stub reply")))))))
    (list :display-name          "Stub"
          :default-model         "stub-model"
          :send-fn               (lambda (_session callback)
                                   (funcall callback resp nil))
          :make-tool-result-fn   (lambda (results)
                                   `((role . "user")
                                     (content . ,(vconcat
                                                  (mapcar (lambda (r)
                                                            `((type . "tool_result")
                                                              (tool_use_id . ,(plist-get r :tool-use-id))
                                                              (content . ,(plist-get r :content))))
                                                          results)))))
          :format-tools-fn       #'identity)))

(defun api-test--register-stub (&optional response)
  "Register a stub provider as `stub' and return it."
  (let ((p (api-test--make-stub-provider response)))
    (copilot-agent-api-register-provider 'stub p)
    p))

;;; ---------- Provider Registry ----------

(ert-deftest api/register-and-retrieve-provider ()
  "Registering a provider makes it retrievable."
  (api-test--register-stub)
  (let ((p (copilot-agent-api--get-provider 'stub)))
    (should p)
    (should (equal (plist-get p :display-name) "Stub"))))

(ert-deftest api/get-unknown-provider-signals-error ()
  "Retrieving an unregistered provider signals an error."
  (should-error (copilot-agent-api--get-provider 'nonexistent-xyz)
                :type 'error))

(ert-deftest api/register-provider-validates-send-fn ()
  "Registering a provider without :send-fn signals an error."
  (should-error
   (copilot-agent-api-register-provider 'bad-provider '(:display-name "Bad"))
   :type 'error))

(ert-deftest api/list-providers-returns-symbols ()
  "list-providers returns a list of symbols."
  (api-test--register-stub)
  (let ((providers (copilot-agent-api-list-providers)))
    (should (listp providers))
    (should (memq 'stub providers))))

;;; ---------- Session Creation ----------

(ert-deftest api/new-session-has-required-keys ()
  "new-session returns a plist with all required keys."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub)))
    (should (plist-member s :provider))
    (should (plist-member s :model))
    (should (plist-member s :messages))
    (should (plist-member s :tools))
    (should (plist-member s :max-tokens))
    (should (plist-member s :approve-all))))

(ert-deftest api/new-session-uses-provider-default-model ()
  "new-session defaults to the provider's :default-model."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub)))
    (should (equal (plist-get s :model) "stub-model"))))

(ert-deftest api/new-session-model-override ()
  "new-session respects an explicit :model kwarg."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub :model "my-model")))
    (should (equal (plist-get s :model) "my-model"))))

(ert-deftest api/new-session-empty-messages ()
  "A freshly created session has an empty messages list."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub)))
    (should (null (plist-get s :messages)))))

(ert-deftest api/new-session-max-tokens-default ()
  "new-session defaults max-tokens to 8192."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub)))
    (should (= (plist-get s :max-tokens) 8192))))

(ert-deftest api/new-session-unknown-provider-errors ()
  "new-session signals an error for an unregistered provider."
  (should-error (copilot-agent-api-new-session 'no-such-provider)
                :type 'error))

;;; ---------- Message Appending ----------

(ert-deftest api/append-message-adds-to-empty-list ()
  "append-message adds a message to an empty session."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub)))
    (copilot-agent-api--append-message s '((role . "user") (content . "hi")))
    (should (= (length (plist-get s :messages)) 1))))

(ert-deftest api/append-message-preserves-order ()
  "append-message preserves insertion order."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub)))
    (copilot-agent-api--append-message s '((role . "user")      (content . "first")))
    (copilot-agent-api--append-message s '((role . "assistant") (content . "second")))
    (copilot-agent-api--append-message s '((role . "user")      (content . "third")))
    (let ((msgs (plist-get s :messages)))
      (should (= (length msgs) 3))
      (should (equal (cdr (assq 'content (nth 0 msgs))) "first"))
      (should (equal (cdr (assq 'content (nth 1 msgs))) "second"))
      (should (equal (cdr (assq 'content (nth 2 msgs))) "third")))))

;;; ---------- JSON / to-alist ----------

(ert-deftest api/to-alist-passthrough-on-alist ()
  "to-alist returns an alist unchanged."
  (let ((a '((command . "ls") (cwd . "/tmp"))))
    (should (equal (copilot-agent-api--to-alist a) a))))

(ert-deftest api/to-alist-nil-returns-nil ()
  "to-alist returns nil for nil input."
  (should (null (copilot-agent-api--to-alist nil))))

(ert-deftest api/to-alist-hash-table ()
  "to-alist converts a hash-table to an alist."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "command" "ls" ht)
    (puthash "cwd"     "/tmp" ht)
    (let ((result (copilot-agent-api--to-alist ht)))
      (should (listp result))
      (should (assq 'command result))
      (should (equal (cdr (assq 'command result)) "ls")))))

(ert-deftest api/to-alist-plist ()
  "to-alist converts a keyword plist to an alist."
  (let* ((pl     '(:command "ls" :cwd "/tmp"))
         (result (copilot-agent-api--to-alist pl)))
    (should (listp result))
    (should (assq 'command result))
    (should (equal (cdr (assq 'command result)) "ls"))))

;;; ---------- Tool Approval ----------

(ert-deftest api/approve-tool-approve-all-skips-prompt ()
  "approve-tool returns t immediately when session :approve-all is t."
  (api-test--register-stub)
  (let ((s (copilot-agent-api-new-session 'stub :approve-all t)))
    (should (eq t (copilot-agent-api--approve-tool "shell_command" '() s nil)))))

(ert-deftest api/approve-tool-calls-on-approve-fn ()
  "approve-tool delegates to on-approve-fn when provided."
  (api-test--register-stub)
  (let* ((s       (copilot-agent-api-new-session 'stub))
         (called  nil)
         (approve (lambda (name _input _session)
                    (setq called name)
                    t)))
    (copilot-agent-api--approve-tool "my_tool" '() s approve)
    (should (equal called "my_tool"))))

(ert-deftest api/approve-tool-on-approve-fn-decline ()
  "approve-tool returns nil when on-approve-fn returns nil."
  (api-test--register-stub)
  (let* ((s       (copilot-agent-api-new-session 'stub))
         (decline (lambda (_n _i _s) nil)))
    (should (null (copilot-agent-api--approve-tool "tool" '() s decline)))))

;;; ---------- Agentic Loop (stubbed) ----------

(ert-deftest api/send-appends-user-message ()
  "api-send appends the user message to session history."
  (api-test--register-stub)
  (let* ((s    (copilot-agent-api-new-session 'stub))
         (done nil))
    (copilot-agent-api-send s "hello"
                            (list :on-done  (lambda () (setq done t))
                                  :on-error (lambda (e) (error "Unexpected: %s" e))
                                  :on-approve (lambda (_n _i _s) t)))
    (should done)
    ;; Session should have user message + assistant message
    (let ((msgs (plist-get s :messages)))
      (should (>= (length msgs) 1))
      (should (equal (cdr (assq 'role (car msgs))) "user"))
      (should (equal (cdr (assq 'content (car msgs))) "hello")))))

(ert-deftest api/send-calls-on-text-callback ()
  "api-send invokes :on-text with assistant text."
  (api-test--register-stub
   (list :text "stub reply" :tool-calls nil :stop-reason "end_turn"
         :raw-content (vector '((type . "text") (text . "stub reply")))))
  (let* ((s        (copilot-agent-api-new-session 'stub))
         (received nil))
    (copilot-agent-api-send s "hello"
                            (list :on-text     (lambda (text) (setq received text))
                                  :on-done     (lambda () nil)
                                  :on-approve  (lambda (_n _i _s) t)))
    (should (equal received "stub reply"))))

(ert-deftest api/send-calls-on-error-callback ()
  "api-send invokes :on-error when the provider returns an HTTP error."
  (copilot-agent-api-register-provider
   'error-stub
   (list :display-name "ErrorStub"
         :default-model "m"
         :send-fn (lambda (_s cb) (funcall cb nil "HTTP 500: Internal Server Error"))
         :make-tool-result-fn (lambda (_) nil)
         :format-tools-fn #'identity))
  (let* ((s   (copilot-agent-api-new-session 'error-stub))
         (err nil))
    (copilot-agent-api-send s "hi"
                            (list :on-error (lambda (m) (setq err m))
                                  :on-approve (lambda (_n _i _s) t)))
    (should (string-match-p "500" err))))

(ert-deftest api/tool-loop-executes-approved-tool ()
  "The agentic loop executes a tool when user approves."
  ;; Provider returns one tool call, then text on second call
  (let ((call-count 0))
    (copilot-agent-api-register-provider
     'tool-stub
     (list :display-name "ToolStub"
           :default-model "m"
           :send-fn
           (lambda (_session callback)
             (cl-incf call-count)
             (if (= call-count 1)
                 ;; First call: request a tool
                 (funcall callback
                          (list :text nil
                                :tool-calls (list (list :id "tid1"
                                                        :name "shell_command"
                                                        :input '((command . "echo done"))))
                                :stop-reason "tool_use"
                                :raw-content (vector '((type . "tool_use") (id . "tid1")
                                                       (name . "shell_command")
                                                       (input . ((command . "echo done"))))))
                          nil)
               ;; Second call: return text (done)
               (funcall callback
                        (list :text "all done"
                              :tool-calls nil
                              :stop-reason "end_turn"
                              :raw-content (vector '((type . "text") (text . "all done"))))
                        nil)))
           :make-tool-result-fn
           (lambda (results)
             `((role . "user")
               (content . ,(vconcat
                            (mapcar (lambda (r)
                                      `((type . "tool_result")
                                        (tool_use_id . ,(plist-get r :tool-use-id))
                                        (content . ,(plist-get r :content))))
                                    results)))))
           :format-tools-fn #'identity))
    (let* ((s          (copilot-agent-api-new-session 'tool-stub))
           (done        nil)
           (tool-called nil))
      (copilot-agent-api-send
       s "run something"
       (list :on-tool-call   (lambda (n _i) (setq tool-called n))
             :on-tool-result (lambda (_n _r) nil)
             :on-approve     (lambda (_n _i _s) t)   ; always approve
             :on-done        (lambda () (setq done t))
             :on-error       (lambda (e) (error "Unexpected error: %s" e))))
      (should done)
      (should (equal tool-called "shell_command"))
      (should (= call-count 2)))))

(ert-deftest api/tool-loop-skips-declined-tool ()
  "The agentic loop records 'declined' when user rejects a tool."
  (let ((call-count 0))
    (copilot-agent-api-register-provider
     'decline-stub
     (list :display-name "DeclineStub"
           :default-model "m"
           :send-fn
           (lambda (_session callback)
             (cl-incf call-count)
             (if (= call-count 1)
                 (funcall callback
                          (list :text nil
                                :tool-calls (list (list :id "tid2"
                                                        :name "delete_file"
                                                        :input '((path . "/important"))))
                                :stop-reason "tool_use"
                                :raw-content (vector '((type . "tool_use") (id . "tid2")
                                                       (name . "delete_file")
                                                       (input . ((path . "/important"))))))
                          nil)
               (funcall callback
                        (list :text "ok skipped" :tool-calls nil
                              :stop-reason "end_turn"
                              :raw-content (vector '((type . "text") (text . "ok skipped"))))
                        nil)))
           :make-tool-result-fn
           (lambda (results)
             `((role . "user")
               (content . ,(vconcat
                            (mapcar (lambda (r)
                                      `((type . "tool_result")
                                        (tool_use_id . ,(plist-get r :tool-use-id))
                                        (content . ,(plist-get r :content))))
                                    results)))))
           :format-tools-fn #'identity))
    (let* ((s           (copilot-agent-api-new-session 'decline-stub))
           (result-text nil))
      (copilot-agent-api-send
       s "delete something"
       (list :on-tool-result (lambda (_n r) (setq result-text r))
             :on-approve     (lambda (_n _i _s) nil)   ; always decline
             :on-done        (lambda () nil)
             :on-error       (lambda (e) (error "Unexpected: %s" e))))
      (should (string-match-p "[Dd]eclined" result-text)))))

(provide 'test-copilot-agent-api)
;;; test-copilot-agent-api.el ends here

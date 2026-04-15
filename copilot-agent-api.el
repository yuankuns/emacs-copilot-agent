;;; copilot-agent-api.el --- Provider registry, HTTP, and agentic loop -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Provider-agnostic core layer.  Responsibilities:
;;   - Provider registry (Anthropic, Gemini, …)
;;   - Async HTTP POST via curl subprocess
;;   - Session management
;;   - Agentic tool-use loop (send → tool calls → send → …)
;;
;; Each provider registers itself by calling `copilot-agent-api-register-provider'
;; with a plist implementing the provider protocol.

;;; Code:

(require 'json)
(require 'copilot-agent-tools)

;;; ---------- Debug Logging ----------

(defcustom copilot-agent-debug nil
  "When non-nil, log raw HTTP request/response bodies and tool call details.
Output goes to the *copilot-agent-debug* buffer.  Useful for diagnosing
tool-call parsing or provider communication issues."
  :type 'boolean
  :group 'copilot-agent)

(defun copilot-agent-api--debug (fmt &rest args)
  "Log FMT+ARGS to *copilot-agent-debug* when `copilot-agent-debug' is non-nil."
  (when copilot-agent-debug
    (with-current-buffer (get-buffer-create "*copilot-agent-debug*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] "))
      (insert (apply #'format fmt args))
      (insert "\n"))))

;; Add providers/ to load-path so (require 'copilot-agent-*) works after
;; package-vc-install (which only adds the package root).  Placed here rather
;; than in copilot-agent.el so every entry point that loads this core file
;; (copilot-agent-status, provider files, etc.) also gets the adjustment.
;;
;; eval-and-compile ensures it runs at byte-compile time too, so compiling
;; test files in the same pass doesn't fail with "Cannot open load file".
;;
;; directory-file-name strips any trailing slash for a canonical form that
;; matches what -L flags and most load-path entries use, avoiding duplicates.
(eval-and-compile
  (let* ((base     (or load-file-name
                       buffer-file-name
                       (locate-library "copilot-agent-api")))
         (prov-dir (and base
                        (directory-file-name
                         (expand-file-name "providers"
                                           (file-name-directory base))))))
    (when (and prov-dir (file-directory-p prov-dir))
      (add-to-list 'load-path prov-dir))))

;;; ---------- Provider Registry ----------

(defvar copilot-agent-api--providers (make-hash-table :test #'eq)
  "Hash-table: provider-symbol -> implementation plist.
Required plist keys:
  :display-name        - Human-readable name string
  :default-model       - Default model string
  :send-fn             - function (session callback)
  :make-tool-result-fn - function (results-list) -> message-alist
  :format-tools-fn     - function (schema-list) -> provider-list")

(defun copilot-agent-api-register-provider (id plist)
  "Register provider ID (a symbol) with implementation PLIST."
  (unless (symbolp id)
    (error "Provider ID must be a symbol, got: %S" id))
  (unless (plist-get plist :send-fn)
    (error "Provider `%s' must supply :send-fn in its implementation plist" id))
  (puthash id plist copilot-agent-api--providers))

(defun copilot-agent-api--get-provider (id)
  "Return implementation plist for provider ID, signalling an error if absent."
  (or (gethash id copilot-agent-api--providers)
      (error "Unknown provider `%s'.  Load the provider file first" id)))

(defun copilot-agent-api-list-providers ()
  "Return list of registered provider symbols."
  (let (result)
    (maphash (lambda (k _) (push k result)) copilot-agent-api--providers)
    (sort result (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

;;; ---------- Session ----------

(defun copilot-agent-api-new-session (provider &rest kwargs)
  "Create a new session plist for PROVIDER (symbol).
Optional KWARGS override defaults:
  :model          - model string
  :system-prompt  - string prepended as system instruction
  :max-tokens     - integer (default 8192)
  :context-buffer - buffer whose directory becomes the tool cwd
  :tools          - tool schema list (defaults to `copilot-agent-tools-schema')
  :approve-all    - t to skip per-tool approval prompts"
  (copilot-agent-api--get-provider provider)   ; validate early
  (let* ((p     (copilot-agent-api--get-provider provider))
         (model (or (plist-get kwargs :model)
                    ;; :default-model-fn is a thunk so setq on the config
                    ;; variable is picked up at session-creation time
                    (when-let ((fn (plist-get p :default-model-fn)))
                      (funcall fn))
                    (plist-get p :default-model))))
    (list :provider       provider
          :model          model
          :messages       '()
          :tools          (or (plist-get kwargs :tools)
                              copilot-agent-tools-schema)
          :system-prompt  (plist-get kwargs :system-prompt)
          :max-tokens     (or (plist-get kwargs :max-tokens) 8192)
          :context-buffer (plist-get kwargs :context-buffer)
          :approve-all    (plist-get kwargs :approve-all))))

(defun copilot-agent-api--append-message (session message)
  "Append MESSAGE alist to SESSION's :messages list (mutates plist)."
  (plist-put session :messages
             (append (plist-get session :messages) (list message))))

;;; ---------- Async HTTP (curl) ----------

(defconst copilot-agent-api--status-sentinel
  "\nCOPILOT_AGENT_HTTP_STATUS:"
  "Marker written by curl -w to separate the response body from the HTTP status.
Must not contain null bytes (dropped by process pipes) and must not plausibly
appear in any API response body.")

(defun copilot-agent-api--curl-post (url headers json-body callback)
  "POST JSON-BODY string to URL with HEADERS list asynchronously via curl.
CALLBACK is called as \(BODY-STRING NIL) on success or
\(NIL ERROR-STRING) on failure."
  (let* ((req-file (make-temp-file "copilot-agent-req" nil ".json"))
         (resp-buf (generate-new-buffer " *copilot-agent-http*")))
    (write-region json-body nil req-file nil 'silent)
    (make-process
     :name    "copilot-agent-curl"
     :buffer  resp-buf
     :command (append
               (list "curl" "--silent" "--show-error"
                     "--max-time" "120"
                     "-w" (concat copilot-agent-api--status-sentinel "%{http_code}")
                     "-X" "POST"
                     "-H" "Content-Type: application/json")
               (mapcan (lambda (h) (list "-H" h)) headers)
               (list "--data" (format "@%s" req-file) url))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (let* ((raw   (with-current-buffer (process-buffer proc)
                             (buffer-string)))
                    (idx   (string-search copilot-agent-api--status-sentinel raw))
                    (body  (if idx (substring raw 0 idx) raw))
                    (code  (if idx
                               (string-to-number
                                (string-trim
                                 (substring raw (+ idx (length copilot-agent-api--status-sentinel)))))
                             0)))
               (copilot-agent-api--debug "HTTP %d  body: %.500s" code body)
               (if (and (>= code 200) (< code 300))
                   (funcall callback body nil)
                 (funcall callback nil
                          (format "HTTP %d: %s" code (string-trim body)))))
           (kill-buffer (process-buffer proc))
           (when (file-exists-p req-file) (delete-file req-file))))))))

;;; ---------- JSON / Data Utilities ----------

(defun copilot-agent-api--to-alist (obj)
  "Normalise OBJ to an alist for use as tool arguments.
Handles: hash-table (from `json-parse-string'), alist, plist, nil."
  (cond
   ((null obj) nil)
   ((hash-table-p obj)
    (let (result)
      (maphash (lambda (k v) (push (cons (intern k) v) result)) obj)
      (nreverse result)))
   ;; plist: (:key val :key2 val2)
   ((and (listp obj) (keywordp (car obj)))
    (cl-loop for (k v) on obj by #'cddr
             collect (cons (intern (substring (symbol-name k) 1)) v)))
   (t obj)))  ; already alist or something else

;;; ---------- Tool Approval ----------

(defconst copilot-agent-api--auto-approved-tools
  '("read_file" "write_file" "list_directory" "find_in_files" "glob" "grep" "edit_file")
  "Tools that are automatically approved without prompting the user.
These are read-only or low-risk write operations where interrupting the
agentic loop for approval provides little safety benefit.")

(defun copilot-agent-api--approve-tool (name input session on-approve-fn)
  "Return t if the user approves running tool NAME with INPUT.
Checks SESSION :approve-all first.  Falls back to ON-APPROVE-FN if provided,
otherwise prompts in the minibuffer with [y]es / [a]ll / [n]o."
  (or (plist-get session :approve-all)
      (member name copilot-agent-api--auto-approved-tools)
      (let ((approved
             (if on-approve-fn
                 (funcall on-approve-fn name input session)
               ;; Built-in minibuffer fallback
               (let* ((args-str
                       (if (listp input)
                           (mapconcat (lambda (p) (format "%s: %S" (car p) (cdr p)))
                                      input "  ")
                         (format "%S" input)))
                      (ch (read-char-choice
                           (format "Run tool `%s'  %s\n[y]es / [a]ll / [n]o: "
                                   name args-str)
                           '(?y ?Y ?a ?A ?n ?N))))
                 (pcase (downcase ch)
                   (?y t)
                   (?a (plist-put session :approve-all t) t)
                   (?n nil))))))
        approved)))

;;; ---------- Agentic Loop ----------

(defun copilot-agent-api-send (session user-input callbacks)
  "Begin an agentic turn with USER-INPUT text in SESSION.
Appends the user message, then starts the send/tool-loop cycle.

CALLBACKS plist keys (all optional):
  :on-thinking    ()                        waiting indicator
  :on-text        (text)                    assistant text chunk
  :on-tool-call   (name input)              before tool execution
  :on-tool-result (name result)             after tool execution
  :on-approve     (name input session)->bool approval UI hook
  :on-done        ()                        turn complete
  :on-error       (message)                 error handler"
  (when (and user-input (not (string-empty-p (string-trim user-input))))
    (copilot-agent-api--append-message
     session `((role . "user") (content . ,user-input))))
  ;; Update tool context from session's buffer
  (when-let ((buf (plist-get session :context-buffer)))
    (copilot-agent-tools-set-context buf))
  (copilot-agent-api--loop session callbacks))

(defun copilot-agent-api--loop (session callbacks)
  "Send current SESSION messages via its provider, then handle the response.
CALLBACKS is the plist passed through from `copilot-agent-api-send'."
  (let* ((provider (copilot-agent-api--get-provider (plist-get session :provider)))
         (send-fn  (plist-get provider :send-fn))
         (on-think (plist-get callbacks :on-thinking))
         (on-error (plist-get callbacks :on-error)))
    (when on-think (funcall on-think))
    (condition-case err
        (funcall send-fn session
                 (lambda (response http-error)
                   (if http-error
                       (progn
                         (when (fboundp 'copilot-agent-status-record-error)
                           (copilot-agent-status-record-error
                            (plist-get session :provider) http-error))
                         (when on-error (funcall on-error http-error)))
                     (copilot-agent-api--handle-response response session callbacks))))
      (error (when on-error (funcall on-error (error-message-string err)))))))

(defun copilot-agent-api--handle-response (response session callbacks)
  "Process parsed RESPONSE plist from a provider for SESSION and continue the loop.
CALLBACKS is the plist passed through from `copilot-agent-api-send'."
  (let* ((text       (plist-get response :text))
         (tool-calls (plist-get response :tool-calls))
         (raw        (plist-get response :raw-content))
         (on-text    (plist-get callbacks :on-text))
         (on-done    (plist-get callbacks :on-done)))
    ;; Show text to user
    (when (and text on-text) (funcall on-text text))
    (copilot-agent-api--debug "handle-response: text=%S tool-calls=%S"
                              (and text (substring text 0 (min 120 (length text))))
                              (mapcar (lambda (tc) (plist-get tc :name)) tool-calls))
    ;; Record this assistant turn in history
    (copilot-agent-api--append-message
     session `((role . "assistant") (content . ,raw)))
    (if (null tool-calls)
        (when on-done (funcall on-done))
      (condition-case err
          (copilot-agent-api--process-tools tool-calls session callbacks)
        (error
         (copilot-agent-api--debug "process-tools error: %s" (error-message-string err))
         (when (plist-get callbacks :on-error)
           (funcall (plist-get callbacks :on-error) (error-message-string err))))))))

(defun copilot-agent-api--process-tools (tool-calls session callbacks)
  "Execute approved TOOL-CALLS for SESSION, append results, then continue the loop.
CALLBACKS is the plist passed through from `copilot-agent-api-send'."
  (let* ((provider       (copilot-agent-api--get-provider (plist-get session :provider)))
         (make-result-fn (plist-get provider :make-tool-result-fn))
         (on-tool-call   (plist-get callbacks :on-tool-call))
         (on-tool-result (plist-get callbacks :on-tool-result))
         (on-approve     (plist-get callbacks :on-approve))
         results)
    (dolist (tc tool-calls)
      (let* ((name  (plist-get tc :name))
             (input (plist-get tc :input))
             (id    (plist-get tc :id)))
        (copilot-agent-api--debug "tool-call: name=%s id=%s input=%S" name id input)
        (when on-tool-call (funcall on-tool-call name input))
        (let* ((approved (copilot-agent-api--approve-tool name input session on-approve))
               (result   (if approved
                             (copilot-agent-tools-execute
                              name (copilot-agent-api--to-alist input))
                           "Tool execution declined by user.")))
          (copilot-agent-api--debug "tool-result: name=%s result=%.200s" name result)
          (when on-tool-result (funcall on-tool-result name result))
          (push (list :tool-use-id id :content result) results))))
    ;; make-tool-result-fn may return either a single message alist (Anthropic,
    ;; Gemini) or a list of message alists (OpenAI-compatible providers such as
    ;; Qwen, which require one {"role":"tool"} message per tool call).
    ;;
    ;; Distinguish the two shapes by inspecting (caar result):
    ;;   single message  : ((role . "user") …)  → caar = symbol `role'
    ;;   list of messages: (((role . "tool") …) …) → caar = cons cell
    ;; Using `listp' on (car result) alone is not sufficient — both a cons pair
    ;; (role . "user") and a proper list satisfy listp in Emacs Lisp.
    (let ((msg-or-list (funcall make-result-fn (nreverse results))))
      (if (symbolp (caar msg-or-list))
          ;; Single message alist — first key is a symbol like 'role
          (copilot-agent-api--append-message session msg-or-list)
        ;; List of message alists — first element is itself an alist
        (dolist (m msg-or-list)
          (copilot-agent-api--append-message session m))))
    (copilot-agent-api--loop session callbacks)))

(provide 'copilot-agent-api)
;;; copilot-agent-api.el ends here

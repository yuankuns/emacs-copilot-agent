;;; test-copilot-agent-ui.el --- ERT tests for copilot-agent-ui -*- lexical-binding: t -*-

;;; Commentary:
;; Regression tests for copilot-agent-ui.el.
;; Covers:
;;   - Buffer is NOT globally read-only (fundamental-mode fix)
;;   - Input area is freely editable
;;   - History area is protected by text property, not buffer-read-only
;;   - history-end-marker advances; input-marker stays put
;;   - Insertion always goes before the separator
;;   - Thinking indicator overlay
;;   - Input get/clear helpers
;;   - Tool approval prompt logic

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (d (list root (expand-file-name "providers" root)))
    (unless (member d load-path) (push d load-path))))

;; Stub auth-source and providers so UI tests never hit the network
(require 'auth-source)
(advice-add 'auth-source-pick-first-password :override (lambda (&rest _) "stub-key")
            '((name . ui-test-auth-stub)))

(require 'copilot-agent-tools)
(require 'copilot-agent-api)
(require 'copilot-agent-ui)

;; Register a stub provider so sessions can be created
(copilot-agent-api-register-provider
 'ui-stub
 (list :display-name "UIStub" :default-model "stub"
       :send-fn (lambda (_s cb)
                  (funcall cb (list :text "ok" :tool-calls nil
                                    :stop-reason "end_turn"
                                    :raw-content (vector '((type . "text") (text . "ok"))))
                           nil))
       :make-tool-result-fn (lambda (_) '((role . "user") (content . [])))
       :format-tools-fn #'identity))

;;; ---------- Helpers ----------

(defmacro with-fresh-chat-buffer (&rest body)
  "Run BODY with a freshly initialised chat buffer, then kill it."
  (declare (indent 0))
  `(progn
     ;; Kill any leftover buffer from a previous test
     (when (get-buffer copilot-agent-ui--buffer-name)
       (kill-buffer copilot-agent-ui--buffer-name))
     (let ((buf (copilot-agent-ui-get-buffer)))
       (unwind-protect
           (with-current-buffer buf
             ,@body)
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

;;; ---------- Bug: special-mode made buffer globally read-only ----------

(ert-deftest ui/buffer-is-not-globally-read-only ()
  "The chat buffer must NOT have buffer-read-only set.
Regression: copilot-agent-mode previously inherited special-mode which
sets buffer-read-only t, making it impossible to type in the input area."
  (with-fresh-chat-buffer
    (should-not buffer-read-only)))

(ert-deftest ui/input-area-accepts-text-insertion ()
  "Text can be inserted at point-max (the input area) without error.
Regression: special-mode parent blocked all typing."
  (with-fresh-chat-buffer
    (goto-char (point-max))
    ;; This must NOT signal 'buffer-read-only
    (should (progn (insert "hello") t))
    (should (string-suffix-p "hello" (buffer-string)))))

(ert-deftest ui/input-area-is-after-input-marker ()
  "The freely editable region starts at copilot-agent-ui--input-marker."
  (with-fresh-chat-buffer
    (should copilot-agent-ui--input-marker)
    ;; Input marker should be near point-max on a fresh buffer
    (should (<= (marker-position copilot-agent-ui--input-marker) (point-max)))))

;;; ---------- History protected by text property, not buffer-read-only ----------

(ert-deftest ui/history-has-read-only-text-property ()
  "The welcome header must carry the `read-only' text property."
  (with-fresh-chat-buffer
    ;; The very first character of the buffer should be in the read-only zone
    (should (get-text-property 1 'read-only))))

(ert-deftest ui/history-modification-signals-error ()
  "Attempting to modify history text without inhibit-read-only signals an error."
  (with-fresh-chat-buffer
    (should-error
     (progn (goto-char 1) (delete-char 1))
     :type 'text-read-only)))

(ert-deftest ui/history-modifiable-with-inhibit-read-only ()
  "History CAN be modified when inhibit-read-only is t."
  (with-fresh-chat-buffer
    ;; This is how programmatic inserts work internally
    (should (let ((inhibit-read-only t))
              (goto-char 1)
              (insert "X")
              t))))

;;; ---------- Markers ----------

(ert-deftest ui/history-end-marker-exists-after-init ()
  "history-end-marker is set after buffer initialisation."
  (with-fresh-chat-buffer
    (should copilot-agent-ui--history-end-marker)
    (should (markerp copilot-agent-ui--history-end-marker))))

(ert-deftest ui/input-marker-exists-after-init ()
  "input-marker is set after buffer initialisation."
  (with-fresh-chat-buffer
    (should copilot-agent-ui--input-marker)
    (should (markerp copilot-agent-ui--input-marker))))

(ert-deftest ui/history-end-marker-before-input-marker ()
  "history-end-marker must be positioned before input-marker."
  (with-fresh-chat-buffer
    (should (< (marker-position copilot-agent-ui--history-end-marker)
               (marker-position copilot-agent-ui--input-marker)))))

(ert-deftest ui/history-end-marker-advances-on-insert ()
  "After inserting history content, history-end-marker advances past it."
  (with-fresh-chat-buffer
    (let ((pos-before (marker-position copilot-agent-ui--history-end-marker)))
      (copilot-agent-ui--history-insert "some new content\n")
      (should (> (marker-position copilot-agent-ui--history-end-marker)
                 pos-before)))))

(ert-deftest ui/input-marker-stays-relative-to-prompt ()
  "input-marker must remain immediately after the '> ' prompt.
Note: the marker's absolute position shifts when history is inserted before
the separator (all markers after the insertion point move forward).
What must stay invariant is the relative position — input-marker is always
right after the two '> ' characters."
  (with-fresh-chat-buffer
    (copilot-agent-ui--history-insert "lots of history text\n")
    (let ((pos (marker-position copilot-agent-ui--input-marker)))
      (should (>= pos 2))
      (should (equal (buffer-substring-no-properties (- pos 2) pos) "> ")))))

;;; ---------- History insertion goes before separator ----------

(ert-deftest ui/history-insert-goes-before-separator ()
  "Inserted history text appears before the ─── separator line."
  (with-fresh-chat-buffer
    (copilot-agent-ui--history-insert "MARKER_TEXT\n")
    (let* ((str   (buffer-string))
           (mpos  (string-search "MARKER_TEXT" str))
           (spos  (string-search "─" str)))
      (should mpos)
      (should spos)
      (should (< mpos spos)))))

(ert-deftest ui/separator-stays-at-bottom-after-multiple-inserts ()
  "The ─── separator remains the last structural element before the input prompt."
  (with-fresh-chat-buffer
    (copilot-agent-ui--history-insert "first\n")
    (copilot-agent-ui--history-insert "second\n")
    (copilot-agent-ui--history-insert "third\n")
    (let* ((str  (buffer-string))
           (spos (string-search "─" str))
           (tpos (string-search "third" str)))
      (should (< tpos spos)))))

;;; ---------- Message rendering ----------

(ert-deftest ui/insert-user-message-contains-text ()
  "insert-user-message renders text in the history area."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-user-message "Hello agent")
    (should (string-search "Hello agent" (buffer-string)))))

(ert-deftest ui/insert-user-message-shows-you-label ()
  "insert-user-message renders a 'You' label."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-user-message "test")
    (should (string-search "You" (buffer-string)))))

(ert-deftest ui/insert-assistant-text-contains-text ()
  "insert-assistant-text renders text in the history area."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-assistant-text "Here is my answer")
    (should (string-search "Here is my answer" (buffer-string)))))

(ert-deftest ui/insert-assistant-text-shows-label ()
  "insert-assistant-text renders an 'Assistant' label."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-assistant-text "answer")
    (should (string-search "Assistant" (buffer-string)))))

(ert-deftest ui/insert-tool-call-shows-tool-name ()
  "insert-tool-call renders the tool name."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-tool-call "shell_command" '((command . "ls")))
    (should (string-search "shell_command" (buffer-string)))))

(ert-deftest ui/insert-tool-call-shows-args ()
  "insert-tool-call renders the tool arguments."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-tool-call "read_file" '((path . "/etc/hosts")))
    (should (string-search "/etc/hosts" (buffer-string)))))

(ert-deftest ui/insert-tool-result-shows-content ()
  "insert-tool-result renders the result string."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-tool-result "shell_command" "file1.txt\nfile2.txt")
    (should (string-search "file1.txt" (buffer-string)))))

(ert-deftest ui/insert-tool-result-short-result-fully-displayed ()
  "Results with fewer lines than the limit are shown in full without truncation."
  (with-fresh-chat-buffer
    (let ((copilot-agent-tool-result-max-lines 5))
      (copilot-agent-ui-insert-tool-result "read_file" "line1\nline2\nline3")
      (let ((str (buffer-string)))
        (should (string-search "line1" str))
        (should (string-search "line2" str))
        (should (string-search "line3" str))
        (should-not (string-search "more lines" str))))))

(ert-deftest ui/insert-tool-result-long-result-truncated ()
  "Results exceeding the limit show only the first N lines plus a count footer."
  (with-fresh-chat-buffer
    (let ((copilot-agent-tool-result-max-lines 3))
      (copilot-agent-ui-insert-tool-result
       "read_file"
       "line1\nline2\nline3\nline4\nline5\nline6")
      (let ((str (buffer-string)))
        ;; First 3 lines visible
        (should (string-search "line1" str))
        (should (string-search "line2" str))
        (should (string-search "line3" str))
        ;; Lines beyond the limit are hidden
        (should-not (string-search "line4" str))
        ;; Footer shows the count of hidden lines
        (should (string-search "3 more lines" str))))))

(ert-deftest ui/insert-tool-result-trailing-newline-not-counted ()
  "A trailing newline in the result must not inflate the line count."
  (with-fresh-chat-buffer
    (let ((copilot-agent-tool-result-max-lines 5))
      ;; 3 lines of content plus a trailing newline — should NOT show truncation.
      (copilot-agent-ui-insert-tool-result "shell_command" "a\nb\nc\n")
      (should-not (string-search "more lines" (buffer-string))))))

(ert-deftest ui/insert-error-shows-message ()
  "insert-error renders the error message."
  (with-fresh-chat-buffer
    (copilot-agent-ui-insert-error "HTTP 500: server exploded")
    (should (string-search "HTTP 500" (buffer-string)))))

;;; ---------- Input helpers ----------

(ert-deftest ui/get-input-returns-typed-text ()
  "get-input returns whatever is in the editable input area."
  (with-fresh-chat-buffer
    (goto-char (point-max))
    (insert "my question")
    (should (equal (copilot-agent-ui--get-input) "my question"))))

(ert-deftest ui/get-input-empty-on-fresh-buffer ()
  "get-input returns an empty string on a fresh buffer."
  (with-fresh-chat-buffer
    (should (equal (copilot-agent-ui--get-input) ""))))

(ert-deftest ui/clear-input-removes-typed-text ()
  "clear-input deletes the user's typed text."
  (with-fresh-chat-buffer
    (goto-char (point-max))
    (insert "something to clear")
    (copilot-agent-ui--clear-input)
    (should (equal (copilot-agent-ui--get-input) ""))))

(ert-deftest ui/clear-input-leaves-prompt-intact ()
  "clear-input does not remove the '> ' prompt."
  (with-fresh-chat-buffer
    (goto-char (point-max))
    (insert "text")
    (copilot-agent-ui--clear-input)
    (should (string-search "> " (buffer-string)))))

;;; ---------- Thinking indicator ----------

(ert-deftest ui/show-thinking-creates-overlay ()
  "show-thinking creates an overlay in the buffer."
  (with-fresh-chat-buffer
    (copilot-agent-ui--show-thinking)
    (should copilot-agent-ui--thinking-overlay)
    (should (overlayp copilot-agent-ui--thinking-overlay))))

(ert-deftest ui/hide-thinking-removes-overlay ()
  "hide-thinking deletes the overlay and nils the variable."
  (with-fresh-chat-buffer
    (copilot-agent-ui--show-thinking)
    (copilot-agent-ui--hide-thinking)
    (should-not copilot-agent-ui--thinking-overlay)))

(ert-deftest ui/show-thinking-twice-does-not-accumulate ()
  "Calling show-thinking twice leaves only one overlay."
  (with-fresh-chat-buffer
    (copilot-agent-ui--show-thinking)
    (copilot-agent-ui--show-thinking)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (= (length overlays) 1)))))

(ert-deftest ui/show-thinking-default-label ()
  "show-thinking with no argument displays 'Thinking…'."
  (with-fresh-chat-buffer
    (copilot-agent-ui--show-thinking)
    (let ((after (overlay-get copilot-agent-ui--thinking-overlay 'after-string)))
      (should (string-match-p "Thinking" after)))))

(ert-deftest ui/show-thinking-running-label ()
  "show-thinking with 'Running…' displays that label."
  (with-fresh-chat-buffer
    (copilot-agent-ui--show-thinking "Running…")
    (let ((after (overlay-get copilot-agent-ui--thinking-overlay 'after-string)))
      (should (string-match-p "Running" after)))))

(ert-deftest ui/show-thinking-label-replaces-previous ()
  "Calling show-thinking with a new label replaces the old one."
  (with-fresh-chat-buffer
    (copilot-agent-ui--show-thinking "Thinking…")
    (copilot-agent-ui--show-thinking "Running…")
    (let ((after (overlay-get copilot-agent-ui--thinking-overlay 'after-string)))
      (should (string-match-p "Running" after))
      (should-not (string-match-p "Thinking" after)))))

;;; ---------- Tool approval ----------

(ert-deftest ui/approve-tool-skips-prompt-when-approve-all ()
  "approve-tool returns t immediately when session :approve-all is set."
  (with-fresh-chat-buffer
    (let ((session (list :approve-all t)))
      (should (copilot-agent-ui-approve-tool "shell_command" '() session)))))

(ert-deftest ui/approve-tool-sets-approve-all-on-a ()
  "Choosing 'a' sets :approve-all on the session for future calls."
  (with-fresh-chat-buffer
    (let ((session (list :approve-all nil)))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?a)))
        (copilot-agent-ui-approve-tool "tool" '() session))
      (should (plist-get session :approve-all)))))

(ert-deftest ui/approve-tool-returns-nil-on-n ()
  "Choosing 'n' returns nil (decline)."
  (with-fresh-chat-buffer
    (let ((session (list :approve-all nil)))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
        (should-not (copilot-agent-ui-approve-tool "tool" '() session))))))

(ert-deftest ui/approve-tool-returns-t-on-y ()
  "Choosing 'y' returns t (approve once)."
  (with-fresh-chat-buffer
    (let ((session (list :approve-all nil)))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y)))
        (should (copilot-agent-ui-approve-tool "tool" '() session))))))

;;; ---------- Context refresh ----------

(ert-deftest ui/refresh-context-picks-mru-file-buffer ()
  "refresh-context updates :context-buffer and :system-prompt to the new file.
This is the regression test for the bug where writing code always targeted the
first file opened, regardless of which file the user had switched to."
  (with-fresh-chat-buffer
    (let* ((old-buf   (generate-new-buffer "*ui-test-old*"))
           (file-buf  (generate-new-buffer "*ui-test-refresh*"))
           (agent-buf (get-buffer copilot-agent-ui--buffer-name))
           ;; Simulate a session originally created for old-buf
           (session   (list :context-buffer old-buf
                            :system-prompt  "base\n\nCurrent file: /tmp/old.el")))
      (with-current-buffer file-buf
        (setq buffer-file-name "/tmp/test-refresh.el"))
      (unwind-protect
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list agent-buf file-buf))))
            (copilot-agent-ui--refresh-context session)
            (should (eq (plist-get session :context-buffer) file-buf))
            ;; System prompt must now mention the new file, not the old one.
            (should (string-match-p "test-refresh\\.el"
                                    (or (plist-get session :system-prompt) "")))
            (should-not (string-match-p "old\\.el"
                                        (plist-get session :system-prompt))))
        (with-current-buffer file-buf (setq buffer-file-name nil))
        (kill-buffer file-buf)
        (kill-buffer old-buf)))))

(ert-deftest ui/refresh-context-no-change-when-same-buffer ()
  "refresh-context does not update session when context buffer is unchanged."
  (with-fresh-chat-buffer
    (let* ((file-buf  (generate-new-buffer "*ui-test-same*"))
           (agent-buf (get-buffer copilot-agent-ui--buffer-name))
           (session   (list :context-buffer file-buf
                            :system-prompt  "original")))
      (with-current-buffer file-buf
        (setq buffer-file-name "/tmp/same.el"))
      (unwind-protect
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list agent-buf file-buf))))
            (copilot-agent-ui--refresh-context session)
            ;; Prompt must NOT be rebuilt when buffer hasn't changed.
            (should (equal (plist-get session :system-prompt) "original")))
        (with-current-buffer file-buf (setq buffer-file-name nil))
        (kill-buffer file-buf)))))

(ert-deftest ui/refresh-context-ignores-agent-buffer ()
  "refresh-context never sets :context-buffer to the agent chat buffer itself."
  (with-fresh-chat-buffer
    (let ((session (list :context-buffer nil)))
      (copilot-agent-ui--refresh-context session)
      (should-not (eq (plist-get session :context-buffer)
                      (get-buffer copilot-agent-ui--buffer-name))))))

(ert-deftest ui/refresh-context-keeps-old-when-no-file-buffer ()
  "refresh-context leaves :context-buffer unchanged when no file buffers exist."
  (with-fresh-chat-buffer
    (let* ((original (get-buffer-create " *ctx-original*"))
           (session  (list :context-buffer original)))
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda () (list (get-buffer copilot-agent-ui--buffer-name)))))
        (copilot-agent-ui--refresh-context session)
        (should (eq (plist-get session :context-buffer) original)))
      (kill-buffer original))))

(provide 'test-copilot-agent-ui)
;;; test-copilot-agent-ui.el ends here

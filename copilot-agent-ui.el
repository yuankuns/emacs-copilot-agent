;;; copilot-agent-ui.el --- Chat buffer UI -*- lexical-binding: t -*-

;;; Commentary:
;; Provides the interactive chat buffer for copilot-agent.
;;
;; Buffer layout (single persistent separator at the bottom):
;;
;;   [welcome header  - read-only text property]
;;   [conversation history - read-only text property]
;;                                ← history-end-marker  (advances on insert)
;;   ────────────────────────── (read-only separator, always at bottom)
;;   >                          ← input-marker (stays put; user types here)
;;   [freely editable input]
;;
;; The buffer itself is NOT read-only (fundamental-mode parent).
;; History is protected by the `read-only' TEXT PROPERTY only.
;; inhibit-read-only is used for programmatic inserts into history.

;;; Code:

(require 'copilot-agent-api)

;;; ---------- Faces ----------

(defgroup copilot-agent-faces nil
  "Faces for Copilot Agent chat buffer."
  :group 'copilot-agent)

(defface copilot-agent-user-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the You label.")

(defface copilot-agent-assistant-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the Assistant label.")

(defface copilot-agent-tool-name-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for tool names.")

(defface copilot-agent-tool-result-face
  '((t :inherit font-lock-doc-face))
  "Face for tool result output.")

(defface copilot-agent-error-face
  '((t :inherit error :weight bold))
  "Face for error messages.")

(defface copilot-agent-separator-face
  '((t :inherit shadow))
  "Face for the separator line.")

(defface copilot-agent-prompt-face
  '((t :inherit minibuffer-prompt :weight bold))
  "Face for the > input prompt.")

(defface copilot-agent-thinking-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the Thinking indicator.")

;;; ---------- Buffer-local state ----------

(defconst copilot-agent-ui--buffer-name "*Copilot Agent*")

(defvar-local copilot-agent-ui--session nil
  "Active session plist for this chat buffer.")

(defvar-local copilot-agent-ui--history-end-marker nil
  "Marker at the end of history, just before the separator.
Has insertion-type t so it advances as content is inserted at that point.")

(defvar-local copilot-agent-ui--input-marker nil
  "Marker at the start of the editable input area (after the '> ' prompt).
Has insertion-type nil so it stays put while history grows.")

(defvar-local copilot-agent-ui--thinking-overlay nil
  "Overlay for the Thinking... indicator.")

;;; ---------- Buffer / window ----------

(defun copilot-agent-ui-get-buffer ()
  "Return the chat buffer, creating and initialising it if needed."
  (or (get-buffer copilot-agent-ui--buffer-name)
      (with-current-buffer (get-buffer-create copilot-agent-ui--buffer-name)
        (copilot-agent-mode)
        (copilot-agent-ui--draw-initial-contents)
        (current-buffer))))

(defun copilot-agent-ui-show (&optional session)
  "Pop the chat buffer to the right side, binding SESSION if given."
  (let ((buf (copilot-agent-ui-get-buffer)))
    (when session
      (with-current-buffer buf
        (setq copilot-agent-ui--session session)))
    (pop-to-buffer buf '((display-buffer-reuse-window
                          display-buffer-in-side-window)
                         (side . right)
                         (window-width . 0.38)))
    (with-current-buffer buf
      (goto-char (point-max)))))

(defun copilot-agent-ui--draw-initial-contents ()
  "Insert the welcome header and the persistent separator+input area."
  (let ((inhibit-read-only t))
    ;; Welcome header
    (insert (propertize "Copilot Agent\n"
                        'face '(:weight bold :height 1.3) 'read-only t))
    (insert (propertize "RET / C-c C-c: send  C-c C-n: new session  C-c C-k: clear\n\n"
                        'face 'shadow 'read-only t))
    ;; Snapshot the position BEFORE inserting the separator.
    ;; We create history-end-marker here AFTER the separator so it isn't
    ;; dragged forward by its own insertion-type-t behaviour.
    (let ((history-end-pos (point)))
      ;; Separator + prompt (read-only, permanently at the bottom)
      (insert (propertize (concat (make-string 60 ?─) "\n")
                          'face 'copilot-agent-separator-face 'read-only t))
      ;; The prompt itself must NOT carry read-only: Emacs blocks insertions
      ;; immediately after a read-only character, which would prevent typing.
      (insert (propertize "> " 'face 'copilot-agent-prompt-face))
      ;; input-marker: stays at the start of the user input area
      (setq copilot-agent-ui--input-marker (point-marker))
      (set-marker-insertion-type copilot-agent-ui--input-marker nil)
      ;; history-end-marker: points to just before the separator.
      ;; insertion-type t means it advances when content is inserted at
      ;; that position, so the separator always stays at the bottom.
      (setq copilot-agent-ui--history-end-marker (copy-marker history-end-pos t)))))

;;; ---------- History insertion ----------

(defun copilot-agent-ui--history-insert (text &optional face)
  "Insert TEXT (with optional FACE) into the history area before the separator.
Always uses the history-end-marker so the separator stays at the bottom."
  (with-current-buffer (copilot-agent-ui-get-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (marker-position copilot-agent-ui--history-end-marker))
        (let ((start (point)))
          (insert text)
          (add-text-properties start (point) '(read-only t))
          (when face
            (put-text-property start (point) 'face face)))))))

;;; ---------- Message rendering ----------

(defun copilot-agent-ui-insert-user-message (text)
  (copilot-agent-ui--history-insert
   (concat "\n" (propertize "You" 'face 'copilot-agent-user-face) "\n"))
  (copilot-agent-ui--history-insert (concat text "\n")))

(defun copilot-agent-ui-insert-assistant-text (text)
  (copilot-agent-ui--history-insert
   (concat "\n" (propertize "Assistant" 'face 'copilot-agent-assistant-face) "\n"))
  (copilot-agent-ui--history-insert (concat text "\n")))

(defun copilot-agent-ui-insert-tool-call (name input)
  (copilot-agent-ui--history-insert
   (concat "\n"
           (propertize (format "[Tool: %s]" name) 'face 'copilot-agent-tool-name-face)
           "\n"))
  (when (listp input)
    (dolist (pair input)
      (copilot-agent-ui--history-insert
       (format "  %s: %S\n" (car pair) (cdr pair))
       'font-lock-comment-face))))

(defun copilot-agent-ui-insert-tool-result (name result)
  (copilot-agent-ui--history-insert
   (format "[Result: %s]\n" name) 'copilot-agent-tool-result-face)
  (copilot-agent-ui--history-insert
   (concat result "\n") 'copilot-agent-tool-result-face))

(defun copilot-agent-ui-insert-error (message)
  (copilot-agent-ui--history-insert
   (concat "\n"
           (propertize (format "Error: %s" message) 'face 'copilot-agent-error-face)
           "\n")))

;;; ---------- Thinking indicator ----------

(defun copilot-agent-ui--show-thinking ()
  (with-current-buffer (copilot-agent-ui-get-buffer)
    (copilot-agent-ui--hide-thinking)
    (let ((ov (make-overlay (marker-position copilot-agent-ui--history-end-marker)
                            (marker-position copilot-agent-ui--history-end-marker))))
      (overlay-put ov 'after-string
                   (propertize "\nThinking…\n" 'face 'copilot-agent-thinking-face))
      (setq copilot-agent-ui--thinking-overlay ov))))

(defun copilot-agent-ui--hide-thinking ()
  (when copilot-agent-ui--thinking-overlay
    (delete-overlay copilot-agent-ui--thinking-overlay)
    (setq copilot-agent-ui--thinking-overlay nil)))

;;; ---------- Input area ----------

(defun copilot-agent-ui--get-input ()
  "Return the user's current input text."
  (with-current-buffer (copilot-agent-ui-get-buffer)
    (buffer-substring-no-properties
     (marker-position copilot-agent-ui--input-marker)
     (point-max))))

(defun copilot-agent-ui--clear-input ()
  "Erase the editable input area."
  (with-current-buffer (copilot-agent-ui-get-buffer)
    (delete-region (marker-position copilot-agent-ui--input-marker)
                   (point-max))))

;;; ---------- Tool approval ----------

(defun copilot-agent-ui-approve-tool (name input session)
  "Prompt in the minibuffer to approve running tool NAME.
Returns t if approved, nil if declined."
  (if (plist-get session :approve-all)
      t
    (let* ((args-str (if (listp input)
                         (mapconcat (lambda (p) (format "%s: %S" (car p) (cdr p)))
                                    input "  ")
                       (format "%S" input)))
           (ch (read-char-choice
                (format "Run tool `%s'  %s\n[y]es / [a]ll / [n]o: " name args-str)
                '(?y ?Y ?a ?A ?n ?N))))
      (pcase (downcase ch)
        (?y t)
        (?a (plist-put session :approve-all t) t)
        (?n nil)))))

;;; ---------- Send ----------

(defun copilot-agent-ui--refresh-context (session)
  "Update SESSION :context-buffer to the most recently focused file buffer.
Iterates `buffer-list' (MRU order) and picks the first live buffer
visiting a file that is not the agent chat buffer itself.  This ensures
that switching to a different file between messages moves the tool
context to that file without requiring a new session."
  (let ((agent-buf (current-buffer))
        (src (cl-find-if
              (lambda (b)
                (and (not (eq b (current-buffer)))
                     (buffer-live-p b)
                     (buffer-file-name b)))
              (buffer-list))))
    (when src
      (plist-put session :context-buffer src)
      (copilot-agent-tools-set-context src))))

(defun copilot-agent-ui-send ()
  "Send the current input to the active agent session."
  (interactive)
  (with-current-buffer (copilot-agent-ui-get-buffer)
    (let ((input   (string-trim (copilot-agent-ui--get-input)))
          (session copilot-agent-ui--session))
      (when (string-empty-p input)
        (user-error "Please enter a message"))
      (unless session
        (user-error "No active session — use M-x copilot-agent to start one"))
      ;; Always re-anchor to the most recently visited file buffer so the
      ;; user does not need to start a new session after switching files.
      (copilot-agent-ui--refresh-context session)
      (copilot-agent-ui-insert-user-message input)
      (copilot-agent-ui--clear-input)
      (copilot-agent-ui--show-thinking)
      (copilot-agent-api-send
       session input
       (list
        :on-thinking    (lambda () (copilot-agent-ui--show-thinking))
        :on-text        (lambda (text)
                          (copilot-agent-ui--hide-thinking)
                          (copilot-agent-ui-insert-assistant-text text))
        :on-tool-call   #'copilot-agent-ui-insert-tool-call
        :on-tool-result #'copilot-agent-ui-insert-tool-result
        :on-approve     (lambda (name input session)
                          (copilot-agent-ui-approve-tool name input session))
        :on-done        (lambda ()
                          (copilot-agent-ui--hide-thinking)
                          (with-current-buffer (copilot-agent-ui-get-buffer)
                            (goto-char (point-max))))
        :on-error       (lambda (msg)
                          (copilot-agent-ui--hide-thinking)
                          (copilot-agent-ui-insert-error msg)))))))

;;; ---------- Major mode ----------

(defvar copilot-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     #'copilot-agent-ui-send)
    (define-key map (kbd "C-c C-c") #'copilot-agent-ui-send)
    (define-key map (kbd "C-c C-n") #'copilot-agent-new-chat)
    (define-key map (kbd "C-c C-k") #'copilot-agent-clear-history)
    (define-key map (kbd "C-c C-p") #'copilot-agent-ui--set-provider)
    map)
  "Keymap for `copilot-agent-mode'.")

(define-derived-mode copilot-agent-mode fundamental-mode "Copilot-Agent"
  "Major mode for the Copilot Agent chat buffer.
The buffer is NOT globally read-only; only the history area is protected
via text properties.  The input area at the bottom is freely editable.
\\{copilot-agent-mode-map}"
  (setq-local scroll-conservatively 101
              truncate-lines        nil
              word-wrap             t)
  (buffer-disable-undo))

;;; ---------- Provider switcher ----------

(defun copilot-agent-ui--set-provider ()
  "Interactively switch the provider for the current session."
  (interactive)
  (let* ((providers (mapcar #'symbol-name (copilot-agent-api-list-providers)))
         (choice    (completing-read "Provider: " providers nil t))
         (sym       (intern choice)))
    (when copilot-agent-ui--session
      (plist-put copilot-agent-ui--session :provider sym)
      (plist-put copilot-agent-ui--session :model
                 (plist-get (copilot-agent-api--get-provider sym) :default-model)))
    (setq copilot-agent-provider sym)
    (message "Switched to provider: %s" choice)))

(provide 'copilot-agent-ui)
;;; copilot-agent-ui.el ends here

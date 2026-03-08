;;; copilot-agent.el --- AI coding agent for Emacs -*- lexical-binding: t -*-

;; Author: emacs-copilot-agent contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ai llm agent tools copilot
;; URL: https://github.com/your-org/emacs-copilot-agent

;;; Commentary:
;; An Emacs AI coding agent similar to VSCode Copilot Chat.
;;
;; Supports multiple LLM providers (Anthropic Claude, Google Gemini, …)
;; via a pluggable provider registry.  Tool use (function calling) enables
;; the agent to run shell commands, read/write files, and search code.
;;
;; All tools are TRAMP-aware: when editing a remote file via SSH, tool
;; commands execute on the remote host transparently.
;;
;; Quick start:
;;   1. Add API key to ~/.authinfo (see provider files for format)
;;   2. (require 'copilot-agent)
;;   3. M-x copilot-agent
;;
;; Key bindings in the chat buffer:
;;   RET / C-c C-c  send message
;;   C-c C-n        new session
;;   C-c C-k        clear history

;;; Code:

(require 'copilot-agent-tools)
(require 'copilot-agent-api)
(require 'copilot-agent-ui)

;;; ---------- Customisation ----------

(defgroup copilot-agent nil
  "AI coding agent powered by LLMs."
  :group 'tools
  :prefix "copilot-agent-")

(defcustom copilot-agent-provider 'anthropic
  "Default LLM provider symbol.  Must be a registered provider."
  :type '(choice (const :tag "Anthropic Claude"       anthropic)
                 (const :tag "Google Gemini"          gemini)
                 (const :tag "Alibaba Qwen (free)"    qwen)
                 (symbol :tag "Other"))
  :group 'copilot-agent)

(defcustom copilot-agent-system-prompt
  "You are an expert coding assistant embedded in Emacs, similar to GitHub Copilot Chat.
You help the user with software engineering tasks: writing code, debugging, refactoring,
explaining concepts, and executing commands.

You have access to tools that let you run shell commands, read and write files, and
search the codebase.  Always prefer using tools to inspect the actual state of the
system rather than making assumptions.

When running shell commands, be concise and targeted.  Ask for approval before
destructive operations.  If the context directory is a remote path (TRAMP/SSH),
your tool commands will run on the remote host automatically."
  "System prompt sent to the LLM at the start of every session."
  :type 'string
  :group 'copilot-agent)

(defcustom copilot-agent-auto-context t
  "If non-nil, automatically include the current buffer's file path in the prompt."
  :type 'boolean
  :group 'copilot-agent)

(defcustom copilot-agent-max-tokens 8192
  "Maximum tokens for LLM responses."
  :type 'integer
  :group 'copilot-agent)

;;; ---------- Session Helpers ----------

(defun copilot-agent--make-session (&optional context-buffer)
  "Create a new session, optionally using CONTEXT-BUFFER as the working context."
  (let* ((buf      (or context-buffer (current-buffer)))
         (prompt   (if (and copilot-agent-auto-context
                            (buffer-file-name buf))
                       (format "%s\n\nCurrent file: %s"
                               copilot-agent-system-prompt
                               (buffer-file-name buf))
                     copilot-agent-system-prompt)))
    (copilot-agent-api-new-session
     copilot-agent-provider
     :system-prompt  prompt
     :max-tokens     copilot-agent-max-tokens
     :context-buffer buf)))

;;; ---------- Interactive Commands ----------

;;;###autoload
(defun copilot-agent ()
  "Open the Copilot Agent chat buffer.
Sets the context to the current buffer so file operations and shell
commands target the right directory (including remote SSH via TRAMP)."
  (interactive)
  (let* ((ctx-buf (current-buffer))
         (session (copilot-agent--make-session ctx-buf)))
    (copilot-agent-tools-set-context ctx-buf)
    (copilot-agent-ui-show session)))

;;;###autoload
(defun copilot-agent-explain-region (beg end)
  "Send the selected region to the agent with an explanation request."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((code    (buffer-substring-no-properties beg end))
         (lang    (or (and (derived-mode-p 'prog-mode)
                           (string-remove-suffix "-mode"
                                                 (symbol-name major-mode)))
                      ""))
         (prompt  (format "Please explain the following %s code:\n\n```%s\n%s\n```"
                          lang lang code))
         (session (copilot-agent--make-session (current-buffer))))
    (copilot-agent-ui-show session)
    (with-current-buffer (copilot-agent-ui-get-buffer)
      (setq copilot-agent-ui--session session))
    ;; Pre-fill and submit
    (copilot-agent-ui-insert-user-message prompt)
    (copilot-agent-ui--show-thinking)
    (copilot-agent-api-send
     session prompt
     (list :on-thinking  #'copilot-agent-ui--show-thinking
           :on-text      (lambda (t)
                           (copilot-agent-ui--hide-thinking)
                           (copilot-agent-ui-insert-assistant-text t))
           :on-tool-call   #'copilot-agent-ui-insert-tool-call
           :on-tool-result #'copilot-agent-ui-insert-tool-result
           :on-approve     (lambda (n i s) (copilot-agent-ui-approve-tool n i s))
           :on-done      (lambda ()
                           (copilot-agent-ui--hide-thinking)
                           (copilot-agent-ui--insert-prompt))
           :on-error     (lambda (m)
                           (copilot-agent-ui--hide-thinking)
                           (copilot-agent-ui-insert-error m)
                           (copilot-agent-ui--insert-prompt))))))

;;;###autoload
(defun copilot-agent-fix-errors ()
  "Send the current buffer's compile/flycheck errors to the agent for fixing."
  (interactive)
  (let* ((errors
          (cond
           ;; Flycheck
           ((and (fboundp 'flycheck-error-list-mode)
                 (bound-and-true-p flycheck-current-errors))
            (mapconcat (lambda (e)
                         (format "%s:%d:%d: %s: %s"
                                 (buffer-file-name)
                                 (flycheck-error-line e)
                                 (flycheck-error-column e)
                                 (flycheck-error-level e)
                                 (flycheck-error-message e)))
                       flycheck-current-errors "\n"))
           ;; Compilation buffer
           ((get-buffer "*compilation*")
            (with-current-buffer "*compilation*"
              (buffer-substring-no-properties (point-min) (point-max))))
           (t (user-error "No errors found (run compilation or flycheck first)"))))
         (prompt (format "Please fix the following errors:\n\n```\n%s\n```" errors))
         (session (copilot-agent--make-session (current-buffer))))
    (copilot-agent-ui-show session)
    (copilot-agent-api-send
     session prompt
     (list :on-text      (lambda (text)
                           (copilot-agent-ui--hide-thinking)
                           (copilot-agent-ui-insert-assistant-text text))
           :on-tool-call   #'copilot-agent-ui-insert-tool-call
           :on-tool-result #'copilot-agent-ui-insert-tool-result
           :on-approve     (lambda (n i s) (copilot-agent-ui-approve-tool n i s))
           :on-done      (lambda ()
                           (copilot-agent-ui--hide-thinking)
                           (copilot-agent-ui--insert-prompt))
           :on-error     (lambda (m)
                           (copilot-agent-ui--hide-thinking)
                           (copilot-agent-ui-insert-error m))))))

;;;###autoload
(defun copilot-agent-new-chat ()
  "Start a fresh agent session, discarding history."
  (interactive)
  (let ((session (copilot-agent--make-session)))
    (with-current-buffer (copilot-agent-ui-get-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq copilot-agent-ui--session  session
            copilot-agent-ui--input-marker nil)
      (copilot-agent-ui--insert-welcome)
      (copilot-agent-ui--insert-prompt))
    (message "New Copilot Agent session started")))

;;;###autoload
(defun copilot-agent-clear-history ()
  "Clear the message history of the current session (keeps the session open)."
  (interactive)
  (with-current-buffer (copilot-agent-ui-get-buffer)
    (when copilot-agent-ui--session
      (plist-put copilot-agent-ui--session :messages '()))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq copilot-agent-ui--input-marker nil)
    (copilot-agent-ui--insert-welcome)
    (copilot-agent-ui--insert-prompt))
  (message "History cleared"))

;;; ---------- Optional Keybinding ----------

(defcustom copilot-agent-keymap-prefix "C-c /"
  "Prefix key for copilot-agent commands.  Set before loading this package."
  :type 'string
  :group 'copilot-agent)

(defvar copilot-agent-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'copilot-agent)
    (define-key map (kbd "e") #'copilot-agent-explain-region)
    (define-key map (kbd "f") #'copilot-agent-fix-errors)
    (define-key map (kbd "n") #'copilot-agent-new-chat)
    map)
  "Keymap for copilot-agent commands.")

(fset 'copilot-agent-command-map copilot-agent-command-map)

(defun copilot-agent-setup-keybindings ()
  "Bind `copilot-agent-keymap-prefix' to the command map."
  (global-set-key (kbd copilot-agent-keymap-prefix) 'copilot-agent-command-map))

;;; ---------- Autoloads for Providers ----------

;; Load providers so they self-register via `with-eval-after-load'.
(defun copilot-agent-load-providers ()
  "Load all bundled providers."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "providers/copilot-agent-anthropic" dir) t)
    (load (expand-file-name "providers/copilot-agent-gemini"    dir) t)
    (load (expand-file-name "providers/copilot-agent-qwen"      dir) t)))

(copilot-agent-load-providers)

(provide 'copilot-agent)
;;; copilot-agent.el ends here

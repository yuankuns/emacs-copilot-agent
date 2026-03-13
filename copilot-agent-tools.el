;;; copilot-agent-tools.el --- Tool definitions and TRAMP-aware execution -*- lexical-binding: t -*-

;;; Commentary:
;; Defines the tool schema (sent to LLMs) and their implementations.
;; All tools are TRAMP-aware: when the active context directory is a remote
;; path (e.g. /ssh:user@host:/path/), commands execute on the remote host
;; transparently via `process-file' and `default-directory'.

;;; Code:

(require 'tramp nil t)

;;; ---------- Universal Tool Schema ----------
;; Uses lowercase JSON-Schema conventions.
;; Each provider converts this to its own wire format.

(defconst copilot-agent-tools-schema
  '(((name . "shell_command")
     (description . "Execute a shell command in the current working directory. \
When the context is a remote SSH path (TRAMP), the command runs on the remote host.")
     (parameters . ((type . "object")
                    (properties
                     . ((command . ((type . "string")
                                   (description . "Shell command to run")))
                        (cwd     . ((type . "string")
                                   (description . "Working directory override (optional)")))))
                    (required . ["command"]))))

    ((name . "read_file")
     (description . "Read the full contents of a file. Works on remote files via TRAMP.")
     (parameters . ((type . "object")
                    (properties
                     . ((path . ((type . "string")
                                 (description . "Absolute or relative file path")))))
                    (required . ["path"]))))

    ((name . "write_file")
     (description . "Write content to a file, creating it (and any parent directories) \
if needed. Works on remote files via TRAMP.")
     (parameters . ((type . "object")
                    (properties
                     . ((path    . ((type . "string")
                                   (description . "Absolute or relative file path")))
                        (content . ((type . "string")
                                   (description . "Full content to write")))))
                    (required . ["path" "content"]))))

    ((name . "list_directory")
     (description . "List the contents of a directory with file sizes and timestamps.")
     (parameters . ((type . "object")
                    (properties
                     . ((path . ((type . "string")
                                 (description . "Directory path (defaults to context directory)")))))
                    (required . []))))

    ((name . "find_in_files")
     (description . "Search for a regex pattern inside files (like grep -rn). \
Returns matches with optional surrounding context lines. \
Use before_context/after_context to see code around each match \
without reading the whole file.")
     (parameters . ((type . "object")
                    (properties
                     . ((pattern        . ((type . "string")
                                          (description . "Extended-regex pattern to search")))
                        (path           . ((type . "string")
                                          (description . "Root directory or file to search")))
                        (glob           . ((type . "string")
                                          (description . "Filename glob filter, e.g. '*.py' (optional)")))
                        (before_context . ((type . "integer")
                                          (description . "Lines of context before each match (like grep -B)")))
                        (after_context  . ((type . "integer")
                                          (description . "Lines of context after each match (like grep -A)")))))
                    (required . ["pattern"]))))

    ((name . "create_directory")
     (description . "Create a directory, including any missing parent directories.")
     (parameters . ((type . "object")
                    (properties
                     . ((path . ((type . "string")
                                 (description . "Directory path to create")))))
                    (required . ["path"]))))

    ((name . "delete_file")
     (description . "Permanently delete a file. Use with caution.")
     (parameters . ((type . "object")
                    (properties
                     . ((path . ((type . "string")
                                 (description . "Path to the file to delete")))))
                    (required . ["path"])))))
  "Universal tool schema list.
Format: list of alists with `name', `description', `parameters'.")

;;; ---------- Execution Context ----------

(defvar copilot-agent-tools--context nil
  "Plist with :directory (string) and :buffer (buffer).
Tracks the active editing context; set via `copilot-agent-tools-set-context'.")

(defun copilot-agent-tools-set-context (buffer)
  "Set execution context from BUFFER's `default-directory'."
  (when (buffer-live-p buffer)
    (setq copilot-agent-tools--context
          (list :directory (buffer-local-value 'default-directory buffer)
                :buffer buffer))))

(defun copilot-agent-tools--ctx-dir ()
  "Return the context working directory."
  (or (plist-get copilot-agent-tools--context :directory)
      default-directory))

(defun copilot-agent-tools--resolve (path)
  "Resolve PATH relative to context directory.
TRAMP paths pass through unchanged.  Local-absolute paths are anchored
to the remote host when the context directory is itself remote (so the
LLM does not need to produce TRAMP syntax).  Relative paths are
expanded against the context directory."
  (cond
   ;; Already a TRAMP path — use as-is
   ((and (featurep 'tramp) (tramp-tramp-file-p path)) path)
   ;; Local-absolute path with a remote context — prepend remote prefix
   ((file-name-absolute-p path)
    (let ((remote (file-remote-p (copilot-agent-tools--ctx-dir))))
      (if remote (concat remote path) path)))
   ;; Relative path — expand against context directory (TRAMP-aware)
   (t (expand-file-name path (copilot-agent-tools--ctx-dir)))))

;;; ---------- TRAMP Utilities ----------

(defun copilot-agent-tools--remote-p (dir)
  "Return non-nil if DIR is a TRAMP remote path."
  (and (featurep 'tramp) (tramp-tramp-file-p dir)))

(defun copilot-agent-tools--host-label (dir)
  "Return \"user@host\" label for a TRAMP DIR, or nil for local paths."
  (when (copilot-agent-tools--remote-p dir)
    (with-parsed-tramp-file-name dir v
      (if (and v-user (not (string-empty-p v-user)))
          (format "%s@%s" v-user v-host)
        v-host))))

;;; ---------- Dispatcher ----------

(defun copilot-agent-tools-execute (name args)
  "Run tool NAME with ARGS (alist decoded from JSON). Returns a result string."
  (condition-case err
      (pcase name
        ("shell_command"    (copilot-agent-tools--shell args))
        ("read_file"        (copilot-agent-tools--read-file args))
        ("write_file"       (copilot-agent-tools--write-file args))
        ("list_directory"   (copilot-agent-tools--list-dir args))
        ("find_in_files"    (copilot-agent-tools--find-in-files args))
        ("create_directory" (copilot-agent-tools--create-dir args))
        ("delete_file"      (copilot-agent-tools--delete-file args))
        (_ (format "Unknown tool: %s" name)))
    (error (format "Tool execution error: %s" (error-message-string err)))))

;;; ---------- Tool Implementations ----------

(defun copilot-agent-tools--shell (args)
  (let* ((command (cdr (assq 'command args)))
         (raw-cwd (cdr (assq 'cwd args)))
         (cwd (if (and raw-cwd (not (string-empty-p raw-cwd)))
                  (copilot-agent-tools--resolve raw-cwd)
                (copilot-agent-tools--ctx-dir)))
         (default-directory cwd)
         (host (copilot-agent-tools--host-label cwd)))
    (with-temp-buffer
      (let* ((exit-code (process-file shell-file-name nil t nil
                                      shell-command-switch command))
             (output (buffer-string))
             (header (if host
                         (format "[remote:%s] $ %s\n" host command)
                       (format "$ %s\n" command))))
        (concat header
                (if (string-empty-p output) "(no output)\n" output)
                (unless (zerop exit-code)
                  (format "\n[exit code: %d]" exit-code)))))))

(defun copilot-agent-tools--read-file (args)
  (let ((path (copilot-agent-tools--resolve (cdr (assq 'path args)))))
    (unless (file-exists-p path)
      (error "File not found: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun copilot-agent-tools--write-file (args)
  (let* ((path    (copilot-agent-tools--resolve (cdr (assq 'path args))))
         (content (cdr (assq 'content args))))
    (make-directory (file-name-directory path) t)
    ;; Use find-file-noselect so that:
    ;; 1. If the file is already open in a buffer, that buffer is updated in-place
    ;;    and the change is immediately visible to the user without a manual revert.
    ;; 2. erase-buffer + insert inside atomic-change-group makes the entire agent
    ;;    write a single undo entry — C-/ once reverts the whole change.
    ;; 3. Works transparently for TRAMP remote paths (find-file-noselect is TRAMP-aware).
    (let ((buf (find-file-noselect path)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          ;; with-undo-amalgamate groups erase+insert into one undo entry so
          ;; the user can revert the entire agent edit with a single C-/.
          (with-undo-amalgamate
            (erase-buffer)
            (insert content)))
        ;; require-final-newline nil: write exactly what the agent produced;
        ;; do not let Emacs silently append a newline.
        (let ((require-final-newline nil))
          (save-buffer))))
    (format "Wrote %d bytes to %s" (length content) path)))

(defun copilot-agent-tools--list-dir (args)
  (let* ((raw  (cdr (assq 'path args)))
         (path (if (and raw (not (string-empty-p raw)))
                   (copilot-agent-tools--resolve raw)
                 (copilot-agent-tools--ctx-dir))))
    (unless (file-directory-p path)
      (error "Not a directory: %s" path))
    (let ((entries (directory-files-and-attributes path nil nil t)))
      (mapconcat
       (lambda (e)
         (let* ((name  (car e))
                (attrs (cdr e))
                (dir?  (eq t (file-attribute-type attrs)))
                (size  (file-attribute-size attrs))
                (mtime (format-time-string
                        "%Y-%m-%d %H:%M"
                        (file-attribute-modification-time attrs))))
           (format "%s  %8s  %s%s"
                   mtime
                   (if dir? "DIR" (number-to-string size))
                   name (if dir? "/" ""))))
       (seq-filter (lambda (e) (not (member (car e) '("." ".."))))
                   entries)
       "\n"))))

(defun copilot-agent-tools--find-in-files (args)
  (let* ((pattern  (cdr (assq 'pattern args)))
         (raw-path (cdr (assq 'path args)))
         (glob     (cdr (assq 'glob args)))
         (before   (cdr (assq 'before_context args)))
         (after    (cdr (assq 'after_context  args)))
         (path     (if (and raw-path (not (string-empty-p raw-path)))
                       (copilot-agent-tools--resolve raw-path)
                     (copilot-agent-tools--ctx-dir)))
         (default-directory path)
         (glob-flag (if (and glob (not (string-empty-p glob)))
                        (format "--include='%s' " glob)
                      ""))
         (ctx-flags (concat (if (and before (> before 0))
                                (format "-B %d " before) "")
                            (if (and after  (> after  0))
                                (format "-A %d " after)  "")))
         (cmd (format "grep -rn -E %s%s'%s' ." glob-flag ctx-flags pattern)))
    (with-temp-buffer
      (process-file shell-file-name nil t nil shell-command-switch cmd)
      (let* ((out   (buffer-string))
             (lines (split-string out "\n" t)))
        (if (null lines)
            (format "No matches for '%s'" pattern)
          (let ((capped (if (> (length lines) 200)
                            (append (seq-take lines 200)
                                    (list (format "... (%d more lines omitted)"
                                                  (- (length lines) 200))))
                          lines)))
            (mapconcat #'identity capped "\n")))))))

(defun copilot-agent-tools--create-dir (args)
  (let ((path (copilot-agent-tools--resolve (cdr (assq 'path args)))))
    (make-directory path t)
    (format "Created: %s" path)))

(defun copilot-agent-tools--delete-file (args)
  (let ((path (copilot-agent-tools--resolve (cdr (assq 'path args)))))
    (unless (file-exists-p path)
      (error "File not found: %s" path))
    (delete-file path)
    (format "Deleted: %s" path)))

(provide 'copilot-agent-tools)
;;; copilot-agent-tools.el ends here

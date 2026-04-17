;;; copilot-agent-tools.el --- Tool definitions and TRAMP-aware execution -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

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
Returns file:line:match triples, capped at 200 lines.")
     (parameters . ((type . "object")
                    (properties
                     . ((pattern . ((type . "string")
                                   (description . "Extended-regex pattern to search")))
                        (path    . ((type . "string")
                                   (description . "Root directory or file to search")))
                        (glob    . ((type . "string")
                                   (description . "Filename glob filter, e.g. '*.py' (optional)")))))
                    (required . ["pattern"]))))

    ((name . "glob")
     (description . "Find files whose names match a glob pattern (e.g. \"*.el\", \"test_*.py\"). \
Returns relative paths one per line, sorted. Use this to discover files by name \
without reading their contents.")
     (parameters . ((type . "object")
                    (properties
                     . ((pattern . ((type . "string")
                                   (description . "Filename glob, e.g. \"*.el\" or \"test_*.py\"")))
                        (path    . ((type . "string")
                                   (description . "Root directory to search (defaults to context directory)")))))
                    (required . ["pattern"]))))

    ((name . "grep")
     (description . "Search for a regex pattern in file contents, returning \
file:line:match triples with optional surrounding context lines. \
Prefer this over read_file when you need to find specific content without \
loading the whole file.")
     (parameters . ((type . "object")
                    (properties
                     . ((pattern        . ((type . "string")
                                          (description . "Extended-regex pattern to search")))
                        (path           . ((type . "string")
                                          (description . "Root directory or file to search")))
                        (glob           . ((type . "string")
                                          (description . "Filename glob filter, e.g. \"*.py\" (optional)")))
                        (before_context . ((type . "integer")
                                          (description . "Lines of context before each match (default 0)")))
                        (after_context  . ((type . "integer")
                                          (description . "Lines of context after each match (default 0)")))))
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
                    (required . ["path"]))))

    ((name . "edit_file")
     (description . "Make a surgical edit to a file by replacing an exact string with a new string. \
Preferred over write_file when modifying existing code, as it avoids rewriting the whole file. \
The old_string must match exactly once in the file; use enough context to make it unique.")
     (parameters . ((type . "object")
                    (properties
                     . ((path       . ((type . "string")
                                      (description . "Absolute or relative file path")))
                        (old_string . ((type . "string")
                                      (description . "Exact string to replace (must appear exactly once)")))
                        (new_string . ((type . "string")
                                      (description . "Replacement string")))))
                    (required . ["path" "old_string" "new_string"])))))
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
  "Run tool NAME with ARGS (alist decoded from JSON).  Return a result string."
  (condition-case err
      (pcase name
        ("shell_command"    (copilot-agent-tools--shell args))
        ("read_file"        (copilot-agent-tools--read-file args))
        ("write_file"       (copilot-agent-tools--write-file args))
        ("list_directory"   (copilot-agent-tools--list-dir args))
        ("find_in_files"    (copilot-agent-tools--find-in-files args))
        ("glob"             (copilot-agent-tools--glob args))
        ("grep"             (copilot-agent-tools--grep args))
        ("create_directory" (copilot-agent-tools--create-dir args))
        ("delete_file"      (copilot-agent-tools--delete-file args))
        ("edit_file"        (copilot-agent-tools--edit-file args))
        (_ (format "Unknown tool: %s" name)))
    (error (format "Tool execution error: %s" (error-message-string err)))))

;;; ---------- Tool Implementations ----------

(defun copilot-agent-tools--shell (args)
  "Execute a shell command described by ARGS and return its output."
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
  "Read and return the contents of the file described by ARGS."
  (let ((path (copilot-agent-tools--resolve (cdr (assq 'path args)))))
    (unless (file-exists-p path)
      (error "File not found: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun copilot-agent-tools--write-file (args)
  "Write content to the file described by ARGS, creating it if needed."
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
  "List directory contents described by ARGS and return a formatted string."
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
  "Search for a regex pattern in files described by ARGS using grep."
  (let* ((pattern (cdr (assq 'pattern args)))
         (raw-path (cdr (assq 'path args)))
         (glob    (cdr (assq 'glob args)))
         (path    (if (and raw-path (not (string-empty-p raw-path)))
                      (copilot-agent-tools--resolve raw-path)
                    (copilot-agent-tools--ctx-dir)))
         (default-directory path)
         (glob-flag (if (and glob (not (string-empty-p glob)))
                        (format "--include='%s' " glob)
                      ""))
         (cmd (format "grep -rn -E %s'%s' ." glob-flag pattern)))
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
  "Create the directory described by ARGS, including any missing parents."
  (let ((path (copilot-agent-tools--resolve (cdr (assq 'path args)))))
    (make-directory path t)
    (format "Created: %s" path)))

(defun copilot-agent-tools--delete-file (args)
  "Delete the file described by ARGS."
  (let ((path (copilot-agent-tools--resolve (cdr (assq 'path args)))))
    (unless (file-exists-p path)
      (error "File not found: %s" path))
    (delete-file path)
    (format "Deleted: %s" path)))

(defun copilot-agent-tools--edit-file (args)
  "Replace OLD_STRING with NEW_STRING in the file at PATH from ARGS.
Signal an error if the file does not exist, if OLD_STRING is not found,
or if OLD_STRING matches more than once (ambiguous edit)."
  (let* ((path       (copilot-agent-tools--resolve (cdr (assq 'path args))))
         (old-string (cdr (assq 'old_string args)))
         (new-string (cdr (assq 'new_string args))))
    (unless (file-exists-p path)
      (error "File not found: %s" path))
    (let* ((buf     (find-file-noselect path))
           (content (with-current-buffer buf (buffer-string)))
           ;; Count occurrences using plain string search (no regex).
           (count   (let ((pos 0) (n 0))
                      (while (setq pos (string-search old-string content pos))
                        (setq n (1+ n))
                        (setq pos (+ pos (length old-string))))
                      n)))
      (cond
       ((= count 0)
        (error "Edit_file: old_string not found in %s" path))
       ((> count 1)
        (error "Edit_file: old_string matches %d times in %s — add more context to make it unique"
               count path)))
      ;; Exactly one match — replace it.
      (let ((new-content (concat
                          (substring content 0 (string-search old-string content))
                          new-string
                          (substring content (+ (string-search old-string content)
                                                (length old-string))))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (with-undo-amalgamate
              (erase-buffer)
              (insert new-content))))
        (let ((require-final-newline nil))
          (with-current-buffer buf (save-buffer))))
      (format "Edited %s: replaced %d chars with %d chars"
              path (length old-string) (length new-string)))))

(defun copilot-agent-tools--glob (args)
  "Find files matching PATTERN under PATH described by ARGS.
Uses `find -name' so it recurses into subdirectories and is TRAMP-aware
via `process-file'.  Returns relative paths, one per line, sorted."
  (let* ((pattern  (cdr (assq 'pattern args)))
         (raw-path (cdr (assq 'path args)))
         (base     (if (and raw-path (not (string-empty-p raw-path)))
                       (copilot-agent-tools--resolve raw-path)
                     (copilot-agent-tools--ctx-dir)))
         ;; Use only the filename portion for -name; strip any leading **/ or path prefix.
         (name-pat (file-name-nondirectory pattern))
         (default-directory base)
         (cmd (format "find . -type f -name '%s' | sort" name-pat)))
    (with-temp-buffer
      (process-file shell-file-name nil t nil shell-command-switch cmd)
      (let* ((out   (string-trim (buffer-string)))
             (lines (if (string-empty-p out) nil (split-string out "\n" t))))
        (if (null lines)
            (format "No files matching '%s' under %s" pattern base)
          (let ((capped (if (> (length lines) 500)
                            (append (seq-take lines 500)
                                    (list (format "... (%d more files omitted)"
                                                  (- (length lines) 500))))
                          lines)))
            (mapconcat #'identity capped "\n")))))))

(defun copilot-agent-tools--grep (args)
  "Search files described by ARGS for PATTERN with optional context lines.
Like `find_in_files' but supports BEFORE_CONTEXT and AFTER_CONTEXT."
  (let* ((pattern  (cdr (assq 'pattern args)))
         (raw-path (cdr (assq 'path args)))
         (glob     (cdr (assq 'glob args)))
         (before   (let ((v (cdr (assq 'before_context args))))
                     (if (and v (wholenump v)) v 0)))
         (after    (let ((v (cdr (assq 'after_context args))))
                     (if (and v (wholenump v)) v 0)))
         (path     (if (and raw-path (not (string-empty-p raw-path)))
                       (copilot-agent-tools--resolve raw-path)
                     (copilot-agent-tools--ctx-dir)))
         (default-directory path)
         (include-flag (if (and glob (not (string-empty-p glob)))
                           (format "--include='%s' " glob)
                         ""))
         (ctx-flags (concat (if (> before 0) (format "-B %d " before) "")
                            (if (> after 0)  (format "-A %d " after)  "")))
         (cmd (format "grep -rn -E %s%s'%s' ." include-flag ctx-flags pattern)))
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

(provide 'copilot-agent-tools)
;;; copilot-agent-tools.el ends here

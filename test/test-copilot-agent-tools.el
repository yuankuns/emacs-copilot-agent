;;; test-copilot-agent-tools.el --- ERT tests for copilot-agent-tools -*- lexical-binding: t -*-

;;; Commentary:
;; Regression tests for copilot-agent-tools.el.
;; Covers: context management, path resolution, TRAMP detection,
;; and every tool implementation (shell, read, write, list, grep, mkdir, delete, dispatch).
;;
;; Run: emacs -batch -L .. -l test-copilot-agent-tools.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load-path when running standalone
(let ((parent (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (unless (member parent load-path)
    (push parent load-path)))

(require 'copilot-agent-tools)

;;; ---------- Helpers ----------

(defmacro with-temp-dir (&rest body)
  "Execute BODY with `default-directory' set to a fresh temporary directory.
The directory is deleted after BODY completes."
  (declare (indent 0))
  `(let ((tmp (make-temp-file "copilot-test-" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory tmp)))
           ,@body)
       (delete-directory tmp t))))

;;; ---------- Context ----------

(ert-deftest tools/set-context-from-live-buffer ()
  "set-context stores :directory and :buffer from a live buffer."
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (copilot-agent-tools-set-context (current-buffer))
    (should (equal (plist-get copilot-agent-tools--context :directory)
                   temporary-file-directory))
    (should (eq (plist-get copilot-agent-tools--context :buffer)
                (current-buffer)))))

(ert-deftest tools/set-context-ignores-dead-buffer ()
  "set-context is a no-op when given a dead buffer."
  (setq copilot-agent-tools--context nil)
  (let ((dead (generate-new-buffer "dead-buf")))
    (kill-buffer dead)
    (copilot-agent-tools-set-context dead))
  (should (null copilot-agent-tools--context)))

(ert-deftest tools/ctx-dir-falls-back-to-default-directory ()
  "ctx-dir returns default-directory when no context is set."
  (let ((copilot-agent-tools--context nil)
        (default-directory "/tmp/"))
    (should (equal (copilot-agent-tools--ctx-dir) "/tmp/"))))

;;; ---------- Path Resolution ----------

(ert-deftest tools/resolve-absolute-passes-through ()
  "Local-absolute paths are returned unchanged when context is also local."
  (let ((copilot-agent-tools--context (list :directory "/ctx/")))
    (should (equal (copilot-agent-tools--resolve "/etc/hosts") "/etc/hosts"))))

(ert-deftest tools/resolve-relative-expands-against-ctx ()
  "Relative paths are expanded against the context directory."
  (let ((copilot-agent-tools--context (list :directory "/ctx/")))
    (should (equal (copilot-agent-tools--resolve "foo.txt") "/ctx/foo.txt"))))

(ert-deftest tools/resolve-empty-uses-ctx-dir ()
  "An empty relative path resolves inside the context directory."
  (let ((copilot-agent-tools--context (list :directory "/mydir/")))
    ;; expand-file-name strips the trailing slash, so just check the prefix
    (should (string-prefix-p "/mydir" (copilot-agent-tools--resolve "")))))

(ert-deftest tools/resolve-local-absolute-in-remote-context ()
  "Local-absolute paths are anchored to the remote host when context is remote.
This is the core TRAMP bug: LLM generates /home/user/file.py but context is
/ssh:host:/home/user/ — without the fix the file was written locally."
  (let ((copilot-agent-tools--context
         (list :directory "/ssh:user@host:/home/user/project/")))
    (should (equal (copilot-agent-tools--resolve "/home/user/project/file.py")
                   "/ssh:user@host:/home/user/project/file.py"))))

(ert-deftest tools/resolve-relative-in-remote-context ()
  "Relative paths are expanded against the remote context directory."
  (let ((copilot-agent-tools--context
         (list :directory "/ssh:user@host:/home/user/project/")))
    (should (equal (copilot-agent-tools--resolve "src/main.py")
                   "/ssh:user@host:/home/user/project/src/main.py"))))

(ert-deftest tools/resolve-tramp-path-passes-through ()
  "A full TRAMP path is never modified regardless of context."
  (let ((copilot-agent-tools--context (list :directory "/ctx/")))
    (should (equal (copilot-agent-tools--resolve "/ssh:host:/etc/hosts")
                   "/ssh:host:/etc/hosts"))))

;;; ---------- TRAMP Detection ----------

(ert-deftest tools/remote-p-local-returns-nil ()
  "A local path is not detected as remote."
  (should-not (copilot-agent-tools--remote-p "/home/user/")))

(ert-deftest tools/remote-p-ssh-path-returns-true ()
  "A TRAMP SSH path is detected as remote (when TRAMP is available)."
  (skip-unless (featurep 'tramp))
  (should (copilot-agent-tools--remote-p "/ssh:user@host:/path/")))

(ert-deftest tools/host-label-local-returns-nil ()
  "host-label returns nil for local paths."
  (should-not (copilot-agent-tools--host-label "/home/user/")))

(ert-deftest tools/host-label-ssh-returns-string ()
  "host-label returns user@host for a TRAMP SSH path."
  (skip-unless (featurep 'tramp))
  (should (equal (copilot-agent-tools--host-label "/ssh:alice@myhost:/path/")
                 "alice@myhost")))

;;; ---------- shell_command ----------

(ert-deftest tools/shell-captures-stdout ()
  "shell_command captures standard output."
  (with-temp-dir
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--shell '((command . "echo hello")))))
      (should (string-match-p "hello" result)))))

(ert-deftest tools/shell-shows-command-in-output ()
  "shell_command includes the command itself in the header."
  (with-temp-dir
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--shell '((command . "echo hi")))))
      (should (string-match-p "echo hi" result)))))

(ert-deftest tools/shell-nonzero-exit-code-reported ()
  "shell_command reports a non-zero exit code."
  (with-temp-dir
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--shell '((command . "exit 42")))))
      (should (string-match-p "42" result)))))

(ert-deftest tools/shell-empty-output-noted ()
  "shell_command says '(no output)' when command produces nothing."
  (with-temp-dir
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--shell '((command . "true")))))
      (should (string-match-p "no output" result)))))

(ert-deftest tools/shell-cwd-override ()
  "shell_command respects explicit cwd argument."
  (with-temp-dir
    (let* ((copilot-agent-tools--context (list :directory "/"))
           (result (copilot-agent-tools--shell
                    `((command . "pwd")
                      (cwd . ,default-directory))))
           ;; directory-file-name strips trailing slash for matching
           (dir-pattern (regexp-quote (directory-file-name default-directory))))
      (should (string-match-p dir-pattern result)))))

;;; ---------- read_file ----------

(ert-deftest tools/read-file-returns-content ()
  "read_file returns the full file content."
  (with-temp-dir
    (let ((path (expand-file-name "hello.txt" default-directory)))
      (write-region "hello world\n" nil path)
      (let* ((copilot-agent-tools--context (list :directory default-directory))
             (result (copilot-agent-tools--read-file `((path . ,path)))))
        (should (equal result "hello world\n"))))))

(ert-deftest tools/read-file-relative-path ()
  "read_file resolves a relative path against the context directory."
  (with-temp-dir
    (write-region "data" nil (expand-file-name "rel.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--read-file '((path . "rel.txt")))))
      (should (equal result "data")))))

(ert-deftest tools/read-file-missing-signals-error ()
  "read_file signals an error for a missing file."
  (with-temp-dir
    (let ((copilot-agent-tools--context (list :directory default-directory)))
      (should-error (copilot-agent-tools--read-file '((path . "/nonexistent/file.txt")))
                    :type 'error))))

;;; ---------- write_file ----------

(ert-deftest tools/write-file-creates-file ()
  "write_file creates a file with the given content."
  (with-temp-dir
    (let* ((path (expand-file-name "out.txt"))
           (copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--write-file
                    `((path . ,path) (content . "written!")))))
      (should (file-exists-p path))
      (should (equal (with-temp-buffer (insert-file-contents path) (buffer-string))
                     "written!"))
      (should (string-match-p "Wrote" result)))))

(ert-deftest tools/write-file-creates-parent-dirs ()
  "write_file creates missing parent directories."
  (with-temp-dir
    (let* ((path (expand-file-name "a/b/c/out.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (copilot-agent-tools--write-file `((path . ,path) (content . "deep")))
      (should (file-exists-p path)))))

(ert-deftest tools/write-file-overwrites-existing ()
  "write_file overwrites an existing file."
  (with-temp-dir
    (let* ((path (expand-file-name "exist.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "old" nil path)
      (copilot-agent-tools--write-file `((path . ,path) (content . "new")))
      (should (equal (with-temp-buffer (insert-file-contents path) (buffer-string))
                     "new")))))

(ert-deftest tools/write-file-updates-open-buffer ()
  "write_file must update a buffer that already has the file open.
The user should see the new content immediately without a manual revert."
  (with-temp-dir
    (let* ((path (expand-file-name "live.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      ;; Open the file in a buffer first (simulates user having it open)
      (write-region "original" nil path)
      (let ((buf (find-file-noselect path)))
        (unwind-protect
            (progn
              ;; Agent writes new content
              (copilot-agent-tools--write-file
               `((path . ,path) (content . "updated by agent")))
              ;; The already-open buffer must reflect the new content
              (with-current-buffer buf
                (should (equal (buffer-string) "updated by agent"))))
          (kill-buffer buf))))))

(ert-deftest tools/write-file-is-undoable ()
  "write_file must record changes in the buffer's undo list so the user can
revert them interactively.  We verify buffer-undo-list directly rather than
calling (undo) because the interactive undo function behaves differently in
batch mode vs an interactive Emacs session."
  (with-temp-dir
    (let* ((path (expand-file-name "undo.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "original" nil path)
      (let ((buf (find-file-noselect path)))
        (unwind-protect
            (progn
              (copilot-agent-tools--write-file
               `((path . ,path) (content . "agent wrote this")))
              (with-current-buffer buf
                ;; buffer-undo-list must be a non-empty list:
                ;;   - not t  (which means undo is disabled)
                ;;   - not nil (which means no history recorded)
                (should (listp buffer-undo-list))
                (should (consp buffer-undo-list))))
          (kill-buffer buf))))))

;;; ---------- list_directory ----------

(ert-deftest tools/list-dir-shows-files ()
  "list_directory includes filenames from the directory."
  (with-temp-dir
    (write-region "" nil (expand-file-name "alpha.txt"))
    (write-region "" nil (expand-file-name "beta.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--list-dir '())))
      (should (string-match-p "alpha.txt" result))
      (should (string-match-p "beta.txt" result)))))

(ert-deftest tools/list-dir-marks-subdirs ()
  "list_directory appends '/' to subdirectory names."
  (with-temp-dir
    (make-directory (expand-file-name "subdir"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--list-dir '())))
      (should (string-match-p "subdir/" result)))))

(ert-deftest tools/list-dir-explicit-path ()
  "list_directory accepts an explicit path argument."
  (with-temp-dir
    (write-region "" nil (expand-file-name "f.txt"))
    (let* ((copilot-agent-tools--context (list :directory "/"))
           (result (copilot-agent-tools--list-dir `((path . ,default-directory)))))
      (should (string-match-p "f.txt" result)))))

(ert-deftest tools/list-dir-non-directory-signals-error ()
  "list_directory errors when given a non-directory path."
  (with-temp-dir
    (let ((path (expand-file-name "file.txt")))
      (write-region "" nil path)
      (let ((copilot-agent-tools--context (list :directory default-directory)))
        (should-error (copilot-agent-tools--list-dir `((path . ,path)))
                      :type 'error)))))

;;; ---------- find_in_files ----------

(ert-deftest tools/find-in-files-matches-pattern ()
  "find_in_files returns lines matching the pattern."
  (with-temp-dir
    (write-region "foo bar\nbaz qux\nfoo again\n" nil (expand-file-name "a.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--find-in-files
                    `((pattern . "foo") (path . ,default-directory)))))
      (should (string-match-p "foo" result)))))

(ert-deftest tools/find-in-files-no-match ()
  "find_in_files reports no matches when pattern is absent."
  (with-temp-dir
    (write-region "hello world\n" nil (expand-file-name "b.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--find-in-files
                    `((pattern . "ZZZNOMATCH") (path . ,default-directory)))))
      (should (string-match-p "[Nn]o match" result)))))

(ert-deftest tools/find-in-files-glob-filter ()
  "find_in_files respects the glob filter."
  (with-temp-dir
    (write-region "target\n" nil (expand-file-name "match.py"))
    (write-region "target\n" nil (expand-file-name "nomatch.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--find-in-files
                    `((pattern . "target")
                      (path    . ,default-directory)
                      (glob    . "*.py")))))
      (should (string-match-p "match.py" result))
      (should-not (string-match-p "nomatch.txt" result)))))

;;; ---------- create_directory ----------

(ert-deftest tools/create-dir-creates-path ()
  "create_directory creates the specified directory."
  (with-temp-dir
    (let* ((new-path (expand-file-name "new/nested/dir"))
           (copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--create-dir `((path . ,new-path)))))
      (should (file-directory-p new-path))
      (should (string-match-p "Created" result)))))

(ert-deftest tools/create-dir-idempotent ()
  "create_directory succeeds even if the directory already exists."
  (with-temp-dir
    (let* ((copilot-agent-tools--context (list :directory default-directory)))
      (copilot-agent-tools--create-dir `((path . ,default-directory)))
      (should (file-directory-p default-directory)))))

;;; ---------- delete_file ----------

(ert-deftest tools/delete-file-removes-file ()
  "delete_file removes an existing file."
  (with-temp-dir
    (let* ((path (expand-file-name "to-delete.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "" nil path)
      (should (file-exists-p path))
      (let ((result (copilot-agent-tools--delete-file `((path . ,path)))))
        (should-not (file-exists-p path))
        (should (string-match-p "Deleted" result))))))

(ert-deftest tools/delete-file-missing-signals-error ()
  "delete_file signals an error when the file does not exist."
  (with-temp-dir
    (let ((copilot-agent-tools--context (list :directory default-directory)))
      (should-error (copilot-agent-tools--delete-file
                     '((path . "/no/such/file.txt")))
                    :type 'error))))

;;; ---------- edit_file ----------

(ert-deftest tools/edit-file-basic-replacement ()
  "edit_file replaces an exact string once and writes the file."
  (with-temp-dir
    (let* ((path (expand-file-name "edit.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "hello world\n" nil path)
      (let ((result (copilot-agent-tools--edit-file
                     `((path . ,path)
                       (old_string . "world")
                       (new_string . "Emacs")))))
        (should (string-match-p "Edited" result))
        (should (equal (with-temp-buffer (insert-file-contents path) (buffer-string))
                       "hello Emacs\n"))))))

(ert-deftest tools/edit-file-not-found-signals-error ()
  "edit_file signals an error when old_string is not in the file."
  (with-temp-dir
    (let* ((path (expand-file-name "nf.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "hello world\n" nil path)
      (should-error
       (copilot-agent-tools--edit-file
        `((path . ,path) (old_string . "NOTHERE") (new_string . "x")))
       :type 'error))))

(ert-deftest tools/edit-file-ambiguous-signals-error ()
  "edit_file signals an error when old_string matches more than once."
  (with-temp-dir
    (let* ((path (expand-file-name "dup.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "foo\nfoo\n" nil path)
      (should-error
       (copilot-agent-tools--edit-file
        `((path . ,path) (old_string . "foo") (new_string . "bar")))
       :type 'error))))

(ert-deftest tools/edit-file-missing-file-signals-error ()
  "edit_file signals an error when the file does not exist."
  (with-temp-dir
    (let ((copilot-agent-tools--context (list :directory default-directory)))
      (should-error
       (copilot-agent-tools--edit-file
        '((path . "/no/such/file.txt") (old_string . "x") (new_string . "y")))
       :type 'error))))

(ert-deftest tools/edit-file-updates-open-buffer ()
  "edit_file updates a buffer that already has the file open."
  (with-temp-dir
    (let* ((path (expand-file-name "live-edit.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "original content\n" nil path)
      (let ((buf (find-file-noselect path)))
        (unwind-protect
            (progn
              (copilot-agent-tools--edit-file
               `((path . ,path)
                 (old_string . "original")
                 (new_string . "modified")))
              (with-current-buffer buf
                (should (string-match-p "modified" (buffer-string)))))
          (kill-buffer buf))))))

(ert-deftest tools/edit-file-dispatched-via-execute ()
  "execute dispatches 'edit_file' correctly."
  (with-temp-dir
    (let* ((path (expand-file-name "disp.txt"))
           (copilot-agent-tools--context (list :directory default-directory)))
      (write-region "abc def\n" nil path)
      (let ((result (copilot-agent-tools-execute
                     "edit_file"
                     `((path . ,path) (old_string . "abc") (new_string . "xyz")))))
        (should (string-match-p "Edited" result))
        (should (equal (with-temp-buffer (insert-file-contents path) (buffer-string))
                       "xyz def\n"))))))

;;; ---------- glob ----------

(ert-deftest tools/glob-finds-matching-files ()
  "glob returns files whose names match the pattern."
  (with-temp-dir
    (write-region "" nil (expand-file-name "foo.el"))
    (write-region "" nil (expand-file-name "bar.el"))
    (write-region "" nil (expand-file-name "baz.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--glob
                    `((pattern . "*.el") (path . ,default-directory)))))
      (should (string-match-p "foo.el" result))
      (should (string-match-p "bar.el" result))
      (should-not (string-match-p "baz.txt" result)))))

(ert-deftest tools/glob-no-match-reports-none ()
  "glob reports no files when pattern matches nothing."
  (with-temp-dir
    (write-region "" nil (expand-file-name "hello.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--glob
                    `((pattern . "*.el") (path . ,default-directory)))))
      (should (string-match-p "[Nn]o files" result)))))

(ert-deftest tools/glob-recurses-into-subdirs ()
  "glob finds files in subdirectories."
  (with-temp-dir
    (make-directory (expand-file-name "sub"))
    (write-region "" nil (expand-file-name "sub/nested.el"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--glob
                    `((pattern . "*.el") (path . ,default-directory)))))
      (should (string-match-p "nested.el" result)))))

(ert-deftest tools/glob-defaults-to-context-dir ()
  "glob uses the context directory when no path is given."
  (with-temp-dir
    (write-region "" nil (expand-file-name "ctx.el"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--glob '((pattern . "*.el")))))
      (should (string-match-p "ctx.el" result)))))

(ert-deftest tools/glob-dispatched-via-execute ()
  "execute dispatches 'glob' correctly."
  (with-temp-dir
    (write-region "" nil (expand-file-name "disp.el"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools-execute
                    "glob" `((pattern . "*.el") (path . ,default-directory)))))
      (should (string-match-p "disp.el" result)))))

;;; ---------- grep ----------

(ert-deftest tools/grep-finds-pattern ()
  "grep returns lines matching the pattern."
  (with-temp-dir
    (write-region "apple\nbanana\napricot\n" nil (expand-file-name "f.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--grep
                    `((pattern . "ap") (path . ,default-directory)))))
      (should (string-match-p "apple" result))
      (should (string-match-p "apricot" result))
      (should-not (string-match-p "banana" result)))))

(ert-deftest tools/grep-no-match-reports-none ()
  "grep reports no matches when pattern is absent."
  (with-temp-dir
    (write-region "hello world\n" nil (expand-file-name "g.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--grep
                    `((pattern . "ZZZNOMATCH") (path . ,default-directory)))))
      (should (string-match-p "[Nn]o match" result)))))

(ert-deftest tools/grep-after-context ()
  "grep includes lines after each match when after_context > 0."
  (with-temp-dir
    (write-region "MATCH\nline-after\nother\n" nil (expand-file-name "ctx.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--grep
                    `((pattern . "MATCH")
                      (path    . ,default-directory)
                      (after_context . 1)))))
      (should (string-match-p "line-after" result)))))

(ert-deftest tools/grep-before-context ()
  "grep includes lines before each match when before_context > 0."
  (with-temp-dir
    (write-region "line-before\nMATCH\nother\n" nil (expand-file-name "bctx.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--grep
                    `((pattern . "MATCH")
                      (path    . ,default-directory)
                      (before_context . 1)))))
      (should (string-match-p "line-before" result)))))

(ert-deftest tools/grep-glob-filter ()
  "grep respects the glob filter and excludes non-matching files."
  (with-temp-dir
    (write-region "target\n" nil (expand-file-name "match.py"))
    (write-region "target\n" nil (expand-file-name "nomatch.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools--grep
                    `((pattern . "target")
                      (path    . ,default-directory)
                      (glob    . "*.py")))))
      (should     (string-match-p "match.py"    result))
      (should-not (string-match-p "nomatch.txt" result)))))

(ert-deftest tools/grep-dispatched-via-execute ()
  "execute dispatches 'grep' correctly."
  (with-temp-dir
    (write-region "hello grep\n" nil (expand-file-name "d.txt"))
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools-execute
                    "grep" `((pattern . "hello") (path . ,default-directory)))))
      (should (string-match-p "hello" result)))))

;;; ---------- execute dispatcher ----------

(ert-deftest tools/execute-dispatches-shell ()
  "execute dispatches 'shell_command' correctly."
  (with-temp-dir
    (let* ((copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools-execute
                    "shell_command" '((command . "echo dispatch")))))
      (should (string-match-p "dispatch" result)))))

(ert-deftest tools/execute-dispatches-read-file ()
  "execute dispatches 'read_file' correctly."
  (with-temp-dir
    (write-region "content" nil (expand-file-name "r.txt"))
    (let* ((path (expand-file-name "r.txt"))
           (copilot-agent-tools--context (list :directory default-directory))
           (result (copilot-agent-tools-execute "read_file" `((path . ,path)))))
      (should (equal result "content")))))

(ert-deftest tools/execute-unknown-tool-returns-error-string ()
  "execute returns an error string for an unknown tool name."
  (let* ((copilot-agent-tools--context (list :directory default-directory))
         (result (copilot-agent-tools-execute "no_such_tool" '())))
    (should (string-match-p "Unknown tool" result))))

(ert-deftest tools/execute-wraps-tool-errors ()
  "execute catches tool errors and returns a string describing the failure."
  (let* ((copilot-agent-tools--context (list :directory default-directory))
         (result (copilot-agent-tools-execute "read_file"
                                              '((path . "/this/file/does/not/exist")))))
    (should (stringp result))
    (should (string-match-p "not/exist\\|not found\\|No such file" result))))

;;; ---------- Tool Schema Sanity ----------

(ert-deftest tools/schema-is-non-empty-list ()
  "copilot-agent-tools-schema is a non-empty list."
  (should (listp copilot-agent-tools-schema))
  (should (> (length copilot-agent-tools-schema) 0)))

(ert-deftest tools/schema-entries-have-required-keys ()
  "Every schema entry has name, description, and parameters."
  (dolist (tool copilot-agent-tools-schema)
    (should (assq 'name tool))
    (should (assq 'description tool))
    (should (assq 'parameters tool))))

(provide 'test-copilot-agent-tools)
;;; test-copilot-agent-tools.el ends here

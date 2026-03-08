;;; run-tests.el --- Test runner for emacs-copilot-agent -*- lexical-binding: t -*-

;;; Commentary:
;; Loads and runs all ERT test suites for the emacs-copilot-agent package.
;;
;; Usage (from the project root):
;;
;;   emacs -batch -l test/run-tests.el
;;
;; Or to run a single suite:
;;
;;   emacs -batch -L . -L providers \
;;         -l test/test-copilot-agent-tools.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Exit code: 0 = all passed, 1 = failures/errors.

;;; Code:

(require 'ert)

;; Set up load paths relative to this file
(let* ((this-dir  (file-name-directory (or load-file-name buffer-file-name)))
       (root      (expand-file-name ".." this-dir))
       (providers (expand-file-name "providers" root))
       (test-dir  this-dir))
  (dolist (d (list root providers test-dir))
    (unless (member d load-path)
      (push d load-path))))

;; Stub auth-source for all test suites so no real credentials are needed
(require 'auth-source)
(advice-add 'auth-source-pick-first-password
            :override (lambda (&rest _) "test-api-key-stub")
            '((name . copilot-agent-test-stub)))

;; Load all test suites
(load "test-copilot-agent-tools"      nil t)
(load "test-copilot-agent-anthropic"  nil t)
(load "test-copilot-agent-gemini"     nil t)
(load "test-copilot-agent-api"        nil t)
(load "test-copilot-agent-ui"         nil t)
(load "test-regressions"              nil t)

;; Print summary and exit with appropriate code
(message "\n=== Running all copilot-agent ERT tests ===\n")
(ert-run-tests-batch-and-exit t)

;;; run-tests.el ends here

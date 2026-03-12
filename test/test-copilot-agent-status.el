;;; test-copilot-agent-status.el --- ERT tests for copilot-agent-status -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for copilot-agent-status.el:
;; context-length formatting, token expiry, error recording/display,
;; known-model lookup, and buffer rendering.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root     (expand-file-name ".." this-dir)))
  (dolist (d (list root (expand-file-name "providers" root)))
    (unless (member d load-path) (push d load-path))))

(require 'auth-source)
(advice-add 'auth-source-pick-first-password :override
            (lambda (&rest _) "test-key-stub")
            '((name . copilot-agent-status-test-stub)))
(require 'copilot-agent-tools)
(require 'copilot-agent-api)
(require 'copilot-agent-status)

;;; ---------- Token formatting ----------

(ert-deftest status/format-tokens-nil ()
  "format-tokens returns \"unknown\" for nil."
  (should (equal (copilot-agent-status--format-tokens nil) "unknown")))

(ert-deftest status/format-tokens-millions ()
  "format-tokens uses M suffix for values >= 1 000 000."
  (should (string-match-p "M" (copilot-agent-status--format-tokens 1048576))))

(ert-deftest status/format-tokens-thousands ()
  "format-tokens uses k suffix for values >= 1 000."
  (should (string-match-p "k" (copilot-agent-status--format-tokens 8192))))

(ert-deftest status/format-tokens-small ()
  "format-tokens returns plain number for small values."
  (should (equal (copilot-agent-status--format-tokens 512) "512")))

;;; ---------- Expiry formatting ----------

(ert-deftest status/format-expiry-nil ()
  "format-expiry returns \"unknown\" for nil."
  (should (equal (copilot-agent-status--format-expiry nil) "unknown")))

(ert-deftest status/format-expiry-past ()
  "format-expiry returns \"EXPIRED\" for a past timestamp."
  (let ((past-ms (- (truncate (* (float-time) 1000)) 10000)))
    (should (equal (copilot-agent-status--format-expiry past-ms) "EXPIRED"))))

(ert-deftest status/format-expiry-future-hours ()
  "format-expiry shows hours for tokens expiring > 1 hour from now."
  (let ((future-ms (+ (truncate (* (float-time) 1000)) (* 2 3600 1000))))
    (should (string-match-p "h" (copilot-agent-status--format-expiry future-ms)))))

(ert-deftest status/format-expiry-future-minutes ()
  "format-expiry shows minutes for tokens expiring in 10-60 minutes."
  (let ((future-ms (+ (truncate (* (float-time) 1000)) (* 15 60 1000))))
    (should (string-match-p "m" (copilot-agent-status--format-expiry future-ms)))))

;;; ---------- Error recording ----------

(ert-deftest status/record-and-retrieve-error ()
  "record-error stores a message; last-error retrieves it."
  (copilot-agent-status-record-error 'test-provider "quota exceeded")
  (should (equal (copilot-agent-status--last-error 'test-provider) "quota exceeded")))

(ert-deftest status/last-error-nil-for-unknown-provider ()
  "last-error returns nil for providers with no recorded error."
  (should (null (copilot-agent-status--last-error 'no-such-provider-xyz))))

(ert-deftest status/record-error-overwrites-previous ()
  "record-error replaces the previous error for the same provider."
  (copilot-agent-status-record-error 'overwrite-test "first")
  (copilot-agent-status-record-error 'overwrite-test "second")
  (should (equal (copilot-agent-status--last-error 'overwrite-test) "second")))

;;; ---------- Known model lookup ----------

(ert-deftest status/known-model-gemini-flash-context ()
  "gemini-2.0-flash has a 1M context window in the lookup table."
  (let ((entry (assoc "gemini-2.0-flash" copilot-agent-status--known-models)))
    (should entry)
    (should (= (plist-get (cdr entry) :context) 1048576))))

(ert-deftest status/known-model-claude-sonnet-context ()
  "claude-sonnet-4-6 has a 200k context window in the lookup table."
  (let ((entry (assoc "claude-sonnet-4-6" copilot-agent-status--known-models)))
    (should entry)
    (should (= (plist-get (cdr entry) :context) 200000))))

(ert-deftest status/known-model-qwen-coder-context ()
  "coder-model has a 128k context window in the lookup table."
  (let ((entry (assoc "coder-model" copilot-agent-status--known-models)))
    (should entry)
    (should (= (plist-get (cdr entry) :context) 131072))))

;;; ---------- Buffer rendering ----------

(ert-deftest status/render-creates-buffer ()
  "render produces a *copilot-agent-status* buffer."
  (cl-letf (((symbol-function 'copilot-agent-api--get-provider)
             (lambda (_) '(:display-name "Test Provider")))
            ((symbol-function 'pop-to-buffer) #'ignore))
    (copilot-agent-status--render 'gemini "gemini-2.0-flash" nil)
    (should (get-buffer "*copilot-agent-status*"))
    (kill-buffer "*copilot-agent-status*")))

(ert-deftest status/render-shows-model-name ()
  "render includes the model name in the buffer."
  (cl-letf (((symbol-function 'copilot-agent-api--get-provider)
             (lambda (_) '(:display-name "Test Provider")))
            ((symbol-function 'copilot-agent-gemini--cli-load-creds)
             (lambda () nil))
            ((symbol-function 'pop-to-buffer) #'ignore))
    (copilot-agent-status--render 'gemini "gemini-2.0-flash" nil)
    (with-current-buffer "*copilot-agent-status*"
      (should (string-match-p "gemini-2.0-flash" (buffer-string))))
    (kill-buffer "*copilot-agent-status*")))

(ert-deftest status/render-shows-context-from-lookup ()
  "render displays context window from the known-models table when no live info."
  (cl-letf (((symbol-function 'copilot-agent-api--get-provider)
             (lambda (_) '(:display-name "Test Provider")))
            ((symbol-function 'copilot-agent-gemini--cli-load-creds)
             (lambda () nil))
            ((symbol-function 'pop-to-buffer) #'ignore))
    (copilot-agent-status--render 'gemini "gemini-2.0-flash" nil)
    (with-current-buffer "*copilot-agent-status*"
      (should (string-match-p "1.0M" (buffer-string))))
    (kill-buffer "*copilot-agent-status*")))

(ert-deftest status/render-shows-live-info-when-provided ()
  "render uses live API info over the lookup table."
  (cl-letf (((symbol-function 'copilot-agent-api--get-provider)
             (lambda (_) '(:display-name "Test Provider")))
            ((symbol-function 'copilot-agent-gemini--cli-load-creds)
             (lambda () nil))
            ((symbol-function 'pop-to-buffer) #'ignore))
    (copilot-agent-status--render 'gemini "gemini-2.0-flash"
                                  '(:context 999999 :output 1234 :name "Gemini Live"))
    (with-current-buffer "*copilot-agent-status*"
      (should (string-match-p "Gemini Live" (buffer-string))))
    (kill-buffer "*copilot-agent-status*")))

(ert-deftest status/render-shows-last-error ()
  "render displays the last recorded API error."
  (copilot-agent-status-record-error 'gemini "quota exceeded: reset in 55s")
  (cl-letf (((symbol-function 'copilot-agent-api--get-provider)
             (lambda (_) '(:display-name "Google Gemini")))
            ((symbol-function 'copilot-agent-gemini--cli-load-creds)
             (lambda () nil))
            ((symbol-function 'pop-to-buffer) #'ignore))
    (copilot-agent-status--render 'gemini "gemini-2.0-flash" nil)
    (with-current-buffer "*copilot-agent-status*"
      (should (string-match-p "quota exceeded" (buffer-string))))
    (kill-buffer "*copilot-agent-status*")))

;;; ---------- API error hook ----------

(ert-deftest status/api-loop-records-http-error ()
  "copilot-agent-api--loop records HTTP errors via status-record-error."
  (let* ((provider-impl (list :send-fn (lambda (_s cb) (funcall cb nil "HTTP 429: quota"))
                              :display-name "mock"))
         (session (list :provider 'mock-status-test :messages '() :tools nil
                        :system-prompt nil :max-tokens 512))
         recorded)
    (puthash 'mock-status-test provider-impl copilot-agent-api--providers)
    (cl-letf (((symbol-function 'copilot-agent-status-record-error)
               (lambda (_p msg) (setq recorded msg))))
      (copilot-agent-api--loop session (list :on-error #'ignore)))
    (should (equal recorded "HTTP 429: quota"))
    (remhash 'mock-status-test copilot-agent-api--providers)))

(provide 'test-copilot-agent-status)
;;; test-copilot-agent-status.el ends here

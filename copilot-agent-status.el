;;; copilot-agent-status.el --- Model status display -*- lexical-binding: t -*-

;;; Commentary:
;; M-x copilot-agent-status  — show current provider, model, context window,
;; token expiry, and last quota message in a read-only buffer.
;;
;; Model metadata is fetched asynchronously from the provider's API when
;; possible (Gemini models.get endpoint); known models fall back to a
;; built-in lookup table.

;;; Code:

(require 'copilot-agent-api)

;;; ---------- Known model metadata ----------

(defconst copilot-agent-status--known-models
  '(;; Gemini
    ("gemini-2.5-pro"          :context 1048576  :output 65536)
    ("gemini-2.0-flash"        :context 1048576  :output 8192)
    ("gemini-2.0-flash-exp"    :context 1048576  :output 8192)
    ("gemini-2.0-pro-exp"      :context 2097152  :output 8192)
    ("gemini-1.5-pro"          :context 2097152  :output 8192)
    ("gemini-1.5-flash"        :context 1048576  :output 8192)
    ;; Anthropic
    ("claude-opus-4-6"         :context 200000   :output 32000)
    ("claude-sonnet-4-6"       :context 200000   :output 64000)
    ("claude-haiku-4-5"        :context 200000   :output 16000)
    ;; Qwen portal
    ("coder-model"             :context 131072   :output 8192)
    ("vision-model"            :context 131072   :output 8192))
  "Fallback model metadata.  Each entry: (MODEL-STRING :context N :output N).")

;;; ---------- Last quota / error tracking ----------

(defvar copilot-agent-status--last-errors (make-hash-table :test #'eq)
  "Hash-table: provider-symbol -> last error string received from the API.")

(defun copilot-agent-status-record-error (provider message)
  "Record the latest error MESSAGE for PROVIDER (used by the send functions)."
  (puthash provider message copilot-agent-status--last-errors))

(defun copilot-agent-status--last-error (provider)
  "Return the last error string for PROVIDER, or nil."
  (gethash provider copilot-agent-status--last-errors))

;;; ---------- Async metadata fetch (Gemini only) ----------

(defun copilot-agent-status--fetch-gemini-model-info (model auth-key-or-token is-bearer callback)
  "Fetch Gemini model metadata for MODEL asynchronously.
AUTH-KEY-OR-TOKEN is an API key or OAuth Bearer token.
IS-BEARER non-nil means send as Authorization header; nil means ?key= param.
CALLBACK is called as (PLIST) where PLIST has :context and :output,
or (nil) on failure."
  (let* ((base  "https://generativelanguage.googleapis.com/v1beta/models/")
         (url   (if is-bearer
                    (concat base model)
                  (format "%s%s?key=%s" base model auth-key-or-token)))
         (hdrs  (if is-bearer
                    (list (concat "Authorization: Bearer " auth-key-or-token))
                  '())))
    ;; Re-use curl-post machinery for a GET by passing an empty body and
    ;; a -X GET override.  Instead, use a raw curl process directly.
    (let* ((resp-buf (generate-new-buffer " *copilot-agent-model-info*"))
           (cmd (append (list "curl" "--silent" "--show-error"
                              "-X" "GET"
                              "-H" "Content-Type: application/json")
                        (mapcan (lambda (h) (list "-H" h)) hdrs)
                        (list url))))
      (make-process
       :name    "copilot-agent-model-info"
       :buffer  resp-buf
       :command cmd
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (unwind-protect
               (let* ((raw  (with-current-buffer (process-buffer proc)
                              (buffer-string)))
                      (data (condition-case _ (json-read-from-string raw) (error nil))))
                 (if data
                     (funcall callback
                              (list :context (cdr (assq 'inputTokenLimit  data))
                                    :output  (cdr (assq 'outputTokenLimit data))
                                    :name    (cdr (assq 'displayName      data))))
                   (funcall callback nil)))
             (kill-buffer (process-buffer proc)))))))))

;;; ---------- Buffer rendering ----------

(defun copilot-agent-status--format-tokens (n)
  "Format N as a human-readable token count."
  (cond ((null n)            "unknown")
        ((>= n 1000000)      (format "%.1fM" (/ (float n) 1000000)))
        ((>= n 1000)         (format "%.0fk" (/ (float n) 1000)))
        (t                   (number-to-string n))))

(defun copilot-agent-status--format-expiry (expires-ms)
  "Return a human-readable string for token expiry at EXPIRES-MS (epoch ms)."
  (if (null expires-ms)
      "unknown"
    (let* ((now-ms   (truncate (* (float-time) 1000)))
           (delta-s  (/ (- expires-ms now-ms) 1000)))
      (if (<= delta-s 0)
          "EXPIRED"
        (let ((h (/ delta-s 3600))
              (m (% (/ delta-s 60) 60))
              (s (% delta-s 60)))
          (cond ((> h 0)  (format "in %dh %02dm" h m))
                ((> m 0)  (format "in %dm %02ds" m s))
                (t        (format "in %ds" s))))))))

(defun copilot-agent-status--insert-row (label value)
  "Insert a formatted LABEL / VALUE row into the current buffer."
  (insert (propertize (format "  %-18s" label) 'face 'font-lock-keyword-face))
  (insert (format "%s\n" value)))

(defun copilot-agent-status--render (provider model live-info)
  "Render the status buffer for PROVIDER / MODEL.
LIVE-INFO is a plist from the API fetch (may be nil)."
  (let* ((buf     (get-buffer-create "*copilot-agent-status*"))
         (prov    (copilot-agent-api--get-provider provider))
         (known   (assoc model copilot-agent-status--known-models))
         (ctx     (or (plist-get live-info :context)
                      (and known (plist-get (cdr known) :context))))
         (out     (or (plist-get live-info :output)
                      (and known (plist-get (cdr known) :output))))
         (disp    (or (plist-get live-info :name) model))
         (last-err (copilot-agent-status--last-error provider)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Copilot Agent — Model Status\n" 'face 'bold))
        (insert (make-string 40 ?─) "\n\n")
        (copilot-agent-status--insert-row
         "Provider:" (or (plist-get prov :display-name) (symbol-name provider)))
        (copilot-agent-status--insert-row "Model:" disp)
        (copilot-agent-status--insert-row
         "Context window:" (copilot-agent-status--format-tokens ctx))
        (copilot-agent-status--insert-row
         "Max output:" (copilot-agent-status--format-tokens out))
        ;; Auth-specific rows
        (cond
         ((eq provider 'gemini)
          (copilot-agent-status--insert-row
           "Auth mode:" (symbol-name copilot-agent-gemini-auth-mode))
          (when (eq copilot-agent-gemini-auth-mode 'cli)
            (let* ((creds   (copilot-agent-gemini--cli-load-creds))
                   (expires (and creds (cdr (assq 'expires creds))))
                   (project (and creds (cdr (assq 'project creds)))))
              (copilot-agent-status--insert-row
               "Token expires:" (copilot-agent-status--format-expiry expires))
              (when project
                (copilot-agent-status--insert-row "GCP project:" project)))))
         ((eq provider 'qwen)
          (let* ((creds   (copilot-agent-qwen--load-creds))
                 (expires (and creds (cdr (assq 'expires creds)))))
            (copilot-agent-status--insert-row
             "Token expires:" (copilot-agent-status--format-expiry expires)))))
        (insert "\n")
        (when last-err
          (insert (propertize "  Last API error:\n" 'face 'font-lock-keyword-face))
          (insert (propertize (format "  %s\n" last-err) 'face 'font-lock-warning-face)))
        (insert "\n")
        (insert (propertize "  q  " 'face 'font-lock-comment-face))
        (insert "close   ")
        (insert (propertize "  g  " 'face 'font-lock-comment-face))
        (insert "refresh\n"))
      (special-mode)
      (local-set-key (kbd "g") #'copilot-agent-status)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;; ---------- Entry point ----------

;;;###autoload
(defun copilot-agent-status ()
  "Display current provider, model, context window, and token expiry.
Model metadata is fetched from the API when possible; falls back to a
built-in lookup table for known models."
  (interactive)
  (require 'copilot-agent-gemini nil t)
  (require 'copilot-agent-qwen   nil t)
  (let* ((provider (bound-and-true-p copilot-agent-provider))
         (session  (and (boundp 'copilot-agent-ui--session)
                        copilot-agent-ui--session))
         (model    (or (and session (plist-get session :model))
                       (and (eq provider 'gemini)
                            (bound-and-true-p copilot-agent-gemini-default-model))
                       (and (eq provider 'qwen)
                            (bound-and-true-p copilot-agent-qwen-default-model))
                       (and provider
                            (let ((p (copilot-agent-api--get-provider provider)))
                              (plist-get p :default-model)))
                       "unknown")))
    ;; Render immediately with what we know, then optionally update with live data
    (copilot-agent-status--render provider model nil)
    ;; Async fetch from Gemini models API
    (when (eq provider 'gemini)
      (condition-case _
          (let* ((is-cli    (eq copilot-agent-gemini-auth-mode 'cli))
                 (key-or-tok (if is-cli
                                 (copilot-agent-gemini--cli-valid-access-token)
                               (copilot-agent-gemini--api-key))))
            (copilot-agent-status--fetch-gemini-model-info
             model key-or-tok is-cli
             (lambda (info)
               (when info
                 (copilot-agent-status--render provider model info)))))
        (error nil)))))

(provide 'copilot-agent-status)
;;; copilot-agent-status.el ends here

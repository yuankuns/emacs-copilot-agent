;;; copilot-agent-anthropic.el --- Anthropic Claude provider -*- lexical-binding: t -*-

;;; Commentary:
;; Implements the copilot-agent provider protocol for Anthropic Claude.
;;
;; Auth-source setup (~/.authinfo or ~/.authinfo.gpg):
;;   machine api.anthropic.com login apikey password YOUR_KEY
;;
;; The provider converts the internal canonical message format (Anthropic-style)
;; to the wire format and parses responses back to the canonical format.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'copilot-agent-api nil t)

;;; ---------- Configuration ----------

(defcustom copilot-agent-anthropic-default-model "claude-sonnet-4-6"
  "Default Anthropic model to use."
  :type 'string
  :group 'copilot-agent)

(defcustom copilot-agent-anthropic-api-version "2023-06-01"
  "Anthropic API version header value."
  :type 'string
  :group 'copilot-agent)

(defconst copilot-agent-anthropic--endpoint
  "https://api.anthropic.com/v1/messages"
  "Anthropic messages API endpoint.")

;;; ---------- Auth ----------

(defun copilot-agent-anthropic--api-key ()
  "Retrieve the Anthropic API key from auth-source.
Add to ~/.authinfo: machine api.anthropic.com login apikey password YOUR_KEY"
  (or (auth-source-pick-first-password :host "api.anthropic.com")
      (error (concat "Anthropic API key not found.  "
                     "Add to ~/.authinfo:\n"
                     "  machine api.anthropic.com login apikey password YOUR_KEY"))))

;;; ---------- Tool Schema Conversion ----------

(defun copilot-agent-anthropic--format-tools (tool-schema)
  "Convert universal TOOL-SCHEMA list to Anthropic `tools' array.
Anthropic calls `parameters' -> `input_schema'; everything else matches."
  (mapcar
   (lambda (tool)
     (let ((params (cdr (assq 'parameters tool))))
       `((name        . ,(cdr (assq 'name tool)))
         (description . ,(cdr (assq 'description tool)))
         (input_schema . ,params))))
   tool-schema))

;;; ---------- Message Conversion ----------

(defun copilot-agent-anthropic--format-messages (messages)
  "Convert canonical MESSAGES list to Anthropic wire format.
Canonical format is already Anthropic-compatible; this is a passthrough
with minor normalization (ensure :content is properly encoded)."
  ;; Canonical format uses Anthropic conventions internally, so messages
  ;; pass through directly.  We just ensure the list is a vector for JSON.
  (vconcat messages))

;;; ---------- Request Building ----------

(defun copilot-agent-anthropic--build-request (session)
  "Build the JSON request body plist for SESSION."
  (let* ((model   (or (plist-get session :model)
                      copilot-agent-anthropic-default-model))
         (msgs    (plist-get session :messages))
         (tools   (plist-get session :tools))
         (system  (plist-get session :system-prompt))
         (max-tok (or (plist-get session :max-tokens) 8192))
         (req     `((model      . ,model)
                    (max_tokens . ,max-tok)
                    (messages   . ,(vconcat msgs)))))
    (when system
      (push `(system . ,system) req))
    (when tools
      (push `(tools . ,(vconcat (copilot-agent-anthropic--format-tools tools)))
            req))
    (json-encode req)))

;;; ---------- Response Parsing ----------

(defun copilot-agent-anthropic--parse-response (raw-json)
  "Parse RAW-JSON string from Anthropic API into a normalized response plist.
Returns plist with :text, :tool-calls, :stop-reason, :error."
  (condition-case err
      (let* ((data        (json-read-from-string raw-json))
             (type        (cdr (assq 'type data)))
             (stop-reason (cdr (assq 'stop_reason data)))
             (content     (cdr (assq 'content data)))
             text tool-calls)
        ;; Check for API-level error
        (when (equal type "error")
          (let ((e (cdr (assq 'error data))))
            (cl-return-from copilot-agent-anthropic--parse-response
              (list :error (format "%s: %s"
                                   (cdr (assq 'type e))
                                   (cdr (assq 'message e)))))))
        ;; Walk content blocks
        (seq-doseq (block (if (vectorp content) content (vconcat content)))
          (let ((btype (cdr (assq 'type block))))
            (cond
             ((equal btype "text")
              (setq text (concat (or text "") (cdr (assq 'text block)))))
             ((equal btype "tool_use")
              (push (list :id    (cdr (assq 'id block))
                          :name  (cdr (assq 'name block))
                          :input (cdr (assq 'input block)))
                    tool-calls)))))
        (let* ((usage        (cdr (assq 'usage data)))
               (input-tokens (and usage (cdr (assq 'input_tokens usage)))))
          (list :text         text
                :tool-calls   (nreverse tool-calls)
                :stop-reason  stop-reason
                :raw-content  content
                :input-tokens input-tokens)))
    (error (list :error (format "Parse error: %s" (error-message-string err))))))

;;; ---------- Provider Send Function ----------

(defun copilot-agent-anthropic-send (session callback)
  "Send SESSION to Anthropic API asynchronously.
CALLBACK is called as (RESPONSE-PLIST ERROR-STRING) when complete.
RESPONSE-PLIST has keys :text :tool-calls :stop-reason :raw-content."
  (let* ((api-key (copilot-agent-anthropic--api-key))
         (body    (copilot-agent-anthropic--build-request session))
         (headers (list (format "x-api-key: %s" api-key)
                        (format "anthropic-version: %s"
                                copilot-agent-anthropic-api-version))))
    (copilot-agent-api--curl-post
     copilot-agent-anthropic--endpoint headers body
     (lambda (raw-output http-error)
       (if http-error
           (funcall callback nil http-error)
         (let ((parsed (copilot-agent-anthropic--parse-response raw-output)))
           (if (plist-get parsed :error)
               (funcall callback nil (plist-get parsed :error))
             (funcall callback parsed nil))))))))

;;; ---------- Tool Result Message Builder ----------

(defun copilot-agent-anthropic-make-tool-result-message (tool-results)
  "Build a user-role message containing TOOL-RESULTS for Anthropic.
TOOL-RESULTS is a list of plists with :tool-use-id and :content."
  (list (cons 'role "user")
        (cons 'content
              (vconcat
               (mapcar (lambda (r)
                         `((type        . "tool_result")
                           (tool_use_id . ,(plist-get r :tool-use-id))
                           (content     . ,(plist-get r :content))))
                       tool-results)))))

;;; ---------- Model List ----------

(defun copilot-agent-anthropic--list-models ()
  "Return the list of known Anthropic model IDs."
  '("claude-opus-4-6"
    "claude-sonnet-4-6"
    "claude-haiku-4-5-20251001"))

(defun copilot-agent-anthropic--set-model (model)
  "Set the active Anthropic model to MODEL."
  (setq copilot-agent-anthropic-default-model model))

;;; ---------- Provider Registration ----------

(with-eval-after-load 'copilot-agent-api
  (copilot-agent-api-register-provider
   'anthropic
   (list :display-name          "Anthropic Claude"
         :default-model         copilot-agent-anthropic-default-model
         :default-model-fn      (lambda () copilot-agent-anthropic-default-model)
         :context-window        200000
         :send-fn               #'copilot-agent-anthropic-send
         :make-tool-result-fn   #'copilot-agent-anthropic-make-tool-result-message
         :format-tools-fn       #'copilot-agent-anthropic--format-tools
         :list-models-fn        #'copilot-agent-anthropic--list-models
         :set-model-fn          #'copilot-agent-anthropic--set-model)))

(provide 'copilot-agent-anthropic)
;;; copilot-agent-anthropic.el ends here

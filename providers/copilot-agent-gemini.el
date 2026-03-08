;;; copilot-agent-gemini.el --- Google Gemini provider -*- lexical-binding: t -*-

;;; Commentary:
;; Implements the copilot-agent provider protocol for Google Gemini.
;;
;; Auth-source setup (~/.authinfo or ~/.authinfo.gpg):
;;   machine generativelanguage.googleapis.com login apikey password YOUR_KEY
;;
;; Gemini differs from Anthropic in several ways handled here:
;;   - Role names: "user" / "model"  (not "user" / "assistant")
;;   - Content format: {role, parts:[{text}]}  (not {role, content})
;;   - Tool calls:  parts:[{functionCall:{name,args}}]
;;   - Tool results: parts:[{functionResponse:{name,response:{result}}}]
;;   - Tool schema types: UPPERCASE  ("STRING" not "string")
;;   - API key passed as query param, not header

;;; Code:

(require 'auth-source)
(require 'json)
(require 'copilot-agent-api nil t)

;;; ---------- Configuration ----------

(defcustom copilot-agent-gemini-default-model "gemini-2.0-flash"
  "Default Gemini model to use."
  :type 'string
  :group 'copilot-agent)

(defconst copilot-agent-gemini--base-url
  "https://generativelanguage.googleapis.com/v1beta/models"
  "Gemini API base URL.")

;;; ---------- Auth ----------

(defun copilot-agent-gemini--api-key ()
  "Retrieve the Gemini API key from auth-source.
Add to ~/.authinfo:
  machine generativelanguage.googleapis.com login apikey password YOUR_KEY"
  (or (auth-source-pick-first-password
       :host "generativelanguage.googleapis.com")
      (error (concat "Gemini API key not found.  "
                     "Add to ~/.authinfo:\n"
                     "  machine generativelanguage.googleapis.com "
                     "login apikey password YOUR_KEY"))))

(defun copilot-agent-gemini--endpoint (model)
  "Return the generateContent endpoint URL for MODEL."
  (format "%s/%s:generateContent" copilot-agent-gemini--base-url model))

;;; ---------- Tool Schema Conversion ----------

(defun copilot-agent-gemini--upcase-types (params)
  "Recursively convert JSON-Schema type strings in PARAMS to Gemini uppercase.
Only the value of `type' keys is uppercased; other string values are left alone.
e.g. {type: \"string\"} -> {type: \"STRING\"}"
  (cond
   ((vectorp params)
    (vconcat (mapcar #'copilot-agent-gemini--upcase-types (append params nil))))
   ((listp params)
    (mapcar (lambda (pair)
              (cons (car pair)
                    (if (eq (car pair) 'type)
                        (upcase (cdr pair))          ; only uppercase type values
                      (copilot-agent-gemini--upcase-types (cdr pair)))))
            params))
   (t params)))   ; strings and other atoms pass through unchanged

(defun copilot-agent-gemini--format-tools (tool-schema)
  "Convert universal TOOL-SCHEMA list to Gemini `tools' array.
Gemini wraps declarations in a `function_declarations' key and uses
uppercase type names."
  (list
   (list
    (cons 'function_declarations
          (vconcat
           (mapcar
            (lambda (tool)
              (let ((params (copilot-agent-gemini--upcase-types
                             (cdr (assq 'parameters tool)))))
                `((name        . ,(cdr (assq 'name tool)))
                  (description . ,(cdr (assq 'description tool)))
                  (parameters  . ,params))))
            tool-schema))))))

;;; ---------- Message Conversion ----------

(defun copilot-agent-gemini--canonical->gemini-role (role)
  "Convert canonical role string (\"user\"/\"assistant\") to Gemini role."
  (if (equal role "assistant") "model" role))

(defun copilot-agent-gemini--canonical->parts (content)
  "Convert canonical message CONTENT to a Gemini `parts' vector.
CONTENT may be a string (simple text) or a vector of content blocks."
  (cond
   ;; Simple string → single text part
   ((stringp content)
    (vector `((text . ,content))))
   ;; Vector of blocks (tool_use, text, tool_result, …)
   ((or (vectorp content) (listp content))
    (vconcat
     (delq nil
           (mapcar
            (lambda (block)
              (let ((type (cdr (assq 'type block))))
                (cond
                 ((equal type "text")
                  `((text . ,(cdr (assq 'text block)))))
                 ((equal type "tool_use")
                  `((functionCall
                     . ((name . ,(cdr (assq 'name block)))
                        (args . ,(cdr (assq 'input block)))))))
                 ((equal type "tool_result")
                  `((functionResponse
                     . ((name     . ,(cdr (assq 'tool_use_id block)))
                        (response . ((result . ,(cdr (assq 'content block)))))))))
                 (t nil))))
            (if (vectorp content) (append content nil) content)))))
   (t (vector `((text . ,(format "%s" content)))))))

(defun copilot-agent-gemini--native-parts-p (content)
  "Return t if CONTENT is a native Gemini parts vector (not canonical typed blocks).
Native Gemini parts have keys like `text'/`functionCall'/`thought'; canonical
blocks always carry a `type' key.  This lets convert-message pass native parts
through unchanged, preserving `thoughtSignature' fields required by thinking models."
  (and content
       (or (vectorp content) (listp content))
       (let ((first (if (vectorp content)
                        (and (> (length content) 0) (aref content 0))
                      (car content))))
         (and first (listp first) (not (assq 'type first))))))

(defun copilot-agent-gemini--convert-message (msg)
  "Convert a canonical message alist MSG to Gemini wire format.
Three cases for the message content:
  1. MSG has a `parts' key — already Gemini format (tool-result messages).
  2. MSG `content' is native Gemini parts (no canonical `type' key) — pass through
     unchanged so `thoughtSignature' fields are preserved for thinking models.
  3. Otherwise convert canonical typed blocks via `canonical->parts'."
  (let ((role    (cdr (assq 'role msg)))
        (content (cdr (assq 'content msg)))
        (parts   (cdr (assq 'parts msg))))
    `((role  . ,(copilot-agent-gemini--canonical->gemini-role role))
      (parts . ,(cond
                 (parts parts)
                 ((copilot-agent-gemini--native-parts-p content)
                  (if (vectorp content) content (vconcat content)))
                 (t (copilot-agent-gemini--canonical->parts content)))))))

;;; ---------- Request Building ----------

(defun copilot-agent-gemini--build-request (session)
  "Build the JSON request body string for SESSION."
  (let* ((model   (or (plist-get session :model)
                      copilot-agent-gemini-default-model))
         (msgs    (plist-get session :messages))
         (tools   (plist-get session :tools))
         (system  (plist-get session :system-prompt))
         (max-tok (or (plist-get session :max-tokens) 8192))
         (req     `((contents          . ,(vconcat (mapcar
                                                    #'copilot-agent-gemini--convert-message
                                                    msgs)))
                    (generationConfig  . ((maxOutputTokens . ,max-tok))))))
    (when system
      (push `(system_instruction . ((parts . ,(vector `((text . ,system))))))
            req))
    (when tools
      (push `(tools . ,(vconcat (copilot-agent-gemini--format-tools tools)))
            req))
    (json-encode req)))

;;; ---------- Response Parsing ----------

(defun copilot-agent-gemini--parse-response (raw-json)
  "Parse RAW-JSON string from Gemini API into a normalized response plist.
Returns plist with :text :tool-calls :stop-reason :raw-content :error."
  (condition-case err
      (let* ((data       (json-read-from-string raw-json))
             (error-obj  (cdr (assq 'error data)))
             (candidates (cdr (assq 'candidates data))))
        ;; API-level error
        (when error-obj
          (cl-return-from copilot-agent-gemini--parse-response
            (list :error (format "%s: %s"
                                 (cdr (assq 'code error-obj))
                                 (cdr (assq 'message error-obj))))))
        (unless candidates
          (cl-return-from copilot-agent-gemini--parse-response
            (list :error "Gemini response contained no candidates")))
        (let* ((candidate   (aref candidates 0))
               (finish      (cdr (assq 'finishReason candidate)))
               (content     (cdr (assq 'content candidate)))
               (parts       (cdr (assq 'parts content)))
               text tool-calls)
          ;; Walk parts.
          ;; Skip `thought' parts (internal reasoning for thinking models) — they
          ;; must be preserved in raw-content for history but must NOT be shown to
          ;; the user as assistant text.
          (seq-doseq (part (if (vectorp parts) parts (vconcat parts)))
            (cond
             ((cdr (assq 'thought part)) nil)   ; thinking part — skip display
             ((assq 'text part)
              (setq text (concat (or text "") (cdr (assq 'text part)))))
             ((assq 'functionCall part)
              (let* ((fc   (cdr (assq 'functionCall part)))
                     (name (cdr (assq 'name fc)))
                     (args (cdr (assq 'args fc))))
                (push (list :id    name   ; Gemini has no real call IDs; use name so
                            :name  name   ; functionResponse.name matches functionCall.name
                            :input args)
                      tool-calls)))))
          ;; Store the ORIGINAL Gemini parts as raw-content so that thoughtSignature
          ;; fields on functionCall parts are preserved when history is replayed.
          ;; convert-message detects native Gemini parts and passes them through unchanged.
          (let ((ordered-calls (nreverse tool-calls)))
            (list :text        text
                  :tool-calls  ordered-calls
                  :stop-reason finish
                  :raw-content (if (vectorp parts) parts (vconcat parts))))))
    (error (list :error (format "Parse error: %s" (error-message-string err))))))

;;; ---------- Provider Send Function ----------

(defun copilot-agent-gemini-send (session callback)
  "Send SESSION to Gemini API asynchronously.
CALLBACK is called as (RESPONSE-PLIST NIL) or (NIL ERROR-STRING)."
  (let* ((api-key  (copilot-agent-gemini--api-key))
         (model    (or (plist-get session :model)
                       copilot-agent-gemini-default-model))
         (url      (format "%s?key=%s"
                           (copilot-agent-gemini--endpoint model)
                           api-key))
         (body     (copilot-agent-gemini--build-request session)))
    ;; Gemini uses the API key as a query param, no special auth header needed
    (copilot-agent-api--curl-post url '() body
     (lambda (raw-output http-error)
       (if http-error
           (funcall callback nil http-error)
         (let ((parsed (copilot-agent-gemini--parse-response raw-output)))
           (if (plist-get parsed :error)
               (funcall callback nil (plist-get parsed :error))
             (funcall callback parsed nil))))))))

;;; ---------- Tool Result Message Builder ----------

(defun copilot-agent-gemini-make-tool-result-message (tool-results)
  "Build a Gemini user-role message with TOOL-RESULTS.
TOOL-RESULTS is a list of plists: (:tool-use-id ID :content RESULT-STRING)."
  `((role  . "user")
    (parts . ,(vconcat
               (mapcar
                (lambda (r)
                  `((functionResponse
                     . ((name     . ,(plist-get r :tool-use-id))
                        (response . ((result . ,(plist-get r :content))))))))
                tool-results)))))

;;; ---------- Provider Registration ----------

(with-eval-after-load 'copilot-agent-api
  (copilot-agent-api-register-provider
   'gemini
   (list :display-name          "Google Gemini"
         :default-model         copilot-agent-gemini-default-model
         :default-model-fn      (lambda () copilot-agent-gemini-default-model)
         :send-fn               #'copilot-agent-gemini-send
         :make-tool-result-fn   #'copilot-agent-gemini-make-tool-result-message
         :format-tools-fn       #'copilot-agent-gemini--format-tools)))

(provide 'copilot-agent-gemini)
;;; copilot-agent-gemini.el ends here

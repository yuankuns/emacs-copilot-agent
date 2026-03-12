;;; test-regressions.el --- Regression tests for every reported bug -*- lexical-binding: t -*-

;;; Commentary:
;; One ert-deftest per confirmed bug.  Each test is named after the bug it
;; guards against and carries a comment explaining what went wrong.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (d (list root (expand-file-name "providers" root)))
    (unless (member d load-path) (push d load-path))))

(require 'auth-source)
(advice-add 'auth-source-pick-first-password :override (lambda (&rest _) "stub-key")
            '((name . regression-auth-stub)))

(require 'copilot-agent-tools)
(require 'copilot-agent-api)
(require 'copilot-agent-ui)
(require 'copilot-agent-gemini)

;;; ============================================================
;; BUG 1: special-mode parent made buffer globally read-only
;;
;; Symptom: User could not type any character in the chat buffer.
;;          Pressing 's' showed "'s' is not defined" in the echo area
;;          because special-mode's keymap intercepted every key and
;;          buffer-read-only t blocked insertion entirely.
;;
;; Root cause: (define-derived-mode copilot-agent-mode special-mode …)
;;             special-mode calls (setq buffer-read-only t).
;;
;; Fix: Changed parent to fundamental-mode.  History is now protected
;;      solely by the `read-only' TEXT PROPERTY on each inserted string.
;; ============================================================

(ert-deftest regression/special-mode-buffer-not-read-only ()
  "buffer-read-only must be nil in the chat buffer.
Typing was impossible before the fix because special-mode sets
buffer-read-only t for the whole buffer."
  (when (get-buffer "*Copilot Agent*") (kill-buffer "*Copilot Agent*"))
  (unwind-protect
      (with-current-buffer (copilot-agent-ui-get-buffer)
        (should-not buffer-read-only))
    (when (get-buffer "*Copilot Agent*") (kill-buffer "*Copilot Agent*"))))

(ert-deftest regression/can-insert-text-at-point-max ()
  "Inserting text at point-max (input area) must succeed without error.
Before the fix, special-mode's buffer-read-only caused 'Buffer is read-only'."
  (when (get-buffer "*Copilot Agent*") (kill-buffer "*Copilot Agent*"))
  (unwind-protect
      (with-current-buffer (copilot-agent-ui-get-buffer)
        (goto-char (point-max))
        (should (progn (insert "test input") t))
        (should (string-suffix-p "test input" (buffer-string))))
    (when (get-buffer "*Copilot Agent*") (kill-buffer "*Copilot Agent*"))))

(ert-deftest regression/history-still-protected-after-mode-fix ()
  "History text must still be read-only even after the mode fix.
The protection now comes from the `read-only' text property, not buffer-read-only."
  (when (get-buffer "*Copilot Agent*") (kill-buffer "*Copilot Agent*"))
  (unwind-protect
      (with-current-buffer (copilot-agent-ui-get-buffer)
        ;; The welcome header at position 1 must carry the text property
        (should (get-text-property 1 'read-only))
        ;; Attempting to delete it without inhibit-read-only must fail
        (should-error (progn (goto-char 1) (delete-char 1))
                      :type 'text-read-only))
    (when (get-buffer "*Copilot Agent*") (kill-buffer "*Copilot Agent*"))))

;;; ============================================================
;; BUG 2: gemini--upcase-types uppercased ALL strings, not just type values
;;
;; Symptom: Description strings like "Run a shell command" were being sent
;;          to the Gemini API as "RUN A SHELL COMMAND".
;;
;; Root cause: The cond branch `((stringp params) (upcase params))` matched
;;             every string value during recursion, including `description'.
;;
;; Fix: Removed the bare-string branch.  Only the value of a `type' alist
;;      key is uppercased; all other values recurse unchanged.
;; ============================================================

(ert-deftest regression/upcase-types-does-not-touch-description ()
  "description strings must not be uppercased by upcase-types.
Before the fix, any string encountered during recursion was uppercased,
corrupting description fields sent to the Gemini API."
  (require 'copilot-agent-gemini)
  (let* ((params '((type . "object")
                   (description . "a lowercase description")
                   (properties . ((cmd . ((type . "string")
                                          (description . "the command")))))))
         (result (copilot-agent-gemini--upcase-types params)))
    ;; type values must be uppercase
    (should (equal (cdr (assq 'type result)) "OBJECT"))
    (let ((cmd-props (cdr (assq 'cmd (cdr (assq 'properties result))))))
      (should (equal (cdr (assq 'type cmd-props)) "STRING"))
      ;; description must be unchanged
      (should (equal (cdr (assq 'description cmd-props)) "the command")))
    ;; top-level description must be unchanged
    (should (equal (cdr (assq 'description result)) "a lowercase description"))))

(ert-deftest regression/upcase-types-bare-string-is-unchanged ()
  "A bare string passed to upcase-types must be returned as-is.
Before the fix the function always called (upcase params) on any string."
  (require 'copilot-agent-gemini)
  (should (equal (copilot-agent-gemini--upcase-types "object") "object"))
  (should (equal (copilot-agent-gemini--upcase-types "string") "string")))

;;; ============================================================
;; BUG 3: double nreverse in copilot-agent-gemini--parse-response
;;
;; Symptom: When Gemini returned multiple tool calls in one response, the
;;          :tool-calls list was in reverse order (last call appeared first).
;;
;; Root cause: `nreverse' was called twice on the same `tool-calls' list —
;;             once inside the raw-blocks `let*' to build raw-content,
;;             and again in the final plist to build :tool-calls.
;;             The first call mutates the list; the second re-reverses it
;;             back to the push order (reversed).
;;
;; Fix: Call (nreverse tool-calls) exactly once into `ordered-calls' and
;;      reuse that variable in both :tool-calls and raw-content.
;; ============================================================

(ert-deftest regression/gemini-multiple-tool-calls-preserve-order ()
  "Multiple tool calls from Gemini must arrive in declaration order.
Before the fix, the double nreverse returned them in reverse order."
  (require 'copilot-agent-gemini)
  (let* ((json "{\"candidates\":[{\"content\":{\"parts\":[\
{\"functionCall\":{\"name\":\"read_file\",\"args\":{\"path\":\"/a\"}}},\
{\"functionCall\":{\"name\":\"shell_command\",\"args\":{\"command\":\"ls\"}}},\
{\"functionCall\":{\"name\":\"write_file\",\"args\":{\"path\":\"/b\",\"content\":\"x\"}}}\
],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}")
         (result (copilot-agent-gemini--parse-response json))
         (calls  (plist-get result :tool-calls)))
    (should (= (length calls) 3))
    (should (equal (plist-get (nth 0 calls) :name) "read_file"))
    (should (equal (plist-get (nth 1 calls) :name) "shell_command"))
    (should (equal (plist-get (nth 2 calls) :name) "write_file"))))

(ert-deftest regression/gemini-single-tool-call-not-reversed ()
  "A single tool call must not be affected by any reversal logic."
  (require 'copilot-agent-gemini)
  (let* ((json "{\"candidates\":[{\"content\":{\"parts\":[\
{\"functionCall\":{\"name\":\"shell_command\",\"args\":{\"command\":\"pwd\"}}}\
],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}")
         (result (copilot-agent-gemini--parse-response json))
         (calls  (plist-get result :tool-calls)))
    (should (= (length calls) 1))
    (should (equal (plist-get (car calls) :name) "shell_command"))))

;;; ============================================================
;; BUG 4: cl-assert not catchable by ERT should-error
;;
;; Symptom: Test `api/register-provider-validates-send-fn' always FAILED
;;          even though the assertion was being raised, because
;;          `cl-assertion-failed' is not caught by ERT's `should-error'
;;          with :type 'error or :type 'cl-assertion-failed.
;;
;; Root cause: cl-assert signals (signal 'cl-assertion-failed …) whose
;;             condition hierarchy in Emacs 29 is not recognised by ERT's
;;             condition-case wrapper as a subtype of `error'.
;;
;; Fix: Replaced (cl-assert …) with explicit (unless … (error …)) so the
;;      condition is a plain `error', which ERT handles correctly.
;; ============================================================

(ert-deftest regression/register-provider-without-send-fn-signals-plain-error ()
  "Registering a provider without :send-fn must signal a plain `error'.
Before the fix, cl-assert was used; its cl-assertion-failed condition
was not catchable by ERT's should-error."
  (should-error
   (copilot-agent-api-register-provider 'test-bad '(:display-name "Bad"))
   :type 'error))

(ert-deftest regression/register-provider-error-message-mentions-send-fn ()
  "The error message for a missing :send-fn must mention the requirement."
  (let ((caught nil))
    (condition-case err
        (copilot-agent-api-register-provider 'test-bad2 '(:display-name "Bad"))
      (error (setq caught (error-message-string err))))
    (should caught)
    (should (string-match-p "send-fn" caught))))

;;; ============================================================
;; BUG 5: write_file result string said "Wrote" but test checked "Written"
;;
;; Symptom: tools/write-file-creates-file failed because the implementation
;;          returns "Wrote N bytes to PATH" but the test matched "Written".
;;
;; Root cause: Mismatch between implementation string and test expectation.
;;
;; Fix: Updated test to match the actual return value "Wrote".
;; ============================================================

(ert-deftest regression/write-file-result-string-format ()
  "write_file returns a string starting with 'Wrote', not 'Written'."
  (let* ((tmp  (make-temp-file "copilot-reg-test" nil ".txt"))
         (copilot-agent-tools--context (list :directory temporary-file-directory))
         (result (copilot-agent-tools--write-file `((path . ,tmp) (content . "hi")))))
    (unwind-protect
        (progn
          (should (string-match-p "Wrote" result))
          (should-not (string-match-p "Written" result)))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; ============================================================
;; BUG 6: resolve-empty-uses-ctx-dir expected trailing slash
;;
;; Symptom: The test checked (string-prefix-p "/mydir/" result) but
;;          expand-file-name returns "/mydir" (no trailing slash) for "".
;;
;; Root cause: Test assertion used the wrong expected prefix.
;;
;; Fix: Changed to (string-prefix-p "/mydir" result) to match actual output.
;; ============================================================

(ert-deftest regression/resolve-empty-string-gives-ctx-dir-prefix ()
  "Resolving an empty string gives a path inside the context directory.
expand-file-name strips the trailing slash so we match without it."
  (let ((copilot-agent-tools--context (list :directory "/mydir/")))
    (let ((result (copilot-agent-tools--resolve "")))
      (should (string-prefix-p "/mydir" result))
      ;; The old (wrong) test asserted the trailing slash — confirm it's NOT required
      (should-not (string= result "/mydir/")))))

;;; ============================================================
;; BUG 7: shell-cwd-override test used string-trim with "/" char arg
;;
;; Symptom: (string-trim dir "/") strips leading slash, producing
;;          "tmp/copilot-test-xxx/" which doesn't match the pwd output
;;          "/tmp/copilot-test-xxx" (no trailing slash).
;;
;; Root cause: Wrong pattern construction in the test assertion.
;;
;; Fix: Use (regexp-quote (directory-file-name dir)) to match the actual path.
;; ============================================================

(ert-deftest regression/shell-cwd-override-matches-actual-pwd-output ()
  "shell_command pwd output must match the cwd path without trailing slash.
The old test used (string-trim dir \"/\") which removed the leading /
and didn't match the actual pwd output."
  (let* ((tmp (make-temp-file "copilot-reg-cwd" t))
         (copilot-agent-tools--context (list :directory tmp))
         (result (copilot-agent-tools--shell
                  `((command . "pwd") (cwd . ,tmp))))
         (expected (regexp-quote (directory-file-name tmp))))
    (unwind-protect
        (should (string-match-p expected result))
      (delete-directory tmp t))))

;;; ============================================================
;; BUG 8: Null-byte sentinel dropped by process pipe causes raw JSON display
;;
;; Symptom: The chat buffer displayed the raw JSON API response instead of
;;          the parsed text.  Every reply appeared as a JSON blob.
;;
;; Root cause: copilot-agent-api--status-sentinel used "\x00HTTPSTATUS\x00:"
;;             (containing null bytes).  Null bytes are silently dropped when
;;             process output passes through Emacs' process filter.
;;             string-search never found the sentinel, sep-i was nil,
;;             the status code read as 0, and the whole body was passed to
;;             the error callback as "HTTP 0: <json>", which was then
;;             displayed verbatim in the chat buffer.
;;
;; Fix: Changed sentinel to "COPILOT_AGENT_HTTP_STATUS:" (no null bytes).
;; ============================================================

(ert-deftest regression/status-sentinel-contains-no-null-bytes ()
  "The HTTP status sentinel must not contain null bytes.
Null bytes are dropped by Emacs process pipes, making string-search fail."
  (should-not (string-match-p "\x00" copilot-agent-api--status-sentinel)))

(ert-deftest regression/curl-output-parsing-finds-sentinel ()
  "curl output with the sentinel must be split correctly into body and status."
  ;; Simulate what curl writes: body + sentinel + status-code
  (let* ((body    "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"hi\"}]}}]}")
         (raw     (concat body copilot-agent-api--status-sentinel "200"))
         (sep     copilot-agent-api--status-sentinel)
         (idx     (string-search sep raw))
         (parsed-body   (and idx (substring raw 0 idx)))
         (parsed-status (and idx (string-to-number
                                  (string-trim
                                   (substring raw (+ idx (length sep))))))))
    (should idx)
    (should (equal parsed-body body))
    (should (= parsed-status 200))))

(ert-deftest regression/curl-output-parsing-detects-http-error ()
  "A non-2xx status in the sentinel triggers the error callback path."
  (let* ((error-body "{\"error\":{\"code\":400,\"message\":\"Model not found\"}}")
         (raw        (concat error-body copilot-agent-api--status-sentinel "400"))
         (sep        copilot-agent-api--status-sentinel)
         (idx        (string-search sep raw))
         (code       (and idx (string-to-number
                               (string-trim
                                (substring raw (+ idx (length sep))))))))
    (should idx)
    (should (= code 400))
    ;; Confirm it falls outside the 200-299 success range
    (should-not (and (>= code 200) (< code 300)))))

(ert-deftest regression/null-byte-sentinel-would-fail ()
  "Demonstrates that a null-byte sentinel cannot be found after process pipe simulation.
This test documents WHY the old sentinel was broken."
  (let* ((body       "{\"key\":\"value\"}")
         ;; Simulate null bytes being stripped (as Emacs process pipe does)
         (old-sentinel "\x00HTTPSTATUS\x00:")
         (raw-from-pipe (concat body "HTTPSTATUS:" "200"))  ; null bytes stripped
         (idx (string-search old-sentinel raw-from-pipe)))
    ;; The old sentinel is NOT found because null bytes were dropped
    (should-not idx)))

;;; ============================================================
;; BUG 9: Gemini tool call ID "gemini-tool-NAME" did not match functionResponse.name
;;
;; Symptom: Asking the agent to list a directory caused list_directory to be
;;          called over and over in an infinite loop.
;;
;; Root cause: copilot-agent-gemini--parse-response set each tool call's :id to
;;             (format "gemini-tool-%s" name) — e.g. "gemini-tool-list_directory".
;;             copilot-agent-api--process-tools stores :tool-use-id from that :id.
;;             copilot-agent-gemini-make-tool-result-message then puts that value as
;;             functionResponse.name.
;;             Gemini requires functionResponse.name == functionCall.name exactly.
;;             Because "gemini-tool-list_directory" != "list_directory", Gemini
;;             did not recognise the response and kept requesting the same call.
;;
;; Fix: Use the bare function name as the :id so :tool-use-id == function name.
;; ============================================================

(ert-deftest regression/gemini-tool-id-equals-function-name ()
  "Gemini tool call :id must equal the function name, not 'gemini-tool-NAME'.
The id is used as functionResponse.name; it must match functionCall.name exactly."
  (require 'copilot-agent-gemini)
  (let* ((json "{\"candidates\":[{\"content\":{\"parts\":[\
{\"functionCall\":{\"name\":\"list_directory\",\"args\":{\"path\":\"/tmp\"}}}\
],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}")
         (result (copilot-agent-gemini--parse-response json))
         (tc     (car (plist-get result :tool-calls))))
    ;; :id must equal the bare function name
    (should (equal (plist-get tc :id)   "list_directory"))
    (should (equal (plist-get tc :name) "list_directory"))
    ;; Must NOT have the old "gemini-tool-" prefix
    (should-not (string-prefix-p "gemini-tool-" (plist-get tc :id)))))

(ert-deftest regression/gemini-tool-result-name-matches-call-name ()
  "functionResponse.name built by make-tool-result-message must equal
the original functionCall.name so Gemini recognises the response."
  (require 'copilot-agent-gemini)
  (let* ((tool-results (list (list :tool-use-id "list_directory"
                                   :content "file1.txt\nfile2.txt")))
         (msg  (copilot-agent-gemini-make-tool-result-message tool-results))
         (part (aref (cdr (assq 'parts msg)) 0))
         (fr   (cdr (assq 'functionResponse part))))
    (should (equal (cdr (assq 'name fr)) "list_directory"))))

;;; ============================================================
;; BUG 10a: raw-blocks nested tool calls inside a sub-list (used `list' not `append')
;;
;; Symptom: After executing a tool, Gemini still looped — calling list_directory
;;          repeatedly.  (BUG 9 fixed the immediate response; BUG 10 caused the loop
;;          to resume on the second iteration when history was sent back.)
;;
;; Root cause: In copilot-agent-gemini--parse-response, raw-blocks was built as:
;;   (vconcat (delq nil (list text-block (mapcar ... ordered-calls))))
;; `list' wraps the mapcar result as ONE element: (text-block (tc1 tc2)).
;; After vconcat: [text-block (tc1 tc2)] — tool calls are a nested list, not
;; flat vector siblings.  When canonical->parts iterated the vector, it saw
;; the list (tc1 tc2) as a single block, found no `type' key, and dropped it.
;; Gemini therefore saw an empty model turn and called the tool again.
;;
;; Fix: Use `append' instead of `list' to build a flat block list:
;;   (vconcat (delq nil (append (list text-block) (mapcar ... ordered-calls))))
;;
;; BUG 10b: convert-message produced empty parts for tool-result messages
;;
;; Root cause: copilot-agent-gemini-make-tool-result-message stores the result
;; as ((role . "user") (parts . [...])).  convert-message read (content msg),
;; got nil, and produced an empty parts vector — Gemini saw an empty user turn
;; instead of the functionResponse, so it called the tool again.
;;
;; Fix: convert-message checks for a pre-existing `parts' key and uses it
;; directly, skipping canonical->parts conversion.
;; ============================================================

(ert-deftest regression/gemini-raw-blocks-tool-calls-are-flat ()
  "raw-content must be the original Gemini parts vector stored flat.
Previously raw-blocks were rebuilt from scratch (losing thoughtSignature) and
the tool calls were nested inside a sub-list because `list' was used instead of
`append'.  Now the original parts are stored verbatim as native Gemini format."
  (require 'copilot-agent-gemini)
  (let* ((json "{\"candidates\":[{\"content\":{\"parts\":[\
{\"functionCall\":{\"name\":\"list_directory\",\"args\":{\"path\":\"/tmp\"}}},\
{\"functionCall\":{\"name\":\"read_file\",\"args\":{\"path\":\"/etc/hosts\"}}}\
],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}")
         (result (copilot-agent-gemini--parse-response json))
         (raw    (plist-get result :raw-content)))
    ;; raw-content must be a flat vector with 2 elements (not a nested list)
    (should (vectorp raw))
    (should (= (length raw) 2))
    ;; Each element is a native Gemini part (functionCall), not a canonical type="tool_use" block
    (should (assq 'functionCall (aref raw 0)))
    (should (assq 'functionCall (aref raw 1)))
    ;; Names are preserved from original parts
    (should (equal (cdr (assq 'name (cdr (assq 'functionCall (aref raw 0))))) "list_directory"))
    (should (equal (cdr (assq 'name (cdr (assq 'functionCall (aref raw 1))))) "read_file"))))

(ert-deftest regression/gemini-convert-message-passes-through-parts ()
  "convert-message must pass `parts' messages through unchanged.
Before the fix it read `content', got nil, and produced an empty parts vector."
  (require 'copilot-agent-gemini)
  (let* ((fr-part `((functionResponse
                     . ((name . "list_directory")
                        (response . ((result . "file1\nfile2")))))))
         (msg `((role . "user") (parts . ,(vector fr-part))))
         (converted (copilot-agent-gemini--convert-message msg))
         (parts (cdr (assq 'parts converted))))
    ;; Must not be empty
    (should (> (length parts) 0))
    ;; Must contain the functionResponse part
    (should (assq 'functionResponse (aref parts 0)))))

(ert-deftest regression/gemini-tool-result-roundtrip ()
  "A tool result message built by make-tool-result-message must survive
convert-message with its functionResponse intact."
  (require 'copilot-agent-gemini)
  (let* ((results (list (list :tool-use-id "list_directory" :content "a\nb")))
         (msg      (copilot-agent-gemini-make-tool-result-message results))
         (conv     (copilot-agent-gemini--convert-message msg))
         (parts    (cdr (assq 'parts conv)))
         (part0    (aref parts 0))
         (fr       (cdr (assq 'functionResponse part0))))
    (should (equal (cdr (assq 'name fr)) "list_directory"))
    (should (equal (cdr (assq 'result (cdr (assq 'response fr)))) "a\nb"))))

;;; ============================================================
;; BUG 11: thoughtSignature stripped from functionCall history (thinking models)
;;
;; Symptom: INVALID_ARGUMENT "Function call is missing a thought_signature in
;;          functionCall parts" from Gemini thinking models (gemini-2.5-pro etc.).
;;
;; Root cause: --parse-response rebuilt raw-blocks from scratch, discarding the
;;             `thoughtSignature' field that Gemini attaches to each functionCall
;;             part in thinking-mode responses.  When history was replayed, the
;;             reconstructed functionCall lacked the signature and Gemini rejected it.
;;
;;             Additionally, `thought' parts (internal reasoning) were being included
;;             in the display text sent to the user.
;;
;; Fix: Store the original Gemini `parts' vector as raw-content unchanged.
;;      convert-message detects native Gemini parts (no canonical `type' key) and
;;      passes them through without re-conversion, preserving thoughtSignature.
;;      --parse-response skips parts where (cdr (assq 'thought part)) is truthy
;;      so internal reasoning is never shown to the user.
;; ============================================================

(ert-deftest regression/thought-signature-preserved-in-raw-content ()
  "raw-content must be the original Gemini parts, preserving thoughtSignature."
  (require 'copilot-agent-gemini)
  (let* ((json (concat
                "{\"candidates\":[{\"content\":{\"parts\":["
                "{\"thought\":true,\"text\":\"thinking...\",\"thoughtSignature\":\"SIG123\"},"
                "{\"functionCall\":{\"name\":\"list_directory\",\"args\":{\"path\":\"/tmp\"}},"
                "\"thoughtSignature\":\"SIG456\"}"
                "],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}"))
         (result (copilot-agent-gemini--parse-response json))
         (raw    (plist-get result :raw-content)))
    ;; raw-content must be the original vector (2 parts)
    (should (vectorp raw))
    (should (= (length raw) 2))
    ;; The functionCall part must still have thoughtSignature
    (let ((fc-part (aref raw 1)))
      (should (equal (cdr (assq 'thoughtSignature fc-part)) "SIG456")))))

(ert-deftest regression/thought-parts-not-shown-to-user ()
  "Internal thought parts must be excluded from the :text returned to the user."
  (require 'copilot-agent-gemini)
  (let* ((json (concat
                "{\"candidates\":[{\"content\":{\"parts\":["
                "{\"thought\":true,\"text\":\"internal reasoning\",\"thoughtSignature\":\"SIG\"},"
                "{\"text\":\"Here is the answer.\"}"
                "],\"role\":\"model\"},\"finishReason\":\"STOP\"}]}"))
         (result (copilot-agent-gemini--parse-response json))
         (text   (plist-get result :text)))
    (should (equal text "Here is the answer."))
    (should-not (string-match-p "internal reasoning" (or text "")))))

(ert-deftest regression/native-parts-pass-through-convert-message ()
  "convert-message must pass native Gemini parts through unchanged (no re-conversion)
so thoughtSignature on functionCall parts is not lost."
  (require 'copilot-agent-gemini)
  (let* ((native-parts
          (vector `((functionCall . ((name . "list_directory") (args . ((path . "/tmp")))))
                    (thoughtSignature . "SIG999"))))
         (msg `((role . "assistant") (content . ,native-parts)))
         (conv  (copilot-agent-gemini--convert-message msg))
         (parts (cdr (assq 'parts conv)))
         (part0 (aref parts 0)))
    ;; thoughtSignature must survive
    (should (equal (cdr (assq 'thoughtSignature part0)) "SIG999"))
    ;; functionCall must still be present
    (should (assq 'functionCall part0))))

(ert-deftest regression/native-parts-p-detects-gemini-format ()
  "native-parts-p must return t for Gemini parts and nil for canonical typed blocks."
  (require 'copilot-agent-gemini)
  ;; Gemini native: no `type' key
  (should (copilot-agent-gemini--native-parts-p
           (vector '((functionCall . ((name . "f") (args . nil)))))))
  (should (copilot-agent-gemini--native-parts-p
           (vector '((text . "hello") (thoughtSignature . "x")))))
  ;; Canonical: has `type' key
  (should-not (copilot-agent-gemini--native-parts-p
               (vector '((type . "tool_use") (name . "f")))))
  (should-not (copilot-agent-gemini--native-parts-p
               (vector '((type . "text") (text . "hello")))))
  ;; Edge cases
  (should-not (copilot-agent-gemini--native-parts-p nil))
  (should-not (copilot-agent-gemini--native-parts-p [])))

;;; ============================================================
;; BUG 12: `lambda (t)' is an invalid variable name in lexical-binding mode
;;
;; Symptom: package-vc-install succeeded but all Gemini provider functions
;;          (--native-parts-p, --convert-message, --parse-response, etc.) and
;;          copilot-agent-explain-region appeared undefined after install.
;;
;; Root cause: Two source files used `t' as a lambda parameter name:
;;   providers/copilot-agent-gemini.el line 341: (lambda (t) ...)
;;   copilot-agent.el line 137:                  (lambda (t) ...)
;; In `lexical-binding: t' mode the byte-compiler rejects `t' as a variable
;; because it shadows the Lisp boolean constant:
;;   Error: Invalid lambda variable t
;; This caused batch-byte-compile to fail with exit code 1, so no .elc was
;; produced.  The ERT test suite never caught this because it loads source
;; files interpreted — the interpreter does not enforce the same restriction.
;;
;; Fix: Renamed the parameter to `tier' and `text' respectively.
;; Prevention: GitHub Actions CI now byte-compiles all files on every PR.
;; ============================================================

(ert-deftest regression/no-lambda-t-parameter-in-source-files ()
  "No source file may use `t' as a lambda parameter name.
In lexical-binding mode this is a fatal byte-compile error that prevents
.elc generation by package-vc-install, leaving functions undefined."
  (let* ((root (expand-file-name
                ".." (file-name-directory (or load-file-name buffer-file-name))))
         (source-files
          (append
           (directory-files root t "\\.el\\'")
           (directory-files (expand-file-name "providers" root) t "\\.el\\'")))
         offenders)
    (dolist (f source-files)
      (with-temp-buffer
        (insert-file-contents f)
        ;; Match `t' as a standalone parameter in any position in the lambda list.
        ;; \\_< / \\_> are word-boundary anchors that avoid matching `t-foo'.
        (when (re-search-forward "(lambda\\s-+(\\([^)]*\\_<t\\_>[^)]*\\))" nil t)
          (push (file-name-nondirectory f) offenders))))
    (should-not offenders)))

(ert-deftest regression/source-files-byte-compile-without-errors ()
  "All source .el files must byte-compile without fatal errors.
Uses byte-compile-file with byte-compile-dest-file-function redirected
to a temporary file so no .elc artifacts are left in the source tree.
Catches issues like invalid lambda variable names, unbalanced parens, etc."
  (let* ((root (expand-file-name
                ".." (file-name-directory (or load-file-name buffer-file-name))))
         (files `(,(expand-file-name "copilot-agent-tools.el"  root)
                  ,(expand-file-name "copilot-agent-api.el"    root)
                  ,(expand-file-name "copilot-agent-ui.el"     root)
                  ,(expand-file-name "copilot-agent-status.el" root)
                  ,(expand-file-name "copilot-agent.el"        root)
                  ,(expand-file-name "providers/copilot-agent-anthropic.el"    root)
                  ,(expand-file-name "providers/copilot-agent-gemini.el"       root)
                  ,(expand-file-name "providers/copilot-agent-qwen.el"         root)
                  ,(expand-file-name "providers/copilot-agent-github-copilot.el" root)))
         (error-log nil))
    (dolist (f files)
      (let ((tmp (make-temp-file "copilot-bytecomp-" nil ".elc")))
        (unwind-protect
            (condition-case err
                ;; Redirect .elc output to a temp file so no artifacts land in the tree.
                (let ((byte-compile-dest-file-function (lambda (_) tmp))
                      (byte-compile-error-on-warn nil))
                  (byte-compile-file f))
              (error
               (push (format "%s: %s" (file-name-nondirectory f)
                             (error-message-string err))
                     error-log)))
          (when (file-exists-p tmp) (delete-file tmp)))))
    (should-not error-log)))

;;; ============================================================
;; BUG 13: providers/ not on load-path after package-vc-install
;;
;; Symptom: (require 'copilot-agent-qwen) failed with "Cannot open load
;;          file" because package-vc-install only adds the package root to
;;          load-path, not subdirectories.  The byte-compiler also hit this
;;          during compilation of test files in the same pass.
;;
;; Root cause: Provider files live in providers/ which was never added to
;;             load-path.  copilot-agent-load-providers used full paths so
;;             initial loading worked, but any subsequent (require ...) call
;;             (e.g. from copilot-agent-status) failed.
;;
;; Fix: eval-and-compile block in copilot-agent-api.el (required by all
;;      entry points) adds providers/ to load-path at both compile time
;;      and load time.
;; ============================================================

(ert-deftest regression/providers-on-load-path-after-require-api ()
  "Loading copilot-agent-api must add providers/ to load-path.
Simulates package-vc-install: temporarily restricts load-path to only
the package root (no providers/) then force-loads copilot-agent-api via
`load' (bypassing require's no-op for already-provided features) and
verifies the eval-and-compile block adds providers/ to the local load-path.
Removing or breaking the load-path setup causes this test to fail."
  (let* ((root     (expand-file-name
                    ".." (file-name-directory (or load-file-name buffer-file-name))))
         (prov-dir (directory-file-name (expand-file-name "providers" root)))
         ;; Start from a load-path that does NOT include providers/.
         (load-path (list root)))
    ;; Force-load the file (load always executes, unlike require which is a
    ;; no-op for already-provided features).
    (load (expand-file-name "copilot-agent-api" root) nil t)
    ;; The eval-and-compile block must have added providers/ to load-path.
    (should (member prov-dir load-path))
    ;; All provider features must be requireable with providers/ on load-path.
    (should (require 'copilot-agent-qwen          nil t))
    (should (require 'copilot-agent-anthropic      nil t))
    (should (require 'copilot-agent-gemini         nil t))
    (should (require 'copilot-agent-github-copilot nil t))))

(provide 'test-regressions)
;;; test-regressions.el ends here

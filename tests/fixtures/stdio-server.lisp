(require '#:asdf)
(require '#:sb-posix)
(asdf:load-system '#:yason)

(defvar *write-lock*
  #+sb-thread (sb-thread:make-mutex :name "mcparen fixture output")
  #-sb-thread nil
  "The lock preserving one JSON document per output line.")

(defvar *workers* nil
  "The delayed response threads that must finish before fixture exit.")

(defvar *child-process-identifiers* nil
  "Long-lived child identifiers left to process-group cleanup.")

(defvar *server-request-parent* nil
  "The tool call waiting for the fixture's server-originated request.")

(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when absent."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (if present-p value default)))

(defun json-object (&rest pairs)
  "Return an equal hash table populated by alternating PAIRS."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun write-json (message)
  "Write one compact JSON MESSAGE to standard output."
  (flet ((write-message ()
           "Write MESSAGE while the caller holds the output lock."
           (yason:encode message *standard-output*)
           (terpri *standard-output*)
           (finish-output *standard-output*)))
    #+sb-thread
    (sb-thread:with-mutex (*write-lock*)
      (write-message))
    #-sb-thread
    (write-message)))

(defun write-raw-output (text)
  "Write TEXT directly to standard output while preserving fixture framing."
  (flet ((write-message ()
           "Write TEXT while the caller holds the output lock."
           (write-line text *standard-output*)
           (finish-output *standard-output*)))
    #+sb-thread
    (sb-thread:with-mutex (*write-lock*)
      (write-message))
    #-sb-thread
    (write-message)))

(defun response (request result)
  "Return a successful JSON-RPC response to REQUEST carrying RESULT."
  (json-object "jsonrpc" "2.0"
               "id" (json-get request "id")
               "result" result))

(defun text-result (text)
  "Return a tool result carrying TEXT."
  (json-object
   "content"
   (vector (json-object "type" "text" "text" text))
   "structuredContent"
   (json-object "echo" text)
   "isError" yason:false))

(defun delayed-tool-response (request name delay)
  "Write REQUEST's response for NAME after DELAY seconds."
  (let ((worker
          #+sb-thread
          (sb-thread:make-thread
           (lambda ()
             (sleep delay)
             (write-json
              (response request (text-result name))))
           :name (format nil "mcparen fixture ~A" name))
          #-sb-thread
          nil))
    #+sb-thread
    (push worker *workers*)
    #-sb-thread
    (progn
      (sleep delay)
      (write-json
       (response request (text-result name))))))

(defun flood-stderr ()
  "Emit more diagnostic text than the client retains."
  (dotimes (index 300)
    (format *error-output*
            "discard-~3,'0D-abcdefghijklmnopqrstuvwxyz0123456789~%"
            index))
  (write-line "stderr-tail-marker" *error-output*)
  (finish-output *error-output*))

(defun write-long-stderr-line ()
  "Emit one diagnostic line much larger than the retained stderr tail."
  (write-string
   (make-string (* 1024 1024) :initial-element #\x)
   *error-output*)
  (write-line "stderr-long-line-marker" *error-output*)
  (finish-output *error-output*))

(defun send-progress-notifications (count)
  "Send COUNT ordered progress notifications."
  (dotimes (index count)
    (write-json
     (json-object
      "jsonrpc" "2.0"
      "method" "notifications/progress"
      "params" (json-object "index" index)))))

(defun spawn-child-result ()
  "Start a long-lived child and return its process identifier."
  (let ((identifier (sb-posix:fork)))
    (when (zerop identifier)
      (handler-case
          (loop
            (sleep 30))
        (error ()
          (sb-ext:exit :code 1))))
    (push identifier *child-process-identifiers*)
    (json-object
     "content"
     (vector
      (json-object
       "type" "text"
       "text" (write-to-string identifier)))
     "structuredContent"
     (json-object "pid" identifier)
     "isError" yason:false)))

(defun handle-request (request)
  "Handle one MCP fixture REQUEST."
  (let ((method (json-get request "method")))
    (cond
      ((and (null method)
            (equal
             (json-get request "id" :absent)
             "fixture-server-request"))
       (let* ((result (json-get request "result"))
              (role (json-get result "role")))
         (write-json
          (response
           *server-request-parent*
           (text-result role)))
         (setf *server-request-parent* nil)))
      ((string= method "initialize")
       (unless
           (string= (json-get
                     (json-get request "params")
                     "protocolVersion")
                    "2025-11-25")
         (write-json
          (json-object
           "jsonrpc" "2.0"
           "id" (json-get request "id")
           "error"
           (json-object
            "code" -32602
            "message" "Unexpected protocol version.")))
         (return-from handle-request nil))
       (write-json
        (response
         request
         (json-object
          "protocolVersion" "2025-11-25"
          "capabilities"
          (json-object "tools" (json-object))
          "serverInfo"
          (json-object
           "name" "mcparen-stdio-fixture"
           "version" "1")))))
      ((string= method "notifications/initialized")
       nil)
      ((string= method "notifications/cancelled")
       (format *error-output* "cancelled:~A~%"
               (json-get (json-get request "params") "requestId"))
       (finish-output *error-output*))
      ((string= method "ping")
       (write-json (response request (json-object))))
      ((string= method "tools/call")
       (let* ((params (json-get request "params"))
              (name (json-get params "name")))
         (format *error-output* "received:~A~%" name)
         (finish-output *error-output*)
         (cond
           ((string= name "slow")
            (delayed-tool-response request name 0.25))
           ((string= name "fast")
            (delayed-tool-response request name 0.01))
           ((string= name "never")
            nil)
           ((string= name "utf8")
            (write-json
             (response
              request
              (text-result
               (json-get
                (json-get params "arguments")
                "text")))))
           ((string= name "stderr-flood")
            (flood-stderr)
            (write-json
             (response request (text-result "flooded"))))
           ((string= name "stderr-long-line")
            (write-long-stderr-line)
            (write-json
             (response request (text-result "stderr-drained"))))
           ((string= name "oversized-stdout")
            (write-json
             (response
              request
              (text-result
               (make-string (* 64 1024)
                            :initial-element #\x)))))
           ((string= name "malformed-stdout")
            (write-raw-output "{malformed JSON-RPC"))
           ((string= name "callback-order")
            (send-progress-notifications 32)
            (write-json
             (response request (text-result "callbacks-sent"))))
           ((string= name "callback-overflow")
            (send-progress-notifications 256)
            (write-json
             (response request (text-result "callbacks-flooded"))))
           ((string= name "spawn-child")
            (write-json
             (response request (spawn-child-result))))
           ((string= name "server-request")
            (setf *server-request-parent* request)
            (write-json
             (json-object
              "jsonrpc" "2.0"
              "id" "fixture-server-request"
              "method" "sampling/createMessage"
              "params"
              (json-object "prompt" "fixture"))))
           (t
            (write-json
             (response request (text-result name)))))))
      (t
       (write-json
        (json-object
         "jsonrpc" "2.0"
         "id" (json-get request "id")
         "error"
         (json-object
          "code" -32601
          "message" (format nil "Unknown method ~A." method))))))))

(loop for line = (read-line *standard-input* nil nil)
      while line
      unless (zerop
              (length
               (string-trim '(#\Space #\Tab #\Return #\Newline)
                            line)))
        do
           (handle-request
            (yason:parse
             line
             :json-arrays-as-vectors t
             :json-booleans-as-symbols t
             :json-nulls-as-keyword t)))

#+sb-thread
(dolist (worker *workers*)
  (when (sb-thread:thread-alive-p worker)
    (sb-thread:join-thread worker)))

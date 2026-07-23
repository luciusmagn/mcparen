(in-package #:mcparen)

;;;; -- JSON Values --

(defparameter *json-diagnostic-limit* 4096
  "The maximum characters retained from invalid JSON in diagnostics.")

(defparameter *mcp-maximum-message-characters* (* 16 1024 1024)
  "The default maximum characters accepted in one inbound MCP document.")


;;;; -- Public JSON Values --

(-> json-true-value () t)
(defun json-true-value ()
  "Return the canonical value used for JSON true."
  yason:true)

(-> json-false-value () t)
(defun json-false-value ()
  "Return the canonical value used for JSON false."
  yason:false)

(-> json-null-value () keyword)
(defun json-null-value ()
  "Return the canonical value used for JSON null."
  :null)


;;;; -- JSON Construction and Parsing --

(-> json-object (&rest t) hash-table)
(defun json-object (&rest pairs)
  "Return an equal hash table populated by alternating string keys and values."
  (unless (evenp (length pairs))
    (error 'mcp-protocol-error
           :message "JSON object construction requires key and value pairs."
           :payload nil))
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do
             (unless (stringp key)
               (error 'mcp-protocol-error
                      :message "JSON object keys must be strings."
                      :payload nil))
             (setf (gethash key object) value))
    object))

(-> json-get (hash-table string &optional t) t)
(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when it is absent."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (if present-p value default)))

(-> json-encode (t) string)
(defun json-encode (value)
  "Encode VALUE as one compact JSON document."
  (with-output-to-string (stream)
    (yason:encode value stream)))

(-> mcp-message-too-large-error (string integer) null)
(defun mcp-message-too-large-error (source limit)
  "Signal that inbound SOURCE exceeded LIMIT characters."
  (error 'mcp-message-too-large
         :message
         (format nil "The ~A exceeded the ~D-character safety limit."
                 source limit)
         :method nil
         :payload nil
         :source source
         :limit limit))

(-> stream-read-bounded-line
    (stream integer string)
    (values t boolean))
(defun stream-read-bounded-line (stream limit source)
  "Read one line from STREAM without retaining more than LIMIT characters.

Return the line or NIL, followed by true when end of input was encountered."
  (let ((characters
          (make-array (max 1 (min limit 4096))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0)))
    (loop for character = (read-char stream nil nil)
          do
             (cond
               ((null character)
                (return
                  (values
                   (and (plusp (length characters))
                        (coerce characters 'string))
                   t)))
               ((char= character #\Newline)
                (return (values (coerce characters 'string) nil)))
               ((>= (length characters) limit)
                (mcp-message-too-large-error source limit))
               (t
                (vector-push-extend character characters 4096))))))

(-> stream-read-bounded-string
    (stream integer string)
    string)
(defun stream-read-bounded-string (stream limit source)
  "Read STREAM to a string while rejecting more than LIMIT characters."
  (let ((characters
          (make-array (max 1 (min limit 4096))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0))
        (buffer (make-string 8192)))
    (loop for count = (read-sequence buffer stream)
          while (plusp count)
          do
             (when (> (+ (length characters) count) limit)
               (mcp-message-too-large-error source limit))
             (loop for index below count
                   do (vector-push-extend
                       (char buffer index) characters 8192)))
    (coerce characters 'string)))

(-> json-decode
    (string &key (:limit integer) (:source-name string))
    t)
(defun json-decode
    (source
     &key
       (limit *mcp-maximum-message-characters*)
       (source-name "MCP JSON document"))
  "Decode one complete JSON document from SOURCE within LIMIT characters."
  (when (> (length source) limit)
    (mcp-message-too-large-error source-name limit))
  (handler-case
      (with-input-from-string (stream source)
        (let ((value
                (yason:parse
                 stream
                 :json-arrays-as-vectors t
                 :json-booleans-as-symbols t
                 :json-nulls-as-keyword t)))
          (loop for character = (read-char stream nil nil)
                while character
                unless (find character
                             '(#\Space #\Tab #\Newline #\Return #\Page))
                  do (error "Unexpected text follows the JSON document."))
          value))
    (error (cause)
      (error 'mcp-protocol-error
             :message (format nil "Could not decode MCP JSON: ~A" cause)
             :payload
             (subseq source 0 (min (length source)
                                   *json-diagnostic-limit*))))))

(-> json-true-p (t) boolean)
(defun json-true-p (value)
  "Return true exactly when VALUE represents JSON true."
  (eq value yason:true))

(-> json-boolean-p (t) boolean)
(defun json-boolean-p (value)
  "Return true when VALUE is one of the two exact JSON boolean symbols."
  (or (eq value yason:true)
      (eq value yason:false)))

(-> json-sequence->list (t) list)
(defun json-sequence->list (value)
  "Return JSON array VALUE as a fresh list."
  (unless (vectorp value)
    (error 'mcp-protocol-error
           :message "The MCP server returned a value where an array was required."
           :payload value))
  (coerce value 'list))

(-> bounded-diagnostic (t &key (:limit integer)) string)
(defun bounded-diagnostic (value &key (limit *json-diagnostic-limit*))
  "Return a bounded printed representation of VALUE for a diagnostic."
  (let ((text (with-output-to-string (stream)
                (let ((*print-length* 20)
                      (*print-level* 8))
                  (prin1 value stream)))))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "..."))))


;;;; -- JSON-RPC Validation --

(-> json-rpc-identifier-p (t) boolean)
(defun json-rpc-identifier-p (value)
  "Return true when VALUE is an MCP request identifier."
  (or (stringp value)
      (realp value)))

(-> json-rpc-message-validate (t) keyword)
(defun json-rpc-message-validate (message)
  "Validate MESSAGE as one MCP JSON-RPC object and return its message kind."
  (unless (hash-table-p message)
    (error 'mcp-protocol-error
           :message "The MCP peer emitted a non-object JSON-RPC message."
           :method nil
           :payload message))
  (unless (equal (json-get message "jsonrpc") "2.0")
    (error 'mcp-protocol-error
           :message "The MCP peer emitted invalid JSON-RPC version metadata."
           :method nil
           :payload message))
  (multiple-value-bind (method method-present-p)
      (gethash "method" message)
    (multiple-value-bind (identifier identifier-present-p)
        (gethash "id" message)
      (multiple-value-bind (result result-present-p)
          (gethash "result" message)
        (declare (ignore result))
        (multiple-value-bind (error-value error-present-p)
            (gethash "error" message)
          (cond
            (method-present-p
             (unless (stringp method)
               (error 'mcp-protocol-error
                      :message "The MCP JSON-RPC method is not a string."
                      :method nil
                      :payload message))
             (when (or result-present-p error-present-p)
               (error 'mcp-protocol-error
                      :message
                      "An MCP JSON-RPC request or notification contains response fields."
                      :method method
                      :payload message))
             (multiple-value-bind (params params-present-p)
                 (gethash "params" message)
               (when (and params-present-p
                          (not (hash-table-p params)))
                 (error 'mcp-protocol-error
                        :message "The MCP JSON-RPC params value is not an object."
                        :method method
                        :payload message)))
             (if identifier-present-p
                 (progn
                   (unless (json-rpc-identifier-p identifier)
                     (error 'mcp-protocol-error
                            :message
                            "The MCP JSON-RPC request identifier is invalid."
                            :method method
                            :payload message))
                   ':request)
                 ':notification))
            ((or result-present-p error-present-p)
             (unless (and identifier-present-p
                          (json-rpc-identifier-p identifier))
               (error 'mcp-protocol-error
                      :message
                      "The MCP JSON-RPC response identifier is absent or invalid."
                      :method nil
                      :payload message))
             (when (eq result-present-p error-present-p)
               (error 'mcp-protocol-error
                      :message
                      "An MCP JSON-RPC response must contain exactly one result or error."
                      :method nil
                      :payload message))
             (when error-present-p
               (unless (and (hash-table-p error-value)
                            (integerp (json-get error-value "code"))
                            (stringp (json-get error-value "message")))
                 (error 'mcp-protocol-error
                        :message "The MCP JSON-RPC error object is malformed."
                        :method nil
                        :payload message)))
             ':response)
            (t
             (error 'mcp-protocol-error
                    :message "The MCP peer emitted an unclassified JSON-RPC object."
                    :method nil
                    :payload message))))))))

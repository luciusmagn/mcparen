(in-package #:mcparen)

;;;; -- JSON Values --

(defparameter *json-diagnostic-limit* 4096
  "The maximum characters retained from invalid JSON in diagnostics.")

(defparameter *mcp-maximum-message-characters* (* 16 1024 1024)
  "The default maximum characters accepted in one inbound MCP document.")

(defparameter *json-maximum-depth* 64
  "The maximum nested object and array depth in one JSON document.")

(defparameter *json-maximum-nodes* 100000
  "The maximum value and object-key nodes in one JSON document.")

(defparameter *json-maximum-string-characters* (* 12 1024 1024)
  "The maximum decoded characters in one JSON string.")

(defparameter *json-maximum-aggregate-string-characters* (* 12 1024 1024)
  "The maximum decoded string characters across one JSON document.")

(defparameter *json-maximum-object-key-characters* 4096
  "The maximum decoded characters in one JSON object key.")

(defparameter *json-maximum-object-members* 4096
  "The maximum members in one JSON object.")

(defparameter *json-maximum-array-elements* 65536
  "The maximum elements in one JSON array.")

(defparameter *json-maximum-number-characters* 256
  "The maximum lexical characters in one JSON number.")


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

(-> mcp-message-too-large-error (string integer) null)
(defun mcp-message-too-large-error (source limit)
  "Signal that SOURCE exceeded LIMIT characters."
  (error 'mcp-message-too-large
         :message
         (format nil "The ~A exceeded the ~D-character safety limit."
                 source limit)
         :method nil
         :payload nil
         :source source
         :limit limit))

(-> json--limit-error (string string integer) null)
(defun json--limit-error (source constraint limit)
  "Signal that SOURCE exceeded the structural CONSTRAINT bounded by LIMIT."
  (error 'mcp-protocol-error
         :message
         (format nil "The ~A exceeded the JSON ~A safety limit of ~D."
                 source constraint limit)
         :method nil
         :payload nil))

(-> json--whitespace-p (character) boolean)
(defun json--whitespace-p (character)
  "Return true when CHARACTER is JSON whitespace."
  (not (null
        (find character '(#\Space #\Tab #\Newline #\Return #\Page)))))

(-> json--token-delimiter-p (character) boolean)
(defun json--token-delimiter-p (character)
  "Return true when CHARACTER terminates an unquoted JSON token."
  (or (json--whitespace-p character)
      (not
       (null
        (find character '(#\, #\: #\[ #\] #\{ #\} #\"))))))

(-> json--lexical-preflight (string string) null)
(defun json--lexical-preflight (source source-name)
  "Reject lexically excessive SOURCE before a recursive JSON parser sees it."
  (let ((index 0)
        (source-length (length source))
        (depth 0)
        (nodes 0)
        (aggregate-string-characters 0))
    (labels ((account-node ()
               "Account for one possible decoded tree node."
               (incf nodes)
               (when (> nodes *json-maximum-nodes*)
                 (json--limit-error
                  source-name "node count" *json-maximum-nodes*)))

             (account-string-character (string-characters)
               "Account for one decoded string character."
               (incf aggregate-string-characters)
               (when (> string-characters
                        *json-maximum-string-characters*)
                 (json--limit-error
                  source-name
                  "characters in one string"
                  *json-maximum-string-characters*))
               (when (> aggregate-string-characters
                        *json-maximum-aggregate-string-characters*)
                 (json--limit-error
                  source-name
                  "aggregate string characters"
                  *json-maximum-aggregate-string-characters*)))

             (scan-string ()
               "Scan one quoted string beginning at INDEX."
               (account-node)
               (incf index)
               (let ((string-characters 0))
                 (loop
                   (when (>= index source-length)
                     (error 'mcp-protocol-error
                            :message
                            (format nil
                                    "The ~A contains an unterminated JSON string."
                                    source-name)
                            :method nil
                            :payload nil))
                   (let ((character (char source index)))
                     (cond
                       ((char= character #\")
                        (incf index)
                        (return))
                       ((char= character #\\)
                        (incf index)
                        (when (>= index source-length)
                          (error 'mcp-protocol-error
                                 :message
                                 (format nil
                                         "The ~A ends in a JSON string escape."
                                         source-name)
                                 :method nil
                                 :payload nil))
                        (if (char= (char source index) #\u)
                            (progn
                              (when (>= (+ index 4) source-length)
                                (error 'mcp-protocol-error
                                       :message
                                       (format nil
                                               "The ~A contains a truncated Unicode escape."
                                               source-name)
                                       :method nil
                                       :payload nil))
                              (loop for offset from 1 to 4
                                    unless
                                      (digit-char-p
                                       (char source (+ index offset))
                                       16)
                                      do
                                         (error
                                          'mcp-protocol-error
                                          :message
                                          (format
                                           nil
                                           "The ~A contains an invalid Unicode escape."
                                           source-name)
                                          :method nil
                                          :payload nil))
                              (incf index 5))
                            (incf index))
                        (incf string-characters)
                        (account-string-character string-characters))
                       (t
                        (incf index)
                        (incf string-characters)
                        (account-string-character
                         string-characters)))))))

             (scan-token ()
               "Scan one unquoted scalar token beginning at INDEX."
               (account-node)
               (let ((start index))
                 (loop while
                         (and (< index source-length)
                              (not
                               (json--token-delimiter-p
                                (char source index))))
                       do (incf index))
                 (when (and (< start source-length)
                            (or (digit-char-p (char source start))
                                (char= (char source start) #\-))
                            (> (- index start)
                               *json-maximum-number-characters*))
                   (json--limit-error
                    source-name
                    "characters in one number"
                    *json-maximum-number-characters*)))))
      (loop while (< index source-length)
            for character = (char source index)
            do
               (cond
                 ((json--whitespace-p character)
                  (incf index))
                 ((char= character #\")
                  (scan-string))
                 ((find character '(#\[ #\{))
                  (account-node)
                  (incf depth)
                  (when (> depth *json-maximum-depth*)
                    (json--limit-error
                     source-name "nesting depth" *json-maximum-depth*))
                  (incf index))
                 ((find character '(#\] #\}))
                  (setf depth (max 0 (1- depth)))
                  (incf index))
                 ((find character '(#\, #\:))
                  (incf index))
                 (t
                  (scan-token)))))
    nil))

(-> json--container-p (t) boolean)
(defun json--container-p (value)
  "Return true when VALUE is a JSON object or array."
  (or (hash-table-p value)
      (and (vectorp value)
           (not (stringp value)))))

(-> json--string-encoded-byte-upper-bound (string) integer)
(defun json--string-encoded-byte-upper-bound (string)
  "Return a conservative UTF-8 JSON encoding byte bound for STRING."
  (+ 2
     (loop for character across string
           for code = (char-code character)
           sum
              (cond
                ((or (char= character #\")
                     (char= character #\\))
                 2)
                ((< code 32)
                 6)
                ((< code 128)
                 1)
                ((<= code #xffff)
                 6)
                (t
                 12)))))

(-> json--integer-character-upper-bound (integer) integer)
(defun json--integer-character-upper-bound (integer)
  "Return a safe upper bound on INTEGER's decimal representation length."
  (if (zerop integer)
      1
      (+ (if (minusp integer) 1 0)
         (ceiling (* (integer-length (abs integer)) 30103)
                  100000))))

(-> json--number-character-count (real string) integer)
(defun json--number-character-count (number source-name)
  "Return NUMBER's bounded JSON representation length for SOURCE-NAME."
  (let ((characters
          (cond
            ((integerp number)
             (json--integer-character-upper-bound number))
            ((floatp number)
             (when (or (sb-ext:float-infinity-p number)
                       (sb-ext:float-nan-p number))
               (error 'mcp-protocol-error
                      :message
                      (format nil
                              "The ~A contains a non-finite JSON number."
                              source-name)
                      :method nil
                      :payload nil))
             (length (write-to-string number)))
            (t
             (error 'mcp-protocol-error
                    :message
                    (format nil
                            "The ~A contains a non-JSON numeric value."
                            source-name)
                    :method nil
                    :payload nil)))))
    (when (> characters *json-maximum-number-characters*)
      (json--limit-error
       source-name
       "characters in one number"
       *json-maximum-number-characters*))
    characters))

(-> json-value-measure
    (t &key
       (:source-name string)
       (:maximum-encoded-bytes (or null integer)))
    (values integer integer integer))
(defun json-value-measure
    (value
     &key
       (source-name "MCP JSON value")
       (maximum-encoded-bytes *mcp-maximum-message-characters*))
  "Validate VALUE iteratively and return node, byte, and string-character totals."
  (let ((stack nil)
        (active-containers (make-hash-table :test #'eq))
        (nodes 0)
        (encoded-bytes 0)
        (aggregate-string-characters 0))
    (labels ((add-encoded-bytes (count)
               "Add COUNT to the encoded byte bound."
               (incf encoded-bytes count)
               (when (and maximum-encoded-bytes
                          (> encoded-bytes maximum-encoded-bytes))
                 (mcp-message-too-large-error
                  source-name maximum-encoded-bytes)))

             (account-node ()
               "Account for one tree node."
               (incf nodes)
               (when (> nodes *json-maximum-nodes*)
                 (json--limit-error
                  source-name "node count" *json-maximum-nodes*)))

             (account-string (string &optional object-key-p)
               "Validate and account for STRING."
               (let ((characters (length string)))
                 (when (> characters *json-maximum-string-characters*)
                   (json--limit-error
                    source-name
                    "characters in one string"
                    *json-maximum-string-characters*))
                 (when (and object-key-p
                            (> characters
                               *json-maximum-object-key-characters*))
                   (json--limit-error
                    source-name
                    "characters in one object key"
                    *json-maximum-object-key-characters*))
                 (incf aggregate-string-characters characters)
                 (when (> aggregate-string-characters
                          *json-maximum-aggregate-string-characters*)
                   (json--limit-error
                    source-name
                    "aggregate string characters"
                    *json-maximum-aggregate-string-characters*))
                 (add-encoded-bytes
                  (json--string-encoded-byte-upper-bound string))))

             (schedule (scheduled-value container-depth)
               "Schedule SCHEDULED-VALUE and account for its node."
               (account-node)
               (when (and (json--container-p scheduled-value)
                          (> container-depth *json-maximum-depth*))
                 (json--limit-error
                  source-name "nesting depth" *json-maximum-depth*))
               (push (list ':enter scheduled-value container-depth)
                     stack)))
      (schedule value (if (json--container-p value) 1 0))
      (loop while stack
            for (kind current depth) = (pop stack)
            do
               (if (eq kind ':leave)
                   (remhash current active-containers)
                   (cond
                     ((hash-table-p current)
                      (when (gethash current active-containers)
                        (error 'mcp-protocol-error
                               :message
                               (format nil
                                       "The ~A contains a cyclic JSON object."
                                       source-name)
                               :method nil
                               :payload nil))
                      (let ((members (hash-table-count current)))
                        (when (> members *json-maximum-object-members*)
                          (json--limit-error
                           source-name
                           "members in one object"
                           *json-maximum-object-members*))
                        (add-encoded-bytes
                         (+ 2
                            (if (plusp members) (1- members) 0)
                            members)))
                      (setf (gethash current active-containers) t)
                      (push (list ':leave current depth) stack)
                      (maphash
                       (lambda (key member)
                         (unless (stringp key)
                           (error 'mcp-protocol-error
                                  :message
                                  (format nil
                                          "The ~A contains a non-string JSON object key."
                                          source-name)
                                  :method nil
                                  :payload nil))
                         (account-node)
                         (account-string key t)
                         (schedule
                          member
                          (if (json--container-p member)
                              (1+ depth)
                              depth)))
                       current))
                     ((and (vectorp current)
                           (not (stringp current)))
                      (when (gethash current active-containers)
                        (error 'mcp-protocol-error
                               :message
                               (format nil
                                       "The ~A contains a cyclic JSON array."
                                       source-name)
                               :method nil
                               :payload nil))
                      (let ((elements (length current)))
                        (when (> elements *json-maximum-array-elements*)
                          (json--limit-error
                           source-name
                           "elements in one array"
                           *json-maximum-array-elements*))
                        (add-encoded-bytes
                         (+ 2
                            (if (plusp elements)
                                (1- elements)
                                0))))
                      (setf (gethash current active-containers) t)
                      (push (list ':leave current depth) stack)
                      (loop for element across current
                            do
                               (schedule
                                element
                                (if (json--container-p element)
                                    (1+ depth)
                                    depth))))
                     ((stringp current)
                      (account-string current))
                     ((or (integerp current)
                          (floatp current))
                      (add-encoded-bytes
                       (json--number-character-count
                        current source-name)))
                     ((eq current yason:true)
                      (add-encoded-bytes 4))
                     ((eq current yason:false)
                      (add-encoded-bytes 5))
                     ((or (eq current :null)
                          (null current))
                      (add-encoded-bytes 4))
                     (t
                     (error 'mcp-protocol-error
                             :message
                             (format nil
                                     "The ~A contains a ~S value that is not representable as JSON."
                                     source-name
                                     (type-of current))
                             :method nil
                             :payload nil)))))
      (values nodes encoded-bytes aggregate-string-characters))))

(-> json-encode
    (t &key (:limit integer) (:source-name string))
    string)
(defun json-encode
    (value
     &key
       (limit *mcp-maximum-message-characters*)
       (source-name "outbound MCP JSON document"))
  "Encode VALUE as one compact JSON document within structural and size bounds."
  (json-value-measure
   value :source-name source-name :maximum-encoded-bytes limit)
  (let ((encoded
          (with-output-to-string (stream)
            (yason:encode value stream))))
    (when (> (length encoded) limit)
      (mcp-message-too-large-error source-name limit))
    encoded))

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
  (json--lexical-preflight source source-name)
  (let ((value
          (handler-case
              (with-input-from-string (stream source)
                (let ((decoded
                        (yason:parse
                         stream
                         :json-arrays-as-vectors t
                         :json-booleans-as-symbols t
                         :json-nulls-as-keyword t)))
                  (loop for character = (read-char stream nil nil)
                        while character
                        unless (json--whitespace-p character)
                          do
                             (error
                              "Unexpected text follows the JSON document."))
                  decoded))
            (error (cause)
              (error 'mcp-protocol-error
                     :message
                     (format nil "Could not decode MCP JSON: ~A" cause)
                     :payload
                     (subseq
                      source
                      0
                      (min (length source)
                           *json-diagnostic-limit*)))))))
    (json-value-measure
     value :source-name source-name :maximum-encoded-bytes limit)
    value))

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

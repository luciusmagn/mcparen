(in-package #:mcparen)

;;;; -- Protocol and Client Tests --

(define-test client-initializes-with-supported-protocol
  (let* ((transport
           (make-test-scripted-transport #'test-default-handler))
         (client-capabilities
           (json-object
            "roots"
            (json-object "listChanged" yason:true)))
         (client
           (make-mcp-client
            transport
            :name "fixture-client"
            :version "9"
            :title "Fixture Client"
            :capabilities client-capabilities)))
    (unwind-protect
         (progn
           (mcp-client-connect client)
           (test-assert (mcp-client-connected-p client))
           (test-equal "2025-11-25"
                       (mcp-client-protocol-version client)
                       :test #'string=)
           (test-equal "2025-11-25"
                       (test-scripted-transport-protocol-version
                        transport)
                       :test #'string=)
           (test-equal "mcparen-test-server"
                       (json-get
                        (mcp-client-server-info client)
                        "name")
                       :test #'string=)
           (test-equal "Deterministic local fixture."
                       (mcp-client-instructions client)
                       :test #'string=)
           (test-equal 1
                       (length
                        (test-scripted-transport-requests transport)))
           (let* ((request
                    (first
                     (test-scripted-transport-requests transport)))
                  (params (json-get request "params"))
                  (client-info (json-get params "clientInfo")))
             (test-equal "initialize"
                         (json-get request "method")
                         :test #'string=)
             (test-equal "2.0"
                         (json-get request "jsonrpc")
                         :test #'string=)
             (test-equal "2025-11-25"
                         (json-get params "protocolVersion")
                         :test #'string=)
             (test-assert
              (eq client-capabilities
                  (json-get params "capabilities")))
             (test-equal "fixture-client"
                         (json-get client-info "name")
                         :test #'string=)
             (test-equal "9"
                         (json-get client-info "version")
                         :test #'string=)
             (test-equal "Fixture Client"
                         (json-get client-info "title")
                         :test #'string=))
           (test-equal 1
                       (length
                        (test-scripted-transport-notifications
                         transport)))
           (test-equal
            "notifications/initialized"
            (json-get
             (first
              (test-scripted-transport-notifications transport))
             "method")
            :test #'string=))
      (mcp-client-close client))))

(define-test client-follows-tool-and-resource-pagination
  (labels ((tool (name description &optional annotations)
             "Return one valid MCP tool object."
             (let ((raw
                     (json-object
                      "name" name
                      "description" description
                      "inputSchema"
                      (json-object "type" "object"))))
               (when annotations
                 (setf (gethash "annotations" raw) annotations))
               raw))

           (handler (transport request)
             "Respond to paginated discovery REQUEST."
             (declare (ignore transport))
             (let* ((method (json-get request "method"))
                    (params (json-get request "params"))
                    (cursor (and params
                                 (json-get params "cursor"))))
               (cond
                 ((string= method "initialize")
                  (test-rpc-result request (test-initialize-result)))
                 ((string= method "tools/list")
                  (if cursor
                      (progn
                        (test-equal "tools-two" cursor :test #'string=)
                        (test-rpc-result
                         request
                         (json-object
                          "tools"
                          (vector
                           (tool
                            "second"
                            "Second tool"
                            (json-object
                             "readOnlyHint" yason:false
                             "destructiveHint" yason:false))))))
                      (test-rpc-result
                       request
                       (json-object
                        "tools"
                        (vector
                         (tool
                          "first"
                          "First tool"
                          (json-object
                           "readOnlyHint" yason:true
                           "destructiveHint" yason:false)))
                        "nextCursor" "tools-two"))))
                 ((string= method "resources/list")
                  (if cursor
                      (progn
                        (test-equal "resources-two"
                                    cursor :test #'string=)
                        (test-rpc-result
                         request
                         (json-object
                          "resources"
                          (vector
                           (json-object
                            "uri" "fixture:///two"
                            "name" "two")))))
                      (test-rpc-result
                       request
                       (json-object
                        "resources"
                        (vector
                         (json-object
                          "uri" "fixture:///one"
                          "name" "one"))
                        "nextCursor" "resources-two"))))
                 ((string= method "resources/read")
                  (test-rpc-result
                   request
                   (json-object
                    "contents"
                    (vector
                     (json-object
                      "uri"
                      (json-get params "uri")
                      "text" "fixture contents")))))
                 (t
                  (error "Unexpected pagination test method ~S."
                         method))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport)))
      (unwind-protect
           (let ((tools (mcp-client-list-tools client))
                 (resources (mcp-client-list-resources client)))
             (test-equal '("first" "second")
                         (mapcar #'mcp-tool-name tools))
             (test-assert (mcp-tool-read-only-p (first tools)))
             (test-assert
              (not (mcp-tool-destructive-p (first tools))))
             (test-assert
              (not (mcp-tool-read-only-p (second tools))))
             (test-assert
              (not (mcp-tool-destructive-p (second tools))))
             (test-equal '("fixture:///one" "fixture:///two")
                         (mapcar
                          (lambda (resource)
                            (json-get resource "uri"))
                          resources))
             (let ((read-result
                     (mcp-client-read-resource
                      client "fixture:///one")))
               (test-equal
                "fixture contents"
                (json-get
                 (first
                  (json-sequence->list
                   (json-get read-result "contents")))
                 "text")
                :test #'string=)))
        (mcp-client-close client)))))

(define-test client-gets-prompt-without-arguments
  (labels ((handler (transport request)
             "Return one prompt without requiring an arguments object."
             (declare (ignore transport))
             (let ((method (json-get request "method")))
               (cond
                 ((string= method "initialize")
                  (test-rpc-result
                   request
                   (json-object
                    "protocolVersion" "2025-11-25"
                    "capabilities"
                    (json-object "prompts" (json-object))
                    "serverInfo"
                    (json-object
                     "name" "prompt-fixture"
                     "version" "1"))))
                 ((string= method "prompts/get")
                  (let ((params (json-get request "params")))
                    (test-equal
                     "summary"
                     (json-get params "name")
                     :test #'string=)
                    (test-assert
                     (null (nth-value 1
                                      (gethash "arguments" params))))
                    (test-rpc-result
                     request
                     (json-object
                      "description" "Resolved prompt."
                      "messages" #()))))
                 (t
                  (error "Unexpected prompt test method ~S." method))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport)))
      (unwind-protect
           (let ((prompt
                   (mcp-client-get-prompt client "summary")))
             (test-equal
              "Resolved prompt."
              (json-get prompt "description")
              :test #'string=))
        (mcp-client-close client)))))

(define-test client-rejects-repeated-pagination-cursor
  (labels ((handler (transport request)
             "Return a repeated cursor for resource discovery."
             (declare (ignore transport))
             (if (string= (json-get request "method") "initialize")
                 (test-rpc-result request (test-initialize-result))
                 (test-rpc-result
                  request
                  (json-object
                   "resources" #()
                   "nextCursor" "repeated")))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport)))
      (unwind-protect
           (let ((condition
                   (test-signals mcp-protocol-error
                     (mcp-client-list-resources client))))
             (test-equal "resources/list"
                         (mcp-protocol-error-method condition)
                         :test #'string=)
             (test-assert
              (search "repeated pagination cursor"
                      (mcp-error-message condition)
                      :test #'char-equal)))
        (mcp-client-close client)))))

(define-test client-bounds-aggregate-pagination-items
  (labels ((resource (name)
             "Return a small resource object named NAME."
             (json-object "uri" name))

           (handler (transport request)
             "Return two two-item resource pages."
             (declare (ignore transport))
             (if (string= (json-get request "method") "initialize")
                 (test-rpc-result request (test-initialize-result))
                 (let* ((params (json-get request "params"))
                        (cursor
                          (and params
                               (json-get params "cursor"))))
                   (if cursor
                       (test-rpc-result
                        request
                        (json-object
                         "resources"
                         (vector
                          (resource "three")
                          (resource "four"))))
                       (test-rpc-result
                        request
                        (json-object
                         "resources"
                         (vector
                          (resource "one")
                          (resource "two"))
                         "nextCursor" "second")))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport))
           (*mcp-pagination-maximum-items* 3))
      (unwind-protect
           (let ((condition
                   (test-signals mcp-protocol-error
                     (mcp-client-list-resources client))))
             (test-assert
              (search "aggregate item count"
                      (mcp-error-message condition)))
             (test-equal
              3
              (length
               (test-scripted-transport-requests transport))))
        (mcp-client-close client)))))

(define-test client-bounds-aggregate-pagination-bytes
  (labels ((handler (transport request)
             "Return individually small pages whose aggregate exceeds the limit."
             (declare (ignore transport))
             (if (string= (json-get request "method") "initialize")
                 (test-rpc-result request (test-initialize-result))
                 (let* ((params (json-get request "params"))
                        (cursor
                          (and params
                               (json-get params "cursor")))
                       (resource
                         (json-object
                          "uri"
                          (make-string 40 :initial-element #\x))))
                   (if cursor
                       (test-rpc-result
                        request
                        (json-object "resources" (vector resource)))
                       (test-rpc-result
                        request
                        (json-object
                         "resources" (vector resource)
                         "nextCursor" "second")))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport))
           (*mcp-pagination-maximum-aggregate-bytes* 105))
      (unwind-protect
           (let ((condition
                   (test-signals mcp-protocol-error
                     (mcp-client-list-resources client))))
             (test-assert
              (search "aggregate encoded bytes"
                      (mcp-error-message condition)))
             (test-equal
              3
              (length
               (test-scripted-transport-requests transport))))
        (mcp-client-close client)))))

(define-test client-bounds-aggregate-pagination-nodes
  (labels ((handler (transport request)
             "Return pages whose combined trees exceed the node limit."
             (declare (ignore transport))
             (if (string= (json-get request "method") "initialize")
                 (test-rpc-result request (test-initialize-result))
                 (let* ((params (json-get request "params"))
                        (cursor
                          (and params
                               (json-get params "cursor")))
                       (resource (json-object "uri" "fixture")))
                   (if cursor
                       (test-rpc-result
                        request
                        (json-object "resources" (vector resource)))
                       (test-rpc-result
                        request
                        (json-object
                         "resources" (vector resource)
                         "nextCursor" "second")))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport))
           (*mcp-pagination-maximum-aggregate-nodes* 8))
      (unwind-protect
           (let ((condition
                   (test-signals mcp-protocol-error
                     (mcp-client-list-resources client))))
             (test-assert
              (search "aggregate node count"
                      (mcp-error-message condition)))
             (test-equal
              3
              (length
               (test-scripted-transport-requests transport))))
        (mcp-client-close client)))))

(define-test client-bounds-pagination-pages
  (labels ((handler (transport request)
             "Return a unique cursor after every empty resource page."
             (declare (ignore transport))
             (if (string= (json-get request "method") "initialize")
                 (test-rpc-result request (test-initialize-result))
                 (test-rpc-result
                  request
                  (json-object
                   "resources" #()
                   "nextCursor"
                   (format nil
                           "cursor-~D"
                           (length
                            (test-scripted-transport-requests
                             transport))))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport))
           (*mcp-pagination-maximum-pages* 2))
      (unwind-protect
           (let ((condition
                   (test-signals mcp-protocol-error
                     (mcp-client-list-resources client))))
             (test-assert
              (search "2-page safety limit"
                      (mcp-error-message condition)))
             (test-equal
              3
              (length
               (test-scripted-transport-requests transport))))
        (mcp-client-close client)))))

(define-test client-preserves-rpc-errors
  (labels ((handler (transport request)
             "Return a structured error for ping."
             (declare (ignore transport))
             (if (string= (json-get request "method") "initialize")
                 (test-rpc-result request (test-initialize-result))
                 (test-rpc-error
                  request -32042 "Fixture refused"
                  (json-object "retryable" yason:false)))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport)))
      (unwind-protect
           (let ((condition
                   (test-signals mcp-rpc-error
                     (mcp-client-ping client))))
             (test-equal -32042 (mcp-rpc-error-code condition))
             (test-equal "ping"
                         (mcp-protocol-error-method condition)
                         :test #'string=)
             (test-assert
              (eq yason:false
                  (json-get
                   (mcp-rpc-error-data condition)
                   "retryable"))))
        (mcp-client-close client)))))

(define-test client-preserves-tool-content-and-structured-result
  (labels ((handler (transport request)
             "Return a mixed tool result."
             (declare (ignore transport))
             (let ((method (json-get request "method")))
               (cond
                 ((string= method "initialize")
                  (test-rpc-result request (test-initialize-result)))
                 ((string= method "tools/call")
                  (test-rpc-result
                   request
                   (json-object
                    "content"
                    (vector
                     (json-object "type" "text" "text" "plain")
                     (json-object
                      "type" "image"
                      "mimeType" "image/png"
                      "data" "AAAA")
                     (json-object
                      "type" "resource_link"
                      "uri" "file:///tmp/x"
                      "name" "x"))
                    "structuredContent"
                    (json-object
                     "answer" 42
                     "nested" (json-object "ok" yason:true))
                    "isError" yason:false)))
                 (t
                  (error "Unexpected tool-result test method ~S."
                         method))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport)))
      (unwind-protect
           (let* ((result
                    (mcp-client-call-tool
                     client "mixed" (json-object "input" "x")))
                  (rendered (mcp-call-result-text result)))
             (test-equal 3 (length (mcp-call-result-content result)))
             (test-assert
              (not (mcp-call-result-error-p result)))
             (test-equal
              42
              (json-get
               (mcp-call-result-structured-content result)
               "answer"))
             (test-assert (search "plain" rendered))
             (test-assert
              (search "Image content (image/png)" rendered))
             (test-assert
              (search "Resource: file:///tmp/x (x)" rendered))
             (test-assert (search "Structured content:" rendered)))
        (mcp-client-close client)))))

(define-test json-rejects-trailing-documents
  (let ((condition
          (test-signals mcp-protocol-error
            (json-decode "{\"first\":1} {\"second\":2}"))))
    (test-assert
     (search "Unexpected text follows"
             (mcp-error-message condition)))))

(define-test json-rejects-documents-over-the-configured-limit
  (let ((condition
          (test-signals mcp-message-too-large
            (json-decode
             (format nil "{\"payload\":\"~A\"}"
                     (make-string 256 :initial-element #\x))
             :limit 64
             :source-name "test JSON input"))))
    (test-equal 64 (mcp-message-too-large-limit condition))
    (test-equal "test JSON input"
                (mcp-message-too-large-source condition)
                :test #'string=)))

(define-test json-preflights-adversarial-depth-and-node-count
  (let ((deep
          (concatenate
           'string
           (make-string 100000 :initial-element #\[)
           (make-string 100000 :initial-element #\])))
        (wide
          (with-output-to-string (stream)
            (write-char #\[ stream)
            (loop repeat 200000
                  for first-p = t then nil
                  unless first-p
                    do (write-char #\, stream)
                  do (write-char #\0 stream))
            (write-char #\] stream))))
    (test-assert
     (< (length deep) *mcp-maximum-message-characters*))
    (test-assert
     (< (length wide) *mcp-maximum-message-characters*))
    (let ((*json-maximum-depth* 32))
      (let ((condition
              (test-signals mcp-protocol-error
                (json-decode deep))))
        (test-assert
         (search "nesting depth" (mcp-error-message condition)))))
    (let ((*json-maximum-nodes* 64))
      (let ((condition
              (test-signals mcp-protocol-error
                (json-decode wide))))
        (test-assert
         (search "node count" (mcp-error-message condition)))))))

(define-test json-validates-decoded-container-and-string-bounds
  (let ((*json-maximum-array-elements* 2))
    (let ((condition
            (test-signals mcp-protocol-error
              (json-decode "[0,1,2]"))))
      (test-assert
       (search "elements in one array"
               (mcp-error-message condition)))))
  (let ((*json-maximum-object-members* 2))
    (let ((condition
            (test-signals mcp-protocol-error
              (json-decode "{\"a\":1,\"b\":2,\"c\":3}"))))
      (test-assert
       (search "members in one object"
               (mcp-error-message condition)))))
  (let ((*json-maximum-aggregate-string-characters* 4))
    (let ((condition
            (test-signals mcp-protocol-error
              (json-decode "[\"abc\",\"de\"]"))))
      (test-assert
       (search "aggregate string characters"
               (mcp-error-message condition)))))
  (let ((*json-maximum-object-key-characters* 4))
    (let ((condition
            (test-signals mcp-protocol-error
              (json-decode "{\"abcde\":0}"))))
      (test-assert
       (search "characters in one object key"
               (mcp-error-message condition))))))

(define-test json-validates-programmatic-values-before-encoding
  (let ((deep 0))
    (loop repeat 100000
          do (setf deep (vector deep)))
    (let ((*json-maximum-depth* 32))
      (let ((condition
              (test-signals mcp-protocol-error
                (json-encode deep))))
        (test-assert
         (search "nesting depth" (mcp-error-message condition))))))
  (let ((cycle (make-array 1)))
    (setf (aref cycle 0) cycle)
    (let ((condition
            (test-signals mcp-protocol-error
              (json-encode cycle))))
      (test-assert
       (search "cyclic JSON array"
               (mcp-error-message condition)))))
  (let ((condition
          (test-signals mcp-protocol-error
            (json-encode (ash 1 100000)))))
    (test-assert
     (search "characters in one number"
             (mcp-error-message condition))))
  (let ((condition
          (test-signals mcp-message-too-large
            (json-encode
             (json-object "payload" (make-string 256))
             :limit 64
             :source-name "test outbound JSON"))))
    (test-equal 64 (mcp-message-too-large-limit condition))
    (test-equal "test outbound JSON"
                (mcp-message-too-large-source condition)
                :test #'string=)))

(define-test json-rpc-rejects-structurally-invalid-messages
  (dolist
      (message
       (list
        (json-object
         "jsonrpc" "1.0" "id" 1 "result" (json-object))
        (json-object
         "jsonrpc" "2.0" "id" yason:true "result" (json-object))
        (json-object
         "jsonrpc" "2.0" "id" 1
         "result" (json-object)
         "error" (json-object "code" -32603 "message" "both"))
        (json-object
         "jsonrpc" "2.0" "id" 1
         "error" (json-object "code" "-32603" "message" "bad code"))
        (json-object
         "jsonrpc" "2.0" "id" 1
         "error" (json-object "code" -32603 "message" 7))
        (json-object
         "jsonrpc" "2.0" "method" "fixture"
         "params" #())
        (json-object
         "jsonrpc" "2.0" "id" 1
         "method" "fixture"
         "result" (json-object))))
    (test-signals mcp-protocol-error
      (json-rpc-message-validate message))))

(define-test client-rejects-malformed-json-rpc-responses
  (labels ((attempt (response)
             "Validate RESPONSE as the result of request identifier 17."
             (test-signals mcp-protocol-error
               (mcp-client--validate-response
                response 17 "fixture/malformed"))))
    (dolist
        (response
         (list
          (json-object
           "jsonrpc" "2.0" "id" 17
           "result" (json-object)
           "error" (json-object
                    "code" -32603
                    "message" "ambiguous"))
          (json-object
           "jsonrpc" "2.0" "id" 17
           "error" (json-object
                    "code" "-32603"
                    "message" "invalid code"))
          (json-object
           "jsonrpc" "2.0" "id" 17
           "error" (json-object
                    "code" -32603
                    "message" 42))
          (json-object
           "jsonrpc" "2.0" "id" 17
           "method" "not-a-response"
           "result" (json-object))))
      (attempt response))))

(define-test json-preserves-false-null-and-empty-array
  (let* ((decoded
           (json-decode
            "{\"false\":false,\"null\":null,\"array\":[],\"true\":true}"))
         (false-value (json-get decoded "false"))
         (null-value (json-get decoded "null"))
         (array-value (json-get decoded "array"))
         (true-value (json-get decoded "true")))
    (test-assert (eq false-value yason:false))
    (test-assert (eq null-value :null))
    (test-assert (vectorp array-value))
    (test-equal 0 (length array-value))
    (test-assert (eq true-value yason:true))
    (test-assert (not (eq false-value null-value)))
    (test-assert (not (eq false-value array-value)))
    (test-assert (not (eq null-value array-value)))
    (test-assert (not (json-true-p false-value)))
    (test-assert (not (json-true-p null-value)))
    (test-assert (not (json-true-p array-value)))
    (test-assert (json-true-p true-value))
    (test-equal nil (json-sequence->list array-value))
    (let* ((encoded (json-encode decoded))
           (roundtrip (json-decode encoded)))
      (test-assert (eq (json-get roundtrip "false") yason:false))
      (test-assert (eq (json-get roundtrip "null") :null))
      (test-assert (vectorp (json-get roundtrip "array")))
      (test-assert (eq (json-get roundtrip "true") yason:true)))
    (dolist (not-an-array
             (list nil :null yason:false (list "not" "an" "array")))
      (test-signals mcp-protocol-error
        (json-sequence->list not-an-array)))))

(define-test tool-annotations-require-exact-json-true
  (labels ((tool-with (value)
             "Return a tool carrying VALUE in both boolean annotations."
             (mcp-client--raw->tool
              (json-object
               "name" "annotation-test"
               "description" "Annotation test."
               "inputSchema" (json-object "type" "object")
               "annotations"
               (json-object
                "readOnlyHint" value
                "destructiveHint" value)))))
    (let ((tool (tool-with yason:false)))
      (test-assert (not (mcp-tool-read-only-p tool)))
      (test-assert (not (mcp-tool-destructive-p tool))))
    (let ((tool (tool-with yason:true)))
      (test-assert (mcp-tool-read-only-p tool))
      (test-assert (mcp-tool-destructive-p tool)))
    (dolist (invalid-value
             (list nil t 0 1 "true" :true :null))
      (test-signals mcp-protocol-error
        (tool-with invalid-value)))))

(define-test tool-definitions-require-object-json-schemas
  (labels ((base-tool ()
             "Return a fresh valid tool definition."
             (json-object
              "name" "schema-test"
              "title" "Schema Test"
              "description" "Valid metadata."
              "inputSchema" (json-object "type" "object")
              "outputSchema" (json-object "type" "object")))

           (reject-field (key value)
             "Require VALUE at KEY to make a fresh tool invalid."
             (let ((tool (base-tool)))
               (setf (gethash key tool) value)
               (test-signals mcp-protocol-error
                 (mcp-client--raw->tool tool)))))
    (reject-field "name" "")
    (reject-field "title" 9)
    (reject-field "description" 9)
    (reject-field "inputSchema" #())
    (reject-field "inputSchema" (json-object "type" "array"))
    (reject-field "inputSchema" (json-object))
    (reject-field "outputSchema" #())
    (reject-field "outputSchema" (json-object "type" "array"))
    (reject-field "annotations" #())
    (let ((tool (base-tool)))
      (setf
       (gethash "annotations" tool)
       (json-object "title" 7))
      (test-signals mcp-protocol-error
        (mcp-client--raw->tool tool)))))

(define-test tool-call-rejects-malformed-error-boolean
  (labels ((handler (transport request)
             "Return malformed isError metadata for one tool call."
             (declare (ignore transport))
             (if (string= (json-get request "method") "initialize")
                 (test-rpc-result request (test-initialize-result))
                 (test-rpc-result
                  request
                  (json-object
                   "content" #()
                   "isError" t)))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport)))
      (unwind-protect
           (test-signals mcp-protocol-error
             (mcp-client-call-tool client "malformed" (json-object)))
        (mcp-client-close client)))))

(define-test tool-call-validates-content-and-structured-result
  (labels ((attempt (result)
             "Call a scripted tool whose server returns RESULT."
             (let* ((transport
                      (make-test-scripted-transport
                       (lambda (ignored request)
                         (declare (ignore ignored))
                         (if
                             (string=
                              (json-get request "method")
                              "initialize")
                             (test-rpc-result
                              request (test-initialize-result))
                             (test-rpc-result request result)))))
                    (client (make-mcp-client transport)))
               (unwind-protect
                    (test-signals mcp-protocol-error
                      (mcp-client-call-tool
                       client "malformed" (json-object)))
                 (mcp-client-close client)))))
    (dolist
        (result
         (list
          (json-object)
          (json-object "content" (list (json-object
                                        "type" "text"
                                        "text" "list")))
          (json-object "content" (vector "not an object"))
          (json-object
           "content" (vector (json-object "text" "missing type")))
          (json-object
           "content" (vector
                      (json-object "type" "text" "text" 7)))
          (json-object
           "content" (vector
                      (json-object
                       "type" "image"
                       "data" 7
                       "mimeType" "image/png")))
          (json-object
           "content" (vector
                      (json-object
                       "type" "audio"
                       "data" "AAAA"
                       "mimeType" 7)))
          (json-object
           "content" (vector
                      (json-object
                       "type" "resource_link"
                       "uri" 7
                       "name" "fixture")))
          (json-object
           "content" (vector
                      (json-object
                       "type" "unknown"
                       "value" 7)))
          (json-object
           "content" #()
           "structuredContent" #())))
      (attempt result))))

(define-test http-rejects-json-rpc-batches
  (test-signals mcp-protocol-error
    (mcp-http--response-message
     "[{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}]")))

(define-test initialize-validates-required-server-fields
  (labels ((attempt (result)
             "Connect to a server returning initialization RESULT."
             (let* ((transport
                      (make-test-scripted-transport
                       (lambda (ignored request)
                         (declare (ignore ignored))
                         (test-rpc-result request result))))
                    (client (make-mcp-client transport)))
               (unwind-protect
                    (test-signals mcp-protocol-error
                      (mcp-client-connect client))
                 (mcp-client-close client)))))
    (dolist
        (invalid
         (list
          (json-object
           "protocolVersion" "2025-11-25"
           "serverInfo"
           (json-object "name" "fixture" "version" "1"))
          (json-object
           "protocolVersion" "2025-11-25"
           "capabilities" (json-object))
          (json-object
           "protocolVersion" "2025-11-25"
           "capabilities" (json-object)
           "serverInfo" (json-object "version" "1"))
          (json-object
           "protocolVersion" "2025-11-25"
           "capabilities" (json-object)
           "serverInfo" (json-object "name" "fixture"))))
      (let ((condition (attempt invalid)))
        (test-equal "initialize"
                    (mcp-protocol-error-method condition)
                    :test #'string=)))))

(define-test initialize-timeout-does-not-send-cancellation
  (let* ((transport
           (make-test-scripted-transport
            (lambda (ignored request)
              (declare (ignore ignored))
              (error 'mcp-timeout
                     :message "Fixture initialization timed out."
                     :transport nil
                     :cause nil
                     :operation (json-get request "method")
                     :seconds 0.01))))
         (client
           (make-mcp-client
            transport :startup-timeout 0.01)))
    (test-signals mcp-timeout
      (mcp-client-connect client))
    (test-assert
     (null
      (test-scripted-transport-notifications transport)))))

(define-test client-gates-features-by-server-capabilities
  (let ((saw-tools-request-p nil))
    (labels ((handler (transport request)
               "Advertise no optional features and reject tool discovery."
               (declare (ignore transport))
               (if (string= (json-get request "method") "initialize")
                   (test-rpc-result
                    request
                    (json-object
                     "protocolVersion" "2025-11-25"
                     "capabilities" (json-object)
                     "serverInfo"
                     (json-object
                      "name" "capability-fixture"
                      "version" "1")))
                   (progn
                     (setf saw-tools-request-p t)
                     (test-rpc-result
                      request (json-object "tools" #()))))))
      (let* ((transport (make-test-scripted-transport #'handler))
             (client (make-mcp-client transport)))
        (unwind-protect
             (test-signals mcp-protocol-error
               (mcp-client-list-tools client))
          (mcp-client-close client))
        (test-assert
         (not saw-tools-request-p)
         "Unsupported discovery must not reach the server.")))))


;;;; -- Standard I/O Transport Tests --

(define-test stdio-matches-concurrent-out-of-order-responses
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 2))
         (slow nil)
         (fast nil))
    (unwind-protect
         (progn
           (mcp-client-connect client)
           (setf
            slow
            (test-start-thread
             (lambda ()
               (mcp-client-call-tool
                client "slow" (json-object)))
             "mcparen slow fixture call"))
           (test-assert
            (test-wait-until
             (lambda ()
               (search "received:slow"
                       (mcp-stdio-transport-stderr-text transport)))
             1.0)
            "The slow request reached the fixture.")
           (setf
            fast
            (test-start-thread
             (lambda ()
               (mcp-client-call-tool
                client "fast" (json-object)))
             "mcparen fast fixture call"))
           (test-equal
            "fast"
            (json-get
             (first
              (mcp-call-result-content
               (test-await-thread fast 1.0)))
             "text")
            :test #'string=)
           (test-assert
            (not (test-thread-result-finished-p slow))
            "The slow response remained pending after the fast response.")
           (test-equal
            "slow"
            (json-get
             (first
              (mcp-call-result-content
               (test-await-thread slow 1.0)))
             "text")
            :test #'string=))
      (mcp-client-close client))))

(define-test stdio-timeout-sends-cancellation
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 0.1)))
    (unwind-protect
         (progn
           (mcp-client-connect client)
           (let ((condition
                   (test-signals mcp-timeout
                     (mcp-client-call-tool
                      client "never" (json-object)
                      :timeout 0.1))))
             (test-equal "JSON-RPC request"
                         (mcp-timeout-operation condition)
                         :test #'string=)
             (test-equal 0.1
                         (mcp-timeout-seconds condition)
                         :test #'=))
           (test-assert
            (test-wait-until
             (lambda ()
               (search "cancelled:"
                       (mcp-stdio-transport-stderr-text transport)))
             1.0)
            "The cancellation notification reached the fixture."))
      (mcp-client-close client))))

(define-test stdio-write-timeouts-close-the-transport
  (dolist (case '((:request . "JSON-RPC request")
                  (:notification . "JSON-RPC notification")))
    (let* ((transport
             (make-mcp-stdio-transport
              (namestring sb-ext:*runtime-pathname*)
              :arguments
              (list
               "--noinform"
               "--disable-debugger"
               "--non-interactive"
               "--eval"
               "(sleep 30)")))
           (payload
             (make-string (* 2 1024 1024) :initial-element #\x))
           (message
             (json-object
              "jsonrpc" "2.0"
              "method" "fixture/blocked-write"
              "params" (json-object "payload" payload)))
           (timeout 1/20))
      (when (eq (first case) ':request)
        (setf (gethash "id" message) 9001))
      (unwind-protect
           (progn
             (mcp-transport-open transport)
             (let ((condition
                     (test-signals mcp-timeout
                       (ecase (first case)
                         (:request
                          (mcp-transport-request
                           transport message timeout))
                         (:notification
                          (mcp-transport-notify
                           transport message timeout))))))
               (test-equal
                (rest case)
                (mcp-timeout-operation condition)
                :test #'string=)
               (test-equal
                timeout
                (mcp-timeout-seconds condition)
                :test #'=))
             (test-assert (not (mcp-transport-open-p transport)))
             (test-assert
              (null (mcp-stdio-transport-process transport))))
        (handler-case
            (mcp-transport-close transport)
          (error ()
            nil))))))

(define-test stdio-resolves-directory-at-each-launch
  (let* ((directory #P"/tmp/first/")
         (transport
           (make-mcp-stdio-transport
            "/bin/true"
            :directory (lambda () directory))))
    (test-equal
     #P"/tmp/first/"
     (getf (mcp-stdio--launch-arguments transport) :directory)
     :test #'equal)
    (setf directory #P"/tmp/second/")
    (test-equal
     #P"/tmp/second/"
     (getf (mcp-stdio--launch-arguments transport) :directory)
     :test #'equal)))

(define-test stdio-retains-bounded-stderr-tail
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 2)))
    (unwind-protect
         (progn
           (test-equal
            "flooded"
            (json-get
             (first
              (mcp-call-result-content
               (mcp-client-call-tool
                client "stderr-flood" (json-object))))
             "text")
            :test #'string=)
           (test-assert
            (test-wait-until
             (lambda ()
               (search "stderr-tail-marker"
                       (mcp-stdio-transport-stderr-text transport)))
             1.0)
            "The stderr reader drained through the marker.")
           (let ((stderr
                   (mcp-stdio-transport-stderr-text transport)))
             (test-assert
              (<= (length stderr) *mcp-stdio-diagnostic-limit*))
             (test-assert (search "stderr-tail-marker" stderr))
             (test-assert
              (not (search "discard-000" stderr)))))
      (mcp-client-close client))))

(define-test stdio-drains-a-single-unbounded-stderr-line-in-chunks
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 3)))
    (unwind-protect
         (progn
           (test-equal
            "stderr-drained"
            (json-get
             (first
              (mcp-call-result-content
               (mcp-client-call-tool
                client "stderr-long-line" (json-object))))
             "text")
            :test #'string=)
           (test-assert
            (test-wait-until
             (lambda ()
               (search
                "stderr-long-line-marker"
                (mcp-stdio-transport-stderr-text transport)))
             1.0)
            "The chunked stderr reader reached the tail marker.")
           (test-assert
            (<=
             (length (mcp-stdio-transport-stderr-text transport))
             *mcp-stdio-diagnostic-limit*)))
      (mcp-client-close client))))

(define-test stdio-rejects-oversized-stdout-before-json-decoding
  (let* ((limit 4096)
         (transport
           (make-test-stdio-transport
            :maximum-message-characters limit))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 3)))
    (unwind-protect
         (let ((condition
                 (test-signals mcp-message-too-large
                   (mcp-client-call-tool
                    client "oversized-stdout" (json-object)))))
           (test-equal limit
                       (mcp-message-too-large-limit condition))
           (test-assert
            (not (mcp-transport-open-p transport))))
      (handler-case
          (mcp-client-close client)
        (error ()
          nil)))))

(define-test stdio-reader-failure-reconnects-on-the-next-request
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 2))
         (first-process-identifier nil))
    (unwind-protect
         (progn
           (mcp-client-connect client)
           (setf first-process-identifier
                 (uiop:process-info-pid
                  (mcp-stdio-transport-process transport)))
           (test-signals mcp-error
             (mcp-client-call-tool
              client "malformed-stdout" (json-object)))
           (test-assert
            (not (mcp-transport-open-p transport))
            "A terminal reader failure must make the process unusable.")
           (test-assert (mcp-client-ping client))
           (test-assert (mcp-transport-open-p transport))
           (test-assert
            (/= first-process-identifier
                (uiop:process-info-pid
                 (mcp-stdio-transport-process transport)))))
      (handler-case
          (mcp-client-close client)
        (error ()
          nil)))))

(define-test stdio-callbacks-run-serially-in-wire-order
  (let ((callback-lock (make-lock "mcparen callback-order test"))
        (received nil)
        (active 0)
        (maximum-active 0))
    (let* ((transport
             (make-test-stdio-transport
              :notification-handler
              (lambda (method params)
                (test-equal
                 "notifications/progress"
                 method :test #'string=)
                (with-lock-held (callback-lock)
                  (incf active)
                  (setf maximum-active
                        (max maximum-active active))
                  (push (json-get params "index") received))
                (sleep 0.005)
                (with-lock-held (callback-lock)
                  (decf active)))))
           (client
             (make-mcp-client
              transport
              :startup-timeout 3
              :tool-timeout 3)))
      (unwind-protect
           (progn
             (test-equal
              "callbacks-sent"
              (json-get
               (first
                (mcp-call-result-content
                 (mcp-client-call-tool
                  client "callback-order" (json-object))))
               "text")
              :test #'string=)
             (test-assert
              (test-wait-until
               (lambda ()
                 (with-lock-held (callback-lock)
                   (= (length received) 32)))
               2.0)
              "Every queued callback completed.")
             (with-lock-held (callback-lock)
               (test-equal
                (loop for index below 32 collect index)
                (nreverse received))
               (test-equal 1 maximum-active)))
        (mcp-client-close client)))))

(define-test stdio-callback-queue-overflow-fails-boundedly
  (let* ((transport
           (make-test-stdio-transport
            :notification-handler
            (lambda (method params)
              (declare (ignore method params))
              (sleep 0.5))))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 3)))
    (unwind-protect
         (progn
           (test-signals mcp-error
             (mcp-client-call-tool
              client "callback-overflow" (json-object)))
           (test-assert
            (not (mcp-transport-open-p transport))
            "Overflow must fail instead of spawning or retaining unbounded work."))
      (handler-case
          (mcp-client-close client)
        (error ()
          nil)))))

(define-test stdio-server-request-handler-can-call-server
  (let ((client nil))
    (let* ((transport
             (make-mcp-stdio-transport
              (namestring sb-ext:*runtime-pathname*)
              :arguments
              (list
               "--noinform"
               "--disable-debugger"
               "--script"
               (namestring
                (test-fixture-pathname
                 "stdio-server.lisp")))
              :request-handler
              (lambda (method params)
                (test-equal
                 "sampling/createMessage"
                 method :test #'string=)
                (test-equal
                 "fixture"
                 (json-get params "prompt")
                 :test #'string=)
                (test-assert
                 (mcp-client-ping client)
                 "The sole reader must remain available.")
                (json-object "role" "assistant"))))
           (created-client
             (make-mcp-client
              transport
              :capabilities
              (json-object "sampling" (json-object))
              :startup-timeout 3
              :tool-timeout 2)))
      (setf client created-client)
      (unwind-protect
           (let ((result
                   (mcp-client-call-tool
                    client "server-request" (json-object))))
             (test-equal
              "assistant"
              (json-get
               (first (mcp-call-result-content result))
               "text")
              :test #'string=))
        (mcp-client-close client)))))

(define-test stdio-close-terminates-and-clears-resources
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client transport :startup-timeout 3))
         (process nil))
    (mcp-client-connect client)
    (setf process (mcp-stdio-transport-process transport))
    (test-assert (uiop:process-alive-p process))
    (let ((identifier (uiop:process-info-pid process)))
      (test-equal
       identifier
       (mcp-stdio-transport-process-group-identifier transport))
      (test-equal identifier (sb-posix:getpgid identifier)))
    (mcp-client-close client)
    (test-assert (not (mcp-transport-open-p transport)))
    (test-assert (null (mcp-stdio-transport-process transport)))
    (test-assert (null (mcp-stdio-transport-reader-thread transport)))
    (test-assert (null (mcp-stdio-transport-error-thread transport)))
    (test-assert
     (not
      (handler-case
          (uiop:process-alive-p process)
        (error ()
          nil))))))

(define-test stdio-close-terminates-process-group-children
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 2))
         (child-identifier nil))
    (unwind-protect
         (let* ((result
                  (mcp-client-call-tool
                   client "spawn-child" (json-object)))
                (structured
                  (mcp-call-result-structured-content result)))
           (setf child-identifier (json-get structured "pid"))
           (test-assert (integerp child-identifier))
           (test-assert
            (probe-file
             (pathname
              (format nil "/proc/~D/" child-identifier))))
           (mcp-client-close client)
           (test-assert
            (test-wait-until
             (lambda ()
               (null
                (probe-file
                 (pathname
                  (format nil "/proc/~D/" child-identifier)))))
             2.0)
            "Closing the transport must kill descendants in its group."))
      (when (mcp-client-connected-p client)
        (mcp-client-close client)))))

(define-test stdio-detach-forgets-resources-without-signaling
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client transport :startup-timeout 3))
         (process nil))
    (unwind-protect
         (progn
           (mcp-client-connect client)
           (setf process (mcp-stdio-transport-process transport))
           (mcp-client-detach client)
           (test-assert (not (mcp-client-connected-p client)))
           (test-assert (not (mcp-transport-open-p transport)))
           (test-assert
            (null (mcp-stdio-transport-process transport)))
           (test-assert
            (null
             (mcp-stdio-transport-reader-thread transport)))
           (test-assert
            (test-wait-until
             (lambda ()
               (not
                (handler-case
                    (uiop:process-alive-p process)
                  (error ()
                    nil))))
             1.0)
            "Closing the only local descriptors lets the fixture exit."))
      (when (and process
                 (handler-case
                     (uiop:process-alive-p process)
                   (error ()
                     nil)))
        (uiop:terminate-process process :urgent t))
      (when process
        (handler-case
            (uiop:wait-process process)
          (error ()
            nil))))))

;;;; -- Streamable HTTP Transport Tests --

(-> test-json-response-body (hash-table t) string)
(defun test-json-response-body (request result)
  "Encode a successful JSON-RPC response to REQUEST carrying RESULT."
  (json-encode (test-rpc-result request result)))

(-> test-sse-body (&rest hash-table) string)
(defun test-sse-body (&rest messages)
  "Encode MESSAGES as consecutive Server-Sent Events."
  (with-output-to-string (stream)
    (dolist (message messages)
      (format stream "event: message~C~Cdata: ~A~C~C~C~C"
              #\Return #\Newline
              (json-encode message)
              #\Return #\Newline #\Return #\Newline))))

(-> test-sse-resume-body (string integer &optional hash-table) string)
(defun test-sse-resume-body (identifier retry &optional message)
  "Encode one SSE event with IDENTIFIER, RETRY, and optional JSON MESSAGE."
  (with-output-to-string (stream)
    (format stream "id: ~A~C~Cretry: ~D~C~C"
            identifier #\Return #\Newline
            retry #\Return #\Newline)
    (when message
      (format stream "data: ~A~C~C"
              (json-encode message)
              #\Return #\Newline))
    (format stream "~C~C" #\Return #\Newline)))

(-> test-http-ping-request (integer) hash-table)
(defun test-http-ping-request (identifier)
  "Return one raw JSON-RPC ping request with IDENTIFIER."
  (json-object
   "jsonrpc" "2.0"
   "id" identifier
   "method" "ping"))

(-> test-http-initialize-request (integer) hash-table)
(defun test-http-initialize-request (identifier)
  "Return one raw JSON-RPC initialize request with IDENTIFIER."
  (json-object
   "jsonrpc" "2.0"
   "id" identifier
   "method" "initialize"
   "params"
   (json-object
    "protocolVersion" "2025-11-25"
    "capabilities" (json-object)
    "clientInfo"
    (json-object "name" "mcparen-test" "version" "1"))))

(define-test http-supports-json-notification-session-and-close
  (let ((saw-initialized-p nil)
        (saw-ping-p nil)
        (saw-delete-p nil))
    (labels ((handler (server request)
               "Serve a stateful JSON MCP session."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (test-equal
                   "session-json"
                   (test-http--header request "mcp-session-id")
                   :test #'string=)
                  (setf saw-delete-p t)
                  (values
                   202
                   (list
                    (cons "Mcp-Session-Id" "session-json"))
                   ""))
                 (t
                  (let* ((message
                           (json-decode
                            (test-http-request-body request)))
                         (method (json-get message "method")))
                    (cond
                      ((string= method "initialize")
                       (test-assert
                        (null
                         (test-http--header
                          request "mcp-session-id")))
                       (values
                        200
                        (list
                         (cons "Content-Type"
                               "application/json")
                         (cons "Mcp-Session-Id"
                               "session-json"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string=
                        method "notifications/initialized")
                       (test-equal
                        "session-json"
                        (test-http--header
                         request "mcp-session-id")
                        :test #'string=)
                       (test-equal
                        "2025-11-25"
                        (test-http--header
                         request "mcp-protocol-version")
                        :test #'string=)
                       (setf saw-initialized-p t)
                       (values 202 nil ""))
                      ((string= method "ping")
                       (setf saw-ping-p t)
                       (values
                        200
                        (list
                         (cons "Content-Type"
                               "application/json"))
                        (test-json-response-body
                         message (json-object))))
                      (t
                       (error "Unexpected JSON HTTP method ~S."
                              method))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client
                 (make-mcp-client
                  transport
                  :startup-timeout 3)))
          (mcp-client-connect client)
          (test-assert (mcp-client-ping client))
          (mcp-client-close client)
          (test-assert saw-initialized-p)
          (test-assert saw-ping-p)
          (test-assert saw-delete-p)
          (test-equal
           4
           (length (test-http-server-requests server))))))))

(define-test http-stages-session-until-initialize-result-is-committed
  (let ((delete-count 0))
    (labels ((handler (server request)
               "Return a session-bearing raw initialize response."
               (declare (ignore server))
               (if (string= (test-http-request-method request) "DELETE")
                   (progn
                     (incf delete-count)
                     (values 202 nil ""))
                   (let ((message
                           (json-decode
                            (test-http-request-body request))))
                     (values
                      200
                      (list
                       (cons "Content-Type" "application/json")
                       (cons "Mcp-Session-Id" "session-pending"))
                      (test-json-response-body
                       message (test-initialize-result)))))))
      (dolist (cleanup '(:close :detach))
        (with-test-http-server (server #'handler)
          (let ((transport
                  (make-mcp-streamable-http-transport
                   (test-http-server-url server))))
            (mcp-transport-open transport)
            (mcp-transport-request
             transport (test-http-initialize-request 41) 2)
            (test-assert
             (null
              (mcp-http-transport-session-identifier transport)))
            (test-equal
             "session-pending"
             (mcp-http-transport-pending-session-identifier
              transport)
             :test #'string=)
            (ecase cleanup
              (:close
               (mcp-transport-close transport))
              (:detach
               (mcp-transport-detach transport)))
            (test-assert
             (null
              (mcp-http-transport-session-identifier transport)))
            (test-assert
             (null
              (mcp-http-transport-pending-session-identifier
               transport)))))))
    (test-equal 0 delete-count)))

(define-test http-discards-unvalidated-initialize-sessions
  (dolist (response-kind '(:rpc-error :invalid-result))
    (let ((delete-count 0))
      (labels ((handler (server request)
                 "Return an unusable initialize response carrying a session."
                 (declare (ignore server))
                 (if
                     (string=
                      (test-http-request-method request)
                      "DELETE")
                     (progn
                       (incf delete-count)
                       (values 202 nil ""))
                     (let ((message
                             (json-decode
                              (test-http-request-body request))))
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json")
                         (cons
                          "Mcp-Session-Id"
                          "session-unvalidated"))
                        (json-encode
                         (ecase response-kind
                           (:rpc-error
                            (test-rpc-error
                             message -32603 "Initialization failed."))
                           (:invalid-result
                            (test-rpc-result
                             message
                             (json-object
                              "protocolVersion" "2025-11-25"
                              "capabilities" (json-object)))))))))))
        (with-test-http-server (server #'handler)
          (let* ((transport
                   (make-mcp-streamable-http-transport
                    (test-http-server-url server)))
                 (client (make-mcp-client transport)))
            (test-signals mcp-error
              (mcp-client-connect client))
            (test-assert
             (null
              (mcp-http-transport-session-identifier transport)))
            (test-assert
             (null
              (mcp-http-transport-pending-session-identifier
               transport)))
            (test-equal 0 delete-count)
            (mcp-client-close client)
            (test-equal 0 delete-count)))))))

(define-test http-rejects-session-headers-outside-initialization
  (let ((delete-count 0))
    (labels ((handler (server request)
               "Return the active session header again on ping."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (incf delete-count)
                  (values 202 nil ""))
                 (t
                  (let* ((message
                           (json-decode
                            (test-http-request-body request)))
                         (method (json-get message "method")))
                    (cond
                      ((string= method "initialize")
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json")
                         (cons "Mcp-Session-Id" "session-stable"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      ((string= method "ping")
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json")
                         (cons "Mcp-Session-Id" "session-stable"))
                        (test-json-response-body
                         message (json-object))))
                      (t
                       (error "Unexpected session test method ~S."
                              method))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client (make-mcp-client transport)))
          (unwind-protect
               (progn
                 (mcp-client-connect client)
                 (let ((condition
                         (test-signals mcp-protocol-error
                           (mcp-client-ping client))))
                   (test-assert
                    (search "outside initialization"
                            (mcp-error-message condition)))))
            (mcp-client-close client))
          (test-equal 1 delete-count))))))

(define-test http-rejects-managed-header-overrides
  (dolist (name
           '("Content-Type"
             "accept"
             "MCP-Session-ID"
             "mcp-protocol-version"
             "Last-Event-ID"))
    (let ((transport
            (make-mcp-streamable-http-transport
             "http://127.0.0.1:1/mcp"
             :headers-function
             (lambda ()
               (list (cons name "override"))))))
      (mcp-transport-open transport)
      (unwind-protect
           (test-signals mcp-transport-error
             (mcp-transport-notify
              transport
              (mcp-client--notification "fixture")
              0.1))
        (mcp-transport-close transport)))))

(define-test http-does-not-follow-redirects
  (let ((followed-p nil))
    (labels ((handler (server request)
               "Redirect the endpoint to a target that must remain unused."
               (if (string= (test-http-request-target request)
                            "/redirect-target")
                   (progn
                     (setf followed-p t)
                     (let ((message
                             (json-decode
                              (test-http-request-body request))))
                       (values
                        200
                        (list
                         (cons "Content-Type"
                               "application/json"))
                        (test-json-response-body
                         message (json-object)))))
                   (values
                    302
                    (list
                     (cons
                      "Location"
                      (format
                       nil
                       "http://127.0.0.1:~D/redirect-target"
                       (test-http-server-port server))))
                    ""))))
      (with-test-http-server (server #'handler)
        (let ((transport
                (make-mcp-streamable-http-transport
                 (test-http-server-url server)
                 :headers-function
                 (lambda ()
                   (list
                    (cons "Authorization"
                          "Bearer fixture-secret"))))))
          (mcp-transport-open transport)
          (unwind-protect
               (test-signals mcp-transport-error
                 (mcp-transport-request
                  transport
                  (json-object
                   "jsonrpc" "2.0"
                   "id" 1
                   "method" "ping")
                  2))
            (mcp-transport-close transport))
          (test-assert
           (not followed-p)
           "Credential-bearing MCP requests must never redirect."))))))

(define-test http-rejects-oversized-json-bodies
  (labels ((handler (server request)
             "Return a JSON response larger than the transport limit."
             (declare (ignore server))
             (let ((message
                     (json-decode
                      (test-http-request-body request))))
               (values
                200
                (list (cons "Content-Type" "application/json"))
                (test-json-response-body
                 message
                 (json-object
                  "payload"
                  (make-string 4096 :initial-element #\x)))))))
    (with-test-http-server (server #'handler)
      (let ((transport
              (make-mcp-streamable-http-transport
               (test-http-server-url server)
               :maximum-message-characters 512)))
        (mcp-transport-open transport)
        (unwind-protect
             (let ((condition
                     (test-signals mcp-message-too-large
                       (mcp-transport-request
                        transport
                        (test-http-ping-request 61)
                        2))))
               (test-equal
                512
                (mcp-message-too-large-limit condition)))
          (mcp-transport-close transport))))))

(define-test http-rejects-oversized-sse-lines-and-events
  (let ((large-line
          (format nil
                  "data: ~A~C~C~C~C"
                  (make-string 1024 :initial-element #\x)
                  #\Return #\Newline #\Return #\Newline))
        (large-event
          (with-output-to-string (stream)
            (format stream "id: aggregate~C~C"
                    #\Return #\Newline)
            (dotimes (index 40)
              (declare (ignore index))
              (format stream "data: ~A~C~C"
                      (make-string 20 :initial-element #\x)
                      #\Return #\Newline))
            (format stream "~C~C" #\Return #\Newline))))
    (dolist (body (list large-line large-event))
      (labels ((handler (server request)
                 "Return the current deliberately oversized SSE BODY."
                 (declare (ignore server request))
                 (values
                  200
                  (list
                   (cons "Content-Type" "text/event-stream"))
                  body)))
        (with-test-http-server (server #'handler)
          (let ((transport
                  (make-mcp-streamable-http-transport
                   (test-http-server-url server)
                   :maximum-message-characters 512)))
            (mcp-transport-open transport)
            (unwind-protect
                 (test-signals mcp-message-too-large
                   (mcp-transport-request
                    transport
                    (test-http-ping-request 62)
                    2))
              (mcp-transport-close transport))))))))

(define-test http-timeout-remains-typed-and-sends-cancellation
  (let ((cancelled-identifier nil))
    (labels ((handler (server request)
               "Delay a tool response while accepting its cancellation."
               (declare (ignore server))
               (let* ((message
                        (json-decode
                         (test-http-request-body request)))
                      (method (json-get message "method")))
                 (cond
                   ((string= method "initialize")
                    (values
                     200
                     (list
                      (cons "Content-Type" "application/json"))
                     (test-json-response-body
                      message (test-initialize-result))))
                   ((string= method "notifications/initialized")
                    (values 202 nil ""))
                   ((string= method "tools/call")
                    (sleep 0.3)
                    (values
                     200
                     (list
                      (cons "Content-Type" "application/json"))
                     (test-json-response-body
                      message
                      (json-object
                       "content"
                       (vector
                        (json-object
                         "type" "text"
                         "text" "too late"))))))
                   ((string= method "notifications/cancelled")
                    (setf cancelled-identifier
                          (json-get
                           (json-get message "params")
                           "requestId"))
                    (values 202 nil ""))
                   (t
                    (error "Unexpected timeout fixture method ~S."
                           method))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client
                 (make-mcp-client
                  transport
                  :startup-timeout 2
                  :tool-timeout 0.08)))
          (unwind-protect
               (progn
                 (mcp-client-connect client)
                 (test-signals mcp-timeout
                   (mcp-client-call-tool
                    client "delayed" (json-object)
                    :timeout 0.08))
                 (test-assert
                  (test-wait-until
                   (lambda () cancelled-identifier)
                   1.0)
                  "The timed-out HTTP request sent notifications/cancelled.")
                 (test-assert
                  (integerp cancelled-identifier)))
            (mcp-client-close client)))))))

(define-test http-resumes-sse-with-event-identifier-and-retry
  (let ((saw-resume-p nil)
        (saw-delete-p nil)
        (ping-request nil))
    (labels ((handler (server request)
               "Close one SSE response and resume it through GET."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (setf saw-delete-p t)
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (test-equal
                   ""
                   (test-http-request-body request)
                   :test #'string=)
                  (test-equal
                   "resume-one"
                   (test-http--header request "last-event-id")
                   :test #'string=)
                  (test-equal
                   "session-resume"
                   (test-http--header request "mcp-session-id")
                   :test #'string=)
                  (test-equal
                   "2025-11-25"
                   (test-http--header
                    request "mcp-protocol-version")
                   :test #'string=)
                  (test-assert
                   (search
                    "text/event-stream"
                    (test-http--header request "accept")
                    :test #'char-equal))
                  (setf saw-resume-p t)
                  (values
                   200
                   (list
                    (cons "Content-Type" "text/event-stream"))
                    (test-sse-resume-body
                    "resume-two"
                    20
                    (test-rpc-result
                     ping-request
                     (json-object)))))
                 (t
                  (let* ((message
                           (json-decode
                            (test-http-request-body request)))
                         (method (json-get message "method")))
                    (cond
                      ((string= method "initialize")
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json")
                         (cons "Mcp-Session-Id" "session-resume"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      ((string= method "ping")
                       (setf ping-request message)
                       (values
                        200
                        (list
                         (cons "Content-Type" "text/event-stream"))
                        (test-sse-resume-body
                         "resume-one" 20)))
                      (t
                       (error "Unexpected resume method ~S."
                              method))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client (make-mcp-client transport)))
          (unwind-protect
               (let* ((started (get-internal-real-time))
                      (response (mcp-client-ping client))
                      (elapsed
                        (/
                         (- (get-internal-real-time) started)
                         internal-time-units-per-second)))
                 (test-assert response)
                 (test-assert saw-resume-p)
                 (test-assert
                  (>= elapsed 0.015)
                  "The server-provided retry interval was respected."))
            (mcp-client-close client))
          (test-assert saw-delete-p))))))

(define-test http-rejects-session-header-on-sse-get
  (let ((ping-request nil))
    (labels ((handler (server request)
               "Return the active session header on an SSE resumption GET."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (values
                   200
                   (list
                    (cons "Content-Type" "text/event-stream")
                    (cons "Mcp-Session-Id" "session-get"))
                   (test-sse-resume-body
                    "get-two"
                    20
                    (test-rpc-result
                     ping-request (json-object)))))
                 (t
                  (let* ((message
                           (json-decode
                            (test-http-request-body request)))
                         (method (json-get message "method")))
                    (cond
                      ((string= method "initialize")
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json")
                         (cons "Mcp-Session-Id" "session-get"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      ((string= method "ping")
                       (setf ping-request message)
                       (values
                        200
                        (list
                         (cons "Content-Type" "text/event-stream"))
                        (test-sse-resume-body "get-one" 20)))
                      (t
                       (error "Unexpected GET session test method ~S."
                              method))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client (make-mcp-client transport)))
          (unwind-protect
               (let ((condition
                       (test-signals mcp-protocol-error
                         (mcp-client-ping client))))
                 (test-assert
                  (search "outside initialization"
                          (mcp-error-message condition))))
            (mcp-client-close client)))))))

(define-test http-sse-resumption-shares-one-request-deadline
  (let ((saw-resume-p nil))
    (labels ((handler (server request)
               "Make retry plus resumed response exceed one deadline."
               (declare (ignore server))
               (if
                   (string=
                    (test-http-request-method request)
                    "GET")
                   (progn
                     (setf saw-resume-p t)
                     (sleep 0.1)
                     (values
                      200
                      (list
                       (cons "Content-Type"
                             "text/event-stream"))
                      (test-sse-resume-body
                       "deadline-two"
                       70
                       (test-rpc-result
                        (test-http-ping-request 74)
                        (json-object)))))
                   (values
                    200
                    (list
                     (cons "Content-Type"
                           "text/event-stream"))
                    (test-sse-resume-body
                     "deadline-one" 70)))))
      (with-test-http-server (server #'handler)
        (let ((transport
                (make-mcp-streamable-http-transport
                 (test-http-server-url server))))
          (mcp-transport-open transport)
          (unwind-protect
               (let ((started (get-internal-real-time)))
                 (test-signals mcp-timeout
                   (mcp-transport-request
                    transport
                    (test-http-ping-request 74)
                    0.13))
                 (let ((elapsed
                         (/
                          (- (get-internal-real-time) started)
                          internal-time-units-per-second)))
                   (test-assert saw-resume-p)
                   (test-assert
                    (< elapsed 0.2)
                    "A resumed GET must not receive a fresh deadline.")))
            (mcp-transport-close transport)))))))

(define-test http-closes-body-when-session-header-is-malformed
  (labels ((handler (server request)
             "Return a complete response with an invalid session header."
             (declare (ignore server))
             (let ((message
                     (json-decode
                      (test-http-request-body request))))
               (values
                200
                (list
                 (cons "Content-Type" "application/json")
                 (cons "Mcp-Session-Id" "invalid session"))
                (test-json-response-body
                 message (json-object))))))
    (with-test-http-server (server #'handler)
      (let ((transport
              (make-mcp-streamable-http-transport
               (test-http-server-url server))))
        (mcp-transport-open transport)
        (unwind-protect
             (let ((before (test-open-file-descriptor-count)))
               (dotimes (index 24)
                 (test-signals mcp-protocol-error
                   (mcp-transport-request
                    transport
                    (test-http-ping-request (+ 100 index))
                    2)))
               (test-assert
                (test-wait-until
                 (lambda ()
                   (with-lock-held
                       ((test-http-server-lock server))
                     (notany
                      #'thread-alive-p
                      (test-http-server-worker-threads server))))
                 1.0)
                "The HTTP fixture finished every malformed response.")
               (let ((after (test-open-file-descriptor-count)))
                 (test-assert
                  (<= (- after before) 2)
                  "Malformed session responses must not leak body streams.")))
          (mcp-transport-close transport))))))

(define-test http-supports-sse-server-messages-and-202-response
  (let ((notifications nil)
        (client-response nil))
    (labels ((handler (server request)
               "Serve one SSE exchange containing server messages."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 (t
                  (let* ((message
                           (json-decode
                            (test-http-request-body request)))
                         (method (json-get message "method"))
                         (identifier
                           (json-get message "id" :absent)))
                    (cond
                      ((and (eql identifier 991)
                            (null method))
                       (setf client-response message)
                       (values 202 nil ""))
                      ((string= method "initialize")
                       (values
                        200
                        (list
                         (cons "Content-Type"
                               "application/json")
                         (cons "Mcp-Session-Id"
                               "session-sse"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string=
                        method "notifications/initialized")
                       (values 202 nil ""))
                      ((string= method "ping")
                       (values
                        200
                        (list
                         (cons "Content-Type"
                               "text/event-stream"))
                        (test-sse-body
                         (json-object
                          "jsonrpc" "2.0"
                          "method" "notifications/progress"
                          "params"
                          (json-object "progress" 1))
                         (json-object
                          "jsonrpc" "2.0"
                          "id" 991
                          "method" "sampling/createMessage"
                          "params"
                          (json-object "prompt" "fixture"))
                         (test-rpc-result
                          message (json-object)))))
                      (t
                       (error "Unexpected SSE HTTP message ~S."
                              message))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)
                  :request-handler
                  (lambda (method params)
                    (test-equal
                     "sampling/createMessage"
                     method :test #'string=)
                    (test-equal
                     "fixture"
                     (json-get params "prompt")
                     :test #'string=)
                    (json-object "role" "assistant"))
                  :notification-handler
                  (lambda (method params)
                    (push
                     (list method (json-get params "progress"))
                     notifications))))
               (client
                 (make-mcp-client
                  transport
                  :capabilities
                  (json-object "sampling" (json-object))
                  :startup-timeout 3)))
          (unwind-protect
               (progn
                 (test-assert (mcp-client-ping client))
                 (test-equal
                  '(("notifications/progress" 1))
                  (nreverse notifications))
                 (test-assert client-response)
                 (test-equal
                  "assistant"
                  (json-get
                   (json-get client-response "result")
                   "role")
                  :test #'string=))
            (mcp-client-close client)))))))

(define-test http-reinitializes-on-expired-session
  (let ((initialize-count 0)
        (expired-p nil))
    (labels ((handler (server request)
               "Expire the first stateful HTTP session once."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 (t
                  (let* ((message
                           (json-decode
                            (test-http-request-body request)))
                         (method (json-get message "method")))
                    (cond
                      ((string= method "initialize")
                       (incf initialize-count)
                       (values
                        200
                        (list
                         (cons "Content-Type"
                               "application/json")
                         (cons
                          "Mcp-Session-Id"
                          (if (= initialize-count 1)
                              "session-old"
                              "session-new")))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string=
                        method "notifications/initialized")
                       (values 202 nil ""))
                      ((string= method "resources/list")
                       (let ((session
                               (test-http--header
                                request "mcp-session-id")))
                         (if (and
                              (string= session "session-old")
                              (not expired-p))
                             (progn
                               (setf expired-p t)
                               (values
                                404
                                (list
                                 (cons "Content-Type"
                                       "text/plain"))
                                "expired"))
                             (progn
                               (test-equal
                                "session-new" session
                                :test #'string=)
                               (values
                                200
                                (list
                                 (cons "Content-Type"
                                       "application/json"))
                                (test-json-response-body
                                 message
                                 (json-object
                                  "resources"
                                  (vector
                                   (json-object
                                    "uri"
                                    "fixture:///after-reconnect"
                                    "name"
                                    "after-reconnect")))))))))
                      (t
                       (error
                        "Unexpected reinitialize HTTP method ~S."
                        method))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client
                 (make-mcp-client
                  transport
                  :startup-timeout 3)))
          (unwind-protect
               (let ((resources
                       (mcp-client-list-resources client)))
                 (test-equal 2 initialize-count)
                 (test-assert expired-p)
                 (test-equal
                  "fixture:///after-reconnect"
                  (json-get (first resources) "uri")
                  :test #'string=)
                 (test-equal
                  "session-new"
                  (mcp-http-transport-session-identifier
                   transport)
                  :test #'string=))
            (mcp-client-close client)))))))

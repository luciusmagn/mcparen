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

(define-test client-negotiates-supported-older-protocol
  (labels ((handler (transport request)
             "Select MCP 2025-06-18 and expose one tool."
             (declare (ignore transport))
             (let ((method (json-get request "method")))
               (cond
                 ((string= method "initialize")
                  (test-rpc-result
                   request
                   (json-object
                    "protocolVersion" "2025-06-18"
                    "capabilities" (json-object "tools" (json-object))
                    "serverInfo"
                    (json-object
                     "name" "older-protocol-fixture"
                     "version" "1"))))
                 ((string= method "tools/list")
                  (test-rpc-result
                   request
                   (json-object
                    "tools"
                    (vector
                     (json-object
                      "name" "older-tool"
                      "description" "A tool from an older session."
                      "inputSchema" (json-object "type" "object")
                      "execution"
                      (json-object "taskSupport" "required"))))))
                 ((string= method "tools/call")
                  (test-rpc-result
                   request
                   (json-object
                    "content"
                    (vector
                     (json-object "type" "text" "text" "called")))))
                 (t
                  (error "Unexpected older-protocol method ~S." method))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport)))
      (unwind-protect
           (progn
             (mcp-client-connect client)
             (test-equal
              "2025-06-18"
              (mcp-client-protocol-version client)
              :test #'string=)
             (test-equal
              "2025-06-18"
              (test-scripted-transport-protocol-version transport)
              :test #'string=)
             (let* ((initialize-request
                      (first
                       (test-scripted-transport-requests transport)))
                    (initialize-params
                      (json-get initialize-request "params")))
               (test-equal
                "2025-11-25"
                (json-get initialize-params "protocolVersion")
                :test #'string=))
             (let ((tool (first (mcp-client-list-tools client))))
               (test-equal
                "forbidden"
                (mcp-tool-task-support tool)
                :test #'string=)
               (test-equal
                "called"
                (json-get
                 (first
                  (mcp-call-result-content
                   (mcp-client-call-tool
                    client tool (json-object))))
                 "text")
                :test #'string=)))
        (mcp-client-close client)))))

(define-test client-rejects-unsupported-negotiated-protocol
  (let* ((transport
           (make-test-scripted-transport
            (lambda (ignored request)
              (declare (ignore ignored))
              (test-rpc-result
               request
               (json-object
                "protocolVersion" "2025-03-26"
                "capabilities" (json-object)
                "serverInfo"
                (json-object
                 "name" "unsupported-protocol-fixture"
                 "version" "1"))))))
         (client (make-mcp-client transport)))
    (let ((condition
            (test-signals mcp-protocol-error
              (mcp-client-connect client))))
      (test-equal
       "initialize"
       (mcp-protocol-error-method condition)
       :test #'string=)
      (test-assert
       (search "unsupported protocol"
               (mcp-error-message condition)
               :test #'char-equal))
      (test-assert (not (mcp-client-connected-p client)))
      (test-assert (not (test-scripted-transport-open-p transport)))
      (test-assert (null (mcp-client-protocol-version client)))
      (test-assert
       (null
        (test-scripted-transport-protocol-version transport))))))

(define-test client-connection-generation-tracks-lifecycle
  (let* ((transport
           (make-test-scripted-transport #'test-default-handler))
         (client (make-mcp-client transport)))
    (test-equal 0 (mcp-client-connection-generation client))
    (unwind-protect
         (progn
           (mcp-client-connect client)
           (test-equal
            1
            (mcp-client-connection-generation client))
           (mcp-client-connect client)
           (test-equal
            1
            (mcp-client-connection-generation client))
           (mcp-client-close client)
           (test-equal
            2
            (mcp-client-connection-generation client))
           (mcp-client-connect client)
           (test-equal
            3
            (mcp-client-connection-generation client)))
      (when (mcp-client-connected-p client)
        (mcp-client-close client)))))

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
                     client
                     (test-tool "mixed")
                     (json-object "input" "x")))
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

(define-test tool-execution-validates-task-support
  (let ((tool (test-tool "absent-execution")))
    (test-assert (null (mcp-tool-execution tool)))
    (test-equal
     "forbidden"
     (mcp-tool-task-support tool)
     :test #'string=)
    (test-assert (not (mcp-tool-task-required-p tool))))
  (let* ((execution (json-object))
         (tool
           (test-tool
            "implicit-forbidden"
            :execution execution)))
    (test-assert (eq execution (mcp-tool-execution tool)))
    (test-equal
     "forbidden"
     (mcp-tool-task-support tool)
     :test #'string=))
  (dolist (task-support '("forbidden" "optional" "required"))
    (let* ((execution
             (json-object "taskSupport" task-support))
           (tool
             (test-tool
              task-support
              :execution execution)))
      (test-assert (eq execution (mcp-tool-execution tool)))
      (test-equal
       task-support
       (mcp-tool-task-support tool)
       :test #'string=)
      (test-equal
       (string= task-support "required")
       (mcp-tool-task-required-p tool))))
  (dolist (invalid-execution
           (list nil :null yason:false #() "required"))
    (test-signals mcp-protocol-error
      (test-tool
       "invalid-execution"
       :execution invalid-execution)))
  (dolist (invalid-task-support
           (list nil
                 :null
                 yason:false
                 #()
                 7
                 "Required"
                 "OPTIONAL"
                 "allowed"))
    (test-signals mcp-protocol-error
      (test-tool
       "invalid-task-support"
       :execution
       (json-object
        "taskSupport" invalid-task-support)))))

(define-test tool-call-rejects-required-task-before-transport-io
  (let* ((transport
           (make-test-scripted-transport
            (lambda (ignored-transport ignored-request)
              (declare (ignore ignored-transport ignored-request))
              (error "A rejected task tool reached the transport."))))
         (client (make-mcp-client transport))
         (tool
           (test-tool
            "required-task"
            :execution
            (json-object "taskSupport" "required"))))
    (let ((condition
            (test-signals mcp-task-execution-unsupported
              (mcp-client-call-tool client tool (json-object)))))
      (test-assert
       (eq tool
           (mcp-task-execution-unsupported-tool condition)))
      (test-equal
       "tools/call"
       (mcp-protocol-error-method condition)
       :test #'string=)
      (test-assert
       (eq (mcp-tool-raw tool)
           (mcp-protocol-error-payload condition))))
    (test-assert
     (not (test-scripted-transport-open-p transport)))
    (test-assert
     (null (test-scripted-transport-requests transport)))
    (test-assert
     (null (test-scripted-transport-notifications transport)))
    (test-equal
     0
     (test-scripted-transport-close-count transport))))

(define-test tool-call-allows-forbidden-and-optional-task-support
  (labels ((handler (transport request)
             "Initialize or echo the called tool name."
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
                     (json-object
                      "type" "text"
                      "text"
                      (json-get
                       (json-get request "params")
                       "name"))))))
                 (t
                  (error "Unexpected method ~S." method))))))
    (let* ((transport (make-test-scripted-transport #'handler))
           (client (make-mcp-client transport))
           (forbidden
             (test-tool
              "forbidden"
              :execution
              (json-object "taskSupport" "forbidden")))
           (optional
             (test-tool
              "optional"
              :execution
              (json-object "taskSupport" "optional"))))
      (unwind-protect
           (progn
             (test-equal
              "forbidden"
              (json-get
               (first
                (mcp-call-result-content
                 (mcp-client-call-tool
                  client forbidden (json-object))))
               "text")
              :test #'string=)
             (test-equal
              "optional"
              (json-get
               (first
                (mcp-call-result-content
                 (mcp-client-call-tool
                  client optional (json-object))))
               "text")
              :test #'string=))
        (mcp-client-close client)))))

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
             (mcp-client-call-tool
              client
              (test-tool "malformed")
              (json-object)))
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
                       client
                       (test-tool "malformed")
                       (json-object)))
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
                client
                (test-tool "slow")
                (json-object)))
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
                client
                (test-tool "fast")
                (json-object)))
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
                      client
                      (test-tool "never")
                      (json-object)
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

(define-test stdio-launches-with-an-explicit-utf-8-external-format
  (let ((transport
          (make-mcp-stdio-transport "/bin/true")))
    (test-equal
     ':utf-8
     (getf (mcp-stdio--launch-arguments transport) :external-format))))

(define-test stdio-round-trips-non-ascii-text
  (let* ((transport (make-test-stdio-transport))
         (client
           (make-mcp-client
            transport
            :startup-timeout 3
            :tool-timeout 2))
         (text "Příliš žluťoučký kůň úpěl ďábelské ódy 😀"))
    (unwind-protect
         (let ((result
                 (mcp-client-call-tool
                  client
                  (test-tool "utf8")
                  (json-object "text" text))))
           (test-equal
            text
            (json-get
             (first (mcp-call-result-content result))
             "text")
            :test #'string=)
           (test-equal
            text
            (json-get
             (mcp-call-result-structured-content result)
             "echo")
            :test #'string=))
      (mcp-client-close client))))

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
                client
                (test-tool "stderr-flood")
                (json-object))))
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
                client
                (test-tool "stderr-long-line")
                (json-object))))
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
                    client
                    (test-tool "oversized-stdout")
                    (json-object)))))
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
              client
              (test-tool "malformed-stdout")
              (json-object)))
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
                  client
                  (test-tool "callback-order")
                  (json-object))))
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
              client
              (test-tool "callback-overflow")
              (json-object)))
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
                    client
                    (test-tool "server-request")
                    (json-object))))
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
                   client
                   (test-tool "spawn-child")
                   (json-object)))
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

(-> test-http-idle-get-p (test-http-request) boolean)
(defun test-http-idle-get-p (request)
  "Return true when REQUEST is the initial owned idle GET."
  (and (string= (test-http-request-method request) "GET")
       (null (test-http--header request "last-event-id"))))

(-> test-http-keepalive-streaming-body () test-http-streaming-body)
(defun test-http-keepalive-streaming-body ()
  "Return a streaming body that remains open until its client closes."
  (make-test-http-streaming-body
   (lambda (stream)
     (loop
       (format stream ": keepalive~C~C~C~C"
               #\Return #\Newline
               #\Return #\Newline)
       (finish-output stream)
       (sleep 0.02)))))

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
                 ((test-http-idle-get-p request)
                  (values 405 nil ""))
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
           5
           (length (test-http-server-requests server))))))))

(define-test http-uses-negotiated-protocol-in-subsequent-headers
  (let ((saw-initialized-p nil)
        (saw-ping-p nil))
    (labels ((handler (server request)
               "Select MCP 2025-06-18 and inspect later request headers."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 ((test-http-idle-get-p request)
                  (test-equal
                   "2025-06-18"
                   (test-http--header
                    request "mcp-protocol-version")
                   :test #'string=)
                  (values 405 nil ""))
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
                          request "mcp-protocol-version")))
                       (test-equal
                        "2025-11-25"
                        (json-get
                         (json-get message "params")
                         "protocolVersion")
                        :test #'string=)
                       (let ((result (test-initialize-result)))
                         (setf (gethash "protocolVersion" result)
                               "2025-06-18")
                         (values
                          200
                          (list
                           (cons "Content-Type" "application/json")
                           (cons "Mcp-Session-Id"
                                 "session-older-protocol"))
                          (test-json-response-body message result))))
                      ((string= method "notifications/initialized")
                       (test-equal
                        "2025-06-18"
                        (test-http--header
                         request "mcp-protocol-version")
                        :test #'string=)
                       (setf saw-initialized-p t)
                       (values 202 nil ""))
                      ((string= method "ping")
                       (test-equal
                        "2025-06-18"
                        (test-http--header
                         request "mcp-protocol-version")
                        :test #'string=)
                       (setf saw-ping-p t)
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json"))
                        (test-json-response-body
                         message (json-object))))
                      (t
                       (error "Unexpected fallback HTTP method ~S."
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
               (progn
                 (mcp-client-connect client)
                 (test-equal
                  "2025-06-18"
                  (mcp-http-transport-protocol-version transport)
                  :test #'string=)
                 (test-assert (mcp-client-ping client))
                 (test-assert saw-initialized-p)
                 (test-assert saw-ping-p))
            (mcp-client-close client)))))))

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
                 ((test-http-idle-get-p request)
                  (values 405 nil ""))
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
               (if (test-http-idle-get-p request)
                   (values 405 nil "")
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
                               method)))))))
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
                    client
                    (test-tool "delayed")
                    (json-object)
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
                 ((test-http-idle-get-p request)
                  (values 405 nil ""))
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
                 ((test-http-idle-get-p request)
                  (values 405 nil ""))
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
                 ((test-http-idle-get-p request)
                  (values 405 nil ""))
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
                 ((test-http-idle-get-p request)
                  (values 405 nil ""))
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
               (progn
                 (mcp-client-connect client)
                 (test-equal
                  1
                 (mcp-client-connection-generation client))
                 (let ((resources
                         (mcp-client-list-resources client)))
                   (test-equal 2 initialize-count)
                   (test-assert expired-p)
                   (test-equal
                    2
                    (mcp-client-connection-generation client))
                   (test-equal
                    "fixture:///after-reconnect"
                    (json-get (first resources) "uri")
                    :test #'string=)
                   (test-equal
                    "session-new"
                    (mcp-http-transport-session-identifier
                     transport)
                    :test #'string=)))
            (mcp-client-close client)))))))

(define-test http-idle-get-delivers-notifications-and-server-requests
  (let ((idle-get-count 0)
        (notifications nil)
        (client-response nil))
    (labels ((handler (server request)
               "Serve unsolicited messages through one owned idle GET."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (incf idle-get-count)
                  (test-equal
                   "session-idle"
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
                  (values
                   200
                   (list
                    (cons "Content-Type" "text/event-stream"))
                   (make-test-http-streaming-body
                    (lambda (stream)
                      (write-string
                       (test-sse-body
                        (json-object
                         "jsonrpc" "2.0"
                         "method" "notifications/progress"
                         "params" (json-object "progress" 7))
                        (json-object
                         "jsonrpc" "2.0"
                         "id" 701
                         "method" "sampling/createMessage"
                         "params" (json-object "prompt" "idle")))
                       stream)
                      (finish-output stream)
                      (loop
                        (sleep 0.02)
                        (format stream ": keepalive~C~C~C~C"
                                #\Return #\Newline
                                #\Return #\Newline)
                        (finish-output stream))))))
                 (t
                  (let* ((message
                           (json-decode
                            (test-http-request-body request)))
                         (method (json-get message "method"))
                         (identifier
                           (json-get message "id" :absent)))
                    (cond
                      ((and (eql identifier 701)
                            (null method))
                       (setf client-response message)
                       (values 202 nil ""))
                      ((string= method "initialize")
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json")
                         (cons "Mcp-Session-Id" "session-idle"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      (t
                       (error
                        "Unexpected idle delivery message ~S."
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
                     "idle"
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
                  (json-object "sampling" (json-object)))))
          (unwind-protect
               (progn
                 (mcp-client-connect client)
                 (test-assert
                  (test-wait-until
                   (lambda ()
                     (and notifications client-response))
                   2.0)
                  "The idle listener delivered both server messages.")
                 (test-equal
                  '(("notifications/progress" 7))
                  (nreverse notifications))
                 (test-equal
                  "assistant"
                  (json-get
                   (json-get client-response "result")
                   "role")
                  :test #'string=)
                 (test-equal 1 idle-get-count))
            (mcp-client-close client)))))))

(define-test http-idle-get-resumes-with-event-identifier-and-retry
  (let ((get-times nil)
        (last-event-identifiers nil)
        (notifications nil))
    (labels ((handler (server request)
               "Close idle streams and observe their resumptions."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (push (get-internal-real-time) get-times)
                  (push
                   (test-http--header request "last-event-id")
                   last-event-identifiers)
                  (case (length get-times)
                    (1
                     (values
                      200
                      (list
                       (cons "Content-Type" "text/event-stream"))
                      (test-sse-resume-body
                       "idle-one"
                       20
                       (json-object
                        "jsonrpc" "2.0"
                        "method" "notifications/progress"
                        "params" (json-object "progress" 1)))))
                    (2
                     (values
                      200
                      (list
                       (cons "Content-Type" "text/event-stream"))
                      (test-sse-resume-body
                       "idle-two"
                       20
                       (json-object
                        "jsonrpc" "2.0"
                        "method" "notifications/progress"
                        "params" (json-object "progress" 2)))))
                    (t
                     (values 405 nil ""))))
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
                         (cons "Mcp-Session-Id" "session-resume-idle"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      (t
                       (error
                        "Unexpected idle resumption message ~S."
                        message))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)
                  :notification-handler
                  (lambda (method params)
                    (push
                     (list method (json-get params "progress"))
                     notifications))))
               (client (make-mcp-client transport)))
          (unwind-protect
               (progn
                 (mcp-client-connect client)
                 (test-assert
                  (test-wait-until
                   (lambda ()
                     (eq
                      (mcp-http-transport-listener-support-state
                       transport)
                      ':unsupported))
                   2.0)
                  "The idle listener completed both resumptions.")
                 (test-equal
                  '(nil "idle-one" "idle-two")
                  (nreverse last-event-identifiers))
                 (test-equal
                  '(("notifications/progress" 1)
                    ("notifications/progress" 2))
                  (nreverse notifications))
                 (let ((times (nreverse get-times)))
                   (test-assert
                    (>=
                     (/
                      (- (second times) (first times))
                      internal-time-units-per-second)
                     0.015)
                    "The idle listener honored the server retry delay.")))
            (mcp-client-close client)))))))

(define-test http-idle-get-405-disables-listener-without-spinning
  (let ((idle-get-count 0))
    (labels ((handler (server request)
               "Reject the optional idle GET endpoint."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (incf idle-get-count)
                  (values 405 nil ""))
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
                         (cons "Mcp-Session-Id" "session-no-get"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      (t
                       (error
                        "Unexpected unsupported GET message ~S."
                        message))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client (make-mcp-client transport)))
          (unwind-protect
               (progn
                 (mcp-client-connect client)
                 (test-assert
                  (test-wait-until
                   (lambda ()
                     (eq
                      (mcp-http-transport-listener-support-state
                       transport)
                      ':unsupported))
                   1.0)
                  "The listener remembered HTTP 405.")
                 (sleep 0.1)
                 (test-equal 1 idle-get-count))
            (mcp-client-close client)))))))

(define-test http-idle-get-404-reinitializes-at-next-client-boundary
  (let ((initialize-count 0)
        (idle-get-count 0))
    (labels ((handler (server request)
               "Expire the first session from its owned idle GET."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (incf idle-get-count)
                  (if (= idle-get-count 1)
                      (values 404 nil "expired")
                      (values 405 nil "")))
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
                         (cons "Content-Type" "application/json")
                         (cons
                          "Mcp-Session-Id"
                          (if (= initialize-count 1)
                              "session-expiring"
                              "session-restored")))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      ((string= method "ping")
                       (test-equal
                        "session-restored"
                        (test-http--header request "mcp-session-id")
                        :test #'string=)
                       (values
                        200
                        (list
                         (cons "Content-Type" "application/json"))
                        (test-json-response-body
                         message (json-object))))
                      (t
                       (error
                        "Unexpected idle expiry message ~S."
                        message))))))))
      (with-test-http-server (server #'handler)
        (let* ((transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client (make-mcp-client transport)))
          (unwind-protect
               (progn
                 (mcp-client-connect client)
                 (test-assert
                  (test-wait-until
                   (lambda ()
                     (not (mcp-transport-open-p transport)))
                   1.0)
                  "The idle GET expired the first session.")
                 (test-equal
                  1
                  (mcp-client-connection-generation client))
                 (test-assert (mcp-client-ping client))
                 (test-equal 2 initialize-count)
                 (test-assert
                  (test-wait-until
                   (lambda () (= idle-get-count 2))
                   1.0)
                  "The restored session started its idle listener.")
                 (test-equal 2 idle-get-count)
                 (test-equal
                  2
                  (mcp-client-connection-generation client)))
            (mcp-client-close client)))))))

(define-test http-close-stops-idle-listener-before-delete
  (let ((delete-count 0)
        (listener-stopped-before-delete-p nil)
        (transport nil))
    (labels ((handler (server request)
               "Hold an idle GET open and observe close ordering."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (incf delete-count)
                  (let ((thread
                          (mcp-http-transport-listener-thread
                           transport)))
                    (setf listener-stopped-before-delete-p
                          (or (null thread)
                              (not (thread-alive-p thread)))))
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (values
                   200
                   (list
                    (cons "Content-Type" "text/event-stream"))
                   (test-http-keepalive-streaming-body)))
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
                         (cons "Mcp-Session-Id" "session-close"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      (t
                       (error
                        "Unexpected bounded close message ~S."
                        message))))))))
      (with-test-http-server (server #'handler)
        (setf
         transport
         (make-mcp-streamable-http-transport
          (test-http-server-url server)
          :connect-timeout 0.2))
        (let ((client (make-mcp-client transport)))
          (mcp-client-connect client)
          (test-assert
           (test-wait-until
            (lambda ()
              (mcp-http-transport-listener-body transport))
            1.0)
           "The idle response body became owned by the listener.")
          (sleep 0.4)
          (test-assert
           (thread-alive-p
            (mcp-http-transport-listener-thread transport))
           "An established idle stream has no finite read timeout.")
          (let* ((started (get-internal-real-time))
                 (ignored (mcp-client-close client))
                 (elapsed
                   (/
                    (- (get-internal-real-time) started)
                    internal-time-units-per-second)))
            (declare (ignore ignored))
            (test-assert
             (< elapsed 1.5)
             "Closing an idle listener remained bounded."))
          (test-equal 1 delete-count)
          (test-assert listener-stopped-before-delete-p)
          (test-assert
           (null
            (mcp-http-transport-listener-thread transport)))
          (test-assert
           (null
            (mcp-http-transport-listener-body transport))))))))

(define-test http-detach-stops-idle-listener-without-delete-or-fd-leak
  (let ((delete-count 0)
        (initialize-count 0))
    (labels ((handler (server request)
               "Hold one idle GET open for each detached session."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (incf delete-count)
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (values
                   200
                   (list
                    (cons "Content-Type" "text/event-stream"))
                   (test-http-keepalive-streaming-body)))
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
                         (cons "Content-Type" "application/json")
                         (cons
                          "Mcp-Session-Id"
                          (format nil
                                  "session-detach-~D"
                                  initialize-count)))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      (t
                       (error
                        "Unexpected detach message ~S."
                        message))))))))
      (with-test-http-server (server #'handler)
        (let* ((before (test-open-file-descriptor-count))
               (transport
                 (make-mcp-streamable-http-transport
                  (test-http-server-url server)))
               (client (make-mcp-client transport)))
          (dotimes (index 6)
            (declare (ignore index))
            (mcp-client-connect client)
            (test-assert
             (test-wait-until
              (lambda ()
                (mcp-http-transport-listener-body transport))
              1.0)
             "The detached session established its idle GET.")
            (mcp-client-detach client)
            (test-assert
             (null
              (mcp-http-transport-listener-thread transport)))
            (test-assert
             (null
              (mcp-http-transport-listener-body transport))))
          (test-equal 0 delete-count)
          (test-equal 6 initialize-count)
          (test-assert
           (test-wait-until
            (lambda ()
              (with-lock-held
                  ((test-http-server-lock server))
                (notany
                 #'thread-alive-p
                 (test-http-server-worker-threads server))))
            2.0)
           "Every detached HTTP fixture connection closed.")
          (let ((after (test-open-file-descriptor-count)))
            (test-assert
             (<= (- after before) 2)
             "Detached idle listeners must not leak file descriptors.")))))))

(define-test http-bounds-sse-retry-values
  (dolist (value '("0" "1" "60000" "00060000"))
    (test-assert
     (mcp-http--valid-retry-value-p value)))
  (dolist (value '("" "60001" "99999999" "123456789" "1x"))
    (test-assert
     (not (mcp-http--valid-retry-value-p value)))))

(define-test http-idle-get-rejects-json-rpc-responses
  (labels ((handler (server request)
             "Emit a forbidden response through the idle GET stream."
             (declare (ignore server))
             (cond
               ((string= (test-http-request-method request) "DELETE")
                (values 202 nil ""))
               ((string= (test-http-request-method request) "GET")
                (values
                 200
                 (list
                  (cons "Content-Type" "text/event-stream"))
                 (test-sse-body
                  (test-rpc-result
                   (test-http-ping-request 808)
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
                       (cons "Mcp-Session-Id" "session-response"))
                      (test-json-response-body
                       message (test-initialize-result))))
                    ((string= method "notifications/initialized")
                     (values 202 nil ""))
                    (t
                     (error
                      "Unexpected idle response message ~S."
                      message))))))))
    (with-test-http-server (server #'handler)
      (let* ((transport
               (make-mcp-streamable-http-transport
                (test-http-server-url server)))
             (client (make-mcp-client transport)))
        (unwind-protect
             (progn
               (mcp-client-connect client)
               (test-assert
                (test-wait-until
                 (lambda ()
                   (mcp-http-transport-listener-failure transport))
                 1.0)
                "The idle listener rejected the response message.")
               (test-assert
                (typep
                 (mcp-http-transport-listener-failure transport)
                 'mcp-protocol-error))
               (test-assert
                (search
                 "emitted a JSON-RPC response"
                 (mcp-error-message
                  (mcp-http-transport-listener-failure
                   transport)))))
          (mcp-client-close client))))))

(define-test http-idle-get-bounds-consecutive-no-progress-resumptions
  (let ((idle-get-count 0)
        (old-limit
          *mcp-http-idle-no-progress-resumption-limit*)
        (client nil))
    (labels ((handler (server request)
               "Close idle streams without emitting valid events."
               (declare (ignore server))
               (cond
                 ((string= (test-http-request-method request) "DELETE")
                  (values 202 nil ""))
                 ((string= (test-http-request-method request) "GET")
                  (incf idle-get-count)
                  (values
                   200
                   (list
                    (cons "Content-Type" "text/event-stream"))
                   (test-sse-resume-body "no-progress" 0)))
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
                         (cons "Mcp-Session-Id" "session-no-progress"))
                        (test-json-response-body
                         message (test-initialize-result))))
                      ((string= method "notifications/initialized")
                       (values 202 nil ""))
                      (t
                       (error
                        "Unexpected no-progress message ~S."
                        message))))))))
      (unwind-protect
           (progn
             (setf *mcp-http-idle-no-progress-resumption-limit* 3)
             (with-test-http-server (server #'handler)
               (let ((transport
                       (make-mcp-streamable-http-transport
                        (test-http-server-url server))))
                 (setf client (make-mcp-client transport))
                 (unwind-protect
                      (progn
                        (mcp-client-connect client)
                        (test-assert
                         (test-wait-until
                          (lambda ()
                            (mcp-http-transport-listener-failure
                             transport))
                          1.0)
                         "The no-progress resumption bound fired.")
                        (test-equal 3 idle-get-count)
                        (test-assert
                         (search
                          "no-progress limit"
                          (mcp-error-message
                           (mcp-http-transport-listener-failure
                            transport)))))
                   (mcp-client-close client)
                   (setf client nil)))))
        (when client
          (mcp-client-close client))
        (setf
         *mcp-http-idle-no-progress-resumption-limit*
         old-limit)))))

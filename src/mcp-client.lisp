(in-package #:mcparen)

;;;; -- Protocol Values --

(defparameter *mcp-protocol-version* "2025-11-25"
  "The single MCP protocol version supported by this client.")

(defparameter *mcp-default-startup-timeout* 15
  "The default initialization and discovery timeout in seconds.")

(defparameter *mcp-default-tool-timeout* 60
  "The default tool-call timeout in seconds.")

(defparameter *mcp-pagination-limit* 1000
  "The defensive maximum number of pages read from one MCP list method.")

(defparameter *mcp-pagination-restart-limit* 8
  "The maximum session changes tolerated while reading one paginated list.")

(defclass mcp-tool ()
  ((name
    :initarg :name
    :reader mcp-tool-name
    :type string
    :documentation "The server-local MCP tool name.")
   (title
    :initarg :title
    :initform nil
    :reader mcp-tool-title
    :type t
    :documentation "The optional human-readable display title.")
   (description
    :initarg :description
    :initform ""
    :reader mcp-tool-description
    :type string
    :documentation "The model-visible MCP tool description.")
   (input-schema
    :initarg :input-schema
    :reader mcp-tool-input-schema
    :type hash-table
    :documentation "The JSON Schema accepted by the MCP tool.")
   (output-schema
    :initarg :output-schema
    :initform nil
    :reader mcp-tool-output-schema
    :type t
    :documentation "The optional JSON Schema produced by the MCP tool.")
   (annotations
    :initarg :annotations
    :initform nil
    :reader mcp-tool-annotations
    :type t
    :documentation "Optional behavior hints supplied by the MCP server.")
   (raw
    :initarg :raw
    :reader mcp-tool-raw
    :type hash-table
    :documentation "The complete portable MCP tool object."))
  (:documentation "Validated metadata for one server-provided MCP tool."))

(-> mcp-tool-read-only-p (mcp-tool) boolean)
(defun mcp-tool-read-only-p (tool)
  "Return true only when TOOL explicitly declares itself read-only."
  (let ((annotations (mcp-tool-annotations tool)))
    (and (hash-table-p annotations)
         (json-true-p (json-get annotations "readOnlyHint")))))

(-> mcp-tool-destructive-p (mcp-tool) boolean)
(defun mcp-tool-destructive-p (tool)
  "Return true unless TOOL explicitly declares itself non-destructive."
  (let ((annotations (mcp-tool-annotations tool)))
    (if (hash-table-p annotations)
        (multiple-value-bind (value present-p)
            (gethash "destructiveHint" annotations)
          (if present-p
              (json-true-p value)
              t))
        t)))

(defclass mcp-call-result ()
  ((content
    :initarg :content
    :initform nil
    :reader mcp-call-result-content
    :type list
    :documentation "The ordered MCP content blocks.")
   (structured-content
    :initarg :structured-content
    :initform nil
    :reader mcp-call-result-structured-content
    :type t
    :documentation "The optional structured tool result.")
   (error-p
    :initarg :error-p
    :initform nil
    :reader mcp-call-result-error-p
    :type boolean
    :documentation "True when the server reports a tool execution error.")
   (raw
    :initarg :raw
    :reader mcp-call-result-raw
    :type hash-table
    :documentation "The complete portable CallToolResult object."))
  (:documentation "The ordered and structured result of one MCP tool call."))

(-> mcp-content-block-type (hash-table) t)
(defun mcp-content-block-type (block)
  "Return the type string of MCP content BLOCK."
  (json-get block "type"))

(-> mcp-call-result-text (mcp-call-result) string)
(defun mcp-call-result-text (result)
  "Render RESULT into a faithful text projection without discarding blocks."
  (let ((sections nil))
    (dolist (block (mcp-call-result-content result))
      (let ((type (mcp-content-block-type block)))
        (push
         (cond
           ((and (stringp type)
                 (string= type "text")
                 (stringp (json-get block "text")))
            (json-get block "text"))
           ((and (stringp type) (string= type "resource_link"))
            (format nil "Resource: ~A~@[ (~A)~]"
                    (or (json-get block "uri") "unknown")
                    (json-get block "name")))
           ((and (stringp type) (string= type "image"))
            (format nil "Image content (~A)"
                    (or (json-get block "mimeType")
                        "unknown media type")))
           ((and (stringp type) (string= type "audio"))
            (format nil "Audio content (~A)"
                    (or (json-get block "mimeType")
                        "unknown media type")))
           (t
            (json-encode block)))
         sections)))
    (when (mcp-call-result-structured-content result)
      (push
       (format nil "Structured content:~%~A"
               (json-encode
                (mcp-call-result-structured-content result)))
       sections))
    (format nil "~{~A~^~2%~}" (nreverse sections))))


;;;; -- Client Lifecycle --

(defclass mcp-client ()
  ((transport
    :initarg :transport
    :reader mcp-client-transport
    :type mcp-transport
    :documentation "The connection transport.")
   (client-info
    :initarg :client-info
    :reader mcp-client-client-info
    :type hash-table
    :documentation "The implementation information sent during initialization.")
   (client-capabilities
    :initarg :client-capabilities
    :reader mcp-client-client-capabilities
    :type hash-table
    :documentation "The optional client capabilities offered to the server.")
   (startup-timeout
    :initarg :startup-timeout
    :reader mcp-client-startup-timeout
    :type real
    :documentation "The initialization and discovery timeout in seconds.")
   (tool-timeout
    :initarg :tool-timeout
    :reader mcp-client-tool-timeout
    :type real
    :documentation "The default tool-call timeout in seconds.")
   (lock
    :initform (make-lock "mcparen client")
    :reader mcp-client-lock
    :type t
    :documentation "The lock serializing lifecycle and protocol requests.")
   (next-request-identifier
    :initform 1
    :accessor mcp-client-next-request-identifier
    :type (integer 1)
    :documentation "The next positive JSON-RPC request identifier.")
   (connection-generation
    :initform 0
    :accessor mcp-client-connection-generation
    :type (integer 0)
    :documentation "The generation distinguishing reinitialized sessions.")
   (connected-p
    :initform nil
    :accessor mcp-client-connected-p
    :type boolean
    :documentation "True after initialization and its ready notification.")
   (protocol-version
    :initform nil
    :accessor mcp-client-protocol-version
    :type t
    :documentation "The negotiated MCP protocol version.")
   (server-capabilities
    :initform nil
    :accessor mcp-client-server-capabilities
    :type t
    :documentation "The capabilities returned by the MCP server.")
   (server-info
    :initform nil
    :accessor mcp-client-server-info
    :type t
    :documentation "The implementation information returned by the server.")
   (instructions
    :initform nil
    :accessor mcp-client-instructions
    :type t
    :documentation "Optional server instructions for the client."))
  (:documentation "One restartable, thread-safe MCP client session."))

(-> make-mcp-client
    (t
     &key (:name string)
          (:version string)
          (:title t)
          (:capabilities hash-table)
          (:startup-timeout real)
          (:tool-timeout real))
    mcp-client)
(defun make-mcp-client
    (transport
     &key (name "mcparen")
       (version "0.1.0")
       title
       (capabilities (json-object))
       (startup-timeout *mcp-default-startup-timeout*)
       (tool-timeout *mcp-default-tool-timeout*))
  "Create a lazy MCP client over TRANSPORT."
  (unless (typep transport 'mcp-transport)
    (error 'mcp-protocol-error
           :message "An MCP client requires an MCP transport."
           :method nil
           :payload nil))
  (unless (and (plusp (length name)) (plusp (length version)))
    (error 'mcp-protocol-error
           :message "MCP client name and version must be non-empty."
           :method nil
           :payload nil))
  (unless (and (plusp startup-timeout) (plusp tool-timeout))
    (error 'mcp-protocol-error
           :message "MCP startup and tool timeouts must be positive."
           :method nil
           :payload nil))
  (let ((client-info
          (json-object "name" name "version" version)))
    (when title
      (setf (gethash "title" client-info) title))
    (make-instance 'mcp-client
                   :transport transport
                   :client-info client-info
                   :client-capabilities capabilities
                   :startup-timeout startup-timeout
                   :tool-timeout tool-timeout)))

(-> mcp-client--next-identifier-unlocked (mcp-client) integer)
(defun mcp-client--next-identifier-unlocked (client)
  "Return and advance CLIENT's request identifier while its lock is held."
  (prog1 (mcp-client-next-request-identifier client)
    (incf (mcp-client-next-request-identifier client))))

(-> mcp-client--notification (string &optional t) hash-table)
(defun mcp-client--notification (method &optional params)
  "Return a JSON-RPC notification for METHOD and optional PARAMS."
  (let ((message (json-object "jsonrpc" "2.0" "method" method)))
    (when params
      (setf (gethash "params" message) params))
    message))

(-> mcp-client--validate-response
    (hash-table t string)
    hash-table)
(defun mcp-client--validate-response (response identifier method)
  "Return RESPONSE's result or signal its structured JSON-RPC error."
  (unless (and (eq (json-rpc-message-validate response) ':response)
               (equal (json-get response "id" :absent) identifier))
    (error 'mcp-protocol-error
           :message
           (format nil "The MCP response for ~A has invalid JSON-RPC metadata."
                   method)
           :method method
           :payload response))
  (multiple-value-bind (rpc-error error-present-p)
      (gethash "error" response)
    (when error-present-p
      (error 'mcp-rpc-error
             :message
             (format nil "~A failed: ~A"
                     method
                     (json-get rpc-error "message"))
             :method method
             :payload response
             :code (json-get rpc-error "code")
             :data (json-get rpc-error "data")))
    (json-get response "result")))

(-> mcp-client--send-cancellation
    (mcp-client t real)
    null)
(defun mcp-client--send-cancellation (client identifier timeout)
  "Best-effort cancel IDENTIFIER after its request deadline."
  (handler-case
      (mcp-transport-notify
       (mcp-client-transport client)
       (mcp-client--notification
        "notifications/cancelled"
        (json-object
         "requestId" identifier
         "reason" "Client request timeout"))
       (min timeout 2))
    (error ()
      nil))
  nil)

(-> mcp-client--request-with-identifier
    (mcp-client string t real t)
    t)
(defun mcp-client--request-with-identifier
    (client method params timeout identifier)
  "Send METHOD with PARAMS and preallocated IDENTIFIER."
  (let ((request (json-object
                   "jsonrpc" "2.0"
                   "id" identifier
                   "method" method)))
    (when params
      (setf (gethash "params" request) params))
    (handler-case
        (mcp-client--validate-response
         (mcp-transport-request
          (mcp-client-transport client)
          request timeout)
         identifier method)
      (mcp-timeout (condition)
        (when (mcp-client-connected-p client)
          (mcp-client--send-cancellation client identifier timeout))
        (error condition)))))

(-> mcp-client--initialize-unlocked (mcp-client) mcp-client)
(defun mcp-client--initialize-unlocked (client)
  "Open and initialize CLIENT while its protocol lock is held."
  (mcp-transport-open (mcp-client-transport client))
  (let* ((identifier (mcp-client--next-identifier-unlocked client))
         (requested-version *mcp-protocol-version*)
         (result
           (mcp-client--request-with-identifier
            client
            "initialize"
            (json-object
             "protocolVersion" requested-version
             "capabilities" (mcp-client-client-capabilities client)
             "clientInfo" (mcp-client-client-info client))
            (mcp-client-startup-timeout client)
            identifier)))
    (unless (hash-table-p result)
      (error 'mcp-protocol-error
             :message "The MCP initialize result is not an object."
             :method "initialize"
             :payload result))
    (let ((version (json-get result "protocolVersion")))
      (unless (and (stringp version)
                   (string= version *mcp-protocol-version*))
        (mcp-transport-close (mcp-client-transport client))
        (error 'mcp-protocol-error
               :message
               (format nil "The MCP server selected unsupported protocol ~S."
                       version)
               :method "initialize"
               :payload result))
      (let ((capabilities (json-get result "capabilities"))
            (server-info (json-get result "serverInfo")))
        (unless (and
                 (hash-table-p capabilities)
                 (hash-table-p server-info)
                 (let ((name (json-get server-info "name"))
                       (server-version (json-get server-info "version")))
                   (and (stringp name)
                        (plusp (length name))
                        (stringp server-version)
                        (plusp (length server-version)))))
          (mcp-transport-close (mcp-client-transport client))
          (error 'mcp-protocol-error
                 :message
                 "The MCP initialize result lacks valid capabilities or serverInfo."
                 :method "initialize"
                 :payload result))
      (mcp-transport-set-protocol-version
       (mcp-client-transport client) version)
      (setf (mcp-client-protocol-version client) version
            (mcp-client-server-capabilities client)
            capabilities
            (mcp-client-server-info client)
            server-info
            (mcp-client-instructions client)
            (json-get result "instructions"))))
    (mcp-transport-notify
     (mcp-client-transport client)
     (mcp-client--notification "notifications/initialized")
     (mcp-client-startup-timeout client))
    (setf (mcp-client-connected-p client) t)
    (incf (mcp-client-connection-generation client))
    client))

(-> mcp-client--connect-unlocked (mcp-client) mcp-client)
(defun mcp-client--connect-unlocked (client)
  "Initialize CLIENT and clean up every partial connection on failure."
  (handler-case
      (mcp-client--initialize-unlocked client)
    (error (cause)
      (handler-case
          (mcp-transport-close (mcp-client-transport client))
        (error ()
          nil))
      (setf (mcp-client-connected-p client) nil
            (mcp-client-protocol-version client) nil
            (mcp-client-server-capabilities client) nil
            (mcp-client-server-info client) nil
            (mcp-client-instructions client) nil)
      (error cause))))

(-> mcp-client-connect (mcp-client) mcp-client)
(defun mcp-client-connect (client)
  "Initialize CLIENT if necessary and return it."
  (with-lock-held ((mcp-client-lock client))
    (unless (and (mcp-client-connected-p client)
                 (mcp-transport-open-p
                  (mcp-client-transport client)))
      (setf (mcp-client-connected-p client) nil)
      (mcp-client--connect-unlocked client)))
  client)

(-> mcp-client-close (mcp-client) null)
(defun mcp-client-close (client)
  "Close CLIENT and clear negotiated session state."
  (with-lock-held ((mcp-client-lock client))
    (unwind-protect
         (mcp-transport-close (mcp-client-transport client))
      (setf (mcp-client-connected-p client) nil
            (mcp-client-protocol-version client) nil
            (mcp-client-server-capabilities client) nil
            (mcp-client-server-info client) nil
            (mcp-client-instructions client) nil)
      (incf (mcp-client-connection-generation client))))
  nil)

(-> mcp-client-detach (mcp-client) null)
(defun mcp-client-detach (client)
  "Forget inherited transport resources without affecting their owning process."
  (with-lock-held ((mcp-client-lock client))
    (mcp-transport-detach (mcp-client-transport client))
    (setf (mcp-client-connected-p client) nil
          (mcp-client-protocol-version client) nil
          (mcp-client-server-capabilities client) nil
          (mcp-client-server-info client) nil
          (mcp-client-instructions client) nil)
    (incf (mcp-client-connection-generation client)))
  nil)

(defmacro with-mcp-client ((variable client) &body body)
  "Connect CLIENT as VARIABLE for BODY, then close it during unwinding."
  `(let ((,variable (mcp-client-connect ,client)))
     (unwind-protect
          (progn ,@body)
       (mcp-client-close ,variable))))

(-> mcp-client--request
    (mcp-client string &optional t real)
    t)
(defun mcp-client--request
    (client method &optional params
     (timeout (mcp-client-startup-timeout client)))
  "Connect CLIENT and send one request, allowing concurrent operations."
  (mcp-client-connect client)
  (multiple-value-bind (identifier generation)
      (with-lock-held ((mcp-client-lock client))
        (values (mcp-client--next-identifier-unlocked client)
                (mcp-client-connection-generation client)))
    (handler-case
        (mcp-client--request-with-identifier
         client method params timeout identifier)
      (mcp-session-expired ()
        (with-lock-held ((mcp-client-lock client))
          (when (= generation (mcp-client-connection-generation client))
            (mcp-transport-close (mcp-client-transport client))
            (setf (mcp-client-connected-p client) nil)
            (mcp-client--connect-unlocked client)))
        (let ((retry-identifier
                (with-lock-held ((mcp-client-lock client))
                  (mcp-client--next-identifier-unlocked client))))
          (mcp-client--request-with-identifier
           client method params timeout retry-identifier))))))


;;;; -- Server Features --

(-> mcp-client--require-capability (mcp-client string string) hash-table)
(defun mcp-client--require-capability (client name operation)
  "Return advertised capability NAME or reject unsupported OPERATION."
  (mcp-client-connect client)
  (let* ((capabilities (mcp-client-server-capabilities client))
         (capability
           (and (hash-table-p capabilities)
                (json-get capabilities name))))
    (unless (hash-table-p capability)
      (error 'mcp-protocol-error
             :message
             (format nil
                     "The MCP server does not advertise ~A support for ~A."
                     name operation)
             :method operation
             :payload capabilities))
    capability))

(-> mcp-client-ping (mcp-client) boolean)
(defun mcp-client-ping (client)
  "Return true after SERVER acknowledges one ping."
  (mcp-client--request client "ping")
  t)

(-> mcp-client--list-all (mcp-client string string) list)
(defun mcp-client--list-all (client method result-key)
  "Read every cursor page of METHOD without carrying cursors across sessions."
  (let ((restart-marker (gensym "MCP-PAGINATION-RESTART-")))
    (labels ((generation ()
               "Return CLIENT's current connection generation."
               (with-lock-held ((mcp-client-lock client))
                 (mcp-client-connection-generation client)))

             (read-session (initial-generation)
               "Read one paginated list within INITIAL-GENERATION."
               (let ((items nil)
                     (cursor nil)
                     (seen-cursors (make-hash-table :test #'equal)))
                 (loop for page from 1 to *mcp-pagination-limit*
                       for params = (and cursor
                                         (json-object "cursor" cursor))
                       for result = (mcp-client--request client method params)
                       do
                          (unless (= initial-generation (generation))
                            (return restart-marker))
                          (unless (hash-table-p result)
                            (error 'mcp-protocol-error
                                   :message
                                   (format nil
                                           "~A returned a non-object result."
                                           method)
                                   :method method
                                   :payload result))
                          (setf items
                                (nconc
                                 items
                                 (json-sequence->list
                                  (json-get result result-key))))
                          (let ((next-cursor
                                  (json-get result "nextCursor")))
                            (unless next-cursor
                              (return items))
                            (unless (stringp next-cursor)
                              (error 'mcp-protocol-error
                                     :message
                                     (format nil
                                             "~A returned a non-string cursor."
                                             method)
                                     :method method
                                     :payload result))
                            (when (gethash next-cursor seen-cursors)
                              (error 'mcp-protocol-error
                                     :message
                                     (format nil
                                             "~A repeated pagination cursor ~S."
                                             method next-cursor)
                                     :method method
                                     :payload result))
                            (setf (gethash next-cursor seen-cursors) t
                                  cursor next-cursor))
                       finally
                          (error 'mcp-protocol-error
                                 :message
                                 (format nil
                                         "~A exceeded the ~D-page safety limit."
                                         method *mcp-pagination-limit*)
                                 :method method
                                 :payload nil)))))
      (loop repeat *mcp-pagination-restart-limit*
            do
               (mcp-client-connect client)
               (let ((result (read-session (generation))))
                 (unless (eq result restart-marker)
                   (return result)))
            finally
               (error 'mcp-protocol-error
                      :message
                      (format nil
                              "~A changed sessions ~D times during pagination."
                              method *mcp-pagination-restart-limit*)
                      :method method
                      :payload nil)))))

(-> mcp-client--raw->tool (t) mcp-tool)
(defun mcp-client--raw->tool (raw)
  "Validate RAW and return its MCP-TOOL projection."
  (unless (hash-table-p raw)
    (error 'mcp-protocol-error
           :message "tools/list returned a non-object tool."
           :method "tools/list"
           :payload raw))
  (let ((name (json-get raw "name"))
        (title (json-get raw "title" :absent))
        (description (json-get raw "description" :absent))
        (input-schema (json-get raw "inputSchema"))
        (output-schema (json-get raw "outputSchema" :absent))
        (annotations (json-get raw "annotations" :absent)))
    (unless (and (stringp name)
                 (plusp (length name))
                 (or (eq title :absent) (stringp title))
                 (or (eq description :absent) (stringp description))
                 (hash-table-p input-schema)
                 (equal (json-get input-schema "type") "object")
                 (or (eq output-schema :absent)
                     (and (hash-table-p output-schema)
                          (equal
                           (json-get output-schema "type")
                           "object"))))
      (error 'mcp-protocol-error
             :message "tools/list returned invalid tool metadata."
             :method "tools/list"
             :payload raw))
    (unless (eq annotations :absent)
      (unless (hash-table-p annotations)
        (error 'mcp-protocol-error
               :message "tools/list returned malformed tool annotations."
               :method "tools/list"
               :payload raw))
      (multiple-value-bind (annotation-title title-present-p)
          (gethash "title" annotations)
        (when (and title-present-p
                   (not (stringp annotation-title)))
          (error 'mcp-protocol-error
                 :message
                 "tools/list returned a non-string annotation title."
                 :method "tools/list"
                 :payload raw)))
      (dolist (key '("readOnlyHint"
                     "destructiveHint"
                     "idempotentHint"
                     "openWorldHint"))
        (multiple-value-bind (value present-p)
            (gethash key annotations)
          (when (and present-p
                     (not (json-boolean-p value)))
            (error 'mcp-protocol-error
                   :message
                   (format nil
                           "tools/list returned non-boolean annotation ~A."
                           key)
                   :method "tools/list"
                   :payload raw)))))
    (make-instance 'mcp-tool
                   :name name
                   :title (unless (eq title :absent) title)
                   :description
                   (if (eq description :absent) "" description)
                   :input-schema input-schema
                   :output-schema
                   (unless (eq output-schema :absent) output-schema)
                   :annotations
                   (unless (eq annotations :absent) annotations)
                   :raw raw)))

(-> mcp-client--optional-string-field-p (hash-table string) boolean)
(defun mcp-client--optional-string-field-p (object key)
  "Return true when optional string field KEY in OBJECT is valid."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (or (not present-p)
        (stringp value))))

(-> mcp-client--content-block-valid-p (hash-table) boolean)
(defun mcp-client--content-block-valid-p (block)
  "Return true when BLOCK has the required shape for its MCP content type."
  (let ((type (json-get block "type")))
    (and
     (stringp type)
     (cond
       ((string= type "text")
        (stringp (json-get block "text")))
       ((or (string= type "image")
            (string= type "audio"))
        (and (stringp (json-get block "data"))
             (stringp (json-get block "mimeType"))))
       ((string= type "resource_link")
        (and (stringp (json-get block "uri"))
             (stringp (json-get block "name"))
             (mcp-client--optional-string-field-p block "title")
             (mcp-client--optional-string-field-p block "description")
             (mcp-client--optional-string-field-p block "mimeType")))
       ((string= type "resource")
        (let ((resource (json-get block "resource")))
          (and
           (hash-table-p resource)
           (stringp (json-get resource "uri"))
           (mcp-client--optional-string-field-p resource "mimeType")
           (multiple-value-bind (text text-present-p)
               (gethash "text" resource)
             (multiple-value-bind (blob blob-present-p)
                 (gethash "blob" resource)
               (and
                (not (eq text-present-p blob-present-p))
                (if text-present-p
                    (stringp text)
                    (stringp blob))))))))
       (t
        nil)))))

(-> mcp-client-list-tools (mcp-client) list)
(defun mcp-client-list-tools (client)
  "Return every MCP tool, following opaque pagination cursors safely."
  (mcp-client--require-capability client "tools" "tools/list")
  (mapcar #'mcp-client--raw->tool
          (mcp-client--list-all client "tools/list" "tools")))

(-> mcp-client-call-tool
    (mcp-client string t &key (:timeout real))
    mcp-call-result)
(defun mcp-client-call-tool
    (client name arguments &key (timeout (mcp-client-tool-timeout client)))
  "Call server tool NAME with JSON object ARGUMENTS."
  (mcp-client--require-capability client "tools" "tools/call")
  (unless (and (stringp name) (plusp (length name)))
    (error 'mcp-protocol-error
           :message "An MCP tool call requires a non-empty name."
           :method "tools/call"
           :payload nil))
  (unless (hash-table-p arguments)
    (error 'mcp-protocol-error
           :message "MCP tool arguments must be a JSON object."
           :method "tools/call"
           :payload arguments))
  (let ((result
          (mcp-client--request
           client
           "tools/call"
           (json-object "name" name "arguments" arguments)
           timeout)))
    (unless (hash-table-p result)
      (error 'mcp-protocol-error
             :message "tools/call returned a non-object result."
             :method "tools/call"
             :payload result))
    (let ((content (json-sequence->list (json-get result "content"))))
      (unless (and (every #'hash-table-p content)
                   (every #'mcp-client--content-block-valid-p content))
        (error 'mcp-protocol-error
               :message "tools/call returned an invalid content block."
               :method "tools/call"
               :payload result))
      (multiple-value-bind (error-value error-present-p)
          (gethash "isError" result)
        (when (and error-present-p
                   (not (json-boolean-p error-value)))
          (error 'mcp-protocol-error
                 :message "tools/call returned a non-boolean isError value."
                 :method "tools/call"
                 :payload result))
        (multiple-value-bind (structured structured-present-p)
            (gethash "structuredContent" result)
          (when (and structured-present-p
                     (not (hash-table-p structured)))
            (error 'mcp-protocol-error
                   :message
                   "tools/call returned non-object structuredContent."
                   :method "tools/call"
                   :payload result)))
        (make-instance 'mcp-call-result
                       :content content
                       :structured-content
                       (json-get result "structuredContent")
                       :error-p (and error-present-p
                                     (json-true-p error-value))
                       :raw result)))))

(-> mcp-client-list-resources (mcp-client) list)
(defun mcp-client-list-resources (client)
  "Return every server resource, following opaque pagination cursors."
  (mcp-client--require-capability client "resources" "resources/list")
  (mcp-client--list-all client "resources/list" "resources"))

(-> mcp-client-list-resource-templates (mcp-client) list)
(defun mcp-client-list-resource-templates (client)
  "Return every resource template, following opaque pagination cursors."
  (mcp-client--require-capability
   client "resources" "resources/templates/list")
  (mcp-client--list-all
   client "resources/templates/list" "resourceTemplates"))

(-> mcp-client-read-resource (mcp-client string) hash-table)
(defun mcp-client-read-resource (client uri)
  "Read resource URI and return its complete result object."
  (mcp-client--require-capability client "resources" "resources/read")
  (let ((result
          (mcp-client--request
           client "resources/read" (json-object "uri" uri))))
    (unless (hash-table-p result)
      (error 'mcp-protocol-error
             :message "resources/read returned a non-object result."
             :method "resources/read"
             :payload result))
    result))

(-> mcp-client-list-prompts (mcp-client) list)
(defun mcp-client-list-prompts (client)
  "Return every server prompt, following opaque pagination cursors."
  (mcp-client--require-capability client "prompts" "prompts/list")
  (mcp-client--list-all client "prompts/list" "prompts"))

(-> mcp-client-get-prompt
    (mcp-client string &optional (or null hash-table))
    hash-table)
(defun mcp-client-get-prompt (client name &optional arguments)
  "Resolve prompt NAME with optional string-valued ARGUMENTS."
  (mcp-client--require-capability client "prompts" "prompts/get")
  (let ((params (json-object "name" name)))
    (when arguments
      (setf (gethash "arguments" params) arguments))
    (let ((result (mcp-client--request client "prompts/get" params)))
      (unless (hash-table-p result)
        (error 'mcp-protocol-error
               :message "prompts/get returned a non-object result."
               :method "prompts/get"
               :payload result))
      result)))

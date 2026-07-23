(in-package #:mcparen)

;;;; -- Streamable HTTP Transport --

(defparameter *mcp-http-default-connect-timeout* 10
  "The default Streamable HTTP connection timeout in seconds.")

(defparameter *mcp-http-default-sse-retry-milliseconds* 1000
  "The default delay before resuming an interrupted MCP SSE stream.")

(defparameter *mcp-http-sse-resumption-limit* 1024
  "The maximum resumptions attempted for one Streamable HTTP request.")

(defparameter *mcp-http-sse-retry-maximum-digits* 8
  "The maximum decimal digits accepted in one SSE retry field.")

(defparameter *mcp-http-sse-retry-maximum-milliseconds* 60000
  "The maximum accepted SSE reconnection delay in milliseconds.")

(defparameter *mcp-http-idle-no-progress-resumption-limit* 32
  "The maximum consecutive idle GET resumptions without a valid event.")

(defparameter *mcp-http-idle-listener-join-timeout* 1.0
  "Seconds allowed for an idle GET listener to stop after its body closes.")

(defparameter *mcp-http-managed-header-names*
  '("Content-Type"
    "Accept"
    "Mcp-Session-Id"
    "MCP-Protocol-Version"
    "Last-Event-ID")
  "Headers owned by the Streamable HTTP transport.")

(-> mcp-http--identity-exchange-scope (function) t)
(defun mcp-http--identity-exchange-scope (function)
  "Call FUNCTION synchronously and preserve all values it returns."
  (funcall function))

(defclass mcp-streamable-http-transport (mcp-transport)
  ((url
    :initarg :url
    :reader mcp-http-transport-url
    :type string
    :documentation "The Streamable HTTP endpoint.")
   (headers-function
    :initarg :headers-function
    :initform (constantly nil)
    :reader mcp-http-transport-headers-function
    :type function
    :documentation
    "A function returning request headers. It is called for every request.")
   (exchange-scope-function
    :initarg :exchange-scope-function
    :initform #'mcp-http--identity-exchange-scope
    :reader mcp-http-transport-exchange-scope-function
    :type function
    :documentation
    "A function that synchronously calls a zero-argument thunk around each
complete HTTP exchange, preserving its values and unwinding on failures.")
   (connect-timeout
    :initarg :connect-timeout
    :initform *mcp-http-default-connect-timeout*
    :reader mcp-http-transport-connect-timeout
    :type real
    :documentation "The connection timeout in seconds.")
   (request-handler
    :initarg :request-handler
    :initform nil
    :reader mcp-http-transport-request-handler
    :type t
    :documentation "An optional function handling server-originated requests.")
   (notification-handler
    :initarg :notification-handler
    :initform nil
    :reader mcp-http-transport-notification-handler
    :type t
    :documentation "An optional function receiving server notifications.")
   (maximum-message-characters
    :initarg :maximum-message-characters
    :initform *mcp-maximum-message-characters*
    :reader mcp-http-transport-maximum-message-characters
    :type integer
    :documentation "The maximum characters accepted in one HTTP JSON document.")
   (state-lock
    :initform (make-lock "mcparen HTTP state")
    :reader mcp-http-transport-state-lock
    :type t
    :documentation "The lock protecting session and negotiated protocol state.")
   (session-identifier
    :initform nil
    :accessor mcp-http-transport-session-identifier
    :type t
    :documentation "The server-issued opaque MCP session identifier.")
   (pending-session-identifier
    :initform nil
    :accessor mcp-http-transport-pending-session-identifier
    :type t
    :documentation
    "A valid initialize response session awaiting client validation.")
   (protocol-version
    :initform nil
    :accessor mcp-http-transport-protocol-version
    :type t
    :documentation "The negotiated MCP protocol version.")
   (listener-condition-variable
    :initform (make-condition-variable)
    :reader mcp-http-transport-listener-condition-variable
    :type t
    :documentation "The condition interrupting listener reconnection waits.")
   (listener-thread
    :initform nil
    :accessor mcp-http-transport-listener-thread
    :type t
    :documentation "The owned idle Streamable HTTP GET listener thread.")
   (listener-body
    :initform nil
    :accessor mcp-http-transport-listener-body
    :type t
    :documentation "The response body currently read by the idle listener.")
   (listener-stopping-p
    :initform t
    :accessor mcp-http-transport-listener-stopping-p
    :type boolean
    :documentation "True when the idle listener must stop or remain stopped.")
   (listener-support-state
    :initform ':unknown
    :accessor mcp-http-transport-listener-support-state
    :type keyword
    :documentation
    "The idle GET support state for the current initialized session.")
   (listener-generation
    :initform 0
    :accessor mcp-http-transport-listener-generation
    :type (integer 0)
    :documentation "The token preventing an old listener from changing new state.")
   (listener-failure
    :initform nil
    :accessor mcp-http-transport-listener-failure
    :type t
    :documentation "The terminal protocol failure from the idle listener.")
   (open-p
    :initform nil
    :accessor mcp-http-transport-open-p
    :type boolean
    :documentation "True after the logical HTTP transport has opened."))
  (:documentation "A synchronous MCP Streamable HTTP client transport."))

(-> make-mcp-streamable-http-transport
    (string &key (:headers-function function)
                 (:exchange-scope-function function)
                 (:connect-timeout real)
                 (:request-handler t)
                 (:notification-handler t)
                 (:maximum-message-characters integer))
    mcp-streamable-http-transport)
(defun make-mcp-streamable-http-transport
    (url &key (headers-function (constantly nil))
              (exchange-scope-function
                #'mcp-http--identity-exchange-scope)
              (connect-timeout *mcp-http-default-connect-timeout*)
              request-handler notification-handler
              (maximum-message-characters *mcp-maximum-message-characters*))
  "Create a Streamable HTTP transport for URL.

HEADERS-FUNCTION should resolve credentials only when called so callers do not
need to retain secret header values in a long-lived client object.

EXCHANGE-SCOPE-FUNCTION receives a zero-argument thunk for each complete HTTP
exchange. It must call the thunk synchronously exactly once and preserve all
returned values. The default calls the thunk without adding a scope."
  (unless (and (stringp url) (plusp (length url)))
    (error 'mcp-transport-error
           :message "An MCP Streamable HTTP endpoint must be a non-empty URL."
           :transport nil
           :cause nil))
  (unless (and (realp connect-timeout) (plusp connect-timeout))
    (error 'mcp-transport-error
           :message "The MCP HTTP connection timeout must be positive."
           :transport nil
           :cause nil))
  (unless (functionp exchange-scope-function)
    (error 'mcp-transport-error
           :message "The MCP HTTP exchange scope must be a function."
           :transport nil
           :cause nil))
  (unless (and (integerp maximum-message-characters)
               (plusp maximum-message-characters))
    (error 'mcp-transport-error
           :message "The MCP HTTP message limit must be a positive integer."
           :transport nil
           :cause nil))
  (make-instance 'mcp-streamable-http-transport
                 :url url
                 :headers-function headers-function
                 :exchange-scope-function exchange-scope-function
                 :connect-timeout connect-timeout
                 :request-handler request-handler
                 :notification-handler notification-handler
                 :maximum-message-characters maximum-message-characters))

(-> mcp-http--call-with-exchange-scope
    (mcp-streamable-http-transport function)
    t)
(defun mcp-http--call-with-exchange-scope (transport function)
  "Call FUNCTION inside TRANSPORT's complete HTTP exchange scope."
  (funcall
   (mcp-http-transport-exchange-scope-function transport)
   function))

(defmethod mcp-transport-open-p
    ((transport mcp-streamable-http-transport))
  "Return the logical open state of the HTTP transport."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (mcp-http-transport-open-p transport)))

(defmethod mcp-transport-open
    ((transport mcp-streamable-http-transport))
  "Open the logical HTTP transport without making an eager request."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (setf (mcp-http-transport-open-p transport) t))
  transport)

(defmethod mcp-transport-set-protocol-version
    ((transport mcp-streamable-http-transport) version)
  "Add VERSION to subsequent Streamable HTTP requests."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (setf (mcp-http-transport-protocol-version transport) version))
  transport)

(defmethod mcp-transport-commit-initialize-session
    ((transport mcp-streamable-http-transport))
  "Promote the session staged by a validated InitializeResult."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (when (mcp-http-transport-pending-session-identifier transport)
      (when (mcp-http-transport-session-identifier transport)
        (error 'mcp-protocol-error
               :message
               "The MCP HTTP transport already has an active session."
               :method "initialize"
               :payload nil))
      (setf (mcp-http-transport-session-identifier transport)
            (mcp-http-transport-pending-session-identifier transport)
            (mcp-http-transport-pending-session-identifier transport)
            nil)))
  transport)

(-> mcp-http--header-name= (t string) boolean)
(defun mcp-http--header-name= (candidate expected)
  "Return true when CANDIDATE names HTTP header EXPECTED."
  (and (or (stringp candidate) (symbolp candidate))
       (string-equal (string candidate) expected)))

(-> mcp-http--header-value (t string) t)
(defun mcp-http--header-value (headers name)
  "Return case-insensitive header NAME from hash-table or alist HEADERS."
  (cond
    ((hash-table-p headers)
     (loop for key being the hash-keys of headers
             using (hash-value value)
           when (mcp-http--header-name= key name)
             do (return value)))
    ((listp headers)
     (loop for entry in headers
           when (and (consp entry)
                     (mcp-http--header-name= (first entry) name))
             do (return (rest entry))))
    (t
     nil)))

(-> mcp-http--valid-header-value-p (t) boolean)
(defun mcp-http--valid-header-value-p (value)
  "Return true when VALUE is a non-empty HTTP header value without line breaks."
  (and (stringp value)
       (plusp (length value))
       (not (find #\Return value))
       (not (find #\Newline value))))

(-> mcp-http--valid-session-identifier-p (t) boolean)
(defun mcp-http--valid-session-identifier-p (value)
  "Return true when VALUE contains only visible ASCII session characters."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (<= #x21 (char-code character) #x7e))
              value)))

(-> mcp-http--request-headers
    (mcp-streamable-http-transport
     &key (:method keyword) (:last-event-identifier t))
    list)
(defun mcp-http--request-headers
    (transport &key (method ':post) last-event-identifier)
  "Resolve and validate the headers for one request over TRANSPORT."
  (let ((headers
          (handler-case
              (copy-tree
               (funcall (mcp-http-transport-headers-function transport)))
            (error (cause)
              (error 'mcp-transport-error
                     :message "Could not resolve MCP HTTP request headers."
                     :transport transport
                     :cause cause)))))
    (unless (listp headers)
      (error 'mcp-transport-error
             :message "The MCP HTTP header provider must return an alist."
             :transport transport
             :cause nil))
    (dolist (entry headers)
      (unless (and (consp entry)
                   (or (stringp (first entry)) (symbolp (first entry)))
                   (mcp-http--valid-header-value-p (rest entry)))
        (error 'mcp-transport-error
               :message "The MCP HTTP header provider returned an invalid header."
               :transport transport
               :cause nil))
      (when (find (first entry)
                  *mcp-http-managed-header-names*
                  :test #'mcp-http--header-name=)
        (error 'mcp-transport-error
               :message
               (format nil
                       "The MCP HTTP header provider cannot override ~A."
                       (first entry))
               :transport transport
               :cause nil)))
    (when (and last-event-identifier
               (not
                (mcp-http--valid-header-value-p
                 last-event-identifier)))
      (error 'mcp-protocol-error
             :message "The MCP SSE event identifier is not a valid HTTP header value."
             :method nil
             :payload nil))
    (with-lock-held ((mcp-http-transport-state-lock transport))
      (append
       headers
       (ecase method
         (:post
          (list (cons "Content-Type" "application/json")
                (cons "Accept" "application/json, text/event-stream")))
         (:get
          (list (cons "Accept" "text/event-stream")))
         (:delete
          nil))
       (when last-event-identifier
         (list (cons "Last-Event-ID" last-event-identifier)))
       (when (mcp-http-transport-session-identifier transport)
         (list
          (cons "Mcp-Session-Id"
                (mcp-http-transport-session-identifier transport))))
       (when (mcp-http-transport-protocol-version transport)
         (list
          (cons "MCP-Protocol-Version"
                (mcp-http-transport-protocol-version transport))))))))

(-> mcp-http--close-body (t) null)
(defun mcp-http--close-body (body)
  "Close a response BODY stream without masking the protocol outcome."
  (when (and (streamp body) (open-stream-p body))
    (handler-case
        (close body)
      (error ()
        nil)))
  nil)

(-> mcp-http--deadline (real) integer)
(defun mcp-http--deadline (timeout)
  "Return the internal-real-time deadline TIMEOUT seconds from now."
  (+ (get-internal-real-time)
     (ceiling (* timeout internal-time-units-per-second))))

(-> mcp-http--remaining-seconds (integer) real)
(defun mcp-http--remaining-seconds (deadline)
  "Return non-negative seconds remaining before DEADLINE."
  (max 0
       (/ (- deadline (get-internal-real-time))
          internal-time-units-per-second)))

(-> mcp-http--timeout-condition-p (condition) boolean)
(defun mcp-http--timeout-condition-p (condition)
  "Return true when CONDITION represents a local network or runtime timeout."
  (labels ((named-type-p (package-name type-name)
             "Test CONDITION against TYPE-NAME when its package is present."
             (let* ((package (find-package package-name))
                    (symbol (and package
                                 (find-symbol type-name package))))
               (and symbol
                    (ignore-errors (typep condition symbol))))))
    (or (typep condition 'sb-ext:timeout)
        (named-type-p "SB-SYS" "IO-TIMEOUT")
        (named-type-p "USOCKET" "TIMEOUT-ERROR")
        (named-type-p "USOCKET" "DEADLINE-TIMEOUT-ERROR"))))

(-> mcp-http--signal-timeout
    (mcp-streamable-http-transport real
     &key (:operation string) (:cause t))
    null)
(defun mcp-http--signal-timeout
    (transport timeout &key operation cause)
  "Signal a typed timeout for OPERATION after configured TIMEOUT seconds."
  (error 'mcp-timeout
         :message
         (format nil "~A timed out after ~,2F seconds." operation timeout)
         :transport transport
         :cause cause
         :operation operation
         :seconds timeout))

(-> mcp-http--require-time-remaining
    (mcp-streamable-http-transport integer
     &key (:timeout real) (:operation string))
    real)
(defun mcp-http--require-time-remaining
    (transport deadline &key timeout operation)
  "Return time remaining before DEADLINE or signal OPERATION timeout."
  (let ((remaining (mcp-http--remaining-seconds deadline)))
    (unless (plusp remaining)
      (mcp-http--signal-timeout
       transport timeout
       :operation operation
       :cause nil))
    remaining))

(-> mcp-http--body->string
    (mcp-streamable-http-transport t string)
    string)
(defun mcp-http--body->string (transport body source)
  "Read response BODY within TRANSPORT's configured character limit."
  (let ((limit (mcp-http-transport-maximum-message-characters transport)))
    (cond
      ((null body)
       "")
      ((stringp body)
       (when (> (length body) limit)
         (mcp-message-too-large-error source limit))
       body)
      ((streamp body)
       (stream-read-bounded-string body limit source))
      (t
       (error 'mcp-protocol-error
              :message "The MCP HTTP response body is not readable."
              :method nil
              :payload nil)))))

(-> mcp-http--response-message
    (string &key (:limit integer))
    hash-table)
(defun mcp-http--response-message
    (body &key (limit *mcp-maximum-message-characters*))
  "Decode one JSON-RPC object from an HTTP response BODY."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) body)))
    (when (zerop (length trimmed))
      (error 'mcp-protocol-error
             :message "The MCP HTTP server returned an empty JSON response."
             :method nil
             :payload nil))
    (let ((message
            (json-decode
             trimmed
             :limit limit
             :source-name "MCP HTTP JSON response")))
      (json-rpc-message-validate message)
      message)))

(-> mcp-http--expire-session
    (mcp-streamable-http-transport t t)
    null)
(defun mcp-http--expire-session (transport request-session cause)
  "Clear REQUEST-SESSION when it is current and signal its expiry."
  (when request-session
    (with-lock-held ((mcp-http-transport-state-lock transport))
      (when (equal
             request-session
             (mcp-http-transport-session-identifier transport))
        (setf (mcp-http-transport-session-identifier transport) nil
              (mcp-http-transport-pending-session-identifier transport) nil
              (mcp-http-transport-protocol-version transport) nil
              (mcp-http-transport-open-p transport) nil)))
    (error 'mcp-session-expired
           :message "The MCP HTTP session expired."
           :transport transport
           :cause cause))
  nil)

(-> mcp-http--stage-initialize-session-header
    (mcp-streamable-http-transport t
     &key (:request-session t) (:initialize-request-p boolean))
    null)
(defun mcp-http--stage-initialize-session-header
    (transport response-session
     &key request-session initialize-request-p)
  "Validate and stage RESPONSE-SESSION only for an initialize response."
  (when initialize-request-p
    (with-lock-held ((mcp-http-transport-state-lock transport))
      (setf (mcp-http-transport-pending-session-identifier transport) nil)))
  (when (and initialize-request-p request-session)
    (error 'mcp-protocol-error
           :message
           "An MCP initialize request carried an existing session identifier."
           :method "initialize"
           :payload nil))
  (when response-session
    (unless (mcp-http--valid-session-identifier-p response-session)
      (error 'mcp-protocol-error
             :message "The MCP server returned an invalid session identifier."
             :method nil
             :payload nil))
    (unless initialize-request-p
      (error 'mcp-protocol-error
             :message
             "The MCP server returned a session identifier outside initialization."
             :method nil
             :payload nil))
    (with-lock-held ((mcp-http-transport-state-lock transport))
      (setf (mcp-http-transport-pending-session-identifier transport)
            response-session)))
  nil)

(-> mcp-http--exchange
    (mcp-streamable-http-transport keyword
     &key (:message t)
          (:deadline integer)
          (:timeout real)
          (:operation string)
          (:last-event-identifier t))
    (values t integer t))
(defun mcp-http--exchange
    (transport method
     &key message deadline timeout operation last-event-identifier)
  "Perform one bounded HTTP METHOD and return its owned body, status, and headers."
  (unless (mcp-transport-open-p transport)
    (error 'mcp-transport-error
           :message "The MCP HTTP transport is closed."
           :transport transport
           :cause nil))
  (let* ((remaining
           (mcp-http--require-time-remaining
            transport deadline
            :timeout timeout
            :operation operation))
         (request-headers
           (mcp-http--request-headers
            transport
            :method method
            :last-event-identifier last-event-identifier))
         (request-session
           (mcp-http--header-value request-headers "Mcp-Session-Id"))
         (initialize-request-p
           (and (eq method ':post)
                (hash-table-p message)
                (string= (json-get message "method" "")
                         "initialize"))))
    (handler-case
        (multiple-value-bind (body status headers)
            (apply
             #'dexador:request
             (mcp-http-transport-url transport)
             :method method
             :headers request-headers
             :want-stream t
             :connect-timeout
             (min remaining
                  (mcp-http-transport-connect-timeout transport))
             :read-timeout remaining
             :max-redirects 0
             :force-string t
             :keep-alive nil
             (when message
               (list
                :content
                (json-encode
                 message
                 :limit
                 (mcp-http-transport-maximum-message-characters
                  transport)))))
          (let ((body-released-p nil))
            (unwind-protect
                 (progn
                   (unless (<= 200 status 299)
                     (when (= status 404)
                       (mcp-http--expire-session
                        transport request-session nil))
                     (error 'mcp-transport-error
                            :message
                            (format nil
                                    "The MCP HTTP server returned status ~D."
                                    status)
                            :transport transport
                            :cause nil))
                   (mcp-http--stage-initialize-session-header
                    transport
                    (mcp-http--header-value headers "Mcp-Session-Id")
                    :request-session request-session
                    :initialize-request-p initialize-request-p)
                   (setf body-released-p t)
                   (values body status headers))
              (unless body-released-p
                (mcp-http--close-body body)))))
      (dexador.error:http-request-failed (cause)
        (let ((status (dexador.error:response-status cause)))
          (mcp-http--close-body (dexador.error:response-body cause))
          (when (eql status 404)
            (mcp-http--expire-session
             transport request-session cause))
          (error 'mcp-transport-error
                 :message
                 (format nil "The MCP HTTP server returned status ~A."
                         status)
                 :transport transport
                 :cause cause)))
      (mcp-error (condition)
        (error condition))
      (error (cause)
        (if (mcp-http--timeout-condition-p cause)
            (mcp-http--signal-timeout
             transport timeout
             :operation operation
             :cause cause)
            (error 'mcp-transport-error
                   :message
                   (format nil "The MCP HTTP request failed: ~A" cause)
                   :transport transport
                   :cause cause))))))

(-> mcp-http--server-response
    (t t t)
    hash-table)
(defun mcp-http--server-response (identifier result error-value)
  "Return a JSON-RPC response to a server-originated request."
  (let ((response
          (json-object "jsonrpc" "2.0" "id" identifier)))
    (if error-value
        (setf (gethash "error" response) error-value)
        (setf (gethash "result" response) result))
    response))

(-> mcp-http--send-server-response
    (mcp-streamable-http-transport hash-table
     &key (:deadline integer) (:timeout real))
    null)
(defun mcp-http--send-server-response
    (transport response &key deadline timeout)
  "POST one JSON-RPC RESPONSE generated for the server."
  (multiple-value-bind (body status headers)
      (mcp-http--exchange
       transport ':post
       :message response
       :deadline deadline
       :timeout timeout
       :operation "MCP HTTP server response")
    (declare (ignore headers))
    (unwind-protect
         (progn
           (unless (= status 202)
             (error 'mcp-protocol-error
                    :message
                    (format nil
                            "The MCP server acknowledged a client response with status ~D, not 202."
                            status)
                    :method nil
                    :payload nil))
           (let ((text
                   (mcp-http--body->string
                    transport body
                    "MCP HTTP server-response acknowledgement")))
             (unless (zerop
                      (length
                       (string-trim
                        '(#\Space #\Tab #\Newline #\Return)
                        text)))
               (error 'mcp-protocol-error
                      :message
                      "The MCP server-response acknowledgement contained a body."
                      :method nil
                      :payload nil))))
      (mcp-http--close-body body)))
  nil)

(-> mcp-http--dispatch-server-message
    (mcp-streamable-http-transport hash-table
     &key (:expected-identifier t)
          (:deadline (or null integer))
          (:timeout (or null real))
          (:reject-responses-p boolean))
    (values t boolean))
(defun mcp-http--dispatch-server-message
    (transport message
     &key expected-identifier deadline timeout reject-responses-p)
  "Dispatch MESSAGE and return its response plus a matching-response flag."
  (let ((kind (json-rpc-message-validate message))
        (identifier (json-get message "id" :absent))
        (method (json-get message "method")))
    (cond
      ((and (eq kind ':response) reject-responses-p)
       (error 'mcp-protocol-error
              :message
              "The MCP idle GET stream emitted a JSON-RPC response."
              :method nil
              :payload message))
      ((and (eq kind ':response)
            (equal identifier expected-identifier))
       (values message t))
      ((eq kind ':request)
       (let ((handler (mcp-http-transport-request-handler transport)))
         (let ((response
                 (handler-case
                     (cond
                       ((string= method "ping")
                        (mcp-http--server-response
                         identifier (json-object) nil))
                       (handler
                        (mcp-http--server-response
                         identifier
                         (funcall
                          handler method (json-get message "params"))
                         nil))
                       (t
                        (mcp-http--server-response
                         identifier
                         nil
                         (json-object
                          "code" -32601
                          "message"
                          (format nil
                                  "Client method ~S is not supported."
                                  method)))))
                   (mcp-rpc-error (cause)
                     (mcp-http--server-response
                      identifier
                      nil
                      (json-object
                       "code" (mcp-rpc-error-code cause)
                       "message" (mcp-error-message cause)
                       "data" (mcp-rpc-error-data cause))))
                   (error (cause)
                     (mcp-http--server-response
                      identifier
                      nil
                      (json-object
                       "code" -32603
                       "message"
                       "The MCP client request handler failed."
                       "data"
                       (bounded-diagnostic cause :limit 512)))))))
           (let* ((response-timeout
                    (or timeout
                        (mcp-http-transport-connect-timeout transport)))
                  (response-deadline
                    (or deadline
                        (mcp-http--deadline response-timeout))))
             (mcp-http--send-server-response
              transport response
              :deadline response-deadline
              :timeout response-timeout))))
       (values nil nil))
      ((eq kind ':notification)
       (let ((handler
               (mcp-http-transport-notification-handler transport)))
         (when handler
           (funcall handler method (json-get message "params"))))
       (values nil nil))
      ((eq kind ':response)
       ;; A late response for another request may share an SSE stream. Its
       ;; owning request will receive its response through its own stream.
       (values nil nil))
      (t
       (error 'mcp-protocol-error
              :message "The MCP SSE stream emitted an unclassified message."
              :method nil
              :payload message)))))

(-> mcp-http--content-type-p (t string) boolean)
(defun mcp-http--content-type-p (header expected)
  "Return true when HEADER's media type equals EXPECTED."
  (and
   (stringp header)
   (let* ((separator (position #\; header))
          (media-type
            (string-trim
             '(#\Space #\Tab)
             (subseq header 0 separator))))
     (string-equal media-type expected))))

(-> mcp-http--sse-field (string) (values string string))
(defun mcp-http--sse-field (line)
  "Return the field name and normalized value from one SSE LINE."
  (let ((separator (position #\: line)))
    (if separator
        (let ((value (subseq line (1+ separator))))
          (values
           (subseq line 0 separator)
           (if (and (plusp (length value))
                    (char= (char value 0) #\Space))
               (subseq value 1)
               value)))
        (values line ""))))

(-> mcp-http--valid-retry-value-p (string) boolean)
(defun mcp-http--valid-retry-value-p (value)
  "Return true when VALUE is a bounded decimal SSE retry interval."
  (and (plusp (length value))
       (<= (length value) *mcp-http-sse-retry-maximum-digits*)
       (every #'digit-char-p value)
       (<= (parse-integer value)
           *mcp-http-sse-retry-maximum-milliseconds*)))

(-> mcp-http--read-sse-fragment
    (mcp-streamable-http-transport stream t
     &key (:deadline (or null integer))
          (:timeout (or null real))
          (:last-event-identifier t)
          (:retry-milliseconds integer)
          (:reject-responses-p boolean))
    (values t boolean t integer boolean))
(defun mcp-http--read-sse-fragment
    (transport stream expected-identifier
     &key deadline timeout last-event-identifier retry-milliseconds
       reject-responses-p)
  "Read one SSE fragment and return response state needed for resumption."
  (let ((data-lines nil)
        (data-characters 0)
        (matching-response nil)
        (matching-p nil)
        (progress-p nil)
        (limit (mcp-http-transport-maximum-message-characters transport)))
    (labels ((clear-event ()
               "Clear the accumulated data for the next SSE event."
               (setf data-lines nil
                     data-characters 0))

             (finish-event ()
               "Decode and dispatch the accumulated SSE event data."
               (when data-lines
                 (let* ((document
                          (format nil "~{~A~^~%~}"
                                  (nreverse data-lines)))
                        (trimmed
                          (string-trim
                           '(#\Space #\Tab #\Newline #\Return)
                           document)))
                   (clear-event)
                   (when (plusp (length trimmed))
                     (setf progress-p t)
                     (multiple-value-setq
                         (matching-response matching-p)
                       (mcp-http--dispatch-server-message
                        transport
                        (json-decode
                         trimmed
                         :limit limit
                         :source-name "MCP SSE event data")
                        :expected-identifier expected-identifier
                        :deadline deadline
                        :timeout timeout
                        :reject-responses-p reject-responses-p)))))))
      (loop
        (when deadline
          (mcp-http--require-time-remaining
           transport deadline
           :timeout timeout
           :operation "MCP HTTP request"))
        (multiple-value-bind (raw-line end-of-file-p)
            (stream-read-bounded-line
             stream limit "MCP SSE line")
          (when raw-line
            (let ((line (string-right-trim '(#\Return) raw-line)))
              (cond
                ((zerop (length line))
                 (finish-event))
                ((char= (char line 0) #\:)
                 nil)
                (t
                 (multiple-value-bind (field value)
                     (mcp-http--sse-field line)
                   (cond
                     ((string= field "data")
                      (let ((additional
                              (+ (length value)
                                 (if data-lines 1 0))))
                        (when (> (+ data-characters additional) limit)
                          (mcp-message-too-large-error
                           "MCP SSE event data" limit))
                        (incf data-characters additional)
                        (push value data-lines)))
                     ((string= field "id")
                      (unless (find (code-char 0) value)
                        (setf last-event-identifier value)))
                     ((and (string= field "retry")
                           (mcp-http--valid-retry-value-p value))
                      (setf retry-milliseconds
                            (parse-integer value)))))))))
          (when matching-p
            (return))
          (when end-of-file-p
            (finish-event)
            (return)))))
    (values matching-response
            matching-p
            last-event-identifier
            retry-milliseconds
            progress-p)))

(-> mcp-http--sleep-before-resumption
    (mcp-streamable-http-transport integer
     &key (:deadline integer)
          (:timeout real)
          (:operation string))
    null)
(defun mcp-http--sleep-before-resumption
    (transport retry-milliseconds &key deadline timeout operation)
  "Respect RETRY-MILLISECONDS without extending OPERATION's DEADLINE."
  (let* ((remaining
           (mcp-http--require-time-remaining
            transport deadline
            :timeout timeout
            :operation operation))
         (delay (/ retry-milliseconds 1000)))
    (if (< remaining delay)
        (progn
          (sleep remaining)
          (mcp-http--signal-timeout
           transport timeout
           :operation operation
           :cause nil))
        (sleep delay)))
  nil)

(-> mcp-http--require-sse-body
    (t t string)
    stream)
(defun mcp-http--require-sse-body (body headers method)
  "Return BODY as an SSE stream after validating its content type."
  (unless (mcp-http--content-type-p
           (mcp-http--header-value headers "Content-Type")
           "text/event-stream")
    (error 'mcp-protocol-error
           :message
           (format nil
                   "The MCP HTTP response for ~A is not text/event-stream."
                   method)
           :method method
           :payload nil))
  (unless (streamp body)
    (error 'mcp-protocol-error
           :message "The MCP SSE response is not a stream."
           :method method
           :payload nil))
  body)

(-> mcp-http--read-sse-response
    (mcp-streamable-http-transport stream t
     &key (:deadline integer) (:timeout real))
    hash-table)
(defun mcp-http--read-sse-response
    (transport initial-stream identifier &key deadline timeout)
  "Read and resume SSE fragments until IDENTIFIER's response arrives."
  (let ((stream initial-stream)
        (last-event-identifier nil)
        (retry-milliseconds *mcp-http-default-sse-retry-milliseconds*))
    (loop for resumption-count from 0
          do
             (multiple-value-bind
                   (response matching-p next-event-identifier next-retry)
                 (unwind-protect
                      (mcp-http--read-sse-fragment
                       transport stream identifier
                       :deadline deadline
                       :timeout timeout
                       :last-event-identifier last-event-identifier
                       :retry-milliseconds retry-milliseconds)
                   (mcp-http--close-body stream))
               (setf last-event-identifier next-event-identifier
                     retry-milliseconds next-retry)
               (when matching-p
                 (return response)))
             (unless (and (stringp last-event-identifier)
                          (plusp (length last-event-identifier)))
               (error 'mcp-protocol-error
                      :message
                      (format nil
                              "The MCP SSE stream ended before response identifier ~S and supplied no resumable event identifier."
                              identifier)
                      :method nil
                      :payload nil))
             (when (>= resumption-count *mcp-http-sse-resumption-limit*)
               (error 'mcp-protocol-error
                      :message
                      (format nil
                              "The MCP SSE request exceeded its ~D-resumption safety limit."
                              *mcp-http-sse-resumption-limit*)
                      :method nil
                      :payload nil))
             (mcp-http--sleep-before-resumption
              transport retry-milliseconds
              :deadline deadline
              :timeout timeout
              :operation "MCP HTTP request")
             (multiple-value-bind (body status headers)
                 (mcp-http--exchange
                  transport ':get
                  :deadline deadline
                  :timeout timeout
                  :operation "MCP HTTP SSE resumption"
                  :last-event-identifier last-event-identifier)
               (declare (ignore status))
               (handler-case
                   (setf stream
                         (mcp-http--require-sse-body
                          body headers "SSE resumption"))
                 (error (cause)
                   (mcp-http--close-body body)
                   (error cause)))))))

(-> mcp-http--listener-active-p-unlocked
    (mcp-streamable-http-transport integer)
    boolean)
(defun mcp-http--listener-active-p-unlocked (transport generation)
  "Return true when GENERATION is TRANSPORT's active idle listener."
  (and (= generation
          (mcp-http-transport-listener-generation transport))
       (not (mcp-http-transport-listener-stopping-p transport))
       (mcp-http-transport-open-p transport)))

(-> mcp-http--listener-stop-requested-p
    (mcp-streamable-http-transport integer)
    boolean)
(defun mcp-http--listener-stop-requested-p (transport generation)
  "Return true when listener GENERATION must stop."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (not
     (mcp-http--listener-active-p-unlocked transport generation))))

(-> mcp-http--listener-publish-body
    (mcp-streamable-http-transport integer stream)
    boolean)
(defun mcp-http--listener-publish-body (transport generation body)
  "Publish BODY when GENERATION still owns TRANSPORT's listener."
  (let ((published-p
          (with-lock-held ((mcp-http-transport-state-lock transport))
            (when
                (mcp-http--listener-active-p-unlocked
                 transport generation)
              (setf (mcp-http-transport-listener-body transport) body
                    (mcp-http-transport-listener-support-state transport)
                    ':supported)
              t))))
    (unless published-p
      (mcp-http--close-body body))
    (if published-p t nil)))

(-> mcp-http--listener-clear-body
    (mcp-streamable-http-transport integer t)
    null)
(defun mcp-http--listener-clear-body (transport generation body)
  "Forget BODY when it still belongs to listener GENERATION."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (when (and
           (= generation
              (mcp-http-transport-listener-generation transport))
           (eq body (mcp-http-transport-listener-body transport)))
      (setf (mcp-http-transport-listener-body transport) nil)))
  nil)

(-> mcp-http--listener-mark-expired
    (mcp-streamable-http-transport integer)
    null)
(defun mcp-http--listener-mark-expired (transport generation)
  "Expire the session owned by listener GENERATION."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (when (= generation
             (mcp-http-transport-listener-generation transport))
      (setf (mcp-http-transport-open-p transport) nil
            (mcp-http-transport-session-identifier transport) nil
            (mcp-http-transport-pending-session-identifier transport) nil
            (mcp-http-transport-protocol-version transport) nil
            (mcp-http-transport-listener-stopping-p transport) t
            (mcp-http-transport-listener-support-state transport)
            ':unknown)
      (condition-notify
       (mcp-http-transport-listener-condition-variable transport))))
  nil)

(-> mcp-http--listener-mark-unsupported
    (mcp-streamable-http-transport integer)
    null)
(defun mcp-http--listener-mark-unsupported (transport generation)
  "Remember that idle GET is unsupported for listener GENERATION."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (when (= generation
             (mcp-http-transport-listener-generation transport))
      (setf (mcp-http-transport-listener-stopping-p transport) t
            (mcp-http-transport-listener-support-state transport)
            ':unsupported)
      (condition-notify
       (mcp-http-transport-listener-condition-variable transport))))
  nil)

(-> mcp-http--listener-record-failure
    (mcp-streamable-http-transport integer t)
    null)
(defun mcp-http--listener-record-failure
    (transport generation condition)
  "Record terminal listener CONDITION for GENERATION."
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (when (= generation
             (mcp-http-transport-listener-generation transport))
      (setf (mcp-http-transport-listener-stopping-p transport) t
            (mcp-http-transport-listener-support-state transport) ':failed
            (mcp-http-transport-listener-failure transport) condition)
      (condition-notify
       (mcp-http-transport-listener-condition-variable transport))))
  nil)

(-> mcp-http--listener-wait-before-resumption
    (mcp-streamable-http-transport integer integer)
    boolean)
(defun mcp-http--listener-wait-before-resumption
    (transport generation retry-milliseconds)
  "Wait interruptibly before resuming idle listener GENERATION."
  (let ((deadline
          (mcp-http--deadline (/ retry-milliseconds 1000))))
    (with-lock-held ((mcp-http-transport-state-lock transport))
      (loop
        for remaining = (mcp-http--remaining-seconds deadline)
        unless
            (mcp-http--listener-active-p-unlocked transport generation)
          do (return nil)
        unless (plusp remaining)
          do (return t)
        do
           (condition-wait
            (mcp-http-transport-listener-condition-variable transport)
            (mcp-http-transport-state-lock transport)
            :timeout remaining)))))

(-> mcp-http--idle-get
    (mcp-streamable-http-transport t)
    (values t t keyword))
(defun mcp-http--idle-get (transport last-event-identifier)
  "Open one unbounded idle GET stream or return a lifecycle outcome."
  (let* ((headers
           (mcp-http--request-headers
            transport
            :method ':get
            :last-event-identifier last-event-identifier))
         (request-session
           (mcp-http--header-value headers "Mcp-Session-Id"))
         (connect-timeout
           (mcp-http-transport-connect-timeout transport)))
    (handler-case
        (multiple-value-bind (body status response-headers)
            (sb-ext:with-timeout connect-timeout
              (dexador:request
               (mcp-http-transport-url transport)
               :method :get
               :headers headers
               :want-stream t
               :connect-timeout connect-timeout
               :read-timeout nil
               :max-redirects 0
               :force-string t
               :keep-alive nil))
          (let ((released-p nil))
            (unwind-protect
                 (progn
                   (cond
                     ((= status 404)
                      (return-from mcp-http--idle-get
                        (values nil nil ':expired)))
                     ((= status 405)
                      (return-from mcp-http--idle-get
                        (values nil nil ':unsupported)))
                     ((/= status 200)
                      (return-from mcp-http--idle-get
                        (values nil nil ':retry))))
                   (mcp-http--stage-initialize-session-header
                    transport
                    (mcp-http--header-value
                     response-headers "Mcp-Session-Id")
                    :request-session request-session
                    :initialize-request-p nil)
                   (setf released-p t)
                   (values body response-headers ':stream))
              (unless released-p
                (mcp-http--close-body body)))))
      (dexador.error:http-request-failed (cause)
        (let ((status (dexador.error:response-status cause)))
          (mcp-http--close-body (dexador.error:response-body cause))
          (values
           nil nil
           (cond
             ((eql status 404) ':expired)
             ((eql status 405) ':unsupported)
             (t ':retry)))))
      (sb-ext:timeout ()
        (values nil nil ':retry))
      (mcp-error (condition)
        (error condition))
      (error ()
        (values nil nil ':retry)))))

(-> mcp-http--listener-no-progress-condition () mcp-protocol-error)
(defun mcp-http--listener-no-progress-condition ()
  "Return the failure used when idle GET reconnects without useful events."
  (make-condition
   'mcp-protocol-error
   :message
   (format nil
           "The MCP idle GET listener exceeded its ~D-resumption no-progress limit."
           *mcp-http-idle-no-progress-resumption-limit*)
   :method nil
   :payload nil))

(-> mcp-http--idle-listener-loop
    (mcp-streamable-http-transport integer)
    null)
(defun mcp-http--idle-listener-loop (transport generation)
  "Receive unsolicited server messages for one initialized HTTP session."
  (unwind-protect
       (handler-case
           (let ((last-event-identifier nil)
                 (retry-milliseconds
                   *mcp-http-default-sse-retry-milliseconds*)
                 (no-progress-count 0))
             (loop
               (when
                   (mcp-http--listener-stop-requested-p
                    transport generation)
                 (return))
               (unless
                   (mcp-http--call-with-exchange-scope
                    transport
                    (lambda ()
                      (handler-case
                          (block exchange
                            (multiple-value-bind (body headers outcome)
                                (mcp-http--idle-get
                                 transport last-event-identifier)
                              (case outcome
                                (:expired
                                 (mcp-http--listener-mark-expired
                                  transport generation)
                                 (return-from exchange nil))
                                (:unsupported
                                 (mcp-http--listener-mark-unsupported
                                  transport generation)
                                 (return-from exchange nil))
                                (:retry
                                 (incf no-progress-count))
                                (:stream
                                 (let ((progress-p nil))
                                   (unwind-protect
                                        (handler-case
                                            (progn
                                              (mcp-http--require-sse-body
                                               body headers "idle GET")
                                              (unless
                                                  (mcp-http--listener-publish-body
                                                   transport generation body)
                                                (return-from exchange nil))
                                              (multiple-value-bind
                                                    (ignored-response
                                                     ignored-matching-p
                                                     next-event-identifier
                                                     next-retry
                                                     fragment-progress-p)
                                                  (mcp-http--read-sse-fragment
                                                   transport body nil
                                                   :last-event-identifier
                                                   last-event-identifier
                                                   :retry-milliseconds
                                                   retry-milliseconds
                                                   :reject-responses-p t)
                                                (declare
                                                 (ignore
                                                  ignored-response
                                                  ignored-matching-p))
                                                (setf
                                                 last-event-identifier
                                                 next-event-identifier
                                                 retry-milliseconds next-retry
                                                 progress-p
                                                 fragment-progress-p)))
                                          (mcp-error (condition)
                                            (mcp-http--listener-record-failure
                                             transport generation condition)
                                            (return-from exchange nil))
                                          (error ()
                                            (when
                                                (mcp-http--listener-stop-requested-p
                                                 transport generation)
                                              (return-from exchange nil))))
                                     (mcp-http--close-body body)
                                     (mcp-http--listener-clear-body
                                      transport generation body))
                                   (if progress-p
                                       (setf no-progress-count 0)
                                       (incf no-progress-count))))))
                            (when
                                (>= no-progress-count
                                    *mcp-http-idle-no-progress-resumption-limit*)
                              (mcp-http--listener-record-failure
                               transport generation
                               (mcp-http--listener-no-progress-condition))
                              (return-from exchange nil))
                            t)
                        (error (condition)
                          (mcp-http--listener-record-failure
                           transport generation condition)
                          nil))))
                 (return))
               (unless
                   (mcp-http--listener-wait-before-resumption
                    transport generation retry-milliseconds)
                 (return))))
         (error (condition)
           (mcp-http--listener-record-failure
            transport generation condition)))
    (with-lock-held ((mcp-http-transport-state-lock transport))
      (when (and
             (= generation
                (mcp-http-transport-listener-generation transport))
             (eq (current-thread)
                 (mcp-http-transport-listener-thread transport)))
        (setf (mcp-http-transport-listener-thread transport) nil
              (mcp-http-transport-listener-body transport) nil))
      (condition-notify
       (mcp-http-transport-listener-condition-variable transport))))
  nil)

(-> mcp-http--join-listener-thread (t real) boolean)
(defun mcp-http--join-listener-thread (thread timeout)
  "Join THREAD within TIMEOUT, destroying it only after the bounded wait."
  (cond
    ((or (null thread) (eq thread (current-thread)))
     t)
    (t
     (let ((deadline (mcp-http--deadline timeout)))
       (loop while (thread-alive-p thread)
             while
               (plusp (mcp-http--remaining-seconds deadline))
             do (sleep 0.01))
       (when (thread-alive-p thread)
         (handler-case
             (destroy-thread thread)
           (error ()
             nil)))
       (unless (thread-alive-p thread)
         (handler-case
             (join-thread thread)
           (error ()
             nil)))
       (not (thread-alive-p thread))))))

(-> mcp-http--stop-idle-listener
    (mcp-streamable-http-transport)
    null)
(defun mcp-http--stop-idle-listener (transport)
  "Stop TRANSPORT's listener, closing its body before bounded joining."
  (multiple-value-bind (thread body)
      (with-lock-held ((mcp-http-transport-state-lock transport))
        (incf (mcp-http-transport-listener-generation transport))
        (setf (mcp-http-transport-listener-stopping-p transport) t)
        (condition-notify
         (mcp-http-transport-listener-condition-variable transport))
        (values
         (mcp-http-transport-listener-thread transport)
         (mcp-http-transport-listener-body transport)))
    (mcp-http--close-body body)
    (mcp-http--join-listener-thread
     thread *mcp-http-idle-listener-join-timeout*)
    (with-lock-held ((mcp-http-transport-state-lock transport))
      (when (eq thread
                (mcp-http-transport-listener-thread transport))
        (setf (mcp-http-transport-listener-thread transport) nil))
      (when (eq body
                (mcp-http-transport-listener-body transport))
        (setf (mcp-http-transport-listener-body transport) nil))))
  nil)

(-> mcp-http--start-idle-listener
    (mcp-streamable-http-transport)
    mcp-streamable-http-transport)
(defun mcp-http--start-idle-listener (transport)
  "Start exactly one idle GET listener for TRANSPORT's ready session."
  (mcp-http--stop-idle-listener transport)
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (when (mcp-http-transport-open-p transport)
      (let ((generation
              (incf
               (mcp-http-transport-listener-generation transport))))
        (setf (mcp-http-transport-listener-stopping-p transport) nil
              (mcp-http-transport-listener-support-state transport)
              ':unknown
              (mcp-http-transport-listener-failure transport) nil
              (mcp-http-transport-listener-body transport) nil
              (mcp-http-transport-listener-thread transport)
              (make-thread
               (lambda ()
                 (mcp-http--idle-listener-loop
                  transport generation))
               :name "mcparen HTTP idle listener")))))
  transport)

(defmethod mcp-transport-session-ready
    ((transport mcp-streamable-http-transport))
  "Start the owned idle GET listener for the initialized HTTP session."
  (mcp-http--start-idle-listener transport))

(-> mcp-http--call-with-timeout
    (mcp-streamable-http-transport real function
     &key (:operation string))
    t)
(defun mcp-http--call-with-timeout
    (transport timeout function &key operation)
  "Call FUNCTION under one absolute TIMEOUT for HTTP OPERATION."
  (handler-case
      (sb-ext:with-timeout timeout
        (funcall function))
    (mcp-error (condition)
      (error condition))
    (sb-ext:timeout (cause)
      (mcp-http--signal-timeout
       transport timeout
       :operation operation
       :cause cause))))

(defmethod mcp-transport-request
    ((transport mcp-streamable-http-transport) request timeout)
  "Send REQUEST and read JSON or resumable SSE under one deadline."
  (let ((deadline (mcp-http--deadline timeout))
        (method (json-get request "method"))
        (identifier (json-get request "id")))
    (mcp-http--call-with-exchange-scope
     transport
     (lambda ()
       (mcp-http--call-with-timeout
        transport timeout
        (lambda ()
          (multiple-value-bind (body status headers)
              (mcp-http--exchange
               transport ':post
               :message request
               :deadline deadline
               :timeout timeout
               :operation "MCP HTTP request")
            (declare (ignore status))
            (unwind-protect
                 (let ((content-type
                         (mcp-http--header-value headers "Content-Type")))
                   (cond
                     ((mcp-http--content-type-p
                       content-type "text/event-stream")
                      (mcp-http--require-sse-body body headers method)
                      (mcp-http--read-sse-response
                       transport body identifier
                       :deadline deadline
                       :timeout timeout))
                     ((mcp-http--content-type-p
                       content-type "application/json")
                      (mcp-http--response-message
                       (mcp-http--body->string
                        transport body "MCP HTTP JSON response")
                       :limit
                       (mcp-http-transport-maximum-message-characters
                        transport)))
                     (t
                      (error 'mcp-protocol-error
                             :message
                             (format nil
                                     "The MCP HTTP response has unsupported content type ~S."
                                     content-type)
                             :method method
                             :payload nil))))
              (mcp-http--close-body body))))
        :operation "MCP HTTP request")))))

(defmethod mcp-transport-notify
    ((transport mcp-streamable-http-transport) notification timeout)
  "Send NOTIFICATION and require the protocol-defined empty 202 response."
  (let ((deadline (mcp-http--deadline timeout)))
    (mcp-http--call-with-exchange-scope
     transport
     (lambda ()
       (mcp-http--call-with-timeout
        transport timeout
        (lambda ()
          (multiple-value-bind (body status headers)
              (mcp-http--exchange
               transport ':post
               :message notification
               :deadline deadline
               :timeout timeout
               :operation "MCP HTTP notification")
            (declare (ignore headers))
            (unwind-protect
                 (progn
                   (unless (= status 202)
                     (error 'mcp-protocol-error
                            :message
                            (format nil
                                    "The MCP server acknowledged a notification with status ~D, not 202."
                                    status)
                            :method (json-get notification "method")
                            :payload nil))
                   (let ((text
                           (mcp-http--body->string
                            transport body
                            "MCP HTTP notification acknowledgement")))
                     (unless (zerop
                              (length
                               (string-trim
                                '(#\Space #\Tab #\Newline #\Return)
                                text)))
                       (error 'mcp-protocol-error
                              :message
                              "The MCP notification acknowledgement contained a body."
                              :method (json-get notification "method")
                              :payload nil))))
              (mcp-http--close-body body))))
        :operation "MCP HTTP notification"))))
  nil)

(defmethod mcp-transport-close
    ((transport mcp-streamable-http-transport))
  "End the server session when possible and clear all negotiated state."
  (mcp-http--stop-idle-listener transport)
  (multiple-value-bind (open-p session-identifier)
      (with-lock-held ((mcp-http-transport-state-lock transport))
        (values
         (mcp-http-transport-open-p transport)
         (mcp-http-transport-session-identifier transport)))
    (unwind-protect
         (when (and open-p session-identifier)
           (handler-case
               (mcp-http--call-with-exchange-scope
                transport
                (lambda ()
                  (dexador:request
                   (mcp-http-transport-url transport)
                   :method :delete
                   :headers
                   (mcp-http--request-headers
                    transport :method ':delete)
                   :connect-timeout
                   (mcp-http-transport-connect-timeout transport)
                   :read-timeout
                   (mcp-http-transport-connect-timeout transport)
                   :max-redirects 0
                   :force-string t
                   :keep-alive nil)))
             (error ()
               nil)))
      (with-lock-held ((mcp-http-transport-state-lock transport))
        (setf (mcp-http-transport-open-p transport) nil
              (mcp-http-transport-session-identifier transport) nil
              (mcp-http-transport-pending-session-identifier transport) nil
              (mcp-http-transport-protocol-version transport) nil
              (mcp-http-transport-listener-stopping-p transport) t
              (mcp-http-transport-listener-support-state transport) ':unknown
              (mcp-http-transport-listener-failure transport) nil))))
  nil)

(defmethod mcp-transport-detach
    ((transport mcp-streamable-http-transport))
  "Forget inherited HTTP session state without terminating the parent session."
  (mcp-http--stop-idle-listener transport)
  (with-lock-held ((mcp-http-transport-state-lock transport))
    (setf (mcp-http-transport-open-p transport) nil
          (mcp-http-transport-session-identifier transport) nil
          (mcp-http-transport-pending-session-identifier transport) nil
          (mcp-http-transport-protocol-version transport) nil
          (mcp-http-transport-listener-stopping-p transport) t
          (mcp-http-transport-listener-support-state transport) ':unknown
          (mcp-http-transport-listener-failure transport) nil))
  nil)

(in-package #:mcparen)

;;;; -- Test Registry --

(defvar *test-cases* nil
  "The registered Mcparen test names and functions.")

(defmacro define-test (name &body body)
  "Define and register one test named NAME."
  `(progn
     (setf *test-cases*
           (remove ',name *test-cases* :key #'first))
     (push (list ',name (lambda () ,@body)) *test-cases*)
     ',name))

(defmacro test-assert (form &optional description)
  "Signal a test failure unless FORM returns true."
  `(unless ,form
     (error "Assertion failed: ~A~@[ (~A)~]"
            ',form ,description)))

(defmacro test-equal (expected form &key (test '#'equal))
  "Signal a test failure unless FORM equals EXPECTED under TEST."
  (let ((expected-value (gensym "EXPECTED"))
        (actual-value   (gensym "ACTUAL")))
    `(let ((,expected-value ,expected)
           (,actual-value ,form))
       (unless (funcall ,test ,expected-value ,actual-value)
         (error "Expected ~S, got ~S from ~S."
                ,expected-value ,actual-value ',form))
       ,actual-value)))

(defmacro test-signals (condition-type &body body)
  "Evaluate BODY and return the signaled CONDITION-TYPE condition."
  (let ((condition (gensym "CONDITION")))
    `(handler-case
         (progn
           ,@body
           (error "Expected condition ~S, but no condition was signaled."
                  ',condition-type))
       (,condition-type (,condition)
         ,condition))))


;;;; -- Scripted Transport --

(defclass test-scripted-transport (mcp-transport)
  ((handler
    :initarg :handler
    :reader test-scripted-transport-handler
    :type function
    :documentation "The function producing one response for each request.")
   (requests
    :initform nil
    :accessor test-scripted-transport-requests
    :type list
    :documentation "Requests observed in chronological order after projection.")
   (notifications
    :initform nil
    :accessor test-scripted-transport-notifications
    :type list
    :documentation "Notifications observed in chronological order after projection.")
   (protocol-version
    :initform nil
    :accessor test-scripted-transport-protocol-version
    :type t
    :documentation "The negotiated protocol version.")
   (open-p
    :initform nil
    :accessor test-scripted-transport-open-p
    :type boolean
    :documentation "True while this scripted transport is open.")
   (close-count
    :initform 0
    :accessor test-scripted-transport-close-count
    :type (integer 0)
    :documentation "The number of explicit close operations.")
   (detach-count
    :initform 0
    :accessor test-scripted-transport-detach-count
    :type (integer 0)
    :documentation "The number of detach operations."))
  (:documentation "An in-memory transport for deterministic protocol tests."))

(-> make-test-scripted-transport (function) test-scripted-transport)
(defun make-test-scripted-transport (handler)
  "Return an in-memory MCP transport using HANDLER for requests."
  (make-instance 'test-scripted-transport :handler handler))

(defmethod mcp-transport-open ((transport test-scripted-transport))
  "Open scripted TRANSPORT."
  (setf (test-scripted-transport-open-p transport) t)
  transport)

(defmethod mcp-transport-open-p ((transport test-scripted-transport))
  "Return the open state of scripted TRANSPORT."
  (test-scripted-transport-open-p transport))

(defmethod mcp-transport-request
    ((transport test-scripted-transport) request timeout)
  "Record REQUEST and obtain its response from TRANSPORT's handler."
  (declare (ignore timeout))
  (unless (mcp-transport-open-p transport)
    (error 'mcp-transport-error
           :message "The scripted MCP transport is closed."
           :transport transport
           :cause nil))
  (setf (test-scripted-transport-requests transport)
        (nconc (test-scripted-transport-requests transport)
               (list request)))
  (funcall (test-scripted-transport-handler transport)
           transport request))

(defmethod mcp-transport-notify
    ((transport test-scripted-transport) notification timeout)
  "Record NOTIFICATION on scripted TRANSPORT."
  (declare (ignore timeout))
  (unless (mcp-transport-open-p transport)
    (error 'mcp-transport-error
           :message "The scripted MCP transport is closed."
           :transport transport
           :cause nil))
  (setf (test-scripted-transport-notifications transport)
        (nconc (test-scripted-transport-notifications transport)
               (list notification)))
  nil)

(defmethod mcp-transport-set-protocol-version
    ((transport test-scripted-transport) version)
  "Record VERSION on scripted TRANSPORT."
  (setf (test-scripted-transport-protocol-version transport) version)
  transport)

(defmethod mcp-transport-close ((transport test-scripted-transport))
  "Close scripted TRANSPORT."
  (incf (test-scripted-transport-close-count transport))
  (setf (test-scripted-transport-open-p transport) nil
        (test-scripted-transport-protocol-version transport) nil)
  nil)

(defmethod mcp-transport-detach ((transport test-scripted-transport))
  "Detach scripted TRANSPORT."
  (incf (test-scripted-transport-detach-count transport))
  (setf (test-scripted-transport-open-p transport) nil
        (test-scripted-transport-protocol-version transport) nil)
  nil)

(-> test-rpc-result (hash-table t) hash-table)
(defun test-rpc-result (request result)
  "Return a successful JSON-RPC response to REQUEST carrying RESULT."
  (json-object "jsonrpc" "2.0"
               "id" (json-get request "id")
               "result" result))

(-> test-rpc-error (hash-table integer string &optional t) hash-table)
(defun test-rpc-error (request code message &optional data)
  "Return a JSON-RPC error response to REQUEST."
  (json-object
   "jsonrpc" "2.0"
   "id" (json-get request "id")
   "error"
   (json-object
    "code" code
    "message" message
    "data" data)))

(-> test-initialize-result () hash-table)
(defun test-initialize-result ()
  "Return a valid MCP initialization result for the supported protocol."
  (json-object
   "protocolVersion" "2025-11-25"
   "capabilities"
   (json-object
    "tools" (json-object "listChanged" yason:false)
    "resources" (json-object "subscribe" yason:false))
   "serverInfo"
   (json-object "name" "mcparen-test-server" "version" "1")
   "instructions" "Deterministic local fixture."))

(-> test-default-handler (test-scripted-transport hash-table) hash-table)
(defun test-default-handler (transport request)
  "Return basic initialization and ping responses for REQUEST."
  (declare (ignore transport))
  (let ((method (json-get request "method")))
    (cond
      ((string= method "initialize")
       (test-rpc-result request (test-initialize-result)))
      ((string= method "ping")
       (test-rpc-result request (json-object)))
      (t
       (error "Unexpected scripted MCP method ~S." method)))))


;;;; -- Bounded Thread Helpers --

(defclass test-thread-result ()
  ((thread
    :initarg :thread
    :accessor test-thread-result-thread
    :type t
    :documentation "The worker thread.")
   (value
    :initform nil
    :accessor test-thread-result-value
    :type t
    :documentation "The value returned by the worker.")
   (condition
    :initform nil
    :accessor test-thread-result-condition
    :type t
    :documentation "The condition signaled by the worker.")
   (finished-p
    :initform nil
    :accessor test-thread-result-finished-p
    :type boolean
    :documentation "True after the worker has unwound."))
  (:documentation "The observable result of one bounded test worker."))

(-> test-start-thread (function string) test-thread-result)
(defun test-start-thread (function name)
  "Run FUNCTION in a named thread and return its result holder."
  (let ((result (make-instance 'test-thread-result :thread nil)))
    (setf
     (test-thread-result-thread result)
     (make-thread
      (lambda ()
        (unwind-protect
             (handler-case
                 (setf (test-thread-result-value result)
                       (funcall function))
               (error (condition)
                 (setf (test-thread-result-condition result)
                       condition)))
          (setf (test-thread-result-finished-p result) t)))
      :name name))
    result))

(-> test-wait-until (function real) boolean)
(defun test-wait-until (predicate timeout)
  "Wait at most TIMEOUT seconds for PREDICATE to return true."
  (let ((deadline (mcp-stdio--deadline timeout)))
    (loop
      (when (funcall predicate)
        (return t))
      (unless (plusp (mcp-stdio--remaining-seconds deadline))
        (return nil))
      (sleep 0.005))))

(-> test-await-thread (test-thread-result real) t)
(defun test-await-thread (result timeout)
  "Return RESULT's worker value, requiring completion within TIMEOUT seconds."
  (unless
      (test-wait-until
       (lambda () (test-thread-result-finished-p result))
       timeout)
    (destroy-thread (test-thread-result-thread result))
    (error "Test worker did not finish within ~,2F seconds." timeout))
  (join-thread (test-thread-result-thread result))
  (when (test-thread-result-condition result)
    (error (test-thread-result-condition result)))
  (test-thread-result-value result))

(-> test-fixture-pathname (string) pathname)
(defun test-fixture-pathname (name)
  "Return the pathname of test fixture NAME."
  (merge-pathnames
   (format nil "tests/fixtures/~A" name)
   (asdf:system-source-directory '#:mcparen)))

(-> make-test-stdio-transport
    (&key (:maximum-message-characters t)
          (:request-handler t)
          (:notification-handler t))
    mcp-stdio-transport)
(defun make-test-stdio-transport
    (&key maximum-message-characters
      request-handler notification-handler)
  "Return a stdio transport connected to the local Common Lisp fixture."
  (make-mcp-stdio-transport
   (namestring sb-ext:*runtime-pathname*)
   :arguments
   (list "--noinform"
         "--disable-debugger"
         "--script"
         (namestring (test-fixture-pathname "stdio-server.lisp")))
   :maximum-message-characters
   (or maximum-message-characters
       *mcp-maximum-message-characters*)
   :request-handler request-handler
   :notification-handler notification-handler))


;;;; -- Local HTTP Fixture --

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:sb-bsd-sockets))

(defclass test-http-request ()
  ((method
    :initarg :method
    :reader test-http-request-method
    :type string
    :documentation "The request method.")
   (target
    :initarg :target
    :reader test-http-request-target
    :type string
    :documentation "The request target.")
   (headers
    :initarg :headers
    :reader test-http-request-headers
    :type list
    :documentation "The lower-case request headers.")
   (body
    :initarg :body
    :reader test-http-request-body
    :type string
    :documentation "The request body."))
  (:documentation "One request observed by the local HTTP fixture."))

(defclass test-http-server ()
  ((socket
    :initarg :socket
    :reader test-http-server-socket
    :type t
    :documentation "The listening socket.")
   (port
    :initarg :port
    :reader test-http-server-port
    :type integer
    :documentation "The bound loopback port.")
   (handler
    :initarg :handler
    :reader test-http-server-handler
    :type function
    :documentation "The function producing HTTP responses.")
   (thread
    :initform nil
    :accessor test-http-server-thread
    :type t
    :documentation "The accept-loop thread.")
   (worker-threads
    :initform nil
    :accessor test-http-server-worker-threads
    :type list
    :documentation "The connection workers started by the accept loop.")
   (requests
    :initform nil
    :accessor test-http-server-requests
    :type list
    :documentation "The requests observed in chronological order.")
   (lock
    :initform (make-lock "mcparen HTTP fixture")
    :reader test-http-server-lock
    :type t
    :documentation "The lock protecting requests and stop state.")
   (stopping-p
    :initform nil
    :accessor test-http-server-stopping-p
    :type boolean
    :documentation "True while the fixture is stopping.")
   (failure
    :initform nil
    :accessor test-http-server-failure
    :type t
    :documentation "The first unexpected accept-loop failure."))
  (:documentation "A bounded raw-socket HTTP server for transport tests."))

(-> test-http--header (test-http-request string) t)
(defun test-http--header (request name)
  "Return case-insensitive header NAME from REQUEST."
  (rest
   (assoc (string-downcase name)
          (test-http-request-headers request)
          :test #'string=)))

(-> test-http--read-request (stream) (or null test-http-request))
(defun test-http--read-request (stream)
  "Read one HTTP request from STREAM, or NIL at end of input."
  (let ((request-line (read-line stream nil nil)))
    (unless request-line
      (return-from test-http--read-request nil))
    (let* ((parts
             (uiop:split-string
              (string-right-trim '(#\Return) request-line)
              :separator '(#\Space)))
           (headers nil))
      (unless (>= (length parts) 2)
        (error "Malformed HTTP request line ~S." request-line))
      (loop for raw = (read-line stream nil nil)
            for line = (and raw (string-right-trim '(#\Return) raw))
            while (and line (plusp (length line)))
            do
               (let ((colon (position #\: line)))
                 (unless colon
                   (error "Malformed HTTP header ~S." line))
                 (push
                  (cons
                   (string-downcase (subseq line 0 colon))
                   (string-trim '(#\Space #\Tab)
                                (subseq line (1+ colon))))
                  headers)))
      (let* ((content-length
               (let ((value
                       (rest
                        (assoc "content-length" headers
                               :test #'string=))))
                 (if value
                     (parse-integer value)
                     0)))
             (body (make-string content-length)))
        (when (plusp content-length)
          (let ((count (read-sequence body stream)))
            (unless (= count content-length)
              (error "HTTP request body ended after ~D of ~D characters."
                     count content-length))))
        (make-instance
         'test-http-request
         :method (first parts)
         :target (second parts)
         :headers (nreverse headers)
         :body body)))))

(-> test-http--reason (integer) string)
(defun test-http--reason (status)
  "Return a conventional HTTP reason phrase for STATUS."
  (case status
    (200 "OK")
    (202 "Accepted")
    (204 "No Content")
    (302 "Found")
    (404 "Not Found")
    (500 "Internal Server Error")
    (t "Test Response")))

(-> test-http--write-response (stream integer list string) null)
(defun test-http--write-response (stream status headers body)
  "Write one HTTP response to STREAM."
  (format stream "HTTP/1.1 ~D ~A~C~C"
          status (test-http--reason status)
          #\Return #\Newline)
  (dolist (header
           (append
            headers
            (list
             (cons "Content-Length"
                   (write-to-string (length body)))
             (cons "Connection" "close"))))
    (format stream "~A: ~A~C~C"
            (first header) (rest header)
            #\Return #\Newline))
  (format stream "~C~C" #\Return #\Newline)
  (write-string body stream)
  (finish-output stream)
  nil)

(-> test-http--serve-connection (test-http-server t) null)
(defun test-http--serve-connection (server socket)
  "Serve one request accepted by SERVER from SOCKET."
  (let ((stream
          (sb-bsd-sockets:socket-make-stream
           socket
           :input t
           :output t
           :element-type 'character
           :external-format :utf-8
           :buffering :none)))
    (unwind-protect
         (let ((request (test-http--read-request stream)))
           (when request
             (with-lock-held ((test-http-server-lock server))
               (setf (test-http-server-requests server)
                     (nconc (test-http-server-requests server)
                            (list request))))
             (multiple-value-bind (status headers body)
                 (funcall (test-http-server-handler server)
                          server request)
               (test-http--write-response
                stream status headers (or body "")))))
      (when (open-stream-p stream)
        (close stream))))
  nil)

(-> test-http--record-failure (test-http-server t) null)
(defun test-http--record-failure (server condition)
  "Record CONDITION as SERVER's first unexpected worker failure."
  (with-lock-held ((test-http-server-lock server))
    (unless (test-http-server-failure server)
      (setf (test-http-server-failure server) condition)))
  nil)

(-> test-http--start-worker (test-http-server t) null)
(defun test-http--start-worker (server socket)
  "Serve SOCKET on a worker so SERVER can accept concurrent requests."
  (let ((worker
          (make-thread
           (lambda ()
             (handler-case
                 (test-http--serve-connection server socket)
               ;; A timed-out HTTP client is allowed to close its response
               ;; stream before the fixture finishes writing.
               (stream-error ()
                 nil)
               (error (condition)
                 (test-http--record-failure server condition))))
           :name "mcparen HTTP fixture connection")))
    (with-lock-held ((test-http-server-lock server))
      (push worker (test-http-server-worker-threads server))))
  nil)

(-> test-http--accept-loop (test-http-server) null)
(defun test-http--accept-loop (server)
  "Accept and serve fixture requests until SERVER stops."
  (handler-case
      (loop
        (when (with-lock-held ((test-http-server-lock server))
                (test-http-server-stopping-p server))
          (return))
        (multiple-value-bind (client address)
            (sb-bsd-sockets:socket-accept
             (test-http-server-socket server))
          (declare (ignore address))
          (test-http--start-worker server client)))
    (error (condition)
      (unless (with-lock-held ((test-http-server-lock server))
                (test-http-server-stopping-p server))
        (test-http--record-failure server condition))))
  nil)

(-> start-test-http-server (function) test-http-server)
(defun start-test-http-server (handler)
  "Start a loopback HTTP fixture using HANDLER."
  (let ((socket
          (make-instance
           'sb-bsd-sockets:inet-socket
           :type :stream
           :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 16)
    (multiple-value-bind (address port)
        (sb-bsd-sockets:socket-name socket)
      (declare (ignore address))
      (let ((server
              (make-instance
               'test-http-server
               :socket socket
               :port port
               :handler handler)))
        (setf
         (test-http-server-thread server)
         (make-thread
          (lambda () (test-http--accept-loop server))
          :name "mcparen HTTP fixture"))
        server))))

(-> test-http-server-url (test-http-server) string)
(defun test-http-server-url (server)
  "Return SERVER's loopback endpoint."
  (format nil "http://127.0.0.1:~D/mcp"
          (test-http-server-port server)))

(-> stop-test-http-server (test-http-server) null)
(defun stop-test-http-server (server)
  "Stop SERVER and require its accept loop to finish."
  (with-lock-held ((test-http-server-lock server))
    (setf (test-http-server-stopping-p server) t))
  (handler-case
      (sb-bsd-sockets:socket-close
       (test-http-server-socket server))
    (error ()
      nil))
  (unless
      (test-wait-until
       (lambda ()
         (not (thread-alive-p
               (test-http-server-thread server))))
       1.0)
    (destroy-thread (test-http-server-thread server)))
  (handler-case
      (join-thread (test-http-server-thread server))
    (error ()
      nil))
  (let ((workers
          (with-lock-held ((test-http-server-lock server))
            (copy-list
             (test-http-server-worker-threads server)))))
    (unless
        (test-wait-until
         (lambda ()
           (notany #'thread-alive-p workers))
         2.0)
      (dolist (worker workers)
        (when (thread-alive-p worker)
          (destroy-thread worker))))
    (dolist (worker workers)
      (handler-case
          (join-thread worker)
        (error ()
          nil))))
  (when (test-http-server-failure server)
    (error (test-http-server-failure server)))
  nil)

(-> test-open-file-descriptor-count () integer)
(defun test-open-file-descriptor-count ()
  "Return the number of file descriptors currently open by this process."
  (length (directory #P"/proc/self/fd/*")))

(defmacro with-test-http-server ((variable handler) &body body)
  "Run BODY with VARIABLE bound to a local HTTP fixture using HANDLER."
  `(let ((,variable (start-test-http-server ,handler)))
     (unwind-protect
          (progn ,@body)
       (stop-test-http-server ,variable))))

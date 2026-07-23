(in-package #:mcparen)

;;;; -- Standard I/O Transport --

(defparameter *mcp-stdio-diagnostic-limit* 8192
  "The maximum stderr characters retained from one MCP server process.")

(defparameter *mcp-stdio-graceful-exit-timeout* 0.5
  "Seconds allowed for a stdio server to exit after stdin closes.")

(defparameter *mcp-stdio-terminate-timeout* 1.0
  "Seconds allowed for a stdio server process group to exit after SIGTERM.")

(defparameter *mcp-stdio-kill-timeout* 0.5
  "Seconds allowed for a stdio server process group to exit after SIGKILL.")

(defparameter *mcp-stdio-thread-join-timeout* 1.0
  "Seconds allowed for each stdio reader thread to stop.")

(defparameter *mcp-stdio-process-group-startup-timeout* 1.0
  "Seconds allowed for a stdio server to enter its dedicated process group.")

(defparameter *mcp-stdio-callback-write-timeout* 5.0
  "Seconds allowed to return a response to a server-originated request.")

(defparameter *mcp-stdio-callback-queue-limit* 128
  "The maximum queued server callbacks waiting for the ordered worker.")

(defclass mcp-stdio-pending-request ()
  ((condition-variable
    :initform (make-condition-variable)
    :reader mcp-stdio-pending-condition-variable
    :type t
    :documentation "The condition signaled when this request completes.")
   (response
    :initform nil
    :accessor mcp-stdio-pending-response
    :type t
    :documentation "The matching decoded JSON-RPC response."))
  (:documentation "One concurrent stdio request awaiting a response."))

(defclass mcp-stdio-transport (mcp-transport)
  ((command
    :initarg :command
    :reader mcp-stdio-transport-command
    :type string
    :documentation "The executable used to start the MCP server.")
   (arguments
    :initarg :arguments
    :initform nil
    :reader mcp-stdio-transport-arguments
    :type list
    :documentation "The exact argv strings following the executable.")
   (directory
    :initarg :directory
    :initform nil
    :reader mcp-stdio-transport-directory
    :type t
    :documentation
    "The optional server directory or a function resolving it at launch.")
   (environment-function
    :initarg :environment-function
    :initform (constantly nil)
    :reader mcp-stdio-transport-environment-function
    :type function
    :documentation
    "A function returning a UIOP environment list, or NIL to inherit it.")
   (request-handler
    :initarg :request-handler
    :initform nil
    :reader mcp-stdio-transport-request-handler
    :type t
    :documentation "An optional function handling JSON-RPC requests from the server.")
   (notification-handler
    :initarg :notification-handler
    :initform nil
    :reader mcp-stdio-transport-notification-handler
    :type t
    :documentation "An optional function receiving server notification method and params.")
   (maximum-message-characters
    :initarg :maximum-message-characters
    :initform *mcp-maximum-message-characters*
    :reader mcp-stdio-transport-maximum-message-characters
    :type integer
    :documentation "The maximum characters accepted in one stdout JSON document.")
   (process
    :initform nil
    :accessor mcp-stdio-transport-process
    :type t
    :documentation "The live UIOP process object.")
   (process-group-p
    :initform nil
    :accessor mcp-stdio-transport-process-group-p
    :type boolean
    :documentation "True when the server was launched in a dedicated process group.")
   (process-group-identifier
    :initform nil
    :accessor mcp-stdio-transport-process-group-identifier
    :type (or null integer)
    :documentation "The positive process group identifier used for shutdown.")
   (input
    :initform nil
    :accessor mcp-stdio-transport-input
    :type t
    :documentation "The stream writing to server stdin.")
   (output
    :initform nil
    :accessor mcp-stdio-transport-output
    :type t
    :documentation "The stream reading server stdout.")
   (error-output
    :initform nil
    :accessor mcp-stdio-transport-error-output
    :type t
    :documentation "The stream draining server stderr.")
   (reader-thread
    :initform nil
    :accessor mcp-stdio-transport-reader-thread
    :type t
    :documentation "The thread decoding server stdout.")
   (error-thread
    :initform nil
    :accessor mcp-stdio-transport-error-thread
    :type t
    :documentation "The thread draining bounded server stderr.")
   (callback-thread
    :initform nil
    :accessor mcp-stdio-transport-callback-thread
    :type t
    :documentation "The sole ordered server callback worker thread.")
   (callback-condition-variable
    :initform (make-condition-variable)
    :reader mcp-stdio-transport-callback-condition-variable
    :type t
    :documentation "The condition signaled when callback work becomes available.")
   (callback-queue
    :initform nil
    :accessor mcp-stdio-transport-callback-queue
    :type list
    :documentation "Ordered callback functions waiting for the worker.")
   (callback-stopping-p
    :initform t
    :accessor mcp-stdio-transport-callback-stopping-p
    :type boolean
    :documentation "True when callback work must no longer begin.")
   (write-lock
    :initform (make-lock "mcparen stdio write")
    :reader mcp-stdio-transport-write-lock
    :type t
    :documentation "The lock preserving one-JSON-document-per-line writes.")
   (state-lock
    :initform (make-lock "mcparen stdio state")
    :reader mcp-stdio-transport-state-lock
    :type t
    :documentation "The lock protecting response and failure state.")
   (pending-requests
    :initform (make-hash-table :test #'equal)
    :reader mcp-stdio-transport-pending-requests
    :type hash-table
    :documentation "Request identifiers mapped to their private wait state.")
   (reader-failure
    :initform nil
    :accessor mcp-stdio-transport-reader-failure
    :type t
    :documentation "The terminal stdout reader failure, when any.")
   (stderr-text
    :initform ""
    :accessor mcp-stdio-transport-stderr-text
    :type string
    :documentation "The bounded tail of server stderr.")
   (closing-p
    :initform nil
    :accessor mcp-stdio-transport-closing-p
    :type boolean
    :documentation "True while deliberate shutdown is in progress.")
   (open-p
    :initform nil
    :accessor mcp-stdio-transport-open-p
    :type boolean
    :documentation "True while the process and streams are usable."))
  (:documentation "A newline-delimited JSON-RPC MCP stdio transport."))

(-> make-mcp-stdio-transport
    (string &key (:arguments list)
                 (:directory t)
                 (:environment-function function)
                 (:request-handler t)
                 (:notification-handler t)
                 (:maximum-message-characters integer))
    mcp-stdio-transport)
(defun make-mcp-stdio-transport
    (command
     &key arguments directory
       (environment-function (constantly nil))
       request-handler notification-handler
       (maximum-message-characters *mcp-maximum-message-characters*))
  "Create a lazy stdio MCP transport for COMMAND and ARGUMENTS.

DIRECTORY may be a pathname designator or a function returning one. A function
is called immediately before each process launch, allowing a reconnect to
follow a caller's current workspace without rebuilding the transport."
  (unless (and (stringp command) (plusp (length command)))
    (error 'mcp-transport-error
           :message "An MCP stdio command must be a non-empty string."
           :transport nil
           :cause nil))
  (unless (every #'stringp arguments)
    (error 'mcp-transport-error
           :message "Every MCP stdio argument must be a string."
           :transport nil
           :cause nil))
  (unless (and (integerp maximum-message-characters)
               (plusp maximum-message-characters))
    (error 'mcp-transport-error
           :message "The MCP stdio message limit must be a positive integer."
           :transport nil
           :cause nil))
  (make-instance 'mcp-stdio-transport
                 :command command
                 :arguments (copy-list arguments)
                 :directory directory
                 :environment-function environment-function
                 :request-handler request-handler
                 :notification-handler notification-handler
                 :maximum-message-characters maximum-message-characters))

(defmethod mcp-transport-open-p ((transport mcp-stdio-transport))
  "Return true while the stdio server process is usable."
  (and (mcp-stdio-transport-open-p transport)
       (let ((process (mcp-stdio-transport-process transport)))
         (and process
              (uiop:process-alive-p process)))))

(-> mcp-stdio--append-stderr (mcp-stdio-transport string) null)
(defun mcp-stdio--append-stderr (transport fragment)
  "Append FRAGMENT to TRANSPORT's bounded stderr tail."
  (with-lock-held ((mcp-stdio-transport-state-lock transport))
    (let* ((combined
             (concatenate 'string
                          (mcp-stdio-transport-stderr-text transport)
                          fragment))
           (start (max 0 (- (length combined)
                            *mcp-stdio-diagnostic-limit*))))
      (setf (mcp-stdio-transport-stderr-text transport)
            (subseq combined start))))
  nil)

(-> mcp-stdio--record-reader-failure (mcp-stdio-transport t) null)
(defun mcp-stdio--record-reader-failure (transport failure)
  "Publish terminal reader FAILURE unless TRANSPORT is deliberately closing."
  (with-lock-held ((mcp-stdio-transport-state-lock transport))
    (unless (mcp-stdio-transport-closing-p transport)
      (setf (mcp-stdio-transport-reader-failure transport) failure
            (mcp-stdio-transport-open-p transport) nil
            (mcp-stdio-transport-callback-stopping-p transport) t
            (mcp-stdio-transport-callback-queue transport) nil)
      (condition-notify
       (mcp-stdio-transport-callback-condition-variable transport))
      (maphash
       (lambda (identifier pending)
         (declare (ignore identifier))
         (condition-notify
          (mcp-stdio-pending-condition-variable pending)))
       (mcp-stdio-transport-pending-requests transport))))
  nil)

(-> mcp-stdio--deadline (real) integer)
(defun mcp-stdio--deadline (timeout)
  "Return the internal-real-time deadline TIMEOUT seconds from now."
  (+ (get-internal-real-time)
     (ceiling (* timeout internal-time-units-per-second))))

(-> mcp-stdio--remaining-seconds (integer) real)
(defun mcp-stdio--remaining-seconds (deadline)
  "Return non-negative seconds remaining before DEADLINE."
  (max 0
       (/ (- deadline (get-internal-real-time))
          internal-time-units-per-second)))

(-> mcp-stdio--close-after-write-timeout (mcp-stdio-transport) t)
(defun mcp-stdio--close-after-write-timeout (transport)
  "Close TRANSPORT after a blocked write and return any cleanup failure."
  (handler-case
      (progn
        (mcp-transport-close transport)
        nil)
    (error (cause)
      cause)))

(-> mcp-stdio--write-message
    (mcp-stdio-transport hash-table integer real string)
    null)
(defun mcp-stdio--write-message
    (transport message deadline timeout operation)
  "Write MESSAGE before DEADLINE or close TRANSPORT and signal a timeout."
  (let ((remaining (mcp-stdio--remaining-seconds deadline)))
    (unless (plusp remaining)
      (error 'mcp-timeout
             :message
             (format nil "~A timed out after ~,2F seconds." operation timeout)
             :transport transport
             :cause (mcp-stdio--close-after-write-timeout transport)
             :operation operation
             :seconds timeout))
    (handler-case
        (sb-ext:with-timeout remaining
          (with-lock-held ((mcp-stdio-transport-write-lock transport))
            (let ((stream (mcp-stdio-transport-input transport)))
              (unless (and stream (open-stream-p stream))
                (error 'mcp-transport-error
                       :message "The MCP stdio input stream is closed."
                       :transport transport
                       :cause nil))
              (write-line
               (json-encode
                message
                :limit
                (mcp-stdio-transport-maximum-message-characters
                 transport))
               stream)
              (finish-output stream))))
      (sb-ext:timeout ()
        (error 'mcp-timeout
               :message
               (format nil "~A timed out after ~,2F seconds."
                       operation timeout)
               :transport transport
               :cause (mcp-stdio--close-after-write-timeout transport)
               :operation operation
               :seconds timeout))
      (mcp-error (condition)
        (error condition))
      (error (cause)
        (error 'mcp-transport-error
               :message "Could not write to the MCP stdio server."
               :transport transport
               :cause cause))))
  nil)

(-> mcp-stdio--server-error-response (t integer string &optional t) hash-table)
(defun mcp-stdio--server-error-response
    (identifier code message &optional data)
  "Return one JSON-RPC error response to a server-originated request."
  (let ((error-object
          (json-object
           "code" code
           "message" message)))
    (when data
      (setf (gethash "data" error-object) data))
    (json-object
     "jsonrpc" "2.0"
     "id" identifier
     "error" error-object)))

(-> mcp-stdio--enqueue-callback (mcp-stdio-transport function) null)
(defun mcp-stdio--enqueue-callback (transport function)
  "Append FUNCTION to TRANSPORT's bounded ordered callback queue."
  (with-lock-held ((mcp-stdio-transport-state-lock transport))
    (when (mcp-stdio-transport-callback-stopping-p transport)
      (error 'mcp-transport-error
             :message "The MCP stdio callback worker is stopped."
             :transport transport
             :cause nil))
    (when (>= (length (mcp-stdio-transport-callback-queue transport))
              *mcp-stdio-callback-queue-limit*)
      (error 'mcp-transport-error
             :message
             (format nil
                     "The MCP stdio callback queue exceeded its ~D-job limit."
                     *mcp-stdio-callback-queue-limit*)
             :transport transport
             :cause nil))
    (setf (mcp-stdio-transport-callback-queue transport)
          (nconc (mcp-stdio-transport-callback-queue transport)
                 (list function)))
    (condition-notify
     (mcp-stdio-transport-callback-condition-variable transport)))
  nil)

(-> mcp-stdio--callback-loop (mcp-stdio-transport) null)
(defun mcp-stdio--callback-loop (transport)
  "Run queued server callbacks one at a time in their wire order."
  (loop
    for callback =
      (with-lock-held ((mcp-stdio-transport-state-lock transport))
        (loop while (and
                     (null (mcp-stdio-transport-callback-queue transport))
                     (not
                      (mcp-stdio-transport-callback-stopping-p transport)))
              do
                 (condition-wait
                  (mcp-stdio-transport-callback-condition-variable transport)
                  (mcp-stdio-transport-state-lock transport)))
        (when (mcp-stdio-transport-callback-stopping-p transport)
          (return-from mcp-stdio--callback-loop nil))
        (pop (mcp-stdio-transport-callback-queue transport)))
    do
       (handler-case
           (funcall callback)
         (error (cause)
           (mcp-stdio--record-reader-failure transport cause))))
  nil)

(-> mcp-stdio--handle-server-request
    (mcp-stdio-transport hash-table)
    null)
(defun mcp-stdio--handle-server-request (transport message)
  "Handle one server-originated JSON-RPC request without blocking the reader."
  (let ((identifier (json-get message "id"))
        (method (json-get message "method"))
        (params (json-get message "params"))
        (handler (mcp-stdio-transport-request-handler transport)))
    (mcp-stdio--enqueue-callback
     transport
     (lambda ()
       (let ((response
               (handler-case
                   (json-object
                    "jsonrpc" "2.0"
                    "id" identifier
                    "result"
                    (cond
                      ((string= method "ping")
                       (json-object))
                      (handler
                       (funcall handler method params))
                      (t
                       (error 'mcp-rpc-error
                              :message
                              (format nil
                                      "Client method ~S is not supported."
                                      method)
                              :method method
                              :payload nil
                              :code -32601
                              :data nil))))
                 (mcp-rpc-error (cause)
                   (mcp-stdio--server-error-response
                    identifier
                    (mcp-rpc-error-code cause)
                    (mcp-error-message cause)
                    (mcp-rpc-error-data cause)))
                 (error (cause)
                   (mcp-stdio--server-error-response
                    identifier -32603
                    "The MCP client request handler failed."
                    (bounded-diagnostic cause :limit 512))))))
         (handler-case
             (mcp-stdio--write-message
              transport response
              (mcp-stdio--deadline *mcp-stdio-callback-write-timeout*)
              *mcp-stdio-callback-write-timeout*
              "MCP stdio server response")
           (error (response-cause)
             (mcp-stdio--record-reader-failure
              transport response-cause))))))
  nil))

(-> mcp-stdio--dispatch-message (mcp-stdio-transport t) null)
(defun mcp-stdio--dispatch-message (transport message)
  "Dispatch one decoded server MESSAGE as a response, request, or notification."
  (ecase (json-rpc-message-validate message)
    (:response
     (let ((identifier (json-get message "id")))
       (with-lock-held ((mcp-stdio-transport-state-lock transport))
         (let ((pending
                 (gethash
                  identifier
                  (mcp-stdio-transport-pending-requests transport))))
           (when pending
             (setf (mcp-stdio-pending-response pending) message)
             (condition-notify
              (mcp-stdio-pending-condition-variable pending)))))))
    (:request
     (mcp-stdio--handle-server-request transport message))
    (:notification
     (let ((handler
             (mcp-stdio-transport-notification-handler transport))
           (method (json-get message "method")))
       (when handler
         (mcp-stdio--enqueue-callback
          transport
          (lambda ()
            (handler-case
                (funcall handler method (json-get message "params"))
              (error (cause)
                (mcp-stdio--append-stderr
                 transport
                 (format nil
                         "notification handler for ~A failed: ~A~%"
                         method cause))))))))))
  nil)

(-> mcp-stdio--reader-loop (mcp-stdio-transport) null)
(defun mcp-stdio--reader-loop (transport)
  "Read and dispatch newline-delimited messages until server stdout closes."
  (handler-case
      (progn
        (loop
          (multiple-value-bind (line end-of-file-p)
              (stream-read-bounded-line
               (mcp-stdio-transport-output transport)
               (mcp-stdio-transport-maximum-message-characters transport)
               "MCP stdio stdout line")
            (when (and line
                       (plusp
                        (length
                         (string-trim
                          '(#\Space #\Tab #\Return #\Newline)
                          line))))
              (mcp-stdio--dispatch-message
               transport
               (json-decode
                line
                :limit
                (mcp-stdio-transport-maximum-message-characters transport)
                :source-name "MCP stdio JSON document")))
            (when end-of-file-p
              (return))))
        (unless (mcp-stdio-transport-closing-p transport)
          (mcp-stdio--record-reader-failure
           transport
           (make-condition
            'mcp-transport-error
            :message "The MCP stdio server closed its output."
            :transport transport
            :cause nil))))
    (error (cause)
      (mcp-stdio--record-reader-failure transport cause)))
  nil)

(-> mcp-stdio--error-loop (mcp-stdio-transport) null)
(defun mcp-stdio--error-loop (transport)
  "Drain server stderr into a bounded diagnostic tail."
  (handler-case
      (let ((buffer
              (make-array 1024
                          :element-type 'character
                          :fill-pointer 0)))
        (labels ((flush-buffer ()
                   "Append the current fixed-size stderr chunk."
                   (when (plusp (length buffer))
                     (mcp-stdio--append-stderr
                      transport
                      (coerce buffer 'string))
                     (setf (fill-pointer buffer) 0))))
          (loop for character =
                  (read-char
                   (mcp-stdio-transport-error-output transport)
                   nil nil)
                while character
                do
                   (vector-push character buffer)
                   (when (or (char= character #\Newline)
                             (= (length buffer)
                                (array-total-size buffer)))
                     (flush-buffer))
                finally
                   (flush-buffer))))
    (error (cause)
      (unless (mcp-stdio-transport-closing-p transport)
        (mcp-stdio--append-stderr
         transport
         (format nil "stderr reader failed: ~A~%" cause)))))
  nil)

(-> mcp-stdio--launch-arguments (mcp-stdio-transport) list)
(defun mcp-stdio--launch-arguments (transport)
  "Return UIOP keyword arguments for opening TRANSPORT."
  (let ((directory
          (handler-case
              (let ((configured
                      (mcp-stdio-transport-directory transport)))
                (if (functionp configured)
                    (funcall configured)
                    configured))
            (error (cause)
              (error 'mcp-transport-error
                     :message
                     "Could not resolve the MCP stdio working directory."
                     :transport transport
                     :cause cause))))
        (environment
          (handler-case
              (funcall
               (mcp-stdio-transport-environment-function transport))
            (error (cause)
              (error 'mcp-transport-error
                     :message "Could not resolve the MCP stdio environment."
                     :transport transport
                     :cause cause)))))
    (append
     (list :input :stream
           :output :stream
           :error-output :stream
           :external-format :utf-8
           :ignore-error-status t)
     (when directory
       (list :directory directory))
     (when environment
       (list :environment environment)))))

(-> mcp-stdio--command (mcp-stdio-transport) list)
(defun mcp-stdio--command (transport)
  "Return the direct launch argv for TRANSPORT."
  (cons (mcp-stdio-transport-command transport)
        (mcp-stdio-transport-arguments transport)))

(-> mcp-stdio--await-process-group
    (mcp-stdio-transport t integer)
    integer)
(defun mcp-stdio--await-process-group (transport process identifier)
  "Wait until PROCESS enters the dedicated group named by IDENTIFIER."
  (let ((deadline
          (mcp-stdio--deadline
           *mcp-stdio-process-group-startup-timeout*)))
    (loop
      for process-group =
        (handler-case
            (sb-posix:getpgid identifier)
          (error ()
            nil))
      when (eql process-group identifier)
        return identifier
      unless (uiop:process-alive-p process)
        do (error 'mcp-transport-error
                  :message
                  "The MCP stdio server exited before entering its process group."
                  :transport transport
                  :cause nil)
      unless (plusp (mcp-stdio--remaining-seconds deadline))
        do (error 'mcp-transport-error
                  :message
                  "Timed out waiting for the MCP stdio server process group."
                  :transport transport
                  :cause nil)
      do (sleep 0.01))))

(defmethod mcp-transport-open ((transport mcp-stdio-transport))
  "Start the configured process and its bounded reader threads."
  (when (mcp-transport-open-p transport)
    (return-from mcp-transport-open transport))
  (when (or (mcp-stdio-transport-process transport)
            (mcp-stdio-transport-input transport)
            (mcp-stdio-transport-output transport)
            (mcp-stdio-transport-error-output transport)
            (mcp-stdio-transport-reader-thread transport)
            (mcp-stdio-transport-error-thread transport)
            (mcp-stdio-transport-callback-thread transport))
    (mcp-transport-close transport))
  (let ((process nil)
        (opened-p nil))
    (unwind-protect
         (handler-case
             (progn
               (setf process
                     (apply #'uiop:launch-program
                            (mcp-stdio--command transport)
                            (mcp-stdio--launch-arguments transport))
                     (mcp-stdio-transport-process transport)
                     process)
               (let ((identifier (uiop:process-info-pid process)))
                 (setf (mcp-stdio-transport-process-group-p transport) t
                       (mcp-stdio-transport-process-group-identifier transport)
                       identifier
                       (mcp-stdio-transport-input transport)
                       (uiop:process-info-input process)
                       (mcp-stdio-transport-output transport)
                       (uiop:process-info-output process)
                       (mcp-stdio-transport-error-output transport)
                       (uiop:process-info-error-output process))
                 (mcp-stdio--await-process-group
                  transport process identifier))
               (setf (mcp-stdio-transport-reader-failure transport) nil
                     (mcp-stdio-transport-stderr-text transport) ""
                     (mcp-stdio-transport-closing-p transport) nil
                     (mcp-stdio-transport-callback-stopping-p transport) nil
                     (mcp-stdio-transport-callback-queue transport) nil
                     (mcp-stdio-transport-open-p transport) t
                     (mcp-stdio-transport-callback-thread transport)
                     (make-thread
                      (lambda ()
                        (mcp-stdio--callback-loop transport))
                      :name "mcparen MCP callbacks")
                     (mcp-stdio-transport-reader-thread transport)
                     (make-thread
                      (lambda ()
                        (mcp-stdio--reader-loop transport))
                      :name "mcparen MCP stdout")
                     (mcp-stdio-transport-error-thread transport)
                     (make-thread
                      (lambda ()
                        (mcp-stdio--error-loop transport))
                      :name "mcparen MCP stderr"))
               (setf opened-p t)
               transport)
           (mcp-error (condition)
             (error condition))
           (error (cause)
             (error 'mcp-transport-error
                    :message
                    (format nil "Could not start MCP stdio command ~S."
                            (mcp-stdio-transport-command transport))
                    :transport transport
                    :cause cause)))
      (unless opened-p
        (handler-case
            (mcp-transport-close transport)
          (error ()
            nil))))))

(-> mcp-stdio--take-response
    (mcp-stdio-transport t mcp-stdio-pending-request integer real)
    hash-table)
(defun mcp-stdio--take-response
    (transport identifier pending deadline timeout)
  "Wait for and remove IDENTIFIER's response before DEADLINE."
  (with-lock-held ((mcp-stdio-transport-state-lock transport))
    (loop
      (let ((response (mcp-stdio-pending-response pending)))
        (when response
          (return response)))
      (when (mcp-stdio-transport-reader-failure transport)
        (let ((failure (mcp-stdio-transport-reader-failure transport))
              (stderr (mcp-stdio-transport-stderr-text transport)))
          (if (or (typep failure 'mcp-protocol-error)
                  (typep failure 'mcp-timeout))
              (error failure)
              (error 'mcp-transport-error
                     :message
                     (format nil "The MCP stdio reader stopped: ~A~@[~%~A~]"
                             failure
                             (and (plusp (length stderr)) stderr))
                     :transport transport
                     :cause failure))))
      (let ((remaining (mcp-stdio--remaining-seconds deadline)))
        (unless (plusp remaining)
          (error 'mcp-timeout
                 :message
                 (format nil "MCP stdio request ~S timed out after ~,2F seconds."
                         identifier timeout)
                 :transport transport
                 :cause nil
                 :operation "JSON-RPC request"
                 :seconds timeout))
        (condition-wait
         (mcp-stdio-pending-condition-variable pending)
         (mcp-stdio-transport-state-lock transport)
         :timeout remaining)))))

(defmethod mcp-transport-request
    ((transport mcp-stdio-transport) request timeout)
  "Serialize REQUEST and wait for its matching JSON-RPC response."
  (unless (mcp-transport-open-p transport)
    (error 'mcp-transport-error
           :message "The MCP stdio transport is closed."
           :transport transport
           :cause nil))
  (let ((identifier (json-get request "id" :absent)))
    (when (eq identifier :absent)
      (error 'mcp-protocol-error
             :message "An MCP request requires a JSON-RPC identifier."
             :method (json-get request "method")
             :payload request))
    (let ((pending (make-instance 'mcp-stdio-pending-request)))
      (with-lock-held ((mcp-stdio-transport-state-lock transport))
        (when (gethash
               identifier
               (mcp-stdio-transport-pending-requests transport))
          (error 'mcp-protocol-error
                 :message
                 (format nil "Duplicate in-flight MCP request identifier ~S."
                         identifier)
                 :method (json-get request "method")
                 :payload nil))
        (setf (gethash
               identifier
               (mcp-stdio-transport-pending-requests transport))
              pending))
      (let ((deadline (mcp-stdio--deadline timeout)))
        (unwind-protect
             (progn
               (mcp-stdio--write-message
                transport request deadline timeout "JSON-RPC request")
               (mcp-stdio--take-response
                transport identifier pending deadline timeout))
          (with-lock-held ((mcp-stdio-transport-state-lock transport))
            (remhash
             identifier
             (mcp-stdio-transport-pending-requests transport))))))))

(defmethod mcp-transport-notify
    ((transport mcp-stdio-transport) notification timeout)
  "Write one stdio NOTIFICATION before its deadline."
  (unless (mcp-transport-open-p transport)
    (error 'mcp-transport-error
           :message "The MCP stdio transport is closed."
           :transport transport
           :cause nil))
  (mcp-stdio--write-message
   transport notification
   (mcp-stdio--deadline timeout)
   timeout
   "JSON-RPC notification")
  nil)

(-> mcp-stdio--close-stream (t) null)
(defun mcp-stdio--close-stream (stream)
  "Close STREAM when it is open, suppressing cleanup failures."
  (when (and stream (streamp stream) (open-stream-p stream))
    (handler-case
        (close stream :abort t)
      (error ()
        nil)))
  nil)

(-> mcp-stdio--process-group-alive-p (mcp-stdio-transport) boolean)
(defun mcp-stdio--process-group-alive-p (transport)
  "Return true when TRANSPORT's dedicated process group still has members."
  (and (mcp-stdio-transport-process-group-p transport)
       (mcp-stdio-transport-process-group-identifier transport)
       (handler-case
           (progn
             (sb-posix:kill
              (- (mcp-stdio-transport-process-group-identifier transport))
              0)
             t)
         (error ()
           nil))))

(-> mcp-stdio--wait-runtime-exit
    (mcp-stdio-transport t real)
    boolean)
(defun mcp-stdio--wait-runtime-exit (transport process timeout)
  "Wait until PROCESS and its dedicated group have both stopped."
  (let ((deadline (mcp-stdio--deadline timeout)))
    (loop
      (unless (or (handler-case
                      (uiop:process-alive-p process)
                    (error ()
                      nil))
                  (mcp-stdio--process-group-alive-p transport))
        (return t))
      (unless (plusp (mcp-stdio--remaining-seconds deadline))
        (return nil))
      (sleep 0.01))))

(-> mcp-stdio--signal-process
    (mcp-stdio-transport t keyword)
    null)
(defun mcp-stdio--signal-process (transport process signal)
  "Send SIGNAL to PROCESS or its dedicated process group."
  (let ((signal-number
          (ecase signal
            (:terminate sb-posix:sigterm)
            (:kill sb-posix:sigkill))))
    (labels ((signal-process ()
               "Signal the direct process when its group is not established."
               (handler-case
                   (uiop:terminate-process
                    process :urgent (eq signal ':kill))
                 (error ()
                   nil))))
      (if (and (mcp-stdio-transport-process-group-p transport)
               (mcp-stdio-transport-process-group-identifier transport))
          (handler-case
              (sb-posix:kill
               (- (mcp-stdio-transport-process-group-identifier transport))
               signal-number)
            (error ()
              (signal-process)))
          (signal-process))))
  nil)

(-> mcp-stdio--reap-process (t) null)
(defun mcp-stdio--reap-process (process)
  "Reap a stopped PROCESS without risking an unbounded wait."
  (unless (handler-case
              (uiop:process-alive-p process)
            (error ()
              nil))
    (handler-case
        (uiop:wait-process process)
      (error ()
        nil)))
  nil)

(-> mcp-stdio--join-thread (t real) boolean)
(defun mcp-stdio--join-thread (thread timeout)
  "Join THREAD within TIMEOUT, destroying a stuck reader after its streams close."
  (cond
    ((or (null thread) (eq thread (current-thread)))
     t)
    (t
     (let ((deadline (mcp-stdio--deadline timeout)))
       (loop while (thread-alive-p thread)
             while (plusp (mcp-stdio--remaining-seconds deadline))
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

(defmethod mcp-transport-close ((transport mcp-stdio-transport))
  "Terminate the server, drain its readers, and forget all external resources."
  (with-lock-held ((mcp-stdio-transport-state-lock transport))
    (setf (mcp-stdio-transport-closing-p transport) t
          (mcp-stdio-transport-callback-stopping-p transport) t
          (mcp-stdio-transport-callback-queue transport) nil)
    (condition-notify
     (mcp-stdio-transport-callback-condition-variable transport)))
  (let ((cleanup-failure nil))
    (let ((process (mcp-stdio-transport-process transport))
          (reader-thread (mcp-stdio-transport-reader-thread transport))
          (error-thread (mcp-stdio-transport-error-thread transport))
          (callback-thread (mcp-stdio-transport-callback-thread transport))
          (process-stopped-p t))
      (mcp-stdio--close-stream (mcp-stdio-transport-input transport))
      (when process
        (unless (mcp-stdio--wait-runtime-exit
                 transport process *mcp-stdio-graceful-exit-timeout*)
          (mcp-stdio--signal-process transport process ':terminate))
        (unless (mcp-stdio--wait-runtime-exit
                 transport process *mcp-stdio-terminate-timeout*)
          (mcp-stdio--signal-process transport process ':kill))
        (setf process-stopped-p
              (mcp-stdio--wait-runtime-exit
               transport process *mcp-stdio-kill-timeout*))
        (when process-stopped-p
          (mcp-stdio--reap-process process)))
      (mcp-stdio--close-stream (mcp-stdio-transport-output transport))
      (mcp-stdio--close-stream
       (mcp-stdio-transport-error-output transport))
      (mcp-stdio--join-thread
       reader-thread *mcp-stdio-thread-join-timeout*)
      (mcp-stdio--join-thread
       error-thread *mcp-stdio-thread-join-timeout*)
      (mcp-stdio--join-thread
       callback-thread *mcp-stdio-thread-join-timeout*)
      (unless process-stopped-p
        (setf cleanup-failure
              (make-condition
               'mcp-transport-error
               :message "The MCP stdio process group survived SIGKILL."
               :transport transport
               :cause nil))))
    (with-lock-held ((mcp-stdio-transport-state-lock transport))
      (let ((closure
              (or cleanup-failure
                  (mcp-stdio-transport-reader-failure transport)
                  (make-condition
                   'mcp-transport-error
                   :message "The MCP stdio transport closed."
                   :transport transport
                   :cause nil))))
        (setf (mcp-stdio-transport-process transport) nil
              (mcp-stdio-transport-process-group-p transport) nil
              (mcp-stdio-transport-process-group-identifier transport) nil
              (mcp-stdio-transport-input transport) nil
              (mcp-stdio-transport-output transport) nil
              (mcp-stdio-transport-error-output transport) nil
              (mcp-stdio-transport-reader-thread transport) nil
              (mcp-stdio-transport-error-thread transport) nil
              (mcp-stdio-transport-callback-thread transport) nil
              (mcp-stdio-transport-callback-queue transport) nil
              (mcp-stdio-transport-callback-stopping-p transport) t
              (mcp-stdio-transport-reader-failure transport) closure
              (mcp-stdio-transport-open-p transport) nil
              (mcp-stdio-transport-closing-p transport) nil))
      (maphash
       (lambda (identifier pending)
         (declare (ignore identifier))
         (condition-notify
          (mcp-stdio-pending-condition-variable pending)))
       (mcp-stdio-transport-pending-requests transport))
      (clrhash (mcp-stdio-transport-pending-requests transport)))
    (when cleanup-failure
      (error cleanup-failure)))
  nil)

(defmethod mcp-transport-detach ((transport mcp-stdio-transport))
  "Close inherited descriptors without signaling the parent-owned server."
  (with-lock-held ((mcp-stdio-transport-state-lock transport))
    (setf (mcp-stdio-transport-closing-p transport) t
          (mcp-stdio-transport-callback-stopping-p transport) t
          (mcp-stdio-transport-callback-queue transport) nil)
    (condition-notify
     (mcp-stdio-transport-callback-condition-variable transport)))
  (mcp-stdio--close-stream (mcp-stdio-transport-input transport))
  (mcp-stdio--close-stream (mcp-stdio-transport-output transport))
  (mcp-stdio--close-stream
   (mcp-stdio-transport-error-output transport))
  (mcp-stdio--join-thread
   (mcp-stdio-transport-reader-thread transport)
   *mcp-stdio-thread-join-timeout*)
  (mcp-stdio--join-thread
   (mcp-stdio-transport-error-thread transport)
   *mcp-stdio-thread-join-timeout*)
  (mcp-stdio--join-thread
   (mcp-stdio-transport-callback-thread transport)
   *mcp-stdio-thread-join-timeout*)
  (with-lock-held ((mcp-stdio-transport-state-lock transport))
    (setf (mcp-stdio-transport-process transport) nil
          (mcp-stdio-transport-process-group-p transport) nil
          (mcp-stdio-transport-process-group-identifier transport) nil
          (mcp-stdio-transport-input transport) nil
          (mcp-stdio-transport-output transport) nil
          (mcp-stdio-transport-error-output transport) nil
          (mcp-stdio-transport-reader-thread transport) nil
          (mcp-stdio-transport-error-thread transport) nil
          (mcp-stdio-transport-callback-thread transport) nil
          (mcp-stdio-transport-callback-queue transport) nil
          (mcp-stdio-transport-callback-stopping-p transport) t
          (mcp-stdio-transport-reader-failure transport)
          (make-condition
           'mcp-transport-error
           :message "The inherited MCP stdio transport detached."
           :transport transport
           :cause nil)
          (mcp-stdio-transport-open-p transport) nil
          (mcp-stdio-transport-closing-p transport) nil)
    (maphash
     (lambda (identifier pending)
       (declare (ignore identifier))
       (condition-notify
        (mcp-stdio-pending-condition-variable pending)))
     (mcp-stdio-transport-pending-requests transport))
    (clrhash (mcp-stdio-transport-pending-requests transport)))
  nil)

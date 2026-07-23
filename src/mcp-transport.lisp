(in-package #:mcparen)

;;;; -- Transport Protocol --

(defclass mcp-transport ()
  ()
  (:documentation "The lifecycle and request boundary of one MCP connection."))

(defgeneric mcp-transport-open (transport)
  (:documentation "Open TRANSPORT if necessary and return it."))

(defgeneric mcp-transport-open-p (transport)
  (:documentation "Return true when TRANSPORT can exchange messages."))

(defgeneric mcp-transport-request (transport request timeout)
  (:documentation
   "Send JSON-RPC REQUEST and return its matching response within TIMEOUT seconds."))

(defgeneric mcp-transport-notify (transport notification timeout)
  (:documentation "Send JSON-RPC NOTIFICATION within TIMEOUT seconds."))

(defgeneric mcp-transport-set-protocol-version (transport version)
  (:documentation "Record the negotiated MCP protocol VERSION on TRANSPORT."))

(defmethod mcp-transport-set-protocol-version
    ((transport mcp-transport) version)
  "Leave transports without version-specific headers unchanged."
  (declare (ignore version))
  transport)

(defgeneric mcp-transport-close (transport)
  (:documentation "Close TRANSPORT and release all external resources."))

(defgeneric mcp-transport-detach (transport)
  (:documentation
   "Forget inherited resources after a fork without signaling their owner."))

(defmethod mcp-transport-detach ((transport mcp-transport))
  "Leave transports without inherited resources unchanged."
  (declare (ignore transport))
  nil)

(defmacro with-open-mcp-transport ((variable transport) &body body)
  "Open TRANSPORT as VARIABLE for BODY, then close it during unwinding."
  `(let ((,variable (mcp-transport-open ,transport)))
     (unwind-protect
          (progn ,@body)
       (mcp-transport-close ,variable))))

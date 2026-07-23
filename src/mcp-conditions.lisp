(in-package #:mcparen)

;;;; -- MCP Conditions --

(define-condition mcp-error (error)
  ((message
    :initarg :message
    :reader mcp-error-message
    :type string
    :documentation "The bounded human-readable failure description."))
  (:report
   (lambda (condition stream)
     (write-string (mcp-error-message condition) stream)))
  (:documentation "The root condition for MCP client failures."))

(define-condition mcp-transport-error (mcp-error)
  ((transport
    :initarg :transport
    :reader mcp-transport-error-transport
    :type t
    :documentation "The transport that failed.")
   (cause
    :initarg :cause
    :initform nil
    :reader mcp-transport-error-cause
    :type t
    :documentation "The underlying implementation condition, when available."))
  (:documentation "A failure while opening, using, or closing an MCP transport."))

(define-condition mcp-protocol-error (mcp-error)
  ((method
    :initarg :method
    :initform nil
    :reader mcp-protocol-error-method
    :type t
    :documentation "The MCP method active at the point of failure.")
   (payload
    :initarg :payload
    :initform nil
    :reader mcp-protocol-error-payload
    :type t
    :documentation "The non-secret protocol payload that failed validation."))
  (:documentation "An invalid or unsupported MCP protocol exchange."))

(define-condition mcp-message-too-large (mcp-protocol-error)
  ((source
    :initarg :source
    :reader mcp-message-too-large-source
    :type string
    :documentation "The transport input whose configured bound was exceeded.")
   (limit
    :initarg :limit
    :reader mcp-message-too-large-limit
    :type integer
    :documentation "The maximum accepted character count."))
  (:documentation "An inbound MCP document exceeded its configured size bound."))

(define-condition mcp-rpc-error (mcp-protocol-error)
  ((code
    :initarg :code
    :reader mcp-rpc-error-code
    :type integer
    :documentation "The JSON-RPC error code.")
   (data
    :initarg :data
    :initform nil
    :reader mcp-rpc-error-data
    :type t
    :documentation "Optional structured JSON-RPC error data."))
  (:documentation "A JSON-RPC error response returned by an MCP server."))

(define-condition mcp-timeout (mcp-transport-error)
  ((operation
    :initarg :operation
    :reader mcp-timeout-operation
    :type string
    :documentation "The operation that exceeded its deadline.")
   (seconds
    :initarg :seconds
    :reader mcp-timeout-seconds
    :type real
    :documentation "The configured timeout in seconds."))
  (:documentation "An MCP transport operation exceeded its deadline."))

(define-condition mcp-session-expired (mcp-transport-error)
  ()
  (:documentation
   "A stateful Streamable HTTP session expired and must be initialized again."))

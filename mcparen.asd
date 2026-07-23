(asdf:defsystem #:mcparen
  :description "A small Model Context Protocol client for Common Lisp."
  :author "Lukáš Hozda"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads
               #:dexador
               #:sb-posix
               #:serapeum
               #:yason)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "mcp-conditions")
                             (:file "json")
                             (:file "mcp-transport")
                             (:file "mcp-http")
                             (:file "mcp-stdio")
                             (:file "mcp-client"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:mcparen/tests))))

(asdf:defsystem #:mcparen/tests
  :description "Tests for Mcparen."
  :depends-on (#:mcparen)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "mcp-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:mcparen '#:run-tests)))

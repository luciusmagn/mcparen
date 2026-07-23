(require '#:asdf)

(defun require-local-system-definition (name expected-source root)
  "Require ASDF system NAME to come from EXPECTED-SOURCE below ROOT."
  (let ((source
          (asdf:system-source-file
           (asdf:find-system name))))
    (unless (and source
                 (equal (truename source)
                        (truename expected-source))
                 (uiop:subpathp source root))
      (error "ASDF resolved ~A to ~A instead of local source ~A."
             name source expected-source))))

(let* ((script (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script))
       (root (uiop:pathname-parent-directory-pathname script-directory))
       (system-definition (merge-pathnames "mcparen.asd" root))
       (quicklisp (merge-pathnames "quicklisp/setup.lisp"
                                   (user-homedir-pathname))))
  (when (uiop:file-exists-p quicklisp)
    (load quicklisp :verbose nil :print nil))
  (asdf:clear-system '#:mcparen/tests)
  (asdf:clear-system '#:mcparen)
  (asdf:load-asd system-definition)
  (require-local-system-definition '#:mcparen system-definition root)
  (require-local-system-definition '#:mcparen/tests system-definition root)
  (let ((warnings nil))
    (handler-bind
        ((warning
           (lambda (condition)
             (unless (typep condition 'style-warning)
               (push (princ-to-string condition) warnings)))))
      (asdf:load-system
       '#:mcparen/tests
       :force '(#:mcparen #:mcparen/tests)))
    (when warnings
      (error "Local Mcparen compilation emitted warnings:~%~{  ~A~%~}"
             (nreverse warnings))))
  (asdf:test-system :mcparen))

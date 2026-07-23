(require '#:asdf)

(let* ((script (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script))
       (root (uiop:pathname-parent-directory-pathname script-directory))
       (quicklisp (merge-pathnames "quicklisp/setup.lisp"
                                   (user-homedir-pathname))))
  (when (uiop:file-exists-p quicklisp)
    (load quicklisp :verbose nil :print nil))
  (asdf:load-asd (merge-pathnames "mcparen.asd" root))
  (if (find-package '#:ql)
      (uiop:symbol-call '#:ql '#:quickload :mcparen/tests :silent t)
      (asdf:load-system :mcparen/tests))
  (asdf:test-system :mcparen))

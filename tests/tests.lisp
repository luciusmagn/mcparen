(in-package #:mcparen)

;;;; -- Test Runner --

(-> run-tests () boolean)
(defun run-tests ()
  "Run every registered Mcparen test and signal when any test fails."
  (let ((failures nil)
        (tests (reverse *test-cases*)))
    (dolist (test tests)
      (destructuring-bind (name function) test
        (format t "~&~A ... " name)
        (finish-output)
        (handler-case
            (progn
              (funcall function)
              (format t "pass~%"))
          (error (condition)
            (push (cons name condition) failures)
            (format t "FAIL~%  ~A~%" condition)))))
    (when failures
      (error "~D of ~D Mcparen tests failed:~%~{  ~A: ~A~%~}"
             (length failures)
             (length tests)
             (loop for (name . condition) in (nreverse failures)
                   append (list name condition))))
    (format t "~&All ~D Mcparen tests passed.~%" (length tests))
    t))

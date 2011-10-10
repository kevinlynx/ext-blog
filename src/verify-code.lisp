;;;;
;;;; verify-code.lisp
;;;; Kevin Lynx
;;;; 9.15.2011
;;;;
(in-package #:ext-blog)

(defun generate-verify-code ()
  (flexi-streams:with-output-to-sequence (stream)
    (setf (hunchentoot:session-value :verify-code) 
          (kl-verify:generate-verify-code stream 75 20 4 'exp))))

(defun auth-verify-code (str)
  (string-equal (hunchentoot:session-value :verify-code) str))


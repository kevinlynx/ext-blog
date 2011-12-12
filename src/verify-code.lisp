;;;; verify-code.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(defun generate-verify-code ()
  (flexi-streams:with-output-to-sequence (stream)
    (setf (hunchentoot:session-value :verify-code) 
          (kl-verify:generate-verify-code stream 75 20 4 'exp))))

(defun auth-verify-code (str)
  (string-equal (hunchentoot:session-value :verify-code) str))


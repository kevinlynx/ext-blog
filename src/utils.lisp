;;;; utils.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(format-timestring
          root-pathname theme-pathname string-empty))

(defun root-pathname ()
  (asdf:component-pathname (asdf:find-system '#:ext-blog)))

(defun theme-pathname ()
  (merge-pathnames "theme/" (root-pathname)))

(defun src-pathname ()
  (merge-pathnames "src/" (root-pathname)))

(defun string-empty (s)
  (or (null s)
      (= (length s) 0)))

(defun get-future-time (secs)
  (+ (get-universal-time) secs))

(defparameter *the-random-state* (make-random-state t))

(defun create-random-string (&optional (n 10) (base 16))
  "Returns a random number \(as a string) with base BASE and N digits."
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base
              (random base *the-random-state*)))))

(defun format-timestring (time)
  "Format a time to a string by `local-time` library"
  (local-time:format-timestring 
    nil 
    (local-time:universal-to-timestamp time)
    :format local-time:+asctime-format+))

(defun read-binary-file (name)
  (with-open-file (in name :element-type '(unsigned-byte 8))
    (let ((ret (make-array (file-length in) 
                           :element-type '(unsigned-byte 8))))
      (read-sequence ret in)
      ret)))

(defclass nil-route (routes:proxy-route) ())

(defmethod routes:route-check-conditions ((route nil-route) bindings)
  t)

(defun @nil-route-require (route)
  (make-instance 'nil-route :target route))


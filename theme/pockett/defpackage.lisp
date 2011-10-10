;;;;
;;;; PocketT theme, ported by Kevin Lynx for ext-blog.
;;;; 6.8.2011
;;;; defpackage.lisp
;;;;
(defpackage #:ext-blog.theme.pockett
  (:use common-lisp ext-blog))

(in-package #:ext-blog.theme.pockett)

(defclass theme () ())

(defparameter *theme-inst* (make-instance 'theme))

(defun pockett-root-pathname ()
  (merge-pathnames "pockett/" (theme-pathname)))

(defmethod ext-blog:theme-name ((theme theme))
  "PocketT")

(defmethod ext-blog:theme-desc ((theme theme))
  "Designed by Nyssa Brown, ported by Kevin Lynx")

(defmethod ext-blog:theme-update ((theme theme))
  (closure-template:compile-template 
    :common-lisp-backend
    (merge-pathnames "pockett.tmpl" (pockett-root-pathname))))

(defmethod ext-blog:theme-type ((theme theme))
  :normal)

(defmethod ext-blog:theme-resources ((theme theme))
  '("pockett/style.css" "pockett/images/"))

(ext-blog:theme-update *theme-inst*)


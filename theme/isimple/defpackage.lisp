;;;;
;;;; iSimple theme, ported by Kevin Lynx for ext-blog.
;;;; 6.12.2011
;;;; defpackage.lisp
;;;;
(defpackage #:ext-blog.theme.isimple
  (:use common-lisp ext-blog))

(in-package #:ext-blog.theme.isimple)

(defclass theme () ())

(defparameter *theme-inst* (make-instance 'theme))

(defun isimple-root-pathname ()
  (merge-pathnames "isimple/" (theme-pathname)))

(defmethod ext-blog:theme-name ((theme theme))
  "iSimple")

(defmethod ext-blog:theme-desc ((theme theme))
  "iSimple theme, ported by Kevin Lynx")

(defmethod ext-blog:theme-update ((theme theme))
  (closure-template:compile-template 
    :common-lisp-backend
    (merge-pathnames "isimple.tmpl" (isimple-root-pathname))))

(defmethod ext-blog:theme-type ((theme theme))
  :normal)

(defmethod ext-blog:theme-resources ((theme theme))
  '("isimple/style.css"
      "isimple/css/"
      "isimple/js/"
      "isimple/img/"))

(ext-blog:theme-update *theme-inst*)


;;;;
;;;; Default admin theme.
;;;; 6.8.2011
;;;; defpackage.lisp
;;;;
(defpackage #:ext-blog.theme.default-admin
  (:use common-lisp ext-blog))

(in-package #:ext-blog.theme.default-admin)

(defparameter *theme-inst* 'default-admin)

(defun admin-root-pathname ()
  (merge-pathnames "default-admin/" (theme-pathname)))

(defmethod ext-blog:theme-name ((theme (eql *theme-inst*)))
  "Default admin")

(defmethod ext-blog:theme-desc ((theme (eql *theme-inst*)))
  "Ported by Kevin Lynx")

(defmethod ext-blog:theme-update ((theme (eql *theme-inst*)))
  (closure-template:compile-template 
    :common-lisp-backend
    (merge-pathnames "admin.tmpl" (admin-root-pathname))))

(defmethod ext-blog:theme-type ((theme (eql *theme-inst*)))
  :admin)

(defmethod ext-blog:theme-resources ((theme (eql *theme-inst*)))
  '("default-admin/css/" "default-admin/js/" "default-admin/images/"
    "default-admin/style/default/"
    "default-admin/style/default/images/"
    "default-admin/editor/*/*/"
    "default-admin/editor/*/"
    "default-admin/editor/" "default-admin/style/"))

(ext-blog:theme-update *theme-inst*)


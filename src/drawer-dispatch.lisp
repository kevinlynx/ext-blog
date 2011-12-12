;;;; drawer-dispatcher.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(render-page))

(defclass drawer-dispatcher() ())

(defgeneric render-page (blog theme route args) 
  (:documentation "Render blog page by the blog theme"))

(defun get-theme (blog data)
  (if blog
    (let ((type (getf data :type)))
      (case type
        (:admin (blog-admin-theme blog))
        (:normal (blog-theme blog))
        (:api nil)))
    ;; the blog has not been initialized, so we use a temp theme.
    ;; but if there's no :admin theme, it will crash.
    (get-default-theme :admin)))

(defmethod restas:render-object ((drawer drawer-dispatcher) (data list))
  (let* ((blog (getf data :blog))
         (theme (get-theme blog data)))
    (if theme
      (render-page blog theme (restas:route-symbol restas:*route*) (getf data :args))
      (car (getf data :args)))))

(setf *default-render-method* (make-instance 'drawer-dispatcher))


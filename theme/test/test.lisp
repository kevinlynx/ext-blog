
(in-package #:ext-blog.theme.test)

(export '(+theme-inst+ theme))

(defclass theme () ())

(defparameter *theme-inst* (make-instance 'theme))

(defmethod ext-blog:theme-name ((inst theme))
  (format t "ext-blog.theme.test:theme-name~%")
  "test")

(defmethod ext-blog:theme-update ((inst theme))
  (format t "ext-blog.theme.test:update"))

(defmethod ext-blog:theme-desc ((inst theme))
  "This is a test theme")

(defmethod ext-blog:theme-type ((inst theme))
  :test-theme)


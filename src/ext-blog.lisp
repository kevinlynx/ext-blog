;;;;
;;;; ext-blog.lisp
;;;; Kevin Lynx
;;;; 6.4.2011
;;;;
(in-package #:ext-blog)

(export '(start))

(defun set-log ()
  (let ((access-log-path "log/ext-blog-access-log")
        (message-log-path "log/ext-blog-message-log"))
    (ensure-directories-exist access-log-path)
    (ensure-directories-exist message-log-path)
    (setf hunchentoot:*access-log-pathname* access-log-path)
    (setf hunchentoot:*message-log-pathname* message-log-path)))

(defun start (&key (port 8080))
  (kl-verify:load-font "data/wenquanyi_12ptb.pcf")
  (load-themes)
  (mount-file-publisher)
  (load-blog)
  (set-log)
  (xml-rpc-methods:set-metaweblog-api)
  (restas:start 'ext-blog :port port))

;;; If use this, sometimes the file publisher will not work, wired bug
#|
(restas:define-initialization (context)
  (declare (ignore context))
  (load-themes)
  (mount-file-publisher)
  (load-blog)
  (set-log)
  (xml-rpc-methods:set-metaweblog-api))
|#


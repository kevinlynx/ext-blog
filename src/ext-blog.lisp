;;;; ext-blog.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(start))

(defun get-acceptor (port)
  (find port restas::*acceptors* :key #'hunchentoot:acceptor-port))

(defun set-log (acceptor)
  (let ((access-log-path (merge-pathnames "ext-blog-access.log" *log-path*))
        (message-log-path (merge-pathnames "ext-blog-message.log" *log-path*)))
    (ensure-directories-exist access-log-path)
    (ensure-directories-exist message-log-path)
    (setf (hunchentoot:acceptor-access-log-destination acceptor) access-log-path)
    (setf (hunchentoot:acceptor-message-log-destination acceptor) message-log-path)))

(defun start (&key (port 8080))
  (when (probe-file *font-path*)
      (kl-verify:load-font *font-path*))
  (load-themes)
  (mount-file-publisher)
  (load-blog)
  (xml-rpc-methods:set-metaweblog-api)
  (restas:start 'ext-blog :port port)
  (set-log (get-acceptor port)))

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


;;;; ext-blog.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(start))

(defparameter *access-log-path* 
  (and *enable-log* (merge-pathnames "ext-blog-access.log" *log-path*)))
(defparameter *message-log-path* 
  (and *enable-log* (merge-pathnames "ext-blog-message.log" *log-path*)))

(defun create-log-path ()
  (when *enable-log*
    (ensure-directories-exist *access-log-path*)
    (ensure-directories-exist *message-log-path*)))

(defclass ext-blog-acceptor (restas:restas-acceptor)
  ()
  (:default-initargs
   :access-log-destination *access-log-path*
   :message-log-destination *message-log-path*))

(defun start (&key (port 8080))
  (when (probe-file *font-path*)
      (kl-verify:load-font *font-path*))
  (load-themes)
  (mount-file-publisher)
  (load-blog)
  (xml-rpc-methods:set-metaweblog-api)
  (create-log-path)
  (restas:start 'ext-blog :port port :acceptor-class 'ext-blog-acceptor))


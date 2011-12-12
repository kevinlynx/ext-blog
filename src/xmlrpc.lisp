;;;; xmlrpc.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:xml-rpc-methods)

(defparameter *blog* nil)

(defun handle-metaweblog-request (blog)
  "Handle metaweblog http request."
  (setf *blog* blog)
  (let ((stream (hunchentoot:raw-post-data :want-stream t)))
    (s-xml-rpc::handle-xml-rpc-call stream 0)))

(defun set-metaweblog-api ()
  (setf s-xml-rpc:*xml-rpc-package* (find-package :xml-rpc-methods)))

(defun media-object-url (url)
  (s-xml-rpc:xml-rpc-struct "url" url))

(defun save-media-object (media-obj)
  (let* ((name (get-xml-rpc-struct-member media-obj :|name|))
         (path (merge-pathnames name ext-blog:*entry-static-path*)))
    (ensure-directories-exist path)
    (with-open-file (file path :element-type '(unsigned-byte 8)
                          :direction :output)
      (write-sequence (get-xml-rpc-struct-member media-obj :|bits|) file))
    (concatenate 'string ext-blog:*entry-static* name)))

(defmacro check-auth-user (user password method s)
  `(unless (ext-blog:user-auth (ext-blog::blog-user *blog*) ,user ,password)
     (format t "Auth user (~a:~a) failed in ~a~%" ,user ,password ,s)
     (return-from ,method)))

(defun |metaWeblog.newPost| (blogid username password post publish)
  (declare (ignore blogid publish))
  (check-auth-user username password |metaWeblog.newPost| "newPost")
  (ext-blog::create-post *blog* 
                         (s-xml-rpc:get-xml-rpc-struct-member post :|title|)
                         (s-xml-rpc:get-xml-rpc-struct-member post :|description|)))

(defun |metaWeblog.newMediaObject| (blogid username password media-obj)
  (declare (ignore blogid))
  (check-auth-user username password |metaWeblog.newMediaObject| "newMediaObject")
  (let ((url (save-media-object media-obj)))
    (media-object-url url)))


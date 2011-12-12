;;;; post.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(post post-id post-time post-revised-time post-title post-content))

(defclass post ()
  ((id :initform nil
       :initarg :id
       :accessor post-id)
   (time :initform nil
         :initarg :time
         :accessor post-time)
   (revised-time :initform nil
                 :accessor post-revised-time)
   (title :initform nil
          :initarg :title
          :accessor post-title)
   (content :initform nil
            :initarg :content
            :accessor post-content))
  (:documentation "A post object is also called an entry."))

(defvar *post-store-path* (merge-pathnames "posts.store" *store-path*))

(defun gen-post-id (posts)
  (let ((ids (mapcar #'post-id posts)))
    (if ids
      (1+ (reduce #'max ids))
      0)))

(defgeneric store-posts (posts))

(defgeneric load-posts ())

(defmethod store-posts (posts)
 (let ((path *post-store-path*))
   (ensure-directories-exist path)
   (cl-store:store posts path)))

(defmethod load-posts ()
 (let ((path *post-store-path*))
   (when (probe-file path)
     (cl-store:restore path))))


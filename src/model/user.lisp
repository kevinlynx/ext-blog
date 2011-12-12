;;;; user.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(create-user load-user store-user 
          user-auth user-uid user-password))

(defclass user ()
  ((uid :initform nil
        :initarg :uid
        :accessor user-uid)
   (tokens :initform nil
           :accessor user-tokens)
   (password :initform nil
             :initarg :password
             :accessor user-password))
  (:documentation "Represents an admin for a blog"))

(defgeneric load-user ())

(defgeneric store-user (user))

(defun create-user (id pwd)
  "Create a new user."
  (make-instance 'user
                 :uid id
                 :password pwd))

(defun user-auth (user id pwd)
  "Auth a user, if ok return t"
  (and (string= id (user-uid user))
       (string= pwd (user-password user))))

(defun add-user-token (user token)
  (pushnew token (user-tokens user) :test #'string=))

(defun remove-user-token (user token)
  (setf (user-tokens user) (remove-if #'(lambda (tok) (string= tok token)) 
                                      (user-tokens user))))

(defun valid-token-p (user token)
  (find-if #'(lambda (tok) (string= tok token)) (user-tokens user)))

(defvar *user-store-path* (merge-pathnames "users.store" *store-path*))

(defmethod load-user ()
 (let ((path *user-store-path*))
   (when (probe-file path)
     (cl-store:restore path))))

(defmethod store-user (user)
 (let ((path *user-store-path*))
   (ensure-directories-exist path)
   (cl-store:store user path)))


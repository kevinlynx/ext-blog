;;;; auth.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

;;; cookie stuff
(defconstant +cookie-save-time+ (* 60 60 24 30))

(defun set-cookie (user)
  "Set a response cookie to client"
  (unless (cookie "user")
    (let ((token (create-random-string)))
      (add-user-token user token)
      (hunchentoot:set-cookie "remember-me" 
                              :value "1"
                              :expires (get-future-time +cookie-save-time+))
      (hunchentoot:set-cookie "user"
                              :value token
                              :expires (get-future-time +cookie-save-time+)))))

;;; Set 'expires' filed to 0 to delete the cookie in browser.
(defun delete-cookie (user)
  "Delete the cookie"
  (remove-user-token user (cookie "user"))
  (hunchentoot:set-cookie "remember-me" :expires 0)
  (hunchentoot:set-cookie "user" :expires 0))

(defun authed-p (user)
  "Check whether the current session has been authed"
  (let ((authp (hunchentoot:session-value :auth-p)))
    (if authp
      t
      (if (valid-token-p user (cookie "user"))
        (setf (hunchentoot:session-value :auth-p) t)
        nil))))

(defun check-login (dest-url login-url &optional (user (blog-user *blog*)))
  "Check whether it can go to the dest-url, if not, redirect to the login page, and
  store the dest-url in the session. When the user has logined success, redirect him
  to the dest-url."
  (if (not (authed-p user))
    (progn
      (setf (hunchentoot:session-value :dest-url) dest-url)
      (restas:redirect login-url)
      nil)
    t))

(defun do-login (id pwd &optional (user (blog-user *blog*)))
  "Do login process, check whether the password is correct"
  (user-auth user id pwd))

(defun on-login (remember default-url &optional (user (blog-user *blog*)))
  "Login success, store the flag in session, and if has dest-url, redirect to it."
  (setf (hunchentoot:session-value :auth-p) t)
  (when remember
    (set-cookie user))
  (restas:redirect (or (hunchentoot:session-value :dest-url) default-url)))

(defun on-logout (index-url &optional (user (blog-user *blog*)))
  "Logout, clear session"
  (setf (hunchentoot:session-value :auth-p) nil)
  (setf (hunchentoot:session-value :dest-url) nil)
  (delete-cookie user)
  (restas:redirect index-url))


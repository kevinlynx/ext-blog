;;;;
;;;; params.lisp
;;;; Kevin Lynx
;;;; 6.9.2011
;;;;
(in-package #:ext-blog)

(defun params (name)
  (hunchentoot:post-parameter name))

(defun params-list (name)
  (let ((ret nil))
    (mapcar #'(lambda (v)
                (when (string= (car v) name)
                  (setf ret (cons (cdr v) ret))))
            (hunchentoot:post-parameters*))
    ret))

;;; If the browser has cookie, it will send the cookie in every request.
(defun cookie (name)
  (let ((val (hunchentoot:cookie-in name)))
   (format t "cookie: ~a~%" val)
   val))


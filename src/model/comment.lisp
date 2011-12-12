;;;;
;;;; comment.lisp
;;;; Kevin Lynx
;;;; 6.4.2011
;;;;
(in-package #:ext-blog)

(export '(comment comment-id comment-post-id comment-time comment-author
          comment-email comment-url comment-content))

(defclass comment ()
  ((id :initform nil
       :initarg :id
       :accessor comment-id)
   (post-id :initform nil
            :initarg :post-id
            :accessor comment-post-id)
   (time :initform nil
         :initarg :time
         :accessor comment-time)
   (author :initform nil
           :initarg :author
           :accessor comment-author)
   (email :initform nil
          :initarg :email
          :accessor comment-email)
   (url :initform nil
        :initarg :url
        :accessor comment-url)
   (content :initform nil
            :initarg :content
            :accessor comment-content)))

(defgeneric store-comments (comments))

(defgeneric load-comments ())

(defvar *comment-store-path* (merge-pathnames "comments.store" *store-path*))

(defun gen-comment-id (comments)
  (let ((ids (mapcar #'comment-id comments)))
    (if ids
      (1+ (reduce #'max ids))
      0)))

(defmethod store-comments (comments)
 (let ((path *comment-store-path*))
   (ensure-directories-exist path)
   (cl-store:store comments path)))

(defmethod load-comments ()
 (let ((path *comment-store-path*))
   (when (probe-file path)
     (cl-store:restore path))))

(defun get-comments-by-post (comments post-id)
  (remove-if-not #'(lambda (comment) 
                     (= post-id (comment-post-id comment)))
                 comments))


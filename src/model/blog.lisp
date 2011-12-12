;;;; blog.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(blog get-1st-post get-nth-post 
               get-posts-by-page
               get-min-page-id
               get-max-page-id
               get-post-comments get-post-comments-cnt
               blog-author blog-title blog-sub-title blog-comments blog-theme blog-posts))

(defclass blog ()
  ((user :initform nil
         :initarg :user
         :accessor blog-user)
   (title :initform nil
          :initarg :title
          :accessor blog-title)
   (sub-title :initform nil
              :initarg :sub-title
              :accessor blog-sub-title)
   (posts :initform nil
          :accessor blog-posts)
   (comments :initform nil
             :accessor blog-comments)
   (admin-theme :initform nil
                :initarg :admin-theme
                :accessor blog-admin-theme)
   (footer-html :initform nil
                :initarg :footer-html
                :accessor blog-footer-html)
   (theme :initform nil
          :initarg :theme
          :accessor blog-theme))
  (:documentation "Represents a blog, a blog object has a list of
   posts and comments, and other stuff."))

(defvar *blog* nil)

(defun blog-author (blog)
  "Get the blog author name"
  (user-uid (blog-user blog)))

(defun blog-initialized-p ()
  "Check whether the blog has been initialized"
  (and *blog* (blog-user *blog*)))

(defun get-1st-post (blog)
  "Get the first post, usually the recent post."
  (first (blog-posts blog)))

(defun get-nth-post (blog id)
  "Get a post by its id."
  (find id (blog-posts blog) :key #'post-id))

(defun get-recent-posts (blog cnt)
  "Get the most recent count posts"
  (let* ((posts (blog-posts blog))
         (size (length posts)))
    (if posts
      (subseq posts 0
              (if (> cnt size) size cnt))
      nil)))

(defun get-posts-by-page (blog page-cnt id) 
  "Get the nth page of posts, id starts from 0"
  (let* ((posts (blog-posts blog))
         (index (* page-cnt id))
         (size (length posts))
         (want (if (>= (- size index) page-cnt) page-cnt (- size index))))
    (if (< index size)
      (subseq posts index (+ index want))
      nil)))

(defun get-min-page-id (blog page-cnt) 
  (declare (ignore blog page-cnt))
  0)

(defun get-max-page-id (blog page-cnt)
  (floor (/ (1- (length (blog-posts blog))) page-cnt)))

(defun get-recent-comments (blog cnt)
  "Get the most recent count comments"
  (let* ((comments (blog-comments blog))
         (size (length comments)))
    (if comments
      (subseq comments 0
              (if (> cnt size) size cnt))
      nil)))

(defun create-post (blog title content &key 
                         (time (get-universal-time))
                         (id (gen-post-id (blog-posts blog))))
  "Create a post object and store it"
  (setf (blog-posts blog) 
        (cons 
          (make-instance 'post
                         :title title
                         :content content
                         :id id
                         :time time)
          (blog-posts blog)))
  (store-posts (blog-posts blog))
  id)

(defun edit-post (blog id title content)
  "Edit a post by its id"
  (let ((post (get-nth-post blog id)))
    (if post
      (progn
        (setf (post-title post) title)
        (setf (post-content post) content)
        (store-posts (blog-posts blog))
        id)
      nil)))

(defun create-comment (blog post-id author email url content &key 
                            (time (get-universal-time))
                            (id (gen-comment-id (blog-comments blog))))
 "Create a comment object, associate with a post"
 (when (string-empty url) (setf url nil))
 (setf (blog-comments blog) 
       (cons 
         (make-instance 'comment
                        :id id
                        :post-id post-id
                        :author author
                        :email email
                        :url url
                        :content content
                        :time time)
         (blog-comments blog)))
 (store-comments (blog-comments blog)))

(defun delete-comment (blog id)
  "Delete a comment by its id"
  (setf (blog-comments blog)
        (delete id (blog-comments blog) :key #'comment-id))
  (store-comments (blog-comments blog)))

(defun create-blog (author pwd title subtitle theme admin-theme)
  "Create a blog object."
  (setf *blog* 
        (make-instance 'blog
                       :user (create-user author pwd)
                       :title title
                       :sub-title subtitle
                       :theme theme
                       :admin-theme admin-theme)))

(defun get-post-comments (blog post)
  "Get all comments associated with the post."
  (get-comments-by-post (blog-comments blog) (post-id post)))

(defun get-post-comments-cnt (blog post)
  "Get comment count associated with the post."
  (length (get-post-comments blog post)))

(defun delete-post (blog id)
  "Delete a blog by its id, it will also delete all comments associated
  with this post."
  (let ((post (get-nth-post blog id)))
    (when post
      (let ((comments (get-post-comments blog post)))
        (mapc #'(lambda (c) 
                  (setf (blog-comments blog) (remove c (blog-comments blog))))
              comments)
        (store-comments (blog-comments blog)))
      (setf (blog-posts blog) (remove post (blog-posts blog)))
      (store-posts (blog-posts blog)))))


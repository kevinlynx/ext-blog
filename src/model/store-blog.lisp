;;;; store-blog.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(defun alist-field (alist key)
  (cdr (assoc key alist)))

(defun blog->alist (blog)
  "Convert a blog object to an assoc-list, to store."
  (list (cons :title (blog-title blog))
        (cons :sub-title (blog-sub-title blog))
        (cons :admin-theme (theme-name (blog-admin-theme blog)))
        (cons :footer-html (blog-footer-html blog))
        (cons :theme (theme-name (blog-theme blog)))))

(defun alist->blog (alist)
  "Convert an assoc-list back to a blog object"
  (make-instance 
    'blog :title (alist-field alist :title)
    :sub-title (alist-field alist :sub-title)
    :admin-theme (find-theme (alist-field alist :admin-theme))
    :footer-html (alist-field alist :footer-html)
    :theme (find-theme (alist-field alist :theme))))

(defvar *blog-store-path* (merge-pathnames "blog.store" *store-path*))

(defun load-blog ()
  "Load a blog from storage, it will load posts and comments either."
  (let ((path *blog-store-path*))
    (when (probe-file path)
      (setf *blog* (alist->blog (cl-store:restore path)))))
  (when *blog*
    (setf (blog-user *blog*) (load-user))
    (setf (blog-posts *blog*) (load-posts))
    (setf (blog-comments *blog*) (load-comments))))

(defun store-blog (&optional (blog *blog*))
  "Store a blog object."
  (when blog
    (let ((path *blog-store-path*))
      (ensure-directories-exist path)
      (cl-store:store (blog->alist blog) path)))
  (store-user (blog-user blog))
  (store-posts (blog-posts blog))
  (store-comments (blog-comments blog)))


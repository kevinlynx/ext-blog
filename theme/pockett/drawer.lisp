;;;;
;;;; PocketT theme, ported by Kevin Lynx for ext-blog.
;;;; 6.8.2011
;;;; drawer.lisp
;;;;
(in-package #:ext-blog.theme.pockett)

(defgeneric render-content (blog route args)
  (:documentation "Generic function to render page content."))

(defun get-post-by-args (blog args)
  "Get a post object by args"
  (let ((id (car args)))
   (if id 
     (or (get-nth-post blog id)
         (get-1st-post blog))
     (get-1st-post blog))))

(defun render-comment (blog route args)
 (if (has-post-p blog)
   (if (and (not (eql route 'index))
            (not (eql route 'view)))
     nil
     (pockett.view:comment
       (let ((post (get-post-by-args blog args)))
         (list :comment_cnt (get-post-comments-cnt blog post)
               :comments (get-post-comments-data blog post)
               :postid (post-id post)))))
   nil))

(defmethod ext-blog:render-page (blog (theme theme) route args)
  (let ((content (render-content blog route args))
        (comment (render-comment blog route args))
        (header (pockett.view:header (get-head-data blog)))
        (footer (pockett.view:footer (get-footer-data blog))))
    (pockett.view:finalize-page 
      (list :title (blog-title blog)
            :comment comment
            :header header
            :content content
            :footer footer))))

(defmethod render-content (blog (route (eql 'index)) args)
  (pockett.view:content
    (if (has-post-p blog)
      (get-post-data blog (get-1st-post blog))
      (list :title "None" :content "No entry"))))

(defmethod render-content (blog (route (eql 'view)) args)
  (let* ((post (get-post-by-args blog args)))
     (pockett.view:content
       (if post
         (get-post-data blog post)
         (list :title "None" :content "No entry")))))


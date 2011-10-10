;;;;
;;;; iSimple theme, ported by Kevin Lynx for ext-blog.
;;;; 6.12.2011
;;;; drawer.lisp
;;;;
(in-package #:ext-blog.theme.isimple)

(defconstant +page-cnt+ 3)

(defgeneric render-content (blog route args)
  (:documentation "Generic function to render page content."))

(defun get-post-by-args (blog args)
  "Get a post object by args"
  (let ((id (car args)))
   (if id 
     (or (get-nth-post blog id)
         (get-1st-post blog))
     (get-1st-post blog))))

(defun get-post-title-by-args (blog args)
  (let ((id (car args))
        (post))
    (when id
      (setf post (get-nth-post blog id)))
    (if post
      (concatenate 'string (post-title post) " - " (blog-title blog))
      (blog-title blog))))

(defun render-comment (blog route args)
 (if (has-post-p blog)
   (if (and (not (eql route 'index))
            (not (eql route 'view)))
     nil
     (isimple.view:comment
       (let ((post (get-post-by-args blog args)))
         (list :comment_cnt (get-post-comments-cnt blog post)
               :comments (get-post-comments-data blog post)
               :postid (post-id post)))))
   nil))

(defun render-sidebar (blog route args)
  (declare (ignore route args))
  (isimple.view:sidebar 
    (list :posts (get-recent-posts-brief blog 5)
          :comments (get-recent-comments-brief blog 5)
          :feedurl (ext-blog:make-full-feed-url))))

(defmethod ext-blog:render-page (blog (theme theme) route args)
  (let ((content (render-content blog route args))
        (sidebar (render-sidebar blog route args))
        (header (isimple.view:header (get-head-data blog)))
        (footer (isimple.view:footer (get-footer-data blog))))
    (isimple.view:finalize-page 
      (list :title (blog-title blog)
            :header header
            :sidebar sidebar
            :content content
            :footer footer))))

(defmethod ext-blog:render-page (blog (theme theme) (route (eql 'view)) args)
  (let ((content (render-content blog route args))
        (comment (render-comment blog route args))
        (sidebar (render-sidebar blog route args))
        (header (isimple.view:header (get-head-data blog)))
        (footer (isimple.view:footer (get-footer-data blog))))
    (isimple.view:finalize-page 
      (list :title (get-post-title-by-args blog args)
            :header header
            :sidebar sidebar
            :comment comment
            :content content
            :footer footer))))

(defmethod render-content (blog (route (eql 'index)) args)
  (declare (ignore args))
  (isimple.view:page
    (get-page-data blog +page-cnt+ 0)))

(defmethod render-content (blog (route (eql 'page)) args)
  (isimple.view:page
    (get-page-data blog +page-cnt+ (car args))))

(defmethod render-content (blog (route (eql 'view)) args)
  (let* ((post (get-post-by-args blog args)))
     (isimple.view:single
       (if post
         (get-post-data blog post)
         (list :title "None" :content "No entry")))))


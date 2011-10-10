;;;;
;;;; Default admin theme.
;;;; manage.lisp
;;;; Kevin Lynx
;;;; 6.6.2011
;;;;
(in-package #:ext-blog.theme.default-admin)

(defgeneric manage-render-content (blog route args))

(defmethod manage-render-content (blog (route (eql 'manage/new)) args)
  (default.admin.view:newpost nil))

(defmethod manage-render-content (blog (route (eql 'manage/comment)) args)
  (default.admin.view:comments 
    (list :comments (get-all-comments-data blog))))

(defmethod manage-render-content (blog (route (eql 'manage/post)) args)
  (default.admin.view:posts
    (list :posts (get-all-posts-data blog))))

(defmethod manage-render-content (blog (route (eql 'manage/post/edit/id)) args)
  (let ((post (get-nth-post blog (car args))))
    (if post
      (default.admin.view:editpost
        (list :postid (post-id post)
              :title (post-title post) :content (post-content post)))
      "Invalid post id")))

(defmethod manage-render-content (blog (route (eql 'manage/configure/blogger)) args)
  (default.admin.view:configure-blogger (get-blog-data blog)))

(defmethod manage-render-content (blog (route (eql 'manage/configure/profile)) args)
  (default.admin.view:configure-profile (get-author-data blog)))

(defun render-manage-page (blog route args)
  (let ((header (default.admin.view:header))
        (content (manage-render-content blog route args))
        (footer (default.admin.view:footer)))
    (default.admin.view:finalize-page 
      (list :header header
            :content content
            :footer footer))))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'login)) args)
  (default.admin.view:login))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'initialize)) args)
  (default.admin.view:initialize))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'manage)) args)
  (render-manage-page blog 'manage/comment args))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'manage/new)) args)
  (render-manage-page blog route args))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'manage/post)) args)
  (render-manage-page blog route args))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'manage/post/edit/id)) args)
  (render-manage-page blog route args))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'manage/comment)) args)
  (render-manage-page blog route args))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'manage/configure/blogger)) args)
  (render-manage-page blog route args))

(defmethod ext-blog:render-page (blog (theme (eql *theme-inst*)) (route (eql 'manage/configure/profile)) args)
  (render-manage-page blog route args))


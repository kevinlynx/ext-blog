;;;;
;;;; blog-data.lisp
;;;; This is a helper file, to provide some util functions to write a theme.
;;;; Kevin Lynx
;;;; 6.8.2011
;;;;
(in-package #:ext-blog)

(export '(get-post-time-desc
          get-post-data
          get-page-posts-data 
          get-page-data
          has-post-p
          get-blog-data
          get-title-data
          get-head-data
          get-page-nav-url
          get-themes-data
          get-footer-data
          get-author-data
          get-comment-data
          get-post-comments-data
          get-all-comments-data
          get-recent-comments-brief
          get-sys-info
          get-recent-posts-brief
          get-all-posts-data))

(defun get-sys-info ()
  (concatenate 'string "ext-blog " *ext-blog-version*))

(defun get-post-time-desc (post &optional (type :created))
  "Get the time string of the post"
  (if (eql type :created)
    (format-timestring (post-time post))
    (format-timestring (post-revised-time post))))

(defun get-post-data (blog post)
  "Get a post render data, used to render"
  (let* ((id (post-id post))
         (prev (get-nth-post blog (1+ id)))
         (next (get-nth-post blog (1- id)))
         (desc (list :title (post-title post)
                     :id (post-id post)
                     :author (blog-author blog)
                     :commentcnt (get-post-comments-cnt blog post)
                     :content (post-content post))))
   (append 
     (when prev
       (list :prevtitle (post-title prev)
             :prevlink (restas:genurl 'view :id (post-id prev))))
     (when next
       (list :nexttitle (post-title next)
             :nextlink (restas:genurl 'view :id (post-id next))))
     (list :postlink (restas:genurl 'view :id (post-id post))
           :createdtime (get-post-time-desc post :created))
     (when (post-revised-time post)
       (list :revisedtime (get-post-time-desc post)))
     desc)))

(defun get-page-nav-url (blog id page-cnt)
  (let ((min (get-min-page-id blog page-cnt))
        (max (get-max-page-id blog page-cnt))
        (prev (1- id))
        (next (1+ id)))
    (append 
      (when (>= prev min) 
        (list :prevpage (restas:genurl 'page :id prev)))
      (when (<= next max) 
        (list :nextpage (restas:genurl 'page :id next))))))
                             
(defun get-page-posts-data (blog page-cnt id)
  "Get the nth page posts data"
  (let ((posts (get-posts-by-page blog page-cnt id)))
    (if posts
      (loop for post in posts
            collect (get-post-data blog post))
      nil)))

(defun get-page-data (blog page-cnt id)
  (append (list :posts (get-page-posts-data blog page-cnt id))
          (get-page-nav-url blog id page-cnt)))

(defun has-post-p (blog)
  "Check whether the blog has posts"
  (not (null (blog-posts blog))))

(defun get-title-data (blog)
  "Get the blog title render data"
  (list :title (blog-title blog)
        :subtitle (blog-sub-title blog)))

(defun get-theme-data (theme)
  (list :name (theme-name theme)))

(defun get-themes-data (blog)
  "Get all normal themes data"
  (declare (ignore blog))
  (loop for theme in (get-normal-themes)
        collect (get-theme-data theme)))

(defun get-blog-data (blog)
  "Get a blog general description, title and sub-title"
  (list :blogname (blog-title blog)
        :blogdesc (blog-sub-title blog)
        :footerhtml (blog-footer-html blog)
        :curtheme (theme-name (blog-theme blog))
        :themes (get-themes-data blog)))

(defun get-author-data (blog)
  (list :author (blog-author blog)))

(defun get-footer-data (blog)
  (list :author (blog-author blog)
        :sysinfo (get-sys-info)
        :html (blog-footer-html blog)
        :loginurl (restas:genurl 'login)))

(defun get-head-data (blog)
  "Get the blog menu data"
  (list :index (restas:genurl 'index)
        :new (restas:genurl 'new-post)
        :login (restas:genurl 'login)
        :about (restas:genurl 'view :id 0)
        :blogname (blog-title blog)
        :blogdesc (blog-sub-title blog)))

(defun get-comment-data (blog comment)
  "Get a comment data"
  (let ((post (get-nth-post blog (comment-post-id comment))))
    (list :id (comment-id comment)
          :postid (comment-post-id comment)
          :posttitle (post-title post)
          :author (comment-author comment)
          :timedesc (format-timestring (comment-time comment))
          :content (comment-content comment)
          :email (comment-email comment)
          :url (comment-url comment))))

(defun get-post-comments-data (blog post)
  "Get all comments data associated with the post"
  (let ((comments (get-post-comments blog post)))
    (if comments
      (loop for comment in comments
            collect (get-comment-data blog comment))
      nil)))

(defun get-all-comments-data (blog)
  "Get all comments data"
  (let ((comments (blog-comments blog)))
    (if comments
      (loop for comment in comments
            collect (get-comment-data blog comment))
      nil)))
     
(defun get-all-posts-data (blog)
  "Get all posts data"
  (let ((posts (blog-posts blog)))
    (if posts
      (loop for post in posts
            collect (get-post-data blog post))
      nil)))

(defun get-comment-brief (comment)
  "Get a comment brief description"
  (list :id (comment-id comment)
        :postid (comment-post-id comment)
        :brief (comment-content comment)))

(defun get-recent-comments-brief (blog cnt)
  (let ((comments (get-recent-comments blog cnt)))
    (if comments
      (loop for comment in comments
            collect (get-comment-brief comment))
      nil)))

(defun get-post-brief (post)
  (list :url (restas:genurl 'view :id (post-id post))
        :title (post-title post)))

(defun get-recent-posts-brief (blog cnt)
  (let ((posts (get-recent-posts blog cnt)))
    (if posts
      (loop for post in posts
            collect (get-post-brief post))
      nil)))


;;;;
;;;; rss.lisp
;;;; Kevin Lynx
;;;; 7.22.2011
;;;;
(in-package #:ext-blog)

(defun rss-format-pubdate (post)
  (hunchentoot:rfc-1123-date
    (or (post-revised-time post)
        (post-time post))))

(defun rss-get-post-desc (post) 
  (list :title (post-title post)
        :content (post-content post)
        :link (make-full-post-url (post-id post))
        :pubdate (rss-format-pubdate post)))

(defun rss-get-blog-posts (blog cnt)
  (let ((posts (get-recent-posts blog cnt)))
    (if posts
      (loop for post in posts
            collect (rss-get-post-desc post))
      nil)))

(defmethod rss-generate ((type (eql 'default)))
  (ext-blog.rss:rss 
    (list :title (blog-title *blog*)
          :link (make-full-root-url)
          :description (blog-sub-title *blog*)
          :pubdate (hunchentoot:rfc-1123-date)
          :entrylist (rss-get-blog-posts *blog* 10))))


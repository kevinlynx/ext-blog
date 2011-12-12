;;;; routes.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(make-full-root-url make-full-feed-url))

(defun format-args (type &rest args)
  (list :blog *blog* :args args :type type))

(defun make-host ()
  (concatenate 'string "http://" (hunchentoot:host)))

(defun make-full-root-url ()
  (concatenate 'string (make-host) (restas:genurl 'main)))

(defun make-full-post-url (id)
  (concatenate 'string (make-host) (restas:genurl 'view :id id)))

(defun make-full-feed-url ()
  (concatenate 'string (make-host) (restas:genurl 'api/rss)))
  
(restas:define-route main ("")
  (if (blog-initialized-p)
    (restas:redirect 'index)
    (restas:redirect 'initialize)))

(restas:define-route index ("index")
  (format-args :normal))

(restas:define-route view ("view/:id"
                                :parse-vars (list :id #'parse-integer))
  (format-args :normal id))

(restas:define-route page ("page/:id"
                           :parse-vars (list :id #'parse-integer))
  (format-args :normal id))

(restas:define-route new-comment-done ("new-comment"
                                       :method :post)
  (let ((author (params "author"))
        (email (params "email"))
        (verify (params "verify"))
        (content (params "content")))
    (unless (or (not (auth-verify-code verify))
                (string-empty author) (string-empty email) (string-empty content))
      (create-comment *blog* (parse-integer (params "postid"))
                      author  email (params "url") content))
    (restas:redirect 'view :id (params "postid"))))

;;; admin pages
(restas:define-route login ("login")
  (if (authed-p (blog-user *blog*))
    (restas:redirect 'manage)
    (format-args :admin)))

(restas:define-route login-done ("login"
                                 :method :post)
  (if (do-login (params "user") (params "pw"))
    (on-login (params "ispersis") 'manage)
    (restas:redirect 'login)))

(restas:define-route logout ("logout")
  (on-logout 'login)
  (format-args :admin))

(restas:define-route initialize ("initialize")
  (when (blog-initialized-p)
    (restas:redirect 'login))
  (format-args :normal))

;;;; If there's no themes, here is the bug.
(restas:define-route initialize-done ("initialize" :method :post)
  (store-blog (create-blog (params "user") (params "pw") "My blog" "My blog description"
                           (get-default-theme :normal) (get-default-theme :admin)))
  (restas:redirect 'index))

(restas:define-route manage ("manage")
  (when (check-login 'manage 'login)
    (restas:redirect 'manage/post)
    (format-args :admin)))

(restas:define-route manage/new ("manage/new")
  (when (check-login 'manage/new 'login)
    (format-args :admin)))

(restas:define-route manage/new-done ("manage/new" :method :post)
  (when (check-login 'manage/new 'login)
    (if (string-empty (params "title"))
      (restas:redirect 'manage/new)
      (let ((id (create-post *blog* (params "title") (params "content"))))
        (restas:redirect 'view :id id)))
    (format-args :admin)))

(restas:define-route manage/post ("manage/post")
  (when (check-login 'manage/post 'login)
    (format-args :admin)))

(restas:define-route manage/comment ("manage/comment")
  (when (check-login 'manage/comment 'login)
    (format-args :admin)))

(restas:define-route manage/comment/delete/id ("manage/comment/delete/:id"
                                     :parse-vars (list :id #'parse-integer))
  (when (check-login 'manage 'login)
    (delete-comment *blog* id)
    (restas:redirect 'manage/comment)
    (format-args :admin)))

(restas:define-route manage/comment/delete ("manage/comment/delete" 
                                            :method :post)
  (when (check-login 'manage 'login)
    (mapc #'(lambda (id) (delete-comment *blog* (parse-integer id)))
          (params-list "com[]"))
    (restas:redirect 'manage/comment)
    (format-args :admin)))

(restas:define-route manage/post/delete/id ("manage/post/delete/:id"
                                            :parse-vars (list :id #'parse-integer))
  (when (check-login 'manage 'login)
    (delete-post *blog* id)
    (restas:redirect 'manage/post)
    (format-args :admin)))

(restas:define-route manage/post/edit/id ("manage/post/edit/:id"
                                          :parse-vars (list :id #'parse-integer))
  (when (check-login 'manage 'login)
    (format-args :admin id)))

;;; There's a client bug, sometimes we canot receive the modified content
(restas:define-route manage/post/edit/id-done ("manage/post/edit/:id"
                                               :parse-vars (list :id #'parse-integer)
                                               :method :post)
  (when (check-login 'manage 'login)
    (if (not (string-empty (params "title")))
      (progn
        ;(format t "~a~%" (params "content"))
        (edit-post *blog* id (params "title") (params "content"))
        (restas:redirect 'view :id id))
      (restas:redirect 'manage/post/edit/id :id id))
    (format-args :admin)))

(restas:define-route manage/post/delete ("manage/post/delete" 
                                            :method :post)
  (when (check-login 'manage 'login)
    (mapc #'(lambda (id) (delete-post *blog* (parse-integer id)))
          (params-list "post[]"))
    (restas:redirect 'manage/post)
    (format-args :admin)))

(restas:define-route manage/configure ("manage/configure")
  (restas:redirect 'manage/configure/blogger))

(restas:define-route manage/configure/blogger ("manage/configure/blogger")
  (when (check-login 'manage 'login)
    (format-args :admin)))

(restas:define-route manage/configure/blogger-done ("manage/configure/blogger"
                                                    :method :post)
  (when (check-login 'manage 'login)
    (let ((title (params "blogname"))
          (subtitle (params "blogdesc"))
          (footer-html (params "footerhtml"))
          (theme (find-theme (params "theme"))))
      (when theme (setf (blog-theme *blog*) theme))
      (unless (string-empty title) (setf (blog-title *blog*) title))
      (unless (string-empty subtitle) (setf (blog-sub-title *blog*) subtitle))
      (unless (string-empty footer-html) (setf (blog-footer-html *blog*) footer-html))
      (store-blog))
    (restas:redirect 'manage/configure/blogger)
    (format-args :admin)))

(restas:define-route manage/configure/profile ("manage/configure/profile")
  (when (check-login 'manage 'login)
    (format-args :admin)))

(restas:define-route manage/configure/profile-done ("manage/configure/profile"
                                                    :method :post)
  (when (check-login 'manage 'login)
    (let ((author (params "username"))
          (oldpwd (params "oldpwd"))
          (newpwd (params "newpwd")))
      (unless (string-empty author) (setf (user-uid (blog-user *blog*)) author))
      (when (string= (user-password (blog-user *blog*)) oldpwd)
        (setf (user-password (blog-user *blog*)) newpwd))
      (store-blog))
    (restas:redirect 'manage/configure/profile)
    (format-args :admin)))

(restas:define-route api/metaweblog-info ("api/metaweblog")
  (format-args :api "This is the API documentation page"))

(restas:define-route api/metaweblog ("api/metaweblog" :method :post)
  (let ((ret (xml-rpc-methods:handle-metaweblog-request *blog*)))
    (format-args :api ret)))

(defgeneric rss-generate (type))

(restas:define-route api/rss ("feed")
  (format-args :api (rss-generate 'default)))

(restas:define-route verifypic ("verifypic")
  (setf (hunchentoot:content-type*) "image/gif")
  (format-args :api (generate-verify-code)))

;;; temp codes, only for my personal blog, old url route
(restas:define-route blog ("blog")
    (restas:redirect 'main))


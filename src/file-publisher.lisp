;;;;
;;;; file-publisher.lisp
;;;; Kevin Lynx
;;;; 12.12.2011
;;;;
(in-package #:ext-blog)

;;; Used to server theme static files like css, images etc.
(defvar *publish-files* nil)
(defvar *publish-root* (list (theme-pathname) *data-root*))

(defun push-publish-entry-file ()
  (pushnew (merge-pathnames "*/" *entry-static-path*) *publish-files*)
  (pushnew (merge-pathnames "*/*/" *entry-static-path*) *publish-files*))

;;; i.e: "isimple/style.css" "isimple/image/"
(defun push-publish-theme-file (file)
  "Push a file or a directory to be servered"
  (format t "add published theme file (~a)~%" file)
  (pushnew (merge-pathnames file (theme-pathname)) *publish-files*))
            
(defun mount-file-publisher ()
  "Mount restas.file-publisher to serve static files"
  (push-publish-entry-file)
  (restas:mount-submodule 
    -view-static- (#:restas.file-publisher @nil-route-require)
    ;; Damn url path...
    (restas.file-publisher:*baseurl* '("view"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -page-static- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*baseurl* '("page"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -manage-config-static- (#:restas.file-publisher @nil-route-require) 
    (restas.file-publisher:*baseurl* '("manage" "configure"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -manage-post-edit- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*baseurl* '("manage" "post" "edit"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -manage-static- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*baseurl* '("manage"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -static- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*)))


;;;; file-publisher.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
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
  (let ((*package* #.*package*))
    (restas:mount-module -view-static- (#:restas.file-publisher)
      (:url "view/")
      (:decorators '@nil-route-require)
      ;; Damn url path...
      (restas.file-publisher:*directory* *publish-root*)
      (restas.file-publisher:*files* *publish-files*))
    (restas:mount-module -page-static- (#:restas.file-publisher)
      (:url "page/")
      (:decorators '@nil-route-require)
      (restas.file-publisher:*directory* *publish-root*)
      (restas.file-publisher:*files* *publish-files*))
    (restas:mount-module -manage-config-static- (#:restas.file-publisher)
      (:url "manage/configure/")
      (:decorators '@nil-route-require)
      (restas.file-publisher:*directory* *publish-root*)
      (restas.file-publisher:*files* *publish-files*))
    (restas:mount-module -manage-post-edit- (#:restas.file-publisher)
      (:url "manage/post/edit/")
      (:decorators '@nil-route-require)
      (restas.file-publisher:*directory* *publish-root*)
      (restas.file-publisher:*files* *publish-files*))
    (restas:mount-module -manage-static- (#:restas.file-publisher)
      (:url "manage")
      (:decorators '@nil-route-require)
      (restas.file-publisher:*directory* *publish-root*)
      (restas.file-publisher:*files* *publish-files*))
    (restas:mount-module -static- (#:restas.file-publisher)
      (:decorators '@nil-route-require)
      (restas.file-publisher:*directory* *publish-root*)
      (restas.file-publisher:*files* *publish-files*))))


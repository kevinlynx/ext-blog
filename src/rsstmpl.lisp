;;;; rsstmpl.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

;;; You must separate the 'compile tmpl' codes from rss.lisp, because if you do,
;;; you'll get a compile error.
(defun update-rss-tmpl ()
  (closure-template:compile-template 
    :common-lisp-backend
    (merge-pathnames "rss.tmpl" (src-pathname))))

(update-rss-tmpl)


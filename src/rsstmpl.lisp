;;;;
;;;; rsstmpl.lisp
;;;; Kevin Lynx
;;;; 8.2.2011
;;;;
(in-package #:ext-blog)

;;; You must separate the 'compile tmpl' codes from rss.lisp, because if you do,
;;; you'll get a compile error.
(defun update-rss-tmpl ()
  (closure-template:compile-template 
    :common-lisp-backend
    (merge-pathnames "rss.tmpl" (src-pathname))))

(update-rss-tmpl)


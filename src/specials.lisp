;;;;
;;;; specials.lisp
;;;; Kevin Lynx
;;;; 12.12.2011
;;;;
(in-package #:ext-blog)

(export '(*entry-static* *entry-static-path*))

(defvar *data-root* "./")
(defvar *entry-static* "static/")
(defvar *entry-static-path* (merge-pathnames *entry-static* *data-root*))
(defvar *store-path* (merge-pathnames "store/" *data-root*))
(defvar *font-path* (merge-pathnames "font/wenquanyi_12ptb.pcf" *data-root*))
(defvar *log-path* (merge-pathnames "log/" *data-root*))


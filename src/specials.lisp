;;;; specials.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(in-package #:ext-blog)

(export '(*entry-static* *entry-static-path*))

(defvar *data-root* "./")
(defvar *entry-static* "static/")
(defvar *entry-static-path* (merge-pathnames *entry-static* *data-root*))
(defvar *store-path* (merge-pathnames "store/" *data-root*))
(defvar *font-path* (merge-pathnames "font/wenquanyi_12ptb.pcf" *data-root*))
(defvar *log-path* (merge-pathnames "log/" *data-root*))


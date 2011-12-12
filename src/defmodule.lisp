;;;; defmodule.lisp
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)
(restas:define-module #:ext-blog
  (:use common-lisp)
  )

(defpackage #:xml-rpc-methods
  (:use
    :common-lisp
    :s-xml-rpc)
  (:export
    set-metaweblog-api
    handle-metaweblog-request
    |metaWeblog.newPost|
    |metaWeblog.newMediaObject|))


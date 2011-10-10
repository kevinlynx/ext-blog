;;;;
;;;; defmodule.lisp
;;;; Kevin Lynx
;;;; 6.4.2011
;;;;
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


;;;; ext-blog.asd
;;;;
;;;; This file is a part of ext-blog, a common lisp blog engine.
;;;; See file doc/LICENSE for license details.
;;;;
;;;; Author: Kevin Lynx (kevinlynx at gmail dot com)

(in-package :cl-user)

(defpackage :ext-blog-asd
  (:use :cl :asdf))

(in-package :ext-blog-asd)

(defvar *ext-blog-version* "0.9.1")

(export '*ext-blog-version*)

(defsystem ext-blog
    :author "Kevin Lynx <kevinlynx@gmail.com>"
    :version #.*ext-blog-version*
    :depends-on (#:restas #:cl-store #:closure-template #:local-time
                 #:kl-verify #:image #:restas.file-publisher
                 #:s-xml-rpc
                 #:cl-fad)
    :components
    ((:module "src"
              :components
              ((:file "defmodule")
               (:file "utils" :depends-on ("defmodule"))
               (:file "specials" :depends-on ("defmodule"))
               (:file "file-publisher" :depends-on ("utils" "specials"))
               (:file "themes" :depends-on ("file-publisher"))
               (:module "model"
                        :components 
                        ((:file "post")
                         (:file "comment")
                         (:file "user")
                         (:file "blog" :depends-on ("user" "post" "comment"))
                         (:file "store-blog" :depends-on ("blog")))
                        :depends-on ("utils" "specials"))
               (:file "rsstmpl" :depends-on ("utils"))
               (:file "rss" :depends-on ("rsstmpl" "model" "routes"))
               (:file "drawer-dispatch" 
                :depends-on ("defmodule" "model"))
               (:file "params" :depends-on ("utils"))
               (:file "verify-code" :depends-on ("utils"))
               (:file "auth" :depends-on ("defmodule" "model"))
               (:file "routes" 
                :depends-on ("defmodule" "model" "auth" "verify-code"))
               (:file "xmlrpc" :depends-on ("defmodule" "model" "ext-blog"))
               (:file "ext-blog" 
                :depends-on ("specials" "routes" "themes" "drawer-dispatch" "model"))))
     (:module "theme"
              :components
              ((:file "blog-data"))
              :depends-on ("src"))))


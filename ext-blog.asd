;;;;
;;;; ext-blog.asd
;;;; Kevin Lynx
;;;; 6.4.2011
;;;;

(defsystem ext-blog
    :depends-on (#:restas #:cl-store #:closure-template #:local-time
                 #:ext-blog.dep
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


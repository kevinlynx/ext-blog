;;;;
;;;; Default admin theme.
;;;; 6.8.2011
;;;; ext-blog.theme.default-admin.asd
;;;;
(defsystem ext-blog.theme.default-admin
  :depends-on (#:closure-template)
  :components
  ((:file "defpackage")
   (:file "drawer" :depends-on ("defpackage"))))


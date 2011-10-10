;;;;
;;;; iSimple theme, ported by Kevin Lynx for ext-blog.
;;;; 6.12.2011
;;;; ext-blog.theme.isimple.asd
;;;;
(defsystem ext-blog.theme.isimple
  :depends-on (#:closure-template)
  :components
  ((:file "defpackage")
   (:file "drawer" :depends-on ("defpackage"))))


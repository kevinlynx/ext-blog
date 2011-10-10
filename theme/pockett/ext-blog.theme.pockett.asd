;;;;
;;;; PocketT theme, ported by Kevin Lynx for ext-blog.
;;;; 6.8.2011
;;;; ext-blog.theme.pockett.asd
;;;;
(defsystem ext-blog.theme.pockett
  :depends-on (#:closure-template)
  :components
  ((:file "defpackage")
   (:file "drawer" :depends-on ("defpackage"))))


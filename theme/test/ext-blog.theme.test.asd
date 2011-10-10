
(defsystem ext-blog.theme.test
  ;:depends-on (#:ext-blog)
  :components
  ((:file "defpackage")
   (:file "test" :depends-on ("defpackage"))))


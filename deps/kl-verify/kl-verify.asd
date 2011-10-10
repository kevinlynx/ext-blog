
(in-package #:cl-user)

(asdf:defsystem #:kl-verify
  :version "0.01"
  :author "Kevin Lynx <kevinlynx@gmail.com>"
  :depends-on (#:image)
  :components ((:file "defpackage")
	       (:file "verify-code" :depends-on ("defpackage"))))


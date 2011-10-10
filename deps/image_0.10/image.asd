(in-package #:cl-user)

(asdf:defsystem #:image
  :version "0.10"
  :author "Ingvar Mattsson <ingvar@hexapodia.net>"
  :license "MIT"
  :depends-on (#:skippy #:clx #:zpng #:gzip-stream)
  :components ((:file "package")
	       (:file "helpers" :depends-on ("package"))
	       (:file "image" :depends-on ("package"))
	       (:file "image-text" :depends-on ("package" "image"))
	       (:file "x11" :depends-on ("package" "image"))
	       (:file "pcf-constants" :depends-on ("package"))
	       (:file "read-pcf" :depends-on ("package" "pcf-constants"))
	       (:file "imports" :depends-on ("package" "image" "helpers"))
	       (:file "exports" :depends-on ("package" "image" "x11"))))

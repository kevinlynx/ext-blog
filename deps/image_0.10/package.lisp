(in-package #:cl-user)

(defpackage #:net.hexapodia.image
  (:use #:cl)
  (:nicknames #:image)
  (:export #:text #:make-image #:line #:rect #:plot
	   #:poly-line #:ellipsis #:circle #:circle-fill
	   #:export-to-gif #:export-to-x11 #:export-image
	   #:import-image #:height #:width #:put-image #:get-pixel #:cut-image
	   ))

(defpackage #:net.hexapodia.image.pcf
  (:use #:cl)
  (:export #:read-font))


;;;;
;;;; defpackage.lisp
;;;; Kevin Lynx
;;;; Use image library to generate a simple verify code image.
;;;;
(in-package #:cl-user)

(defpackage #:kl-verify
  (:use #:cl)
  (:export #:load-font #:generate-verify-code))


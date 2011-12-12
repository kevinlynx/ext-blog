;;;;
;;;; file-publisher.lisp
;;;; Used to publish static files.
;;;; Kevin Lynx
;;;; 6.14.2011
;;;;
(restas:define-module #:restas.file-publisher
  (:use :cl)
  (:export #:push-file
           #:*directory*
           #:*files*))

(in-package #:restas.file-publisher)

;;; i.e: '("src/model/" "src/routes.lisp")
(defvar *files* nil "A list of files to be published, can be a path.")

;;; i.e: '("src/")
(defvar *directory* nil "The root directory")

(defun parse-native-namestring (thing)
  #+sbcl (sb-ext:parse-native-namestring thing)
  #-sbcl (parse-namestring thing))

(defun publish-file-p (file)
  "Find the file in published list"
  (find-if #'(lambda (path)
               (pathname-match-p file path))
           *files*))
;;; test only
(defun push-file (file)
  "Push a relative file/path"
  (pushnew (merge-pathnames file *directory*) *files*))

(restas:define-route route ("*path" :method :get)
  (let* ((relative-path (parse-native-namestring (format nil "~{~A~^/~}" path)))
         (path (find-if #'(lambda (dir)
                            (let ((path (merge-pathnames relative-path dir)))
                              (and (not (fad:directory-pathname-p path))
                                   (fad:file-exists-p path)
                                   (publish-file-p path))))
                        *directory*)))
    (if path
      (merge-pathnames relative-path path)
      hunchentoot:+http-not-found+)))


;;;;
;;;; ext-blog.dep.asd
;;;; Kevin Lynx
;;;; 12.9.2011
;;;;
(defsystem ext-blog.dep
  :description "a dummy system to help loading the dependency libraries")

(defvar *dep-path* 
 (merge-pathnames "deps/" (asdf:component-pathname (asdf:find-system '#:ext-blog.dep))))

(defun load-dep (path lib)
  (push (merge-pathnames path *dep-path*) asdf:*central-registry*)
  (asdf:load-system lib))

;; load dependent libraries
(load-dep "image_0.10/" :image)
(load-dep "kl-verify/" :kl-verify)
(load-dep "file-publisher/" :restas.file-publisher)


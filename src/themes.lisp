;;;;
;;;; themes.lisp
;;;; Manage theme load operations.
;;;; 6.7.2011
;;;; Kevin Lynx
;;;;
(in-package #:ext-blog)

(export '(theme-name theme-desc theme-type theme-update theme-resources
          mount-file-publisher push-publish-file
          *entry-static* *entry-static-path*
          update-themes load-themes themes-count))

;;; Used to server theme static files like css, images etc.
(defvar *publish-files* nil)
(defvar *publish-root* (theme-pathname))
(defvar *entry-static* "static/")
(defvar *entry-static-path* (merge-pathnames *entry-static*
                                             *publish-root*))

;;; i.e: "isimple/style.css" "isimple/image/"
(defun push-publish-file (file)
  "Push a file or a directory to be servered"
  (format t "add published file (~a)~%" file)
  (pushnew (merge-pathnames file *publish-root*) *publish-files*)) 
            
(defun push-entry-static-path ()
  (push-publish-file (concatenate 'string *entry-static* "*/"))
  (push-publish-file (concatenate 'string *entry-static* "*/*/")))

(defclass nil-route (routes:proxy-route) ())

(defmethod routes:route-check-conditions ((route nil-route) bindings)
  t)

(defun @nil-route-require (route)
  (make-instance 'nil-route :target route))

(defun mount-file-publisher ()
  "Mount restas.file-publisher to serve static files"
  (push-entry-static-path)
  (restas:mount-submodule 
    -view-static- (#:restas.file-publisher @nil-route-require)
    ;; Damn url path...
    (restas.file-publisher:*baseurl* '("view"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -page-static- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*baseurl* '("page"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -manage-config-static- (#:restas.file-publisher @nil-route-require) 
    (restas.file-publisher:*baseurl* '("manage" "configure"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -manage-post-edit- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*baseurl* '("manage" "post" "edit"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -manage-static- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*baseurl* '("manage"))
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*))
  (restas:mount-submodule 
    -static- (#:restas.file-publisher @nil-route-require)
    (restas.file-publisher:*directory* *publish-root*)
    (restas.file-publisher:*files* *publish-files*)))

(defvar *themes* nil)
(defparameter +theme-inst+ "*THEME-INST*")

(defgeneric theme-name (inst)
  (:documentation "Get the theme name"))

(defgeneric theme-desc (inst)
  (:documentation "Get the theme description"))

(defgeneric theme-update (inst)
  (:documentation "Update the theme, reload resources etc."))

(defgeneric theme-type (inst)
  (:method (inst) :normal)
  (:documentation "Theme type can be :normal and :admin"))

(defgeneric theme-resources (inst)
  (:method (inst) nil)
  (:documentation "A list of files/directories to be published"))

(defun theme-asdf-exist-p (name dir)
  "Check whether a theme `asd` file exists"
  (cl-fad:file-exists-p 
    (merge-pathnames (concatenate 'string name ".asd") dir)))

(defun load-theme (name dir)
  "Load a theme named `name` in `dir`"
  (pushnew dir asdf:*central-registry*)
  (format t "loading theme (~s)...~%" name)
  (asdf:load-system name)
  (let* ((pkg (find-package (intern (string-upcase name))))
         (theme (symbol-value (find-symbol +theme-inst+ pkg))))
    (when theme
      (pushnew theme *themes*)
      (mapcar #'push-publish-file (theme-resources theme)))))

(defun create-theme-name (dir)
  "Create a theme name, theme name is `ext-blog.theme.*`"
  (concatenate 'string "ext-blog.theme." 
               (pathname-name (cl-fad:pathname-as-file dir))))

(defun load-themes (&optional 
                     (path (theme-pathname)))
  "Load all themes in directory `theme/`"
  (setf *themes* nil)
  (setf *publish-files* nil)
  (let ((dirs (cl-fad:list-directory path))
        (cnt 0))
    (mapc #'(lambda (dir)
              (let ((name (create-theme-name dir)))
                (when (and (cl-fad:directory-pathname-p dir)
                           (theme-asdf-exist-p name dir))
                  (incf cnt)
                  (load-theme name dir))))
          dirs)
    cnt))

(defun update-themes ()
  "Update all themes"
  (mapc #'(lambda (theme) (theme-update theme)) *themes*))

(defun get-default-theme (type)
  "Get a random theme of type"
  (find-if #'(lambda (theme) (eql (theme-type theme) type)) *themes*))

(defun themes-count ()
  "Get the theme count currently loaded"
  (length *themes*))

(defun get-normal-themes ()
  "Get all normal themes"
  (remove-if #'(lambda (theme) (not (eql (theme-type theme) :normal))) *themes*))

(defun find-theme (name)
  "Find a theme by its name"
  (find-if #'(lambda (theme) (string-equal (theme-name theme) name)) *themes*))


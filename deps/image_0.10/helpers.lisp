(in-package #:net.hexapodia.image)

(defparameter *file-formats* '(("gif" . :gif)
			       ("png" . :png))
  "Association list, mapping file types to image formats")

(defun format-from-name (file-name)
  (let ((pn (pathname file-name)))
    (let ((ftype (string-downcase (pathname-type pn))))
      (cdr (assoc ftype *file-formats* :test #'string=)))))

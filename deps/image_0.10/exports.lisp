(in-package #:net.hexapodia.image)

(defgeneric export-image (image sink &key &allow-other-keys))


(defun export-to-file (image name &key (format :gif) comment)
  (case format
    (:gif (export-to-gif image name comment))
    (:png (export-to-png image name))))

(defun export-to-stream (image stream &key format comment)
  (case format
    (:gif (write-gif-to-stream image stream comment))
    (:png (write-png-to-stream image stream))))

(defmethod export-image (image (filename string) &key format (comment "a"))
  (let ((format (or format
		    (format-from-name filename))))
    (export-to-file image filename :format format :comment comment)))

(defmethod export-image (image (filename pathname) &key format (comment "a"))
  (let ((format (or format
		    (format-from-name filename))))
    (export-to-file image filename :format format :comment comment)))

(defmethod export-image (image (sink xlib:drawable) &key)
  (export-to-x11 image sink))

(defmethod export-image (image (sink xlib:display) &key)
  (export-to-x11 image sink))

(defmethod export-image (image (sink stream) &key (format :gif) (comment "a"))
  (export-to-file image sink :format format :comment comment))

(in-package #:net.hexapodia.image)

(defgeneric import-image (source &key &allow-other-keys))

(defun import-from-file (source &key (format :gif) &allow-other-keys)
  (case format
    (:gif (let ((datastream (skippy:load-data-stream source)))
	    (let ((img (aref (skippy:images datastream) 0))
		  (ctab (skippy:color-table datastream)))
	      (let ((rv (make-image (skippy:width datastream)
				    (skippy:height datastream)))
		    (sdata (skippy:image-data img)))
		(loop for pixel across sdata
		      for offset from 0 by 3
		      do (let ((col (skippy:color-table-entry ctab pixel)))
			   (multiple-value-bind (r g b)
			       (skippy:color-rgb col)
			     (setf (aref (image-data rv) (+ 0 offset)) r)
			     (setf (aref (image-data rv) (+ 1 offset)) g)
			     (setf (aref (image-data rv) (+ 2 offset)) b))))
		rv))))))

(defun import-from-filename (source &key (format :gif) &allow-other-keys)
  (with-open-file (s source)
    (import-from-file source :format format)))

(defmethod import-image ((source stream) &key (format :gif) &allow-other-keys)
  (import-from-file source :format format))

(defmethod import-image ((source string) &key format &allow-other-keys)
  (let ((format (if format format (format-from-name source))))
    (import-from-filename source :format format)))

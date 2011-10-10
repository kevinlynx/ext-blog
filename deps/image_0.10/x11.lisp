(in-package #:net.hexapodia.image)

(defgeneric export-to-x11 (image destination))
(defvar *colormap* (make-hash-table :test #'equal))
(defvar *xlib-colormaps* (make-hash-table))


(defun get-colormap (drawable)
  (let ((display (xlib:window-display drawable)))
    (or (gethash display *xlib-colormaps*)
	(setf (gethash display *xlib-colormaps*)
	      (xlib:screen-default-colormap
	       (car (xlib:display-roots display)))))))

(defun get-color (drawable r g b)
  (let ((cmap (get-colormap drawable)))
    (let ((key (list cmap r g b)))
      (or (gethash key *colormap*)
	  (let* ((color (xlib:alloc-color cmap
					  (xlib:make-color :red (/ r 255.0)
							   :green (/ g 255.0)
							   :blue (/ b 255.))))
		 (gc (xlib:create-gcontext :drawable drawable
					   :background 0
					   :foreground color)))
	    (setf (gethash key *colormap*) gc))))))

(defmethod export-to-x11 (image (dest xlib:drawable))
  (loop for x from 0 below (min (width image)
			     (xlib:drawable-width dest))
	do (loop for y from 0 below (min (height image)
				      (xlib:drawable-height dest))
		 do (let ((base-ix (+ (* x 3) (* y (width image) 3)))
			  (data (image-data image)))
		      (let ((gc (get-color dest
					   (aref data (+ base-ix 0))
					   (aref data (+ base-ix 1))
					   (aref data (+ base-ix 2)))))
			(xlib:draw-point dest gc x y)))))
  dest)

(defmethod export-to-x11 (image (dest xlib:display))
  (let ((win (xlib:create-window :parent (xlib:screen-root (car (xlib:display-roots dest)))
				 :width (width image)
				 :x 0
				 :y 0
				 :height (height image)
				 )))
    (xlib:map-window win)
    (export-to-x11 image win)
    (xlib:display-force-output dest)
    win))



  
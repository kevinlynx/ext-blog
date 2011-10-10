(in-package #:net.hexapodia.image)

;;; Generic image class, shouldn't be needed, but currently back-end is in
;;; the air.
(defvar *text-map* (make-hash-table :test 'equal))
(defvar *clip-region* nil)
(defvar *font-map* nil)

(defclass image ()
  ((height :reader height :initarg :height)
   (width :reader width :initarg :width)
   (image-data :reader image-data :initarg :image-data)
   ))

(defclass sub-image ()
  ((height :reader height :initarg :height)
   (width :reader width :initarg :width)
   (image-data :reader image-data :initarg :image-data)
   (alpha-data :reader alpha-data :initarg :alpha-data)
   ))

(defun call-with-clip-region (region body)
  (let ((*clip-region* region))
    (funcall body)))

(defmacro with-clip-region ((x0 y0 x1 y1) &body body)
  `(call-with-clip-region ,(list x0 y0 x1 y1)
      (lambda () ,@body)))

(defun make-image (width height)
  "Create an IMAGE object, with specified width and height"
  (let ((data (make-array (* height width 3) :element-type '(integer 0 255))))
    (make-instance 'image
		   :height height
		   :width width
		   :image-data data)))

(defun cut-image (image x0 y0 x1 y1 &optional (alpha 1.0))
  "Create a copy of the area of IMAGE that extends from <X0,Y0> to <X1,Y1>, also create an alpha map that is initialised to the value given to ALPHA (default is 1.0). Note that this alpha-map is *not* automatically referenced!"
  (let ((height (1+ (- x1 x0)))
	(width (1+ (- y1 y0))))
    (let ((sub-image (make-instance
		      'sub-image
		      :height height
		      :width width
		      :image-data (make-array (* height width 3)
					      :element-type '(integer 0 255))
		      :alpha-data (make-array (* height width)
					      :element-type 'float
					      :initial-element alpha))))
      (loop for img-src from (+ (+ (* x0 3) (* y0 (width image) 3))) by (* 3 (width image))
	    for cut-dst from 0 by (* 3 width)
	    for y from 0 below height
	    do (setf (subseq (image-data sub-image)
			     cut-dst (1- (+ cut-dst (* 3 width))))
		     (subseq (image-data image)
			     img-src (1- (+ img-src (* 3 width))))))
      sub-image)))

(defun alpha-blend (back fore alpha)
  (declare (type (integer 0 255) back fore)
	   (type float alpha))
  (max 0 (min 255 (round (+ (* back (- 1.0 alpha)) (* fore alpha))))))

(defun put-image-fast (source target sx sy sw sh tx ty)
  (let ((target-width (width target))
	(source-width (width source))
	(target-data (image-data target))
	(source-data (image-data source)))
    (let ((source-base (+ (* sx 3) (* sy source-width 3)))
	  (target-base (+ (* tx 3) (* ty target-width 3))))
      (loop for target-dst from target-base by (* 3 target-width)
	   for source-dst from source-base by (* 3 source-width)
	   for count from 0 below sh
	   do (setf (subseq target-data target-dst
			    (1- (+ target-dst (* 3 sw))))
		    (subseq source-data source-dst
			    (1- (+ source-dst (* 3 sw)))))))))

(defun put-image-map (source alpha-map target sx sy sw sh tx ty)
  (let ((source-width (width source))
	(target-width (width target))
	(source-data (image-data source))
	(target-data (image-data target)))
    (loop for source-base from (+ (* 3 sx) (* 3 sy source-width)) by (* 3 source-width)
	 for target-base from (+ (* 3 tx) (* 3 ty target-width)) by (* 3 target-width)
	 for y from sy below (+ sy sh)
	 do (loop for s-off from source-base by 3
		 for t-off from target-base by 3
		 for x from sx below (+ sx sw)
		 do (let ((alpha (aref alpha-map x y)))
		      (setf (subseq target-data t-off (+ 3 t-off))
			    (map 'array
				 (lambda (sp tp) (alpha-blend tp sp alpha))
				 (subseq source-data s-off (+ 3 s-off))
				 (subseq target-data t-off (+ 3 t-off)))))))))

(defun put-image-slow (source alpha target sx sy sw sh tx ty)
  (let ((source-width (width source))
	(target-width (width target))
	(source-data (image-data source))
	(target-data (image-data target)))
    (loop for source-base from (+ (* 3 sx) (* 3 sy source-width)) by (* 3 source-width)
       for target-base from (+ (* 3 tx) (* 3 ty target-width)) by (* 3 target-width)
       for y from sy below (+ sy sh)
       do (loop for s-off from source-base by 3
	     for t-off from target-base by 3
	     for x from sx below (+ sx sw)
	     do (setf (subseq target-data t-off (+ 3 t-off))
		      (map 'vector (lambda (sp tp) (alpha-blend tp sp alpha))
			   (subseq source-data s-off (+ 3 s-off))
			   (subseq target-data t-off (+ 3 t-off))))))))


(defun put-image (source target x-offset y-offset &key (alpha 1.0) alpha-map (source-x 0) (source-y 0) (source-width (width source)) (source-height (height source)))
  "Copy a rectangle from SOURCE to TARGET, placing the result wit the upper left corner at <X-OFFSET, Y-OFFSET> in the target. Optionally, specily the ALPHA blend parameter to use (defaults to 1.0), an ALPHA-MAP, an array of wXh for the source image, having one alpha-blend per pixel stored in it. It's also possible to specify a sub-rectangle within the source image, with the upper left corner at <SOURCE-X, SOURCE-Y>, having SOURCE-WIDth and SOURCE-HEIGHT."
  (when (> (+ source-x source-width) (width source))
    (setf source-width (- (width source) source-x)))
  (when (> (+ source-y source-height) (height source))
    (setf source-height (- (height source) source-y)))
  (cond (alpha-map (put-image-map source alpha-map target source-x source-y source-width source-height x-offset y-offset))
	((= alpha 1.0) (put-image-fast source target source-x source-y source-width source-height x-offset y-offset))
	(t (put-image-slow source alpha target source-x source-y source-width source-height x-offset y-offset))))

(defun read-font (filespec)
  (when (null *font-map*)
    (setf *font-map* (make-hash-table :test #'equal))
    (setf (gethash "image-default" *font-map*)
	  *text-map*))
  (multiple-value-bind (name map) (net.hexapodia.image.pcf:read-font filespec)
    (setf (gethash name *font-map*) map)))

(defun use-font (name)
  (when *font-map*
    (format t "Preparing!")
    (let ((map (gethash name *font-map*)))
      (when map
	(format t "Can haz map!")
	(setf *text-map* map)))))

(defun define-char (character &rest data)
  (let ((chardata (make-array (list (length data) (length (car data)))
			      :element-type 'bit)))
    (loop for m from 0
	  for item in data
	  do (loop for n from 0
		   for bit in (coerce item 'list)
		   do (setf (aref chardata m n) (if (zerop bit) 0 1))))
    (setf (gethash character *text-map*) chardata)))

(defun distance (x0 y0 x1 y1)
  (let ((dx (- x1 x0))
	(dy (- y1 y0)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun pick (a ix op)
  (funcall op
	   (if *clip-region*
	       (nth ix *clip-region*)
	     a)
	   a))

(defun plot (image x y r g b &optional (alpha 1.0))
  "Draw a pixel in IMAGE, ar position <X Y>, with the specified R, G and B
values. Use ALPHA as alpha-blend value (defaults to 1.0)."
  (when (and (<= (pick 0 0 #'max)
		 x
		 (pick (1- (width image)) 2 #'min))
		   
	     (<= (pick 0 1 #'max)
		 y
		 (pick (1- (height  image)) 3 #'min)))
    (let ((base-ix (+ (* x 3) (* y (width image) 3)))
	  (data (image-data image)))
      (setf (aref data (+ base-ix 0))
	    (alpha-blend (aref data (+ base-ix 0)) r alpha))
      (setf (aref data (+ base-ix 1))
	    (alpha-blend (aref data (+ base-ix 1)) g alpha))
      (setf (aref data (+ base-ix 2))
	    (alpha-blend (aref data (+ base-ix 2)) b alpha)))))

(defun get-pixel (image x y &optional (type 'list))
  "Retrieve the R G B values of the <X,Y> pixel in  IMAGE. Return the triplet as TYPE (defaults to list)."
  (let ((base-ix (+ (* x 3) (* y (width image) 3)))
	(data (image-data image)))
    (map type #'identity (subseq data base-ix (+ 3 base-ix)))))

(defun line (image x0 y0 x1 y1 r g b &optional (alpha 1.0))
  "Draw a line in IMAGE from <X0 Y0> to <X1 Y1>, using R, G and B as colour.
Use ALPHA as alpha-blend value (defaults to 1.0)."
  (let ((xdiff (- x1 x0))
	(ydiff (- y1 y0)))
    (when (> (abs xdiff) (abs ydiff))
      (if (> xdiff 0)
	  (loop for x from x0 to x1
		with y = y0
		for acc = (truncate (abs xdiff) 2) then (+ acc (abs ydiff))
		
		do (when (> acc (abs xdiff))
		     (setf y (+ y (signum ydiff)))
		     (setf acc (- acc (abs xdiff))))
		do (plot image x y r g b alpha))
	(loop for x from x0 downto x1
	      with y = y0
	      for acc = (truncate (abs xdiff) 2) then (+ acc (abs ydiff))

	      do (when (> acc (abs xdiff))
		   (setf y (+ y (signum ydiff)))
		   (setf acc (- acc (abs xdiff))))
	      do (plot image x y r g b alpha))))
    (when (<= (abs xdiff) (abs ydiff))
      (if (> ydiff 0)
	  (loop for y from y0 to y1
		with x = x0
		for acc = (truncate (abs ydiff) 2) then (+ acc (abs xdiff))
		
		do (when (> acc (abs ydiff))
		     (setf x (+ x (signum xdiff)))
		     (setf acc (- acc (abs ydiff))))
		do (plot image x y r g b alpha))
	(loop for y from y0 downto y1
	      with x = x0
	      for acc = (truncate (abs ydiff) 2) then (+ acc (abs xdiff))
	      
	      do (when (> acc (abs ydiff))
		   (setf x (+ x (signum xdiff)))
		   (setf acc (- acc (abs ydiff))))
	      do (plot image x y r g b alpha))))))

(defun text (image text x y r g b &optional (alpha 1.0))
  "Draw the string TEXT in IMAGE, with upper left corner at <X Y>, using
the specified R, G and B values. Use ALPHA as alpha-blend value.

Returns (VALUE <max-x> <max-y>), to get exact bounding rectangle, plot at
<0 0>, with an ALPHA of 0.0 and you will have (VALUES <width> <height>) as
return value."
  (loop for char across text
	with lastbits = 0
	for base-x = x then (+ base-x lastbits)
	for data = (gethash char *text-map*)
	for bits = (or (and data (second (array-dimensions data))) 0)
	for height = (or (and data (first (array-dimensions data))) 0)
	do (progn
	     (setf lastbits bits)
	     (loop for x-off from 0 below bits
		   do (loop for y-off from 0 below height
			    do (unless (zerop (aref data y-off x-off))
				 (plot image (+ base-x x-off) (+ y y-off) r g b alpha)))))
	finally (return (values (+ base-x lastbits)
				(+ y height)))))

(defun rect (image x0 y0 x1 y1 fill r g b &optional (alpha 1.0))
  "Draw a rectangle in IMAGE, with one corner at <X0 Y0> and the diagonally
opposite at <X1 Y1>. Use R, G and B for colour and use ALPHA as alpha-blend
(defaults to 1.0)."
  (line image x0 y0 x1 y0 r g b alpha)
  (line image x1 y0 x1 y1 r g b alpha)
  (line image x1 y1 x0 y1 r g b alpha)
  (line image x0 y1 x0 y0 r g b alpha)
  (when fill
    (loop for x from (1+ x0) to (1- x1)
	  do (line image x (1+ y0) x (1- y1) r g b alpha))))

(defun poly-line (image xy-pairs r g b &optional (alpha 1.0))
  "Draw a poly-line in IMAGE (all corners are specified in XY-PAIRS as
elements in a flat list, as (X0 Y0 X1 Y1 X2 Y2 ...). Use R, G and B as colour
and ALPHA as alpha-blend (defaults to 1.0)."
  (loop for (x y . rest) on xy-pairs by #'cddr
	with x0 = nil
	with y0 = nil
	do (when x0
	     (line image x0 y0 x y r g b alpha))
	do (setf x0 x)
	do (setf y0 y)))

(defun circle-points (x y r)
  (remove-duplicates
   (loop for xn from 0 below (/ r (sqrt 2))
	 for yn = r then (if (> (distance x y (+ x (1+ xn)) (+ y yn))
				   r)
				(1- yn)
			      yn)
	 collect (list (+ x xn) (+ y yn))
	 collect (list (+ x xn) (- y yn))
	 collect (list (- x xn) (+ y yn))
	 collect (list (- x xn) (- y yn))
	 collect (list (+ x yn) (+ y xn))
	 collect (list (+ x yn) (- y xn))
	 collect (list (- x yn) (+ y xn))
	 collect (list (- x yn) (- y xn)))))

(defun bresencircle (image x y diam r g b alpha)
  (let ((points (circle-points x y diam)))
    (loop for (px py) in points
	  do (plot image px py r g b alpha))))

(defun circle-fill (image x-centre y-centre radius r g b &optional (alpha 1.0))
  "Draw a filled circle in IMAGE centred on <X-CENTER Y-CENTRE>, with the
specified DIAMETER. Use the R, G and B values provided and use ALPHA as the
alpha-blend parameter (defaults to 1.0). Fill the circle."
  (let* ((points (circle-points x-centre y-centre radius))
	 (adjust (let ((min (reduce #'min points :key #'cadr)))
		   (if (< min 0)
		       min
		     0))))
    (let ((x-values (make-array (1+ (* 2 radius)) :initial-element nil)))
      (loop for (cx cy) in points
	    do (let* ((ix (- cy (- y-centre radius)))
		      (store (aref x-values ix)))
		 (when store
		   (setf (car store) (min cx (car store)))
		   (setf (cdr store) (max cx (cdr store))))
		 (unless store
		   (setf (aref x-values ix) (cons cx cx)))))
      (loop for (xmin . xmax) across x-values
	    for y from (- y-centre radius)
	    do (cond ((= xmin xmax) (plot image xmin y r g b alpha))
		     (t (line image xmin y xmax y r g b alpha)))))))


(defun circle (image x-centre y-centre radius r g b &optional (alpha 1.0))
  "Draw a circle in IMAGE, centred on <X-CENTER Y-CENTRE>, with the specified
RADIUS. Use the R, G and B values provided and use ALPHA as the alpha-blend
parameter (defaults to 1.0). Does not have a useful return value."
  (bresencircle image x-centre y-centre radius r g b alpha))

(defun nexts (x y dir)
  (case dir
    (0 (let ((y1 (1+ y))) `((,(1- x) ,y1) (,x ,y1) (,(1+ x) ,y1))))
    (1 `((,(1- x) ,y) (,(1- x) ,(1+ y)) (,x ,(1+ y))))
    (2 (let ((x1 (1- x))) `((,x1 ,(1- y)) (,x1 ,y) (,x1 ,(1+ y)))))
    (3 `((,x ,(1- y)) (,(1- x) ,(1- y)) (,(1- x) ,y)))
    (4 (let ((y1 (1- y))) `((,(1+ x) ,y1) (,x ,y1) (,(1- x) ,y1))))
    (5 `((,(1+ x) ,y) (,(1+ x) ,(1- y)) (,x ,(1- y))))
    (6 (let ((x1 (1+ x))) `((,x1 ,(1+ y)) (,x1 ,y) (,x1 ,(1- y)))))
    (7 `((,(1+ x) ,y) (,(1+ x) ,(1+ y)) (,x ,(1+ y))))))

(defun find-dir (x0 y0 x1 y1)
  (cond ((and (= x0 x1) (< y0 y1)) 0)
	((and (> x0 x1) (< y0 y1)) 1)
	((and (> x0 x1) (= y0 y1)) 2)
	((and (> x0 x1) (> y0 y1)) 3)
	((and (= x0 x1) (> y0 y1)) 4)
	((and (< x0 x1) (> y0 y1)) 5)
	((and (< x0 x1) (= y0 y1)) 6)
	((and (< x0 x1) (< y0 y1)) 7)))

(defun ellipsis-points (x0 y0 x1 y1 long-axis)
  (let* ((delta-0-1 (distance x0 y0 x1 y1))
	 (delta-0-rim (+ delta-0-1
			 (/ (- long-axis delta-0-1) 2))))
    (cond ((and (= x0 x1) (= y0 y1))
	   (circle-points x0 y0 (truncate long-axis 2)))
	  (t (let ((xp (+ x0 (* (- x1 x0) (/ delta-0-rim delta-0-1))))
		   (yp (+ y0 (* (- y1 y0) (/ delta-0-rim delta-0-1)))))
	       (labels ((chooser (x y dir)
			  (let ((nexts (nexts x y dir))
				(mindelta long-axis)
				next-set)
			    (loop for (xn yn dir) in nexts
				  do (when (< (abs
					       (- (+ (distance x0 y0 xn yn)
						     (distance x1 y1 xn yn))
						  long-axis))
					      mindelta)
				       (setf next-set (list xn yn))
				       (setf mindelta
					     (abs
					      (- (+ (distance x0 y0 xn yn)
						    (distance x1 y1 xn yn))
						 long-axis)))))
			    (let ((xn (round (car next-set)))
				  (yn (round (cadr next-set))))
			      (list xn yn (find-dir x y xn yn)))))
			)
		 (let* ((base-dir (find-dir x0 y0 xp yp))
			(foo (chooser xp yp base-dir))
			(base-x (car foo))
			(base-y (cadr foo))
			(max-iter (ceiling (* long-axis pi))))
		   (cons (list base-x base-y)
			 (loop for n from 0 below max-iter
			       for (xn yn dir) = (chooser base-x base-y (find-dir xp yp base-x base-y)) then (chooser xn yn dir)
			       until (and (= xn base-x)
					  (= yn base-y))
			       collect (list xn yn))))))))))


(defun ellipsis (image x0 y0 x1 y1 long-axis r g b &optional (alpha 1.0))
  "Draw an ellipsis in IMAGE. The two foci are <X0 Y0> and <X1 Y1>. The long
axis is LONG-AXIS long and the colour of the ellipsis is the specified
R, G and B values. Use ALPHA as the alpha-blend value (defaults to 1.0)."
  (let ((points (ellipsis-points x0 y0 x1 y1 long-axis)))
    (loop for (ex ey) in points
	  do (plot image ex ey r g b alpha))))

(defun ellipsis-fill (image x0 y0 x1 y1 long-axis r g b &optional (alpha 1.0))
  (let ((points (ellipsis-points x0 y0 x1 y1 long-axis))
	(x-values (make-array long-axis :initial-element nil)))
    (let ((min-y (reduce #'min points :key #'cadr)))
      (loop for (ex ey) in points
	    do (let* ((ix (- ey min-y))
		      (slot (aref x-values ix)))
		 (when slot
		   (setf (car slot) (min (car slot) ex))
		   (setf (cdr slot) (max (cdr slot) ex)))
		 (unless slot
		   (setf (aref x-values ix) (cons ex ex)))))
      (loop for (min-x . max-x) across x-values
	    for ey from min-y
	    until (null min-x)
	    do (line image min-x ey max-x ey r g b alpha)))))

(defun make-color-table (image)
  (let ((colortab (make-hash-table :test 'equal))
	(data (image-data image)))
    (loop for n from 0 below (length data) by 3
	  with ix = 0
	  do (let ((key (coerce (subseq data n (+ n 3)) 'list)))
	       (unless (gethash key colortab)
		 (setf (gethash key colortab) ix)
		 (incf ix))))
    colortab))

(defun skippify-color-table (colortab)
  (let ((skippy-tab (skippy:make-color-table))
	(sorted-tab (sort (loop for blah being the hash-keys of colortab
				collect (list (gethash blah colortab) blah))
			  #'<=
			  :key #'car)))
    (loop for (ix (r g b)) in sorted-tab
	  do (skippy:add-color (skippy:rgb-color r g b) skippy-tab))
    skippy-tab))

(defun table-too-big-p (ctab)
  (let ((rev (make-hash-table)))
    (maphash (lambda (k v) (setf (gethash v rev) k)) ctab)
    (> (hash-table-count rev) 256)))

;;;(defun squeeze-color-table (tab image)
;;;  (cond ((<= (hash-table-count tab image) 256) tab)
;;;	(t (let ((freq (make-hash-table)))
;;;	     (loop for n from 0 below (length data) by 3
;;;		   do (let ((key (coerce (subseq data n (+ n 3)) 'list)))
;;;			(let ((ix (gethash key tab)))
;;;			  (incf (gethash ix freq 0)))))
;;;	     (let ((sorted (sort (loop for blah being the hash-keys of freq
;;;				collect (list (gethash blah freq) blah))
;;;				 #'<=
;;;				 :key #'car)))
;;;	       )))))

(defun write-gif-to-stream (image stream &optional (comment "a"))
  ;; Use Xach's skippy library
  (let ((colormap (make-color-table image)))
    (let ((gif-data (make-array (* (image::height image) (image::width image))
				:element-type '(unsigned-byte 8)))
	  (skippy-tab (image::skippify-color-table colormap)))
      (loop for n from 0 below (length gif-data)
	    do (let ((base-ix (* n 3)))
		 (let ((key (list (aref (image-data image) (+ base-ix 0))
				  (aref (image-data image) (+ base-ix 1))
				  (aref (image-data image) (+ base-ix 2)))))
		   (setf (aref gif-data n) (gethash key colormap)))))
      (skippy:write-data-stream
       (skippy:make-data-stream
        :height (image::height image)
        :width (image::width image)
        :color-table skippy-tab
        :comment comment
        :initial-images (list (skippy:make-image
                               :height (height image)
			       :width (width image)
			       :color-table skippy-tab
			       :image-data gif-data)))
       stream))))

(defun export-to-gif (image file-name &optional (comment "a"))
  (with-open-file (stream file-name
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8)
			  :if-does-not-exist :create)
    (write-gif-to-stream image stream comment)))

(defun image->png (image)
  (let* ((png (make-instance 'zpng:png
			     :width (width image)
			     :height (height image)))
	 (height (height image))
	 (width (width image))
	 (png-data (zpng:data-array png))
	 (img (image-data image)))
    (loop for x from 0 below width
	  do (loop for y from 0 below height
		   do (loop for delta from 0 to 2
			    with base-ix = (+ (* x 3) (* y width 3))
			    do (setf (aref png-data y x delta)
				     (aref img (+ base-ix delta))))))
    png))

(defun write-png-to-stream (image stream)
  (zpng:write-png-stream (image->png image) stream))

(defun export-to-png (image file-name) 
  (zpng:write-png (image->png image) file-name))


(in-package :net.hexapodia.image.pcf)

(defclass pcf-font ()
  ((table-count :reader table-count :initarg :table-count)
   (table :reader table :initarg :table)
   (data :accessor data :initarg :data) 
   (data-offset :accessor data-offset :initarg :data-offset)
   (accelerators :accessor accelerators :initarg :accelerators :initform nil)
   (char-tab :accessor char-tab :initarg :char-tab) 
   (property-table :accessor property-table :initarg :property-table)
   (metrics :accessor metrics :initarg :metrics)
   (ink-metrics :accessor ink-metrics :initarg :ink-metrics)
   (bitmap :accessor bitmap :initarg :bitmap)
   (encoding :accessor encoding :initarg :encoding)
   
   ))

(defstruct accelerator
  (format 0 :type integer)
  (noOverlap 0 :type integer)
  (constantMetrics 0 :type integer)
  (terminalFont 0 :type integer)
  (constantWidth 0 :type integer)
  (inkInside 0 :type integer)
  (inkMetrics 0 :type integer)
  (drawDirection 0 :type integer)
  (padding 0 :type integer)
  (fontAscent 0 :type integer)
  (fontDescent 0 :type integer)
  (maxOverlap 0 :type integer)
  (minbounds nil)
  (maxbounds nil)
  (ink-minbounds nil)
  (ink-maxbounds nil))

(defclass character-metrics ()
  ((left-bearing :accessor left-bearing :initarg :left-bearing)
   (right-bearing :accessor right-bearing :initarg :right-bearing) 
   (width :accessor width :initarg :width)
   (ascent :accessor ascent :initarg :ascent)
   (descent :accessor descent :initarg :descent)
   (attributes :accessor attributes :initarg :attributes)
   ))

(defclass character-encoding ()
  ((min-char :reader min-char :initarg :min-char)
   (max-char :reader max-char :initarg :max-char)
   (default-char :reader default-char :initarg :default-char)
   (glyph-table :reader glyph-table :initarg :glyph-table)
   ))

(defclass toc-entry ()
  ((toc-format :reader toc-format :initarg :toc-format)
   (toc-offset :reader toc-offset :initarg :toc-offset)
   (toc-size :reader toc-size :initarg :toc-size)
   (toc-type :reader toc-type :initarg :toc-type)
   ))

(defclass property-entry ()
  ((name-offset :reader name-offset :initarg :name-offset)
   (string-property-p :reader string-property-p :initarg :string-property-p)
   (value :reader value :initarg :value)
   (name :accessor name :initarg :name)
   ))

(defclass bitmap-data ()
  ((bitmap-format :reader bitmap-format :initarg :bitmap-format)
   (bitmap-count :reader bitmap-count :initarg :bitmap-count)
   (offsets :reader offsets :initarg :offsets)
   (data :reader data :initarg :data)
   ))

(defun make-font (table-count stream)
  (let ((table (make-array table-count :initial-element nil)))
    (make-instance 'pcf-font
		   :table-count table-count :table table
		   :data stream
		   :data-offset 8)))

(defun open-compressed-font (font-file)
  (let ((file (open font-file :element-type '(unsigned-byte 8))))
    (gzip-stream:make-gzip-input-stream file)))
     

(defun open-font (font-file)
  (let ((pathname (pathname font-file)))
    (if (string= (pathname-type pathname) "gz")
	(open-compressed-font font-file)
	(open font-file :element-type '(unsigned-byte 8)))))

(defgeneric cmp (seq-a seq-b)
  (:method ((seq-a vector) (seq-b vector)) (every #'equal seq-a seq-b))
  (:method  ((seq-a vector) (seq-b string))
    (every #'equal seq-a (map 'vector #'char-int seq-b)))
  (:method ((seq-a string) (seq-b vector)) (cmp seq-b seq-a))
  (:method ((seq-a string) (seq-b string)) (string= seq-a seq-b)))



(defun make-int (seq format signed)
  (let ((mid (case format
	       (:lsb (reduce (lambda (a b) (+ a (* 256 b))) seq :from-end t))
	       (:msb (reduce (lambda (a b) (+ (* a 256) b)) seq)))))
    (cond ((and signed
		(not (zerop (logand mid
				    (ash 1 (1- (* 8 (length seq))))))))
	   (- 0 (logxor mid (make-int (make-array (length seq) :element-type '(unsigned-byte 8) :initial-element 255) :msb nil)) 1 ))
	  (t mid))))

  
(defun read-int (font octets &key (format :lsb) (signed nil))
  (let ((tmp (make-array octets :element-type '(unsigned-byte 8) :initial-element 0))
	(stream (data font)))
    (read-sequence tmp stream)
    (incf (data-offset font) octets)
    (make-int tmp format signed)))

(defun read-lsbint32 (stream)
  (read-int stream 4))

(defun read-int16 (stream &key (format :lsb))
  (read-int stream 2 :format format :signed t))

(defun read-uint16 (stream &key (format :lsb))
  (read-int stream 2 :format format :signed nil))

(defun read-octet (stream)
  (read-int stream 1))

(defun font-name (filename)
  (let ((pathname (pathname filename)))
    (cond ((string-equal "gz" (pathname-type pathname)) (string-downcase
							 (pathname-name pathname)))
	  (t (format nil "~a.~a"
		     (string-downcase (pathname-name pathname))
		     (string-downcase (pathname-type pathname)))))))

(defun read-toc-entry (font)
  (let* ((type (read-lsbint32 font))
	 (format (read-lsbint32 font))
	 (size (read-lsbint32 font))
	 (offset (read-lsbint32 font)))
    (make-instance 'toc-entry
		   :toc-type type :toc-format format
		   :toc-size size :toc-offset offset)))		

(defun find-toc-entry (font)
  (find-if (lambda (toc-entry)
	     (= (data-offset font) (toc-offset toc-entry)))
	   (table font)))


(defun read-property-entry (font toc-data)
  (declare (ignorable toc-data))
  (let* ((format (read-lsbint32 font))
	 (byte-order (case (logand format +PCF-BYTE-MASK+)
		       (0 :lsb)
		       (t :msb)))
	 (nprops (read-int font 4 :format byte-order))
	 (proptab (make-array nprops)))
	
    (loop for ix from 0 below nprops
       do (setf (aref proptab ix)
		(let* ((offset (read-int font 4 :format byte-order))
		       (prop-p (read-int font 1 :format byte-order))
		       (value  (read-int font 4 :format byte-order)))
		  (make-instance'property-entry
		   :name-offset offset :string-property-p prop-p
		   :value value :name ""))))
    (unless (zerop (mod nprops 4))
      (read-int font (- 4 (mod nprops 4))))
    (let* ((string-bytes (read-int font 4 :format byte-order))
	   (string-data (make-array string-bytes :element-type '(unsigned-byte 8))))
      (read-sequence string-data (data font))
      (incf (data-offset font) string-bytes)
      
      (loop for pent across proptab
	 do (setf (name pent)
		  (;sb-ext:octets-to-string
           flexi-streams:octets-to-string ;; modified by Kevin Lynx 12.7.2011
		   string-data
		   :start (name-offset pent)
		   :end (position 0 string-data :start (name-offset pent))))))
    (setf (property-table font) proptab)))

(defun read-metrics-entry (font compressed-p byte-order)
  (flet ((read-next ()
	   (cond (compressed-p (- (read-int font 1) 128))
		 (t (read-int font 2 :format byte-order :signed t)))))
    (let* ((left-bearing (read-next))
	   (right-bearing (read-next))
	   (width (read-next))
	   (ascent (read-next))
	   (descent (read-next))
	   (attributes (if compressed-p 0
			   (read-int font 2 :format byte-order))))
      (make-instance 'character-metrics
		     :left-bearing left-bearing :right-bearing right-bearing
		     :width width :ascent ascent :descent descent
		     :attributes attributes))))

(defun read-metrics-table (font toc-entry)
  (let ((ink-metrics-p (= (toc-type toc-entry) +PCF-INK-METRICS+)))
    (let* ((format (read-lsbint32 font))
	   (byte-order (case (logand format +PCF-BYTE-MASK+)
			(0 :lsb)
			(t :msb)))
	   (compressed-p (= +PCF-COMPRESSED-METRICS+
			    (logand format +PCF-COMPRESSED-METRICS+))))
      (let* ((metric-count (read-int font (if compressed-p 2 4) :format byte-order))
	     (table (make-array metric-count)))
	(loop for ix from 0 below metric-count
	     do (setf (aref table ix) (read-metrics-entry font compressed-p byte-order)))
	(if ink-metrics-p
	    (setf (ink-metrics font) table)
	    (setf (metrics font) table))))))

(defun read-accelerator (font toc-entry)
  (declare (ignorable toc-entry))
  (let* ((format (read-lsbint32 font))
	 (no-overlap (read-int font 1))
	 (constant-metrics (read-int font 1))
	 (terminal-font (read-int font 1))
	 (constant-width (read-int font 1))
	 (ink-inside (read-int font 1))
	 (draw-direction (read-int font 1))
	 (padding (read-int font 1))
	 (byte-order (case (logand format +PCF-BYTE-MASK+)
			(0 :lsb)
			(t :msb)))
	 (font-ascent (read-int font 4 :format byte-order :signed t))
	 (font-descent (read-int font 4 :format byte-order :signed t))
	 (max-overlap  (read-int font 4 :format byte-order :signed t))
	 (min-bounds (read-metrics-entry font nil byte-order))
	 (max-bounds (read-metrics-entry font nil byte-order)))
    (let* ((ink-min-bounts (if (= +PCF-ACCEL-W-INKBOUNDS+
				  (logand format +PCF-ACCEL-W-INKBOUNDS+))
			       (read-metrics-entry font nil byte-order)
			       min-bounds))
	   (ink-max-bounts (if (= +PCF-ACCEL-W-INKBOUNDS+
				  (logand format +PCF-ACCEL-W-INKBOUNDS+))
			       (read-metrics-entry font nil byte-order)
			       max-bounds)))
      (push (make-accelerator :format format :nooverlap no-overlap
			      :constantmetrics constant-metrics
			      :terminalfont terminal-font
			      :constantwidth constant-width
			      :inkinside ink-inside
			      :drawdirection draw-direction
			      :fontascent font-ascent
			      :fontdescent font-descent :maxoverlap max-overlap
			      :minbounds min-bounds :maxbounds max-bounds
			      :ink-minbounds ink-min-bounts
			      :ink-maxbounds ink-max-bounts)
	    (accelerators font)))))

(defun read-bitmap-table (font toc-entry)
  (declare (ignorable toc-entry))
  (let* ((format (read-lsbint32 font))
	 (byte-order (case (logand format +PCF-BYTE-MASK+)
			(0 :lsb)
			(t :msb)))
	 (glyph-count (read-int font 4 :format byte-order))
	 (offsets (make-array glyph-count)))
    (loop for ix from 0 below glyph-count
	 do (setf (aref offsets ix) (read-int font 4 :format byte-order)))
    (let ((bitmap-sizes (make-array 4)))
      (loop for ix from 0 below 4
	   do (setf (aref bitmap-sizes ix)
		    (read-int font 4 :format byte-order)))
      (let* ((bitmap-count (aref bitmap-sizes (logand format 3)))
	     (bitmap-data (make-array bitmap-count :element-type '(unsigned-byte 8))))
	(read-sequence bitmap-data (data font))
	(incf (data-offset font) bitmap-count)
	(setf (bitmap font) (make-instance 'bitmap-data
					   :bitmap-format format
					   :bitmap-count glyph-count
					   :offsets offsets
					   :data bitmap-data))))))

(defun read-encoding-table (font toc-entry)
  (declare (ignorable toc-entry))
  (let* ((format (read-lsbint32 font))
	 (byte-order (case (logand format +PCF-BYTE-MASK+)
			(0 :lsb)
			(t :msb)))
	 (min-byte-2 (read-int font 2 :format byte-order :signed t))
	 (max-byte-2 (read-int font 2 :format byte-order :signed t))
	 (min-byte-1 (read-int font 2 :format byte-order :signed t))
	 (max-byte-1 (read-int font 2 :format byte-order :signed t))
	 (default (read-int font 2 :format byte-order :signed t)))
    (let* ((glyph-count (* (1+ (- max-byte-2 min-byte-2)) (1+ (- max-byte-1 min-byte-1))))
	   (glyph-map (make-array glyph-count :element-type '(unsigned-byte 16))))
      (loop for ix from 0 below glyph-count
	 do (setf (aref glyph-map ix) (read-int font 2 :format byte-order)))
      (setf (encoding font)
	    (make-instance 'character-encoding
			   :min-char 0
			   :max-char (1- glyph-count)
			   :default-char default
			   :glyph-table glyph-map)))))

(defun generate-font (font)
  (let ((char-tab (make-hash-table))
	(enctab (encoding font))
	(bitmap (bitmap font))
	(metrics (metrics font)))
    (let ((bit-offset (offsets bitmap))
	  (bit-data (data bitmap))
	  (stride (svref #(1 2 4) (logand (bitmap-format bitmap) 3)))
	  (byte-order (case (logand (bitmap-format bitmap) +PCF-BYTE-MASK+)
			(0 :lsb)
			(t :msb)))
	  (bit-order (case (logand (bitmap-format bitmap) +PCF-BIT-MASK+)
		       (0 :lsb)
		       (t :msb))))
      (loop for char-code from (min-char enctab) to (max-char enctab)
	   for ch = (code-char char-code)
	   for map = (aref (glyph-table enctab) char-code)
	   do (unless (= map #xffff)
		(let ((metric (aref metrics map))) 
		  (let ((height (+ (ascent metric) (descent metric)))
			(width (width metric)))
		    (let ((character (make-array (list height width) :element-type 'bit :initial-element 0))
			  (shift (if (eql bit-order :lsb) 1 -1))
			  (base  (if (eql bit-order :lsb)
				     1
				     (ash 1 (1- (* 8 stride))))))
		      (loop for ix-x from 0 below height
			 for offset from (aref bit-offset map) by stride
			 for seq = (subseq bit-data offset (+ offset stride))
			 do (let ((row (make-int seq byte-order nil)))
			      (loop for ix-y from 0 below width
				 for mask = base then (ash mask shift)
				 do (when (= mask (logand mask row))
				      (setf (aref character ix-x ix-y) 1)))))
		      (setf (gethash ch char-tab) character)))))))
    (setf (char-tab font) char-tab)))
				  
			      

(defun read-next-entry (font)
  (let ((toc-data (find-toc-entry font)))
    (case (toc-type toc-data)
      (#.+PCF-PROPERTIES+ (read-property-entry font toc-data))
      (#.+PCF-ACCELERATORS+ (read-accelerator font toc-data))
      ((#.+PCF-METRICS+ #.+PCF-INK-METRICS+) (read-metrics-table font toc-data))
      (#.+PCF-BITMAPS+ (read-bitmap-table font toc-data))
      (#.+PCF-BDF-ENCODINGS+ (read-encoding-table font toc-data))
      ))
  (let ((char (make-array 1 :element-type '(unsigned-byte 8))))
    (loop while (null (find-toc-entry font))
       do (progn (read-sequence char (data font))
		 (incf (data-offset font))))))

(defun read-all-entries (font)
  (let ((old-offset (data-offset font)))
    (read-next-entry font)
    (unless (= old-offset (data-offset font))
      (read-all-entries font))))

(defun read-font (filename)
  (let ((stream (open-font filename))
	(font-name (font-name filename))
	(initial (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence initial stream)
    (when (cmp initial #(1 102 99 112))
      (read-sequence initial stream)
      (let* ((table-count (make-int initial :lsb nil))
	     (font (make-font table-count stream)))
	(loop for ix from 0 below table-count
	     do (setf (aref (table font) ix)
		      (read-toc-entry font)))
	       
	(read-all-entries font)
	(close stream)
	(generate-font font)
	(values font-name (char-tab font) font)))))

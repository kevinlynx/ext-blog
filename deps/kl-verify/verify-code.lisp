;;;;
;;;; verify-code.lisp
;;;; Kevin Lynx
;;;; Use image library to generate a simple verify code image.
;;;;
(in-package :kl-verify)

(defparameter +stringset+ 
             "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun get-rand-char ()
  (elt +stringset+ (random (length +stringset+))))

(defun get-rand-str (cnt)
  (if (<= cnt 0)
    nil
    (concatenate 'string (string (get-rand-char)) (get-rand-str (1- cnt)))))

(defun get-rand-op ()
  (values (random 100) (random 100)))

(defun get-rand-exp ()
  (multiple-value-bind (op1 op2) (get-rand-op)
    (let ((m (random 2)))
      (case m
        (0 (values (format nil "~a+~a=" op1 op2) (+ op1 op2)))
        (1 (values (format nil "~a*~a=" op1 op2) (* op1 op2)))))))

(defun make-image (w h r g b)
  (let ((image (image:make-image w h)))
    (image:rect image 0 0 (1- w) (1- h) t r g b)
    image))

(defun get-str-rect (str image)
  (image:text image str 0 0 0 0 0 0.0))

(defun get-rand-color ()
  (+ (random 200) 56))

(defun draw-char (c image sx ah)
  (multiple-value-bind (w h) (get-str-rect (string c) image)
    (let ((r (get-rand-color))
          (g (get-rand-color))
          (b (get-rand-color))
          (y (random (- ah h))))
      (image:text image (string c) sx y r g b)
      (+ sx w))))
 
(defun get-noise-color ()
  (+ (random 80) 176))

(defun draw-rand-line (image w h)
  (let ((x0 (random w))
        (y0 (random h))
        (x1 (random w))
        (y1 (random h))
        (r (get-noise-color))
        (g (get-noise-color))
        (b (get-noise-color)))
   (image:line image x0 y0 x1 y1 r g b)))

(defun draw-noise (image w h)
  (let ((cnt (+ (random 5) 2)))
    (loop for x from 1 to cnt
      do (draw-rand-line image w h))))

(defgeneric generate-verify-code (name w h str-cnt type))

(defmethod generate-verify-code (stream w h str-cnt (type (eql 'pic)))
  (let* ((image (make-image w h 255 255 255))
         (str (get-rand-str str-cnt))
         (total-w (get-str-rect str image))
         (x (random (floor (/ (- w total-w) 2)))))
    (map nil #' (lambda (c) 
                  (let ((sx (draw-char c image x h)))
                    (setf x (+ sx (random 15)))))
         str)
    (draw-noise image w h)
    ;(image:export-image image stream)
    ; to support flexi:with-output-to-sequence
    (image::export-to-stream image stream :format :gif)
    str))

(defmethod generate-verify-code (stream w h str-cnt (type (eql 'exp)))
  (declare (ignore str-cnt))
  (multiple-value-bind (str ret) (get-rand-exp)
    (let* ((image (make-image w h 255 255 255))
           (x 0) (y 0))
      (multiple-value-bind (sw sh) (get-str-rect str image)
        (setf x (floor (/ (- w sw) 2)))
        (setf y (floor (/ (- h sh) 2))))
      (image:text image str x y 0 0 0)
      (image::export-to-stream image stream :format :gif)
      (format nil "~a" ret))))

(defmethod generate-verify-code ((name string) w h str-cnt type)
  (with-open-file (stream name :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist :create)
    (generate-verify-code stream w h str-cnt type)))

(defun load-font (font)
  (image::read-font font)
  (image::use-font (net.hexapodia.image.pcf::font-name font)))


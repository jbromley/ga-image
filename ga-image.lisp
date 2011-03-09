;;;; ga-image.lisp

(in-package #:ga-image)

;;;
;;; Global parameters
;;;

(defparameter +side-bits+ 3
  "The number of bits used to represent how many sides in a polygon. This can
range from three sides to 10 sides.")

(defparameter +min-sides+ 3
  "The minimum number of vertices for a polygon.")

(defparameter +max-sides+ (+ +min-sides+ (1- (expt 2 +side-bits+)))
  "The maximum number of vertices for a polygon.")

(defparameter +color-bits+ 32
  "The number of bits to represent RGBA colors.")

(defparameter +position-bits+ 10
  "The number of bits in a coordinate component.")

(defparameter +sides-start+ 0
  "The bit position for the start of the bits that specify the number
of sides.")

(defparameter +color-start+ +side-bits+
  "The bit position for the start of the bits that specify the polygon color.")

(defparameter +vertices-start+ (+ +color-start+ +color-bits+)
  "The bit position for the start of the vertex bits.")

(defparameter +vertices-end+ (+ +vertices-start+ 
				(* 2 +max-sides+ +position-bits+))
  "The bit position for the end of the vertices and the end of a
  single polygon.")

(defparameter +transparent+ (make-instance 'cl-colors:rgba :alpha 0)
  "The color for a completely invisible pixel.")


;;;
;;; Global variables
;;;

(defvar *image-width*
  "The width of the target image.")

(defvar *image-height*
  "The height of the target image.")


;;;
;;; Problem definition for the GA engine
;;;

(defclass image-problem ()
  ((target-image :initarg :target-image :reader target-image :initform nil)
   (polygons :initarg :polygons :reader polygons :initform nil)
   (width :accessor width :initform nil)
   (height :accessor height :initform nil))
  (:documentation "The configuration for the evolving image problem."))

(defmethod initialize-instance :after ((problem image-problem) &rest rest)
  "Set the instance's WIDTH and HEIGHT slots from the TARGET-IMAGE."
  (declare (ignore rest))
  (setf (width problem) (cl-cairo2:image-surface-get-width 
			 (target-image problem)))
  (setf (height problem) (cl-cairo2:image-surface-get-height
			  (target-image problem))))

(defun make-image-problem (image-file polygons)
  "Create an instance of IMAGE-PROBLEM with the target image read from
IMAGE-FILE using the number of polygons specified by POLYGONS for the
reconstruction."
  (let ((surface (cl-cairo2:image-surface-create-from-png image-file)))
    (make-instance 'image-problem :target-image surface :polygons polygons)))

(defmethod genome-length ((problem image-problem))
  "Determine the number of bits require dfor a genome to solve the
specified image reconstruction problem."
  (* (polygons problem) 
     (+ +side-bits+ (* 2 +position-bits+ +max-sides+) +color-bits+)))

(defmethod fitness ((problem image-problem) genome)
  "The fitness function for the image reconstruction problem is the
sum of the absolute differences between all channels of all pixels in
the phenotype from GENOME and the TARGET-IMAGE."
  ; TODO fitness function
  0)

(defmethod fitness-comparator ((problem image-problem))
  "Return a fitness comparator function that takes two genomes and
  returns T if the first is more fit according to the characteristics of
  the PROBLEM."
  (lesser-comparator problem))

(defun solve-image (problem population-size mutation-rate)
  "Run the GA engine against the PROBLEM."
  (let* ((gene-pool 
	  (solve problem population-size mutation-rate
		 (fitness-terminator problem
				     (length (target-genome problem)))))
         (best-genome (most-fit-genome gene-pool (fitness-comparator problem))))
    (format t "~%Best = ~F~%Average = ~F~%"
            (fitness problem best-genome)
            (average-fitness problem gene-pool))))


;;; Helper functions for the problem

(defstruct point 
  "Represents a point in 2-D Cartesian plane."
  (x 0)
  (y 0))

(defstruct polygon
  "Represents a polygon with the specified number of sides, color and
list of vertices."
  (sides 3)
  (color +transparent+)
  (vertices '()))

(defun decode-sides (bits)
  "From BITS, decode the number of sides in this polygon."
  (+ +min-sides+ (bit-vector->integer bits)))

(defun bits->color-comp (bits)
  "Convert BITS from an integer RGBA component in the range 0 to 255
to a float RGBA component in the range zero to one."
  (coerce (/ (bit-vector->integer bits) 255) 'float))

(defun decode-color (rgba-bits)
  "From BITS, decode a 32-bit RGBA color."
  (make-instance 'rgba 
		 :red (bits->color-comp (subseq rgba-bits 0 8))
		 :green (bits->color-comp (subseq rgba-bits 8 16))
		 :blue (bits->color-comp (subseq rgba-bits 16 24))
		 :alpha (bits->color-comp (subseq rgba-bits 24))))

(defun bits->float (bits max-val)
  "Convert BITS into a double between 0 and max-val."
  (* (coerce max-val 'float)
     (/ (bit-vector->integer bits) (1- (expt 2 (length bits))))))

(defun decode-point (bits)
  "From BITS, decode a point."
  (make-point :x (bit-vector->integer (subseq bits 0 +position-bits+))
	      :y (bit-vector->integer (subseq bits +position-bits+))))

; TODO decode-vertices feels decidedly non-Lispy.
(defun decode-vertices (sides bits)
  "From BITS, decode the polygon with SIDES number of sides."
  (do ((vertices '())
       (offset1 0 (+ offset1 (* 2 +position-bits+)))
       (offset2 +position-bits+ (+ offset2 (* 2 +position-bits+)))
       (n 0 (1+ n)))
      ((= n sides) vertices)
    (let ((x (bit-vector->integer (subseq bits offset1 offset2)))
	  (y (bit-vector->integer (subseq bits offset2
					  (+ offset2 +position-bits+)))))
      (push (make-point :x x :y y) vertices))))

(defun decode-genome (genome &optional (phenotype '()))
  "Decode GENOME into a list of colored polygons with between three
and ten sides."
  (if (zerop (length genome))
      phenotype
      (let* ((sides (decode-sides (subseq genome 0 +color-start+)))
	     (color (decode-color 
		     (subseq genome +color-start+ +vertices-start+)))
	     (vertices (decode-vertices sides 
		       (subseq genome +vertices-start+ +vertices-end+))))
	(push (make-polygon :sides sides :color color :vertices vertices)
	      phenotype)
	(decode-genome (subseq genome +vertices-end+) phenotype))))

;;;
;;; Rendering functions
;;;

(defun load-png (filename)
  "Load FILENAME into an image surface. The file to be loaded must be
a PNG file."
  (cl-cairo2:image-surface-create-from-png filename))

(defun draw-polygon (poly &optional (context cl-cairo2:*context*))
  (let ((v (rest (polygon-vertices poly)))
	(first-pt (first (polygon-vertices poly))))
    (cl-cairo2:move-to (point-x first-pt) (point-y first-pt) context)
    (loop for pt in v 
       do (cl-cairo2:line-to (point-x pt) (point-y pt) context)))
  (cl-cairo2:close-path context)
  (cl-cairo2:set-source-color (polygon-color poly) context)
  (cl-cairo2:fill-path context))

(defun render-genome-to-surface (problem genome)
  "Renders the polygons represented by GENOME onto a Cairo image
surface. Returns the new surface."
  (let* ((surface (cl-cairo2:create-image-surface :argb32 (width problem)
						  (height problem)))
	 (context (cl-cairo2:create-context surface)))
    (dolist (poly (decode-genome genome))
      (draw-polygon poly context))
    (cl-cairo2:destroy context)
    surface))

(defun render-genome-to-window (problem genome)
  "Renders the polygons represented by GENOME into a window using
PROBLEM to provide image size information. Returns the context so it
can be destroyed later."
  (let ((surface (render-genome-to-surface problem genome))
	(context (cl-cairo2:create-xlib-image-context (width problem) 
						      (height problem))))
    (cl-cairo2:set-source-surface surface 0 0 context)
    (cl-cairo2:paint context)
    (cl-cairo2:destroy surface)
    context))


						 
;;;; ga-image.lisp

(in-package #:ga-image)


;;; global variables

(defvar *image-width* 640
  "Width of the target image.")

(defvar *image-height* 480
  "Height of the target image.")

(defvar *max-sides* 8
  "Maximum number of sides for a polygon.")

(defvar *individual-polygons* 64
  "The number of polygons that make up an individual in the population.")

(defvar *population-size* 32
  "The number of individuals in a population.")



;;; data structures

(defstruct point
  (x 0)
  (y 0))

(defstruct polygon
  (sides 3)
  (vertices '())
  (color (cl-colors:add-alpha cl-colors:+RED+ 0.5)))


;;; data structure functions

(defun translate-point (pt offset)
  "Translate the point PT by the offset in the point OFFSET."
  (make-point :x (+ (point-x pt) (point-x offset))
	      :y (+ (point-y pt) (point-y offset))))



;;; functions to generate random things

(defun random-in-range (min max)
  "Return a random number between MIN and MAX. MAX must be greater
  than MIN."
  (+ min (random (- max min))))

(defun random-in-interval (middle half-width)
  "Return a random number between MIDDLE - HALF-WIDTH and MIDDLE +
HALF-WIDTH."
  (random-in-range (- middle half-width) (+ middle half-width)))

(defun random-angle (&optional (max-angle (* 2 pi)))
  "Return a random angle between 0 and MAX-ANGLE radians."
  (random max-angle))

(defun random-point (&optional (max-x *image-width*) (max-y *image-height*))
  "Create a point at a random location with X between 0 and MAX-X and
  Y between 0 and MAX-Y."
  (make-point :x (random max-x) :y (random max-y)))

(defun random-color (&optional (max-alpha 1.0))
  "Create a random RGBA color with maximum alpha of MAX-ALPHA."
  (make-instance 'cl-colors:rgba :alpha (random max-alpha) :blue (random 1.0)
		 :green (random 1.0) :red (random 1.0)))

(defun random-polygon-2 (&optional (max-alpha 1.0) (max-x *image-width*) 
		       (max-y *image-height*))
  "Create a random polygon that fits in the area defined by MAX-X and MAX-Y
and with a maximum alpha given by MAX-ALPHA."
  (let ((sides (random-in-range 3 *max-sides*))
	(vertices '()))
    (dotimes (i sides)
      (push (make-point :x (random max-x) :y (random max-y)) vertices))
    (make-polygon :sides sides :vertices vertices 
		  :color (random-color max-alpha))))

(defun random-polygon (&optional (max-alpha 1.0) (max-x *image-width*)
			 (max-y *image-height*))
  "Create a random polygon that fits in the area defined by MAX-X and MAX-Y
and with a maximum alpha of MAX-ALPHA. This function chooses a size, and then
generates a random polygon of that size and then adds a random offset such
that the whole polygon will fit insides the image."
  (let ((sides (random-in-range 3 *max-sides*))
	(vertices '())
	(size (random-in-range 1 (min max-x max-y))))
    (let ((offset (make-point :x (random (- max-x size))
			      :y (random (- max-y size)))))
      (dotimes (i sides)
	(push (make-point :x (+ (random size) (point-x offset))
			  :y (+ (random size) (point-y offset))) vertices))
      (make-polygon :sides sides :vertices vertices
		    :color (random-color max-alpha)))))



;;; genetic algorithm functions
(defun create-individual (&optional (num-polygons *individual-polygons*))
  "Creates an individual random image composed of NUM-POLYGONS polygons."
  (let ((individual '()))
    (dotimes (i num-polygons)
      (push (random-polygon) individual))
    individual))

(defun create-initial-population (&optional (population-size *population-size*)
				  (num-polygons *individual-polygons*))
  (let ((pop '()))
    (dotimes (i population-size)
      (push (create-individual num-polygons) pop))
    pop))



;;; rendering functions
(defun draw-polygon (poly &optional (context cl-cairo2:*context*))
  (let ((v (rest (polygon-vertices poly)))
	(first-pt (first (polygon-vertices poly))))
    (cl-cairo2:move-to (point-x first-pt) (point-y first-pt) context)
    (loop for pt in v 
       do (cl-cairo2:line-to (point-x pt) (point-y pt) context)))
  (cl-cairo2:close-path context)
  (cl-cairo2:set-source-color (polygon-color poly) context)
  (cl-cairo2:fill-path context))

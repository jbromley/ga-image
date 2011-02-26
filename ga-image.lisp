;;;; ga-image.lisp

(in-package #:ga-image)


(defvar *image-width* 640
  "Width of the target image.")

(defvar *image-height* 480
  "Height of the target image.")

(defvar *max-sides* 8
  "Maximum number of sides for a polygon.")

(defstruct point
  (x 0)
  (y 0))

(defstruct polygon
  (sides 3)
  (vertices '()))

(defstruct r-polygon
  (sides 3)
  (center (make-point))
  (vertices '()))

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

(defun random-polygon (&optional (max-x *image-width*) (max-y *image-height*))
  "Create a random polygon that fits in the area defined by MAX-X and MAX-Y."
  (let ((sides (random-in-range 3 *max-sides*))
	(vertices '()))
    (dotimes (i sides)
      (push (make-point :x (random max-x) :y (random max-y)) vertices))
    (make-polygon :sides sides :vertices vertices)))

(defun draw-polygon (poly color)
  (let ((v (rest (polygon-vertices poly)))
	(first-pt (first (polygon-vertices poly))))
    (cl-cairo2:move-to (point-x first-pt) (point-y first-pt))
    (loop for pt in v do (cl-cairo2:line-to (point-x pt) (point-y pt))))
  (cl-cairo2:close-path)
  (cl-cairo2:set-source-color color)
  (cl-cairo2:fill-path))
  
    
  
	    
    
	    

      
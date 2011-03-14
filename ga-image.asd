;;;; ga-image.asd

(cl:defpackage :ga-image-system
  (:use :cl :asdf))
(cl:in-package :ga-image-system)

(asdf:defsystem #:ga-image
    :name "ga-image"
    :author "J. Bromley <jbromley@gmail.com>"
    :version "0.1"
    :description "Duplicate an image with a genetic algorithm."
    :depends-on (#:org.softwarematters.ga #:cl-cairo2-xlib #:cl-colors)
    :components ((:file "package")
		 (:file "ga-image" :depends-on ("package"))))



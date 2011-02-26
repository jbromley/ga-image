;;;; ga-image.asd

(cl:defpackage :ga-image-system
  (:use :cl :asdf))
(cl:in-package :ga-image-system)

(asdf:defsystem #:ga-image
    :name "ga-image"
    :author "J. Bromley <jbromley@gmail.com>"
    :version "0.1"
    :description "Duplicate an image with a genetic algorithm."
    :depends-on (#:cl-cairo2 #:cl-colors)
    :components ((:file "package")
		 (:file "ga-image" :depends-on ("package"))))



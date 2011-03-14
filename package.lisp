;;;; package.lisp

(defpackage #:ga-image
  (:nicknames :gai)
  (:use #:cl #:cl-cairo2 #:cl-colors #:org.softwarematters.ga)
  (:export
   #:run-evolution
   #:evolve-image))


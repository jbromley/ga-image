;;;; ga-image.asd

(asdf:defsystem #:nil
  :serial t
  :depends-on (#:cl-cairo2)
  :components ((:file "package")
               (:file "nil")))


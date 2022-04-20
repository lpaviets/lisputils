;;; numbra.asd

(asdf:defsystem #:numbra
  :description "Generic utilities"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:heap)
  :components ((:file "packages")
               (:file "heap+")
               (:file "utilities")
               (:file "macros")
               (:file "algorithms")
               (:file "parsing")))

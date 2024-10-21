;;; numbra.asd

(asdf:defsystem #:numbra
  :description "Generic utilities"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:heap #:trivia)
  :components ((:file "packages")
               (:file "macros")
               (:file "point")
               (:file "quad-tree")
               (:file "heap+")
               (:file "utilities")
               (:file "subsets")
               (:file "bits")
               (:file "queue")
               (:file "hash-table")
               (:file "hset")
               (:file "iter")
               (:file "intervals")
               (:file "union-find")
               (:file "graph-traversals")
               (:file "graph-basics")
               (:file "graph-structure")
               (:file "flows")
               (:file "grids")
               (:file "periods")
               (:file "parsing")
               (:file "printing")
               (:module "extras"
                :components
                ((:file "machine")
                 (:file "aoc")))))

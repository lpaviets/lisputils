(defpackage #:org.numbra.perso
  (:use #:cl)
  (:nicknames #:numbra)
  (:export
   ;; Parsing
   #:read-file-as-lines
   #:read-file-one-line
   #:read-file-as-lines-blocks
   #:read-file-as-integers
   #:read-file-as-sexprs
   #:read-array
   #:read-file-as-array
   ;; Small parsing utils
   #:coma-separated-int-line
   #:split-word-int
   #:parse-digit
   ;; Generic utilities
   #:neighbours
   #:flip
   #:range
   #:permutations
   #:sublists-length
   #:extremum
   #:extremum-array
   #:deepcopy
   #:shuffle
   ;; Algorithms
   #:shortest-path
   #:shortest-path-dec-key
   #:a-star
   #:dfs
   ;; Macros
   #:do-array
   #:do-line
   #:whereas))

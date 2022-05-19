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
   ;; Data structures:
   ;; - Heap
   #:make-heap
   #:heap-push
   #:heap-pop
   #:heap-peek
   #:heap-update
   #:heap-flush
   #:heap-contents
   ;; - Hash tables
   #:ht-count-if
   #:ht-count
   ;; Macros
   #:do-array
   #:do-line
   #:whereas
   ;; Printing
   #:print-array
   #:print-hash))

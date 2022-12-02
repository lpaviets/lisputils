(defpackage #:org.numbra.perso.utils
  (:use #:cl)
  (:export
   #:with-gensyms
   #:do-array
   #:do-line
   #:neighbours
   #:flip
   #:range
   #:lazy-range
   #:permutations
   #:sublists-length
   #:argmax
   #:deepcopy
   #:shuffle
   #:for
   #:make-iterable))

(defpackage #:org.numbra.perso.io
  (:use #:cl)
  (:export
   ;; Read input
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
   ;; Printing
   #:print-array
   #:print-hash))

(defpackage #:org.numbra.perso.ds
  (:use #:cl)
  (:export
   ;; - Points
   #:point
   #:add-point
   #:sub-point
   #:with-point
   ;; - Heap
   #:make-heap
   #:heap-push
   #:heap-pop
   #:heap-peek
   #:heap-update
   #:heap-flush
   #:heap-contents
   ;; - Hash tables
   #:ht-create
   #:ht-count-if
   #:ht-count
   #:ht-from-sequence
   ;; Quad-trees
   #:make-box
   #:make-bounding-box
   #:qtree-insert
   #:make-qtree-from-list
   #:with-box-sides
   #:make-qtree
   #:qtree-intersect-list
   #:qtree-valid-p
   #:qtree-intersect-some))

(defpackage #:org.numbra.perso.algo
  (:use #:cl #:org.numbra.perso.ds)
  (:export
   #:shortest-path
   #:shortest-path-dec-key
   #:a-star
   #:dfs))

(defpackage #:org.numbra.perso.aoc
  (:use #:cl)
  (:export
   #:gen-packages
   #:create-files-templates))

(defpackage #:org.numbra.perso
  (:use #:cl
        #:org.numbra.perso.utils
        #:org.numbra.perso.io
        #:org.numbra.perso.ds
        #:org.numbra.perso.algo
        #:org.numbra.perso.aoc)
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
   #:lazy-range
   #:permutations
   #:sublists-length
   #:argmax
   #:deepcopy
   #:shuffle
   #:for
   #:make-iterable
   ;; Algorithms
   #:shortest-path
   #:shortest-path-dec-key
   #:a-star
   #:dfs
   ;; Data structures:
   ;; - Points
   #:point
   #:add-point
   #:sub-point
   #:with-point
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
   #:ht-create
   #:ht-from-sequence
   ;; Quad-trees
   #:make-box
   #:make-bounding-box
   #:qtree-insert
   #:make-qtree-from-list
   #:with-box-sides
   #:make-qtree
   #:qtree-intersect-list
   #:qtree-valid-p
   #:qtree-intersect-some
   ;; Macros
   #:with-gensyms
   #:do-array
   #:do-line
   ;; Printing
   #:print-array
   #:print-hash
   ;; Advent of code
   #:gen-packages
   #:create-files-templates))

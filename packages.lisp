(defpackage #:org.numbra.perso.utils
  (:use #:cl)
  (:export
   #:with-gensyms
   #:do-array
   #:do-line
   #:do-hash
   #:do-hashkeys
   #:do-hashvalues
   #:dotimes-product
   #:neighbours
   #:flip
   #:range
   #:lazy-range
   #:permutations
   #:sublists-length
   #:array-index-row-major
   #:argmax
   #:argmin
   #:deepcopy
   #:shuffle
   #:ensure-list
   #:substitute-assoc
   #:for
   #:make-iterable
   #:do-subsets
   #:do-sequence-subsets))

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
   #:collect-integers-in-line
   #:split-word-int
   ;; Printing
   #:print-array
   #:print-hash))

(defpackage #:org.numbra.perso.ds
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   ;; - Points
   #:point
   #:point-x
   #:point-y
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
   #:qtree-intersect-some
   ;; Queue
   #:queue
   #:make-queue
   #:queue-push
   #:queue-pop
   #:queue-to-list
   #:queue-empty-p
   #:queue-size
   ;; Union-find
   #:uf-initialize
   #:uf-find
   #:uf-union
   #:equivalence-classes))

(defpackage #:org.numbra.perso.algo
  (:use #:cl #:org.numbra.perso.ds #:org.numbra.perso.utils)
  (:export
   #:shortest-path
   #:shortest-path-dec-key
   #:a-star
   #:dfs
   #:bfs
   #:shortest-path-all-to-all))

(defpackage #:org.numbra.perso
  (:use #:cl
        #:org.numbra.perso.utils
        #:org.numbra.perso.io
        #:org.numbra.perso.ds
        #:org.numbra.perso.algo)
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
   #:collect-integers-in-line
   #:split-word-int
   ;; Generic utilities
   #:neighbours
   #:flip
   #:range
   #:lazy-range
   #:permutations
   #:sublists-length
   #:array-index-row-major
   #:argmax
   #:argmin
   #:deepcopy
   #:shuffle
   #:ensure-list
   #:substitute-assoc
   #:for
   #:make-iterable
   ;; Iterate over subsets
   #:do-subsets
   #:do-sequence-subsets
   ;; Algorithms
   #:shortest-path
   #:shortest-path-dec-key
   #:a-star
   #:dfs
   #:bfs
   #:shortest-path-all-to-all
   ;; Data structures:
   ;; - Points
   #:point
   #:point-x
   #:point-y
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
   ;; Queue
   #:queue
   #:make-queue
   #:queue-push
   #:queue-pop
   #:queue-to-list
   #:queue-empty-p
   #:queue-size
   ;; Union-find
   #:uf-initialize
   #:uf-find
   #:uf-union
   #:equivalence-classes
   ;; Macros
   #:with-gensyms
   #:do-array
   #:do-line
   #:do-hash
   #:do-hashkeys
   #:do-hashvalues
   #:dotimes-product
   ;; Printing
   #:print-array
   #:print-hash
   ;; Advent of code
   #:gen-packages
   #:create-files-templates))

;;;; Extra stuff
(defpackage #:org.numbra.perso.machine
  (:use #:cl #:org.numbra.perso.utils)
  (:export
   #:memory
   #:register
   #:code
   #:ptr
   #:value
   #:defassembly
   #:make-machine
   #:run-machine))

(defpackage #:org.numbra.perso.aoc
  (:use #:cl)
  (:export
   #:gen-packages
   #:create-files-templates))

(defpackage  #:org.numbra.perso.extras
  (:use #:cl #:org.numbra.perso)
  (:local-nicknames (#:aoc #:org.numbra.perso.aoc)
                    (#:machine #:org.numbra.perso.machine)))

(in-package #:org.numbra.perso.extras)
(do-external-symbols (s (find-package "ORG.NUMBRA.PERSO"))
  (export s))

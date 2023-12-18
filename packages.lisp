(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun re-export-from (from in)
    "Export all the (external) symbols of the package named
ORG.NUMBRA.<from> in the package ORG.NUMBRA.<in>

FROM and IN are strings, that will be upcased."
    (let ((package (find-package (concatenate 'string
                                              "ORG.NUMBRA."
                                              (string-upcase in)))))
      (do-external-symbols (s (find-package (concatenate 'string
                                                         "ORG.NUMBRA."
                                                         (string-upcase from))))
        (export s package)))))

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
   #:flip
   #:range
   #:lazy-range
   #:manhattan-distance
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
   #:do-sequence-subsets
   #:lexicographic<
   #:group-by))

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
   #:ht-pop
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
   ;; Intervals
   #:interval
   #:make-interval
   #:with-interval-bounds
   #:interval-from-number
   #:interval-intersection
   #:interval-union
   #:interval-contains-p
   #:interval-equal
   #:interval-empty-p
   #:interval-cardinal
   #:interval-complement
   #:interval-add
   #:interval-addf
   #:interval-sub
   #:interval-subf
   ;; Grids
   #:grid-pos-in-direction
   #:grid-valid-pos-p
   #:grid-opposite-direction
   #:grid-height
   #:grid-width
   #:grid-rotate
   #:grid-neighbours
   #:grid-print
   #:grid-border-length
   #:grid-area-euclidean
   #:grid-area-lattice))

(defpackage #:org.numbra.perso.algo
  (:use #:cl #:org.numbra.perso.ds #:org.numbra.perso.utils)
  (:export
   ;; Graphs
   #:shortest-path
   #:shortest-path-dec-key
   #:a-star
   #:dfs
   #:bfs
   #:shortest-path-all-to-all
   #:connected-components
   ;; Iteration, dynamical system, cycles
   #:find-cycle-dynamical-system
   #:iterate-dynamical-system))

(defpackage #:org.numbra.perso
  (:use #:cl
        #:org.numbra.perso.utils
        #:org.numbra.perso.io
        #:org.numbra.perso.ds
        #:org.numbra.perso.algo))

(dolist (from '("utils" "io" "ds" "algo"))
  (re-export-from (concatenate 'string "PERSO." from) "PERSO"))

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

(re-export-from "PERSO" "PERSO.EXTRAS")

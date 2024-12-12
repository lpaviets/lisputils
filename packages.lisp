(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-personal-package (&rest packages)
    "Normalize a package hierarchy under the ORG.NUMBRA.PERSO
root.

All of PACKAGES can start with an optional dot `.'. If absent, it will
be added.

The resulting package name is

ORG.NUMBRA.PERSO.PACKAGE-1.PACKAGE-2 ...

where PACKAGES == (PACKAGE-1 ...), each package being stripped of its
starting dot."
    (flet ((normalize (pack)
             (cond
               ((string= "" pack) "")
               ((char= (char pack 0) #\.) (string-upcase pack))
               (t (concatenate 'string "." (string-upcase pack))))))
      (format nil "ORG.NUMBRA.PERSO~{~A~}"
              (mapcar #'normalize packages))))

  (defun re-export-from (from in)
    "Export all the (external) symbols of the package named
ORG.NUMBRA.PERSO<from> in the package ORG.NUMBRA.PERSO<in>

FROM and IN are strings, that will be upcased."
    (let ((package (find-package (normalize-personal-package in))))
      (do-external-symbols (s (normalize-personal-package from))
        (export s package)))))

(defmacro defpackage-conduit (common from-list &rest args)
  (let* ((common-pack (normalize-personal-package common))
         (from-list-pack (mapcar (lambda (pack)
                                   (if (string= "" pack)
                                       "ORG.NUMBRA.PERSO"
                                       (normalize-personal-package common pack)))
                                 from-list)))
    `(progn
       (defpackage ,common-pack
         (:use #:cl
               ,@from-list-pack)
         (:export
          ,@(loop :for pack :in from-list-pack
                  :for symbols = (let (acc)
                                   (do-external-symbols (s pack acc)
                                     (push (symbol-name s) acc)))
                  :nconc symbols))
         ,@args))))

(defpackage #:org.numbra.perso.utils
  (:use #:cl)
  (:export
   #:with-gensyms
   #:do-array
   #:do-line
   #:do-line*
   #:do-hash
   #:do-hashkeys
   #:do-hashvalues
   #:dotimes-product
   #:gethash-rec
   #:flip
   #:factorial
   #:binomial
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
   #:group-by
   #:*parenthesis-pairs*
   #:compute-parenthesis))

(defpackage #:org.numbra.perso.bits
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:match-bits-p
   #:with-match-bits))

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

(defpackage #:org.numbra.perso.ds.point
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:point
   #:point-x
   #:point-y
   #:add-point
   #:sub-point
   #:with-point))

(defpackage #:org.numbra.perso.ds.heap
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:make-heap
   #:heap-push
   #:heap-pop
   #:heap-peek
   #:heap-update
   #:heap-flush
   #:heap-contents
   #:heap-contains-p))

(defpackage #:org.numbra.perso.ds.ht
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:ht-create
   #:ht-count-if
   #:ht-count
   #:ht-from-sequence
   #:ht-peek
   #:ht-pop
   #:ht-merge
   #:ht-merge-with-binop
   #:ht-from-plist
   #:ht-to-plist
   #:ht-to-list))

(defpackage #:org.numbra.perso.ds.hset
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export))

(defpackage #:org.numbra.perso.ds.qtree
  (:use #:cl #:org.numbra.perso.ds.point)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:make-box
   #:make-bounding-box
   #:qtree-insert
   #:make-qtree-from-list
   #:with-box-sides
   #:make-qtree
   #:qtree-intersect-list
   #:qtree-valid-p
   #:qtree-intersect-some))

(defpackage #:org.numbra.perso.ds.queue
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:queue
   #:make-queue
   #:queue-push
   #:queue-pop
   #:queue-to-list
   #:queue-empty-p
   #:queue-size))

(defpackage #:org.numbra.perso.ds.uf
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:uf-initialize
   #:uf-find
   #:uf-union
   #:equivalence-classes))

(defpackage #:org.numbra.perso.ds.interval
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:interval
   #:make-interval
   #:interval-bounds
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
   #:interval-subf))

(defpackage #:org.numbra.perso.ds.grid
  (:use #:cl)
  (:local-nicknames (#:utils #:org.numbra.perso.utils))
  (:export
   #:grid-pos-in-direction
   #:grid-valid-pos-p
   #:grid-opposite-direction
   #:grid-at
   #:grid-height
   #:grid-width
   #:grid-rotate
   #:grid-neighbours
   #:grid-print
   #:grid-border-length
   #:grid-area-euclidean
   #:grid-area-lattice
   #:grid-apply-as-sequence))

(defpackage-conduit "ds" ("point"
                          "heap"
                          "ht"
                          "qtree"
                          "queue"
                          "uf"
                          "interval"
                          "grid")
  (:local-nicknames (#:utils #:org.numbra.perso.utils)))

(defpackage #:org.numbra.perso.algo.graphs.traversals
  (:use #:cl #:org.numbra.perso.ds #:org.numbra.perso.utils)
  (:export
   ;; Graphs
   #:shortest-path
   #:shortest-path-dec-key
   #:a-star
   #:dfs
   #:bfs
   #:shortest-path-all-to-all
   #:find-cycle
   #:connected-components
   #:topological-sort
   #:longest-path))

(defpackage #:org.numbra.perso.algo.graphs.flows
  (:use #:cl #:org.numbra.perso.ds
        #:org.numbra.perso.utils
        #:org.numbra.perso.algo.graphs.traversals)
  (:export
   #:minimum-cut
   #:max-flow))

(defpackage #:org.numbra.perso.algo.graphs.structure
  (:use #:cl
        #:org.numbra.perso.ds
        #:org.numbra.perso.utils
        #:org.numbra.perso.algo.graphs.traversals)
  (:export
   #:graph-bipartite
   #:graph-induced))

(defpackage-conduit "algo.graphs" ("traversals"
                                   "flows"
                                   "structure")
  (:use #:org.numbra.perso.ds #:org.numbra.perso.utils)
  (:export
   #:make-random-graph
   #:make-graph-cycle
   #:make-graph-path
   #:make-graph-complete
   #:make-graph-complete-bipartite
   #:make-graph-from-grid))

(defpackage #:org.numbra.perso.algo.dynsys
  (:use #:cl #:org.numbra.perso.ds #:org.numbra.perso.utils)
  (:export
   #:find-cycle-dynamical-system
   #:iterate-dynamical-system))

(defpackage-conduit "algo" ("graphs" "dynsys")
  (:use #:org.numbra.perso.ds #:org.numbra.perso.utils))

(defpackage-conduit "" ("utils" "io" "ds" "algo")
  (:use #:org.numbra.perso.ds #:org.numbra.perso.utils))

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

(defpackage-conduit "extras" ("")
  (:local-nicknames (#:aoc #:org.numbra.perso.aoc)
                    (#:machine #:org.numbra.perso.machine)))

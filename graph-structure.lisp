;; numbra:
;;; Algorithm about the structure of graphs

(in-package #:org.numbra.perso.algo.graphs.structure)

(defun graph-bipartite (edges source &key (test 'eql))
  "Test if the connected component of SOURCE in the graph given by EDGES is bipartite.

See `bfs' for a description of the parameters.

Return NIL if the graph is not bipartite, otherwise return a colouring of the
connected component: this is a hash-table, whose keys are vertices and whose
values are either 0 or 1 depending on the colour. SOURCE is always coloured with
0."
  (let ((colouring (make-hash-table :test test)))
    (setf (gethash source colouring) 0)
    (flet ((toggle-colour (v parent cost visited)
             (declare (ignore cost visited))
             (loop :with colour = (- 1 (gethash v colouring))
                   :for (u . cost) :in (funcall edges v)
                   :for u-colour = (gethash u colouring)
                   :unless (funcall test u parent)
                     :do (cond
                           ((null u-colour)
                            (setf (gethash u colouring) colour))
                           ((/= u-colour colour)
                            (return-from graph-bipartite nil))))))
      (bfs edges source :at-vertex #'toggle-colour)
      colouring)))

(defun graph-induced (edges vertices &key (test 'eql))
  "Return the subgraph of EDGES induced by the list of vertices VERTICES.

VERTICES are compared using TEST."
  (let ((new-edges nil))
    (dolist (v vertices)
      (let* ((v-edges (funcall edges v))
             (cleaned-v-edges (remove-if-not (lambda (u)
                                               (member (car u) vertices :test test))
                                             v-edges)))
        (push (cons v cleaned-v-edges) new-edges)))
    (lambda (v)
      (cdr (assoc v new-edges :test test)))))

;; TODO: (NOT basic) non-stupid (sub)graph isomophism algo.
;; Example: VF2++
;; Refenrence article:
;; https://www.sciencedirect.com/science/article/pii/S0166218X18300829#dfig1
;;
;; Implementation in Python and blog posts about it:
;; https://blog.scientific-python.org/posts/networkx/vf2pp/
;;
;; Actual implementation is here
;; https://github.com/networkx/networkx/blob/main/networkx/algorithms/isomorphism/vf2pp.py

;; Idea: construct a /mapping/ step by step:
;; - VF2(mapping m){
;;   if TotalMapping(m) return m;
;;   Pm <- CandidatePairs(m)
;;   for p in Pm do {
;;     if ValidExtends(p, m) and not Unreachable(p, m)
;;       VF2(extend(p, m))
;;   }
;; }
;;
;; Questions:
;; - CandidatePairs
;; - ValidExtends (consistency, in the original paper)
;; - Unreachable (cutting rules)
;;
;; + some extras optimisations: ordering of the vertices, caching a bunch of
;; stuff
;;
;; Hence:
;; def VF2Order
;; def VF2ProcessLevel

(defun in-mapping (node mapping)
  (assoc node mapping))

(defun in-reverse-mapping (node mapping)
  (rassoc node mapping))

(defun graph-edges (graph node)
  (declare (ignore graph node)))

(defun graph-vertices (graph)
  (declare (ignore graph)))

(defun compute-t-i (graph-1 graph-2 mapping)
  (let* ((t-1 (loop :for node :in mapping
                    :for nbr :in (graph-edges graph-1 (car node))
                    :unless (in-mapping (car nbr) mapping)
                      :collect nbr))
         (t-2 (loop :for node :in mapping
                    :for nbr :in (graph-edges graph-2 (cdr node))
                    :unless (in-reverse-mapping (car nbr) mapping)
                      :collect nbr))
         (t-1-out (loop :for v :in (graph-vertices graph-1)
                        :unless (or (in-mapping v mapping)
                                    (member v t-1))
                          :collect v))
         (t-2-out (loop :for v :in (graph-vertices graph-2)
                        :unless (or (in-reverse-mapping v mapping)
                                    (member v t-2))
                          :collect v)))
    (values t-1 t-2 t-1-out t-2-out)))

(defun update-t-in-out (graph-1 graph-2 t-1 t-2 t-1-out t-2-out v1 v2 mapping)
  (let ((uncovered-1 (loop :for nbr :in (graph-edges graph-1 v1)
                           :unless (in-mapping nbr mapping)
                             :collect nbr))
        (uncovered-2 (loop :for nbr :in (graph-edges graph-2 v2)
                           :unless (in-reverse-mapping nbr mapping)
                             :collect nbr)))
    (setf t-1 (delete v1 t-1)
          t-2 (delete v2 t-2))
    (setf t-1 (nunion t-1 uncovered-1)
          t-2 (nunion t-2 uncovered-2))

    (setf t-1-out (delete t-1-out v1)
          t-2-out (delete t-2-out v2))
    (setf t-1-out (nset-difference t-1-out uncovered-1)
          t-2-out (nset-difference t-2-out uncovered-2))
    (values t-1 t-2 t-1-out t-2-out)))

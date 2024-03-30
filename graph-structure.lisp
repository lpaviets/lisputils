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

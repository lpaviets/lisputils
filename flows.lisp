;;;; Cuts and Flows

(in-package #:org.numbra.perso.algo.graphs.flows)

(defun sr-min-cut (vertices edges-table)
  (let* ((keys (make-hash-table :test (hash-table-test vertices)
                                :size (hash-table-size vertices)))
         (prio-queue (make-heap #'> :key (lambda (x) (gethash x keys)))))
    (do-hashkeys (u vertices)
      (setf (gethash u keys) 0)
      (heap-push u prio-queue))
    (let (s r)
      (loop :while (heap-peek prio-queue)
            :for u = (heap-pop prio-queue)
            :do (shiftf s r u)
                (dolist (v-cost (gethash u edges-table))
                  (destructuring-bind (v . cost) v-cost
                    (when (heap-contains-p v prio-queue)
                      (incf (gethash v keys) cost)
                      (heap-push v prio-queue))))
            :finally (remhash r keys)
                     (return (values (cons keys
                                           (ht-from-sequence (list r)))
                                     s
                                     r))))))

;;; TODO: need to track of which vertices have been merged
(defun merge-vertices (vertices edges-table s r)
  ;; New edges: merge the two sets of edges, adding the costs if
  ;; needed
  (let ((s-edges (gethash s edges-table))
        (r-edges (gethash s edges-table)))
    (do-hash (u cost r-edges)
      (incf (gethash u s-edges 0) cost)
      (incf (gethash s (gethash u edges-table) 0) cost)
      (remhash r (gethash u edges-table)))
    ;; Remove self loops
    (remhash s s-edges)
    (remhash r s-edges)
    ;; Remove references to r everywhere
    (remhash r vertices)
    (remhash r edges-table)))

(defun %minimum-cut (vertices edges-table)
  (if (= 2 (hash-table-count vertices))
      (let (res)
        (do-hashkeys (v vertices)
          (push (ht-from-sequence (list v)) res))
        res)
      (destructuring-bind (cut s r)
          (sr-min-cut vertices edges-table)
        (merge-vertices vertices edges-table s r)
        (let ((new-cut (%minimum-cut vertices edges-table)))
          (if (< (cut-weight new-cut) (cut-weight cut))
              new-cut
              cut)))))

(defun minimum-cut (vertices edges &key (test 'eql))
  "Computes a minimum cut in the UNDIRECTED graph given by edges.

It is the caller responsibility to make sure that EDGES is not
directed.

VERTICES is a hash-table or a sequence of vertices."
  (let ((edges-table (make-hash-table :test test
                                      :size (hash-table-size vertices)))
        (vertices (deepcopy vertices)))
    (do-hashkeys (v vertices)
      (let ((table (make-hash-table :test test)))
        (dolist (edge (funcall edges v))
          (setf (gethash (car edge) table) (cdr edge)))
        (setf (gethash v edges-table) table))))
  (%minimum-cut vertices edges-table))

(defun max-flow (edges source target &key (test 'eql))
  "Computes the maximal flow between SOURCE and TARGET in the graph defined by
EDGES.

Assumptions:

- SOURCE has no in-edges.

- TARGET has no out-edges.

- EDGES is fully undirected, i.e., we never have both (u, v) and (v, u) as
  edges.

The algorithm /might/ work if those assumptions do not hold, but I have no
formal proof of that.

Returns four values:

- The value of the maximal flow.

- A hash-table FLOW whose keys are vertices U and whose values are lists of
cons (V . FLOW-UV) where (U, V) is an edge and FLOW-UV is the flow sent from U
to V in the maximal flow.

- A hash-table CAPACITIES of the same format, containing the maximal capacity of
the edge instead of the value of the flow.

- A list of saturated edges (U V)"
  ;; CAPACITIES and FLOWS are hash-table, whose keys are vertices
  ;; and whose values are lists of conses (VERTEX . X),
  ;; X being either the "initial capacity" of the edge,
  ;; or the current flow.

  (let ((capacities (make-hash-table :test test))
        (residual   (make-hash-table :test test))
        (flows      (make-hash-table :test test))
        (max-flow 0))
    ;; Initialize the tables:
    ;; - Capacities is the actual graph
    ;; - Residual is its "complement", i.e., contains the reverse edges
    ;; - Flows contains the current flows
    (labels ((true-edge-p (u v)
               (member v (gethash u capacities) :test test :key #'car))
             (residual-flow (u v &optional (cap-uv (cdr (assoc v (gethash u capacities) :test test))))
               (if (true-edge-p u v)
                   (- cap-uv (cdr (assoc v (gethash u flows) :test test)))
                   (cdr (assoc u (gethash v flows) :test test)))))
      (bfs edges source
           :target target
           :test test
           :at-vertex
           (lambda (u &rest args)
             (declare (ignore args))
             (loop :for (v . cost) :in (funcall edges u)
                   :do (pushnew (cons v 0)    (gethash u flows) :key #'car :test test)
                       (pushnew (cons v cost) (gethash u capacities) :key #'car :test test)
                       (pushnew (cons u cost) (gethash v residual) :key #'car :test test))))
      ;; Now compute augmenting path: need to have another function to compute
      ;; edges RE, to implement the residual graph
      ;; Definition of RE: ((u, v), r) in RE if and only if

      ;; - (u, v) in EDGES and r = (capacity(u, v) - flow(u, v)) > 0
      ;; - (v, u) in EDGES and r = flow(v, u) > 0
      (loop :with free-edges = (lambda (u)
                                 (let (u-edges)
                                   ;; "true" edges
                                   (loop :for (v . cap-uv) :in (gethash u capacities)
                                         :for val = (residual-flow u v cap-uv)
                                         :when (plusp val)
                                           :do (push (cons v val) u-edges))
                                   ;; "reverse" edges
                                   (loop :for (v . cap-vu) :in (gethash u residual)
                                         :for flow-vu = (cdr (assoc u (gethash v flows) :test test))
                                         :when (plusp flow-vu)
                                           :do (push (cons v flow-vu) u-edges))
                                   u-edges))
            :for parents = (nth-value 2 (bfs free-edges source
                                             :target target
                                             :test test))
            ;; While there is some capacity left in the graph, i.e. a path
            ;; source->target
            :while (gethash target parents)
            :do (let ((path-flow
                        ;; Compute flow on the augmenting path
                        (loop :for s = target :then parent-s
                              :for parent-s = (gethash s parents)
                              :until (funcall test s source)
                              :for res-flow-s = (residual-flow parent-s s)
                              :do (assert (plusp res-flow-s))
                              :minimize res-flow-s)))
                  ;; Increment max flow by flow of the curent augmenting path
                  (incf max-flow path-flow)
                  ;; Update capacities and residual capacities
                  (loop :for s = target :then parent-s
                        :for parent-s = (gethash s parents)
                        :until (funcall test s source)
                        :for true-edge = (true-edge-p parent-s s)
                        :if true-edge
                          :do (incf (cdr (assoc s (gethash parent-s flows) :test test)) path-flow)
                        :else
                          :do (decf (cdr (assoc parent-s (gethash s flows) :test test)) path-flow))))
      (let (saturated)
        (do-hashkeys (u capacities)
          (loop :for (va . cap-uv)  :in (gethash u capacities)
                :for (vb . flow-uv) :in (gethash u flows)
                :do (assert (funcall test va vb))
                :when (= cap-uv flow-uv)
                  :do (push (list u va) saturated)))
        (values max-flow flows capacities saturated)))))

;; numbra:
;;; Algorithms: graph traversals

(in-package #:org.numbra.perso.algo.graphs.traversals)

;;;; GRAPHS

;;; Dijkstra
;;; Algo:
;;; Initialization:
;;; 1. Q <- empty
;;; 2. for each v in V(G): dist[v] <- +inf
;;; 3. Q.insert(source, 0)
;;;
;;; Algo:
;;; while Q is not empty do:
;;;   (u, k) <- delete_min(Q)
;;;   if k == dist[u]:
;;;     for each (u, v) in E(G) do:
;;;       if dist[u] + weight(u, v) < dist[v]:
;;;         Q.insert(v, dist[u] + weight(u, v))
;;;         dist[v] <- dist[u] + weight(u, v)

(defgeneric shortest-path (edges source target &key test)
            (:documentation "Shortest path from SOURCE to TARGET in the graph G determined
by EDGES. Comparison between vertices is done using TEST"))

(defmethod shortest-path ((edges function) source target &key (test 'eql))
  "EDGES is expected to be a function of one element, a vertex (of the
same type as SOURCE and TARGET), and return a list of cons cells, whose car is
an adjacent vertex and whose cdr is the edge's weight. For example:
(funcall EDGES SOURCE) -> ((u1 . w1) (u2 . w2)) ... (un . wn))
where the ui's are exactly the vertices adjacent to SOURCE.

TARGET can also be a function of one argument, a vertex, returning T if this
vertex is a possible target."
  (let  ((distance (make-hash-table :test test))
         (queue (heap:make-heap #'< :key #'cdr))
         (parent (make-hash-table :test test))
         (target-p (if (functionp target)
                       (lambda (v)
                         (funcall target v))
                       (lambda (v)
                         (funcall test v target)))))
    (loop
      :initially
         (setf (gethash source distance) 0)
         (heap:heap-push (cons source 0) queue)
      :while (heap:heap-peek queue)
      :for best = (heap:heap-pop queue)
      :for (vertex . queue-dist) = best
      :for curr-dist = (or (gethash vertex distance)
                           most-positive-fixnum)
      :when (= queue-dist curr-dist)
        :do (loop :for edge :in (funcall edges vertex)
                  :for (other . weight) = edge
                  :for other-new-dist = (+ queue-dist weight)
                  :when (< other-new-dist (or (gethash other distance)
                                              most-positive-fixnum))
                    :do
                       (setf (gethash other distance) other-new-dist)
                       (heap:heap-push (cons other other-new-dist) queue)
                       (setf (gethash other parent) vertex))
      :until (funcall target-p vertex)
      :finally (return (values (gethash vertex distance)
                               parent
                               vertex)))))

(defmethod shortest-path ((edges array) (source integer) (target integer) &key (test 'eql))
  "EDGES is given as a 2D array, representing the adjacency matrix of the graph
(aref EDGES i j) is the weight of the oriented edge between I and J SOURCE and
TARGET are INTEGER, corresponding to valid indices of the array"
  (flet ((fun-edges (vertex)
           (loop :for j :below (array-dimension edges 1)
                 :for weight = (aref edges vertex j)
                 :collect (cons j weight))))
    (shortest-path #'fun-edges source target :test test)))


;;; Other implementation: uses a heap with a 'decrease-key' operation
;;; Way better space-wise, not necessarily the case speed-wise
(defmethod shortest-path-dec-key (edges source target &key (test 'eql))
  "EDGES is expected to be a function of one element, a vertex (of the
same type as SOURCE and TARGET), and return a list of cons cells, whose car is
an adjacent vertex and whose cdr is the edge's weight.

 For example:
(funcall EDGES SOURCE) -> ((u1 . w1) (u2 . w2)) ... (un . wn)) where the ui's
are exactly the vertices adjacent to SOURCE."
  (let  ((distance (make-hash-table :test test))
         (parent (make-hash-table :test test)))
    (declare (special distance))
    (flet ((get-dist (x)
             (or (gethash x distance)
                 most-positive-fixnum)))
      (loop
        :with queue = (make-heap #'< :key #'get-dist)
        :initially
           (setf (gethash source distance) 0)
           (heap-push source queue)
        :while (heap-peek queue)
        :for vertex = (heap-pop queue)
        :for curr-dist = (get-dist vertex)

        :do (loop :for edge :in (funcall edges vertex)
                  :for (other . weight) = edge
                  :for other-new-dist = (+ curr-dist weight)
                  :when (< other-new-dist (get-dist other))
                    :do
                       (setf (gethash other distance) other-new-dist)
                       (heap-push other queue)
                       (setf (gethash other parent) vertex))
        :until (funcall test vertex target)
        :finally (hash-table-count distance)
           (return (values (gethash target distance)
                           parent))))))

;;; Based on the version with a "decrease-key" priority queue.
;;; Somewhat necessary, as one problem of A* is its memory consumption,
;;; so I use the Dijkstra "baseline" which uses less space.
;;; UNTESTED YET, NOT SURE THAT IT REALLY WORKS !
(defun a-star (edges source target heuristic &key (test 'eql))
  "HEURISTIC is a function of one argument, a vertex, returning a value, ideally
close to the distance from the vertex to TARGET.

To ensure optimality of the path, it is required that HEURISTIC never
overestimates the real distance. See `shortest-path-dec-key' for the other
arguments."
  (let  ((distance (make-hash-table :test test))
         (parent (make-hash-table :test test)))
    (declare (special distance))
    (flet ((get-dist (x)
             (let ((dist (gethash x distance)))
               (if dist
                   (+ dist (funcall heuristic x))
                   most-positive-fixnum))))
      (loop
        :with queue = (make-heap #'< :key #'get-dist)
        :initially
           (setf (gethash source distance) 0)
           (heap-push source queue)
        :while (heap-peek queue)
        :for vertex = (heap-pop queue)
        :for curr-dist = (get-dist vertex)

        :do (loop :for edge :in (funcall edges vertex)
                  :for (other . weight) = edge
                  :for other-new-dist = (+ curr-dist weight)
                  :when (< other-new-dist (get-dist other))
                    :do
                       (setf (gethash other distance) other-new-dist)
                       (heap-push other queue)
                       (setf (gethash other parent) vertex))
        :until (funcall test vertex target)
        :finally (hash-table-count distance)
                 (return (values (gethash target distance)
                                 parent))))))

(defun %update-floy-warshall (wij wik wkj)
  (cond
    ((and wij wik wkj)
     (min wij (+ wik wkj)))
    (wij wij)
    ((and wik wkj)
     (+ wik wkj))
    (t nil)))

(defun shortest-path-all-to-all (adjacency-matrix)
  "Computes the shortest path from any vertex to any other vertex
ADJACENCY-MATRIX is a 2D array containing in position (i, j) the distance
between the vertices i and j, or NIL if there is no edge between them"
  (let* ((n (array-dimension adjacency-matrix 0))
         (matrix (deepcopy adjacency-matrix)))
    (dotimes (k n)
      (dotimes (i n)
        (dotimes (j n)
          (setf (aref matrix i j)
                (let ((wij (aref matrix i j))
                      (wik (aref matrix i k))
                      (wkj (aref matrix k j)))
                  (%update-floy-warshall wij wik wkj))))))
    matrix))

;;;; Graph traversal algorithms
;;;DFS
(defun %dfs (edges pending &key at-vertex
                             at-vertex-exit
                             (test 'eql)
                             target
                             (target-test test)
                             parents
                             random)
  (let ((visited (make-hash-table :test test))
        (marker (gensym))
        (order-edges
          (lambda (x parent edges)
            (cond
              ((not random) edges)
              ((or (functionp random)
                   (and (symbolp random)
                        (fboundp random)))
               (funcall random x parent edges))
              ((eq t random) (shuffle edges))
              (t
               (error "Invalid random argument ~A: not a function or T or NIL"
                      random))))))
    (loop :while pending
          :for (x parent cost) = (pop pending)
          :if (and (eq parent marker)
                   at-vertex-exit)
            :do (funcall at-vertex-exit x nil 0 visited)
          :else
            :unless (gethash x visited) :do
              (push (list x marker 0) pending)
              (loop :for (y . cost) :in (funcall order-edges
                                                 x parent (funcall edges x))
                    :unless (or (gethash y visited)
                                (funcall test y parent))
                      :do (push (list y x cost) pending)
                          (setf (gethash y parents) x))
              (setf (gethash x visited) t)
              (when at-vertex
                (funcall at-vertex x parent cost visited))
          :until (and target (funcall target-test x target))))
  (values nil nil parents))

(defun dfs (edges source &key at-vertex
                           at-vertex-exit
                           (test 'eql)
                           target
                           (target-test test)
                           random
                           multi-sources)
  "Depth-First-Search of the graph determined by EDGES.

Returns as a third value a hash-table containing the parent of each vertex, on
the path from SOURCE to it in the DFS order. The first two returned values are
NIL, to have a signature similar to `bfs'.

EDGES is a function of one argument, a vertex, and return a list of cons cells
(NEIGHBOUR . EDGE-WEIGHT).

SOURCE is the initial vertex from which to perform the DFS.

AT-VERTEX is a function of four argument: the vertex being visited, its parent,
the cost of the edge used to reach it, and a hash-table of vertices that are
already explored. It is called when first entering a vertex.

AT-VERTEX-EXIT has the same signature as AT-VERTEX. It is called when the vertex
is done being explored. However, the second and third arguments, which are the
parent and the cost of the edge, are meaningless here and set to NIL and 0
respectively.

TEST is a boolean function of two arguments, comparing vertices.

If TARGET is non-NIL, the DFS stops after reaching a vertex equal (as of
TARGET-TEST) to it.

RANDOM determines whether the edges are searched in a deterministic way or not.
If NIL, the outgoing edges from a vertex are visited in the order given by
EDGES. If T, those edges are randomly shuffled. Otherwise, RANDOM should be a
function of 3 arguments, the current vertex, its parent, and the list of EDGES.

If MULTI-SOURCES is non-NIL, SOURCE is then a list of sources."
  (let ((parents (make-hash-table :test test)))
    (setf (gethash source parents) nil)
    (%dfs edges (if multi-sources (mapcar 'list source) (list (list source)))
          :at-vertex at-vertex
          :at-vertex-exit at-vertex-exit
          :test test
          :target target
          :target-test target-test
          :parents parents
          :random random)))

;;; BFS
(defun %bfs (edges pending &key distance
                             parents
                             at-vertex
                             at-vertex-exit
                             test
                             target
                             target-test
                             random)
  (let ((visited (make-hash-table :test test))
        distance-target
        (marker (gensym))
        (order-edges
          (lambda (x parent edges)
            (cond
              ((not random) edges)
              ((or (functionp random)
                   (and (symbolp random)
                        (fboundp random)))
               (funcall random x parent edges))
              ((eq t random) (shuffle edges))
              (t
               (error "Invalid random argument ~A: not a function or T or NIL"
                      random))))))
    (loop :until (queue-empty-p pending)
          :for (x parent cost) = (queue-pop pending)
          :for at-target-p = (and target (funcall target-test x target))
          :if (and (eq parent marker)
                   at-vertex-exit)
            :do (funcall at-vertex-exit x nil 0 visited)
          :else
            :unless (gethash x visited) :do
              (queue-push pending (list x marker 0))
              ;; Add the neighbours
              (loop :with dist-x = (gethash x distance)
                    :for (y . cost) :in (funcall order-edges
                                                 x parent (funcall edges x))
                    :unless (gethash y visited)
                      :do (queue-push pending (list y x cost))
                          ;; Update the distance/parent of the vertex
                          (setf (gethash y distance)
                                (1+ (gethash x distance))) ; don't consider COST
                          (setf (gethash y parents) x))
              ;; Mark the vertex as visited
              (setf (gethash x visited) t)
              (when at-vertex
                (funcall at-vertex x parent cost visited))
          :until (and at-target-p
                      (setf distance-target (gethash x distance))))
    (values distance-target distance parents)))

(defun bfs (edges source &key at-vertex
                           at-vertex-exit
                           (test 'eql)
                           target
                           (target-test test)
                           random)
  "Breadth-First-Search of the graph determined by EDGES.

EDGES is a function of one argument, a vertex, and return a list of cons cells
(NEIGHBOUR . EDGE-WEIGHT).

SOURCE is the initial vertex from which to perform the BFS.

AT-VERTEX is a function of four argument: the vertex being visited,
its parent, the cost of the edge used to reach it, and a hash-table of
vertices that are already explored.

AT-VERTEX-EXIT has the same signature as AT-VERTEX. It is called when
the vertex is done being explored. However, the second and thirs
arguments, which are the parent and the cost of the edge, are
meaningless here and set to NIL and 0 respectively.

TEST is a boolean function of two arguments, comparing vertices.

If TARGET is non-NIL, the BFS stops after reaching a vertex equal (as
of TARGET-TEST) to it.

RANDOM determines whether the edges are searched in a deterministic
way or not. If NIL, the outgoing edges from a vertex are visited in
the order given by EDGES. If T, those edges are randomly shuffled.
Otherwise, RANDOM should be a function of 3 arguments, the current
vertex, its parent, and the list of EDGES.

Returns three values:
- The distance from SOURCE to TARGET (or NIL if target is unspecified or unreached)
- A hash-table containing the distances from SOURCE to each vertex
- A hash-table containing the parent of each vertex, on the shortest path from
SOURCE to it"
  (let ((distance (make-hash-table :test test))
        (parents (make-hash-table :test test)))
    (setf (gethash source parents) nil)
    (setf (gethash source distance) 0)
    (%bfs edges
          (make-queue (list source))
          :distance distance
          :parents parents
          :at-vertex at-vertex
          :at-vertex-exit at-vertex-exit
          :test test
          :target target
          :target-test target-test
          :random random)))

(defun connected-components (vertices edges &key (test 'eql))
  "Return a list of connected components of a graph.

EDGES and TEST play the same role as in `BFS', which see.

VERTICES is a hash-table or a sequence of vertices that are not yet
explored. Not all vertices need to be present in this table, as long
as they can be reached by applpying EDGES to a vertex that does belong
to VERTICES.

Each returned connected component is a hash-table, whose keys are the
vertices of the component, and whose values are the distance from each
vertex to some unspecified source vertex from VERTICES."
  (let ((unexplored (make-hash-table :test test)))
    (etypecase vertices
      (hash-table (do-hashkeys (x vertices)
                    (setf (gethash x unexplored) t)))
      (sequence (map nil (lambda (x)
                           (setf (gethash x unexplored) t))
                     vertices)))
    (loop :while (plusp (hash-table-count unexplored))
          :for x = (ht-pop unexplored)
          :collect (nth-value 1 (bfs edges x
                                     :at-vertex (lambda (node parent cost explored)
                                                  (declare (ignore parent cost explored))
                                                  (remhash node unexplored))
                                     :test test)))))


;; DOES NOT SEEM TO WORK: infinite loop, don't know why.
;; Probably a bug in (all or some of):
;; - AT-VERTEX-EXIT key argument in DFS
;; - colouring
;; - from/to in compute-cycle vs x/y in cyclep
(defun find-cycle (edges source &key (test 'eql) (directed t))
  "Finds a cycle in the connected component of a graph, starting from
SOURCE.

DIRECTED is NIL if the graph is directed, T otherwise. Setting
DIRECTED to T will give correct results, but might be slower for
undirected graphs.

EDGES, SOURCE and TEST have the same meaning than in `dfs' or `bfs',
which see."
  (let ((parents (make-hash-table :test test))
        (colors  (make-hash-table :test test)))
    (labels ((compute-cycle (from to)
               (loop :for x = to :then (gethash x parents)
                     :collect x
                     :until (or (funcall test x from)
                                (not x))))
             (cyclep (x parent cost visited)
               (declare (ignore cost))
               (setf (gethash x parents) parent)
               (setf (gethash x colors)  :gray)
               (loop :for (y . cost) :in (funcall edges x)
                     :unless (funcall test y parent)
                       :when (if directed
                                 (eq (gethash y colors) :gray)
                                 (gethash y visited))
                         :do (return-from find-cycle
                               (compute-cycle y x))))
             (update-color (x &rest args)
               (declare (ignore args))
               (setf (gethash x colors) :black)))
      (dfs edges
           source
           :at-vertex #'cyclep
           :at-vertex-exit #'update-color
           :test test)
      ;; Only reaches this if we didn't find any cycle
      nil)))

(defun topological-sort (edges source &key (test 'eql) random multi-sources)
  "Computes a topological sort of the graph given by EDGES, starting
from the vertex SOURCE.

Returns two values:

- a list of vertices, where vertices occurring first in the list are
smaller in the order (convention being than SOURCE is the smallest
vertex, and in particular, is the CAR of the returned list).

- a hash-table of parents, as computed by a DFS.

EDGES, SOURCE, MUTLI-SOUCES, TEST and RANDOM have the same meaning than in
`dfs'."

  (let ((colors  (make-hash-table :test test))
        (sort nil))
    (labels ((mark-enter (x &rest args)
               (declare (ignore args))
               (case (gethash x colors)
                 (:black t)
                 (:gray
                  (error "Graph given by ~A, visited from ~A, contains a cycle"
                         edges source))
                 (t (setf (gethash x colors) :gray))))
             (mark-exit (x &rest args)
               (declare (ignore args))
               (setf (gethash x colors) :black)
               (push x sort)))
      (let ((parents (nth-value 2 (dfs edges
                                       source
                                       :at-vertex #'mark-enter
                                       :at-vertex-exit #'mark-exit
                                       :test test
                                       :random random
                                       :multi-sources multi-sources))))
        (values sort parents)))))

(defun out-to-in-edges (vertices edges &key (test 'eql))
  "Converts out-edges into in-edges for the graph given by EDGES.

EDGES and TEST are the same than in `dfs' and `bfs', which see.

VERTICES is a hash-table or a sequence of vertices of which we try to
compute the in-edges.

Returns a hash-table mapping each vertex of VERTICES to its list of
incoming edges, each edge being of the form (VERTEX . COST), where
VERTEX is the initial of the edge."
  (let* ((vertices (etypecase vertices
                     (hash-table
                      (assert (eq (hash-table-test vertices) test))
                      vertices)
                     (sequence (let ((table (make-hash-table :test test)))
                                 (map nil (lambda (x)
                                            (setf (gethash x table) t))
                                      vertices)))))
         (inc-edges (make-hash-table
                     :test test
                     :size (hash-table-size vertices))))
    (do-hashkeys (v vertices)
      (let ((v-out (funcall edges v)))
        (dolist (vu-edge v-out)
          (destructuring-bind (u . cost) vu-edge
            (when (nth-value 1 (gethash u vertices))
              (pushnew (cons v cost)
                       (gethash u inc-edges)
                       :key #'car
                       :test test))))))
    inc-edges))

(defun longest-path (dag source &key (test 'eql) random)
  "Returns the longest path in a DAG. More precisely, returns
a hash-table, whose keys are the vertices V of the graph, and whose
values are the length of the longest path from SOURCE to V.

As a secondary value, return a hash-table, mapping a vertex to the list
of its incoming edges, computed from DAG. This is to make it possible
to compute the actual longest path, and not only to its length.

DAG plays the same role as EDGES in `dfs' or `bfs', so do SOURCE, TEST
and RANDOM."
  (let* ((order (topological-sort dag source :test test :random random))
         (n (length order))
         (distances (make-hash-table :test test :size n))
         (inc-edges (out-to-in-edges order dag :test test)))
    ;; Find longest path recursively
    (labels ((longest-ending-at (u)
               (or (gethash u distances)
                   (setf (gethash u distances)
                         (let ((prev (mapcar #'longest-ending-at
                                             (gethash u inc-edges))))
                           (if prev
                               (1+ (reduce #'max prev))
                               0))))))
      (map nil #'longest-ending-at order)
      (values distances inc-edges))))

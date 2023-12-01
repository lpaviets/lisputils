;;; numbra:
;;; Algorithms

(in-package #:org.numbra.perso.algo)

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
by EDGES.
Comparison between vertices is done using TEST"))

(defmethod shortest-path ((edges function) source target &key (test 'eql))
  "EDGES is expected to be a function of one element, a vertex (of the
same type as SOURCE and TARGET), and return a list of cons cells,
whose car is an adjacent vertex and whose cdr is the edge's weight.
For example:
(funcall EDGES SOURCE) -> ((u1 . w1) (u2 . w2)) ... (un . wn))
where the ui's are exactly the vertices adjacent to SOURCE."
  (let  ((distance (make-hash-table :test test))
         (queue (heap:make-heap #'< :key #'cdr))
         (parent (make-hash-table :test test)))
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
      :until (funcall test vertex target)
      :finally (return (values (gethash target distance)
                               parent)))))

(defmethod shortest-path ((edges array) (source integer) (target integer) &key (test 'eql))
  "EDGES is given as a 2D array, representing the adjacency matrix of the graph
(aref EDGES i j) is the weight of the oriented edge between I and J
SOURCE and TARGET are INTEGER, corresponding to valid indices of the array"
  (flet ((fun-edges (vertex)
           (loop :for j :below (array-dimension edges 1)
                 :for weight = (aref edges vertex j)
                 :collect (cons j weight))))
    (shortest-path #'fun-edges source target :test test)))


;;; Other implementation: uses a heap with a 'decrease-key' operation
;;; Way better space-wise, not necessarily the case speed-wise
(defmethod shortest-path-dec-key (edges source target &key (test 'eql))
  "EDGES is expected to be a function of one element, a vertex (of the
same type as SOURCE and TARGET), and return a list of cons cells,
whose car is an adjacent vertex and whose cdr is the edge's weight.
For example:
(funcall EDGES SOURCE) -> ((u1 . w1) (u2 . w2)) ... (un . wn))
where the ui's are exactly the vertices adjacent to SOURCE."
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
overestimates the real distance.
See `shortest-path-dec-key' for the other arguments."
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
ADJACENCY-MATRIX is a 2D array containing in position (i, j) the
distance between the vertices i and j, or NIL if there is no edge
between them"
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
(defun %dfs (edges pending &key at-vertex (test 'eql) target (target-test test) random)
  (let ((visited (make-hash-table :test test)))
    (loop :while pending
          :for (x parent cost) = (pop pending)
          :unless (gethash x visited) :do
            (loop :for (edge . cost) :in (if random
                                             (shuffle (funcall edges x))
                                             (funcall edges x))
                  :do
                     (push (list edge x cost) pending))
            (setf (gethash x visited) t)
            (when at-vertex
              (funcall at-vertex x parent cost))
          :until (and target (funcall target-test x target)))))

(defun dfs (edges source &key at-vertex (test 'eql) target (target-test test) random)
  "Depth-First-Search of the graph determined by EDGES.
EDGES is a function of one argument, a vertex, and return a list of cons cells
(NEIGHBOUR . EDGE-WEIGHT)
SOURCE is the initial vertex from which to perform the DFS
AT-VERTEX is a function of three argument, the vertex being visited, its parent,
 and the cost of the edge used to reach it.
TEST is how vertices are compared
If TARGET is non-NIL, the DFS stops after reaching a vertex equal (as of TARGET-TEST)
to it.
RANDOM determines whether the edges are searched in a deterministic way or not"
  (%dfs edges (list (list source))
        :at-vertex at-vertex
        :test test
        :target target
        :target-test target-test
        :random random))

;;; BFS
(defun %bfs (edges pending distance parents at-vertex test target target-test random)
  (let ((visited (make-hash-table :test test))
        distance-target)
    (loop :until (queue-empty-p pending)
          :for (x parent cost) = (queue-pop pending)
          :for at-target-p = (and target (funcall target-test x target))
          :unless (gethash x visited) :do
            ;; Add the neighbours
            (loop :with dist-x = (gethash x distance)
                  :for (y . cost) :in (if random
                                          (shuffle (funcall edges x))
                                          (funcall edges x))
                  :do (queue-push pending (list y x cost))
                      ;; Update the distance/parent of the vertex
                      (setf (gethash y distance) (1+ (gethash x distance))) ; don't consider COST
                      (setf (gethash y parents) x))
            ;; Mark the vertex as visited
            (setf (gethash x visited) t)
            (when at-vertex
              (funcall at-vertex x parent cost))
          :until (and at-target-p
                      (setf distance-target (gethash x distance))))
    (values distance-target distance parents)))

(defun bfs (edges source &key at-vertex (test 'eql) target (target-test test) random)
  "Breadth-First-Search of the graph determined by EDGES.
EDGES is a function of one argument, a vertex, and return a list of cons cells
(NEIGHBOUR . EDGE-WEIGHT)
SOURCE is the initial vertex from which to perform the BFS
AT-VERTEX is a function of three argument, the vertex being visited, its parent,
 and the cost of the edge used to reach it.
TEST is how vertices are compared
If TARGET is non-NIL, the BFS stops after reaching a vertex equal (as of TARGET-TEST)
to it.
RANDOM determines whether the edges are searched in a deterministic way or not
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
          distance
          parents
          at-vertex
          test
          target
          target-test
          random)))

;; numbra:
;;; Basic graph stuff

(in-package #:org.numbra.perso.algo.graphs)

(defun make-random-graph (n proba &key self-loops)
  "Generate a undirected graph with N vertices, where each edge appears with
probability PROBA.

If SELF-LOOPS is non-nil, edges with the same start and endpoint are allowed.

Return a function EDGES of one argument: on input a vertex V, that is, an
integer between 0 and N-1, returns a list of cons (NEIGHBOUR . 0) suitable for
e.g. `bfs'."
  (let ((graph (make-array n :initial-element nil)))
    (dotimes (i n)
      (loop :for j :from (1+ i) :below n
            :when (< (random 1.0) proba)
              :do (push (cons j 0) (aref graph i))
                  (push (cons i 0) (aref graph j))))
    (when self-loops
      (dotimes (i n)
        (when (< (random 1.0) proba)
          (push (cons i 0) (aref graph i)))))
    (lambda (v)
      (aref graph v))))

(defun make-graph-cycle (n)
  (lambda (u)
    (assert (<= 0 u (1- n)))
    (let ((prev (mod (1- u) n))
          (next (mod (1+ u) n)))
      `((,prev . 0)
        (,next . 0)))))

(defun make-graph-path (n)
  (lambda (u)
    (assert (<= 0 u (1- n)))
    (let ((prev (mod (1- u) n))
          (next (mod (1+ u) n)))
      (cond
        ((= u 0) `((,next . 0)))
        ((= u (1- n)) `((,prev . 0)))
        (t `((,prev . 0)
             (,next . 0)))))))

(defun make-graph-complete (n)
  (lambda (v)
    (assert (<= 0 v (1- n)))
    t))

(defun make-graph-complete-bipartite (m n)
  (let ((part-1 (loop :for i :below m
                      :collect (cons i 0)))
        (part-2 (loop :for i :below n
                      :collect (cons (+ i n) 0))))
    (lambda (v)
      (assert (<= 0 v (1- (+ m n))))
      (if (< v m)
          part-2
          part-1))))

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

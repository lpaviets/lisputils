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

;;; numbra:
;;; Grids
;;;
;;; Collection of small functions to work with grid-like objects.
;;; Provides function to move within a grid, rotate a square grid ...
;;;
;;; In particular, it uses a concept of DIRECTION: a direction is
;;; a keyword, one of :UP, :DOWN, :LEFT and :RIGHT.
;;;
;;; A POSITION is always a list (X Y) representing a coordinate in the grid.
;;;
;;; - X is the vertical axis
;;;
;;; - Y the horizontal one
;;;
;;; - The upper-left corner in (0, 0)

(in-package #:org.numbra.perso.ds)

(defun grid-pos-in-direction (pos dir &optional (n 1))
  (destructuring-bind (x y) pos
    (ecase dir
      (:up    (list (- x n) y))
      (:down  (list (+ x n) y))
      (:left  (list x (- y n)))
      (:right (list x (+ y n))))))

(defun grid-valid-pos-p (pos grid)
  (array-in-bounds-p grid (first pos) (second pos)))

(defun grid-opposite-direction (dir)
  (ecase dir
    (:up    :down)
    (:down  :up)
    (:left  :right)
    (:right :left)))

(defun grid-height (grid)
  (array-dimension grid 0))

(defun grid-width (grid)
  (array-dimension grid 1))

(defun grid-print (grid)
  (let ((len (nth-value 2 (utils:argmax grid :key (lambda (x)
                                                    (length (write-to-string x)))))))
    (loop :for i :below (grid-height grid) :do
      (loop :for j :below (grid-width grid) :do
        (format t "~vA" (if (<= len 1) 0 (1+ len)) (aref grid i j)))
      (format t "~%"))))

(defun grid-rotate (grid &key ccw)
  "Rotates a square GRID clockwise, or counter-clockwise if CCW is
non-nil.

The grid is modified. An error is signaled if GRID is not a square."
  (let ((height (grid-height grid))
        (width (grid-width grid)))
    (assert (= height width)
            nil
            "~&~A is not a square grid, dimensions are ~Ax~A~%"
            grid height width)
    (loop :for i :from 0 :below (truncate height 2)
          :for aux-1 = (- height i 1)
          :do (loop :for j :from i :below aux-1
                    :for aux-2 = (- height j 1)
                    :do (if ccw
                            (rotatef (aref grid i j)
                                     (aref grid j aux-1)
                                     (aref grid aux-1 aux-2)
                                     (aref grid aux-2 i))
                            (rotatef (aref grid i j)
                                     (aref grid aux-2 i)
                                     (aref grid aux-1 aux-2)
                                     (aref grid j aux-1)))))
    grid))

(defun grid-neighbours (pos grid &key diagonal self)
  "List of positions of the neighbours of POS in GRID.

If DIAGONAL is non-nil, includes the diagonally adjacent neighbours.

If SELF is non-nil, includes POS too."
  (let (neighbours)
    (destructuring-bind (i j) pos
      (dotimes (x 3)
        (dotimes (y 3)
          (let ((next-x (1- (+ x i)))
                (next-y (1- (+ y j))))
            (when (and (or (not (= x y 1)) self)
                       (and (or diagonal
                                (= next-x i)
                                (= next-y j)))
                       (array-in-bounds-p grid next-x next-y))
              (push (list next-x next-y)
                    neighbours))))))
    neighbours))

(defun grid-border-length (corners)
  (loop :with start = (car corners)
        :for (p1 p2) :on corners
        :if p2
          :sum (utils:manhattan-distance p1 p2)
        :else
          :sum (utils:manhattan-distance start p1)))

(defun grid-area-euclidean (corners)
  "Returns the (euclidean) unsigned area of the polygon whose corners are
given by CORNERS, in this order.

Uses the \"shoelace formula\" internally."
  (abs (/ (loop :with (xs ys) = (car corners)
                :for ((xi yi) (xi+1 yi+1)) :on corners
                :unless xi+1
                  :do (setf xi+1 xs
                            yi+1 ys)
                :sum (- (* xi yi+1) (* xi+1 yi)))
          2)))

(defun grid-area-lattice (corners)
  "Returns the unsigned area of the polygon whose corners are given by
CORNERS, in this order. The area here is defined as the numbers of
integer points that either belong to the \"border\" of the polygon, or
are inside it.

This function assumes that CORNERS correspond to an axis-aligned
polygon, i.e. two consecutive corners are either horizontally or
vertically aligned.

This uses the shoelace-formula and the Pick's theorem."
  (+ 1 (grid-area-euclidean corners) (truncate (grid-border-length corners) 2)))

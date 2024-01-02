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

(in-package #:org.numbra.perso.ds.grid)

(defvar *directions* '(:up :down :left :right))
(defvar *diagonals*  '(:upright :upleft :downright :downleft))
(defvar *all-directions* (append *directions* *diagonals*))

;; (defclass grid ()
;;   ((content :initarg :content :accessor grid-content)))

;; (defclass square-grid (grid)
;;   ())

;; (defclass sparse-grid (grid)
;;   ((default :initarg :default :accessor grid-default
;;             :documentation
;;             "Default value for valid grid positions that are not stored.")))

;; (defclass periodic-grid (grid)
;;   ((grid-period :initarg :period :reader grid-period)
;;    (occupied :initform (make-instance 'sparse-grid) :reader grid-occupied)))

;; (defgeneric grid-at (grid))
;; (defgeneric (setf grid-at) (grid))
;; (defgeneric grid-height (grid))
;; (defgeneric grid-width (grid))
;; (defgeneric grid-valid-pos-p (pos grid)
;;   (:method (pos (grid grid))
;;     (and (<= 0 (first pos) (1- (grid-height grid)))
;;          (<= 0 (second pos) (1- (grid-width grid))))))

(defun grid-at (pos grid)
  (aref grid (first pos) (second pos)))

(defun (setf grid-at) (val pos grid)
  (setf (aref grid (first pos) (second pos)) val))

(defun grid-pos-in-direction (pos dir &key (steps 1))
  (destructuring-bind (x y) pos
    (ecase dir
      (:up    (list (- x steps) y))
      (:down  (list (+ x steps) y))
      (:left  (list x (- y steps)))
      (:right (list x (+ y steps)))
      (:upright   (list (- x steps) (+ y steps)))
      (:upleft    (list (- x steps) (- y steps)))
      (:downright (list (+ x steps) (+ y steps)))
      (:downleft  (list (+ x steps) (- y steps))))))

(defun grid-valid-pos-p (pos grid)
  (array-in-bounds-p grid (first pos) (second pos)))

(defun grid-opposite-direction (dir)
  (ecase dir
    (:up    :down)
    (:down  :up)
    (:left  :right)
    (:right :left)
    (:upright   :downleft)
    (:upleft    :downright)
    (:downright :upleft)
    (:downleft  :upright)))

(defun grid-height (grid)
  (array-dimension grid 0))

(defun grid-width (grid)
  (array-dimension grid 1))

(defun grid-print (grid &key key replace (test #'eql))
  (flet ((convert (x)
           (when key (setf x (funcall key x)))
           (when replace
             (setf x (or (cdr (assoc x replace :test test)) x)))
           (princ-to-string x)))
    (let ((len (nth-value 2
                          (utils:argmax grid :key (lambda (x)
                                                    (length (convert x)))))))
      (loop :for i :below (grid-height grid) :do
        (loop :for j :below (grid-width grid) :do
          (format t "~vA" (if (<= len 1) 0 (1+ len)) (convert (aref grid i j))))
        (format t "~%")))))

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
                            (rotatef (grid-at (list i j) grid)
                                     (grid-at (list j aux-1) grid)
                                     (grid-at (list aux-1 aux-2) grid)
                                     (grid-at (list aux-2 i) grid))
                            (rotatef (grid-at (list i j) grid)
                                     (grid-at (list aux-2 i) grid)
                                     (grid-at (list aux-1 aux-2) grid)
                                     (grid-at (list j aux-1) grid)))))
    grid))

(defun %grid-torus-wrap (pos grid)
  (loop :for x :in pos
        :for d :in (array-dimensions grid)
        :collect (mod x d)))

(defun grid-neighbours (pos grid &key diagonal self walls (test #'eql) key torus)
  "List of positions of the neighbours of POS in GRID. This assumes that
POS is a valid position of GRID.

If DIAGONAL is non-nil, includes the diagonally adjacent neighbours.

If SELF is non-nil, includes POS too.

When WALLS is non-NIL, it is used to filter the list of neighbours.
That is, (I J) is not included in the return list if (AREF GRID I J)
is equal to WALLS, as tested by TEST.

If KEY is non-NIL, use it on (AREF GRID I J) before comparing it to
WALLS.

If TORUS is non-NIL, view GRID as a torus. This means that e.g. the
neighbour in direction :UP of the position (0 Y) becomes (H Y), where
H is the maximal row of the grid."
  (loop :for dir :in (if diagonal *all-directions* *directions*)
        :for next = (grid-pos-in-direction pos dir)
        :for wrapped = (if torus (%grid-torus-wrap next grid) next)
        :when (and (grid-valid-pos-p wrapped grid)
                   (or (not walls)
                       (not (funcall test
                                     (grid-at wrapped grid)
                                     (if key (funcall key walls) walls)))))
          :collect wrapped :into neighbours
        :finally (return-from grid-neighbours
                   (remove-duplicates
                    (if self
                        (cons (copy-seq pos) neighbours)
                        neighbours)
                    :test #'equal))))

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

(defun grid-apply-as-sequence (grid function &rest args)
  "Apply FUNCTION to ARGS with GRID viewed as a (one-dimensional)
sequence.

In ARGS, the special keyword :% is used to denote the positions at
which the GRID should be inserted in the arguments list.

For example:

(GRID-APPLY-AS-SEQUENCE #'POSITION GRID ITEM :% :TEST #'EQUAL)

is equivalent to

(POSITION ITEM GRID-SEQ :TEST #'EQUAL)

where GRID-SEQ is a \"view\" on GRID as if it was a sequence.

As a special case, if no :% symbol is present in ARGS, GRID is used as
the SECOND argument instead. This is because most sequence functions
take the sequence as a 2nd argument, e.g. FIND or MAP. If ARGS is NIL,
then it is used as the first argument."
  (let* ((view (make-array (array-total-size grid) :displaced-to grid))
         (new-args (cond
                     ((member :% args) (substitute view :% args))
                     ((endp args) (list view))
                     (t (list* (first args)
                               view
                               (cdr args))))))
    (apply function new-args)))

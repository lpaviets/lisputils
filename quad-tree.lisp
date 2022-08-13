;;; numbra:
;;; Quad-trees

(in-package #:numbra)

;;; Quad-box

(defclass quad-box ()
  ((left :accessor left :initarg :left)
   (bot :accessor bot :initarg :bot)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defun make-box (left bot width height)
  (make-instance 'quad-box :left left :bot bot :width width :height height))

(defgeneric make-bounding-box (object)
  (:documentation "Make a quad-box from OBJECT"))

(defmethod make-bounding-box ((box quad-box))
  box)

(defun right (box)
  (+ (left box) (width box)))

(defun top (box)
  (+ (bot box) (height box)))

(defmacro with-box-sides ((left bot right top) box &body body)
  (let ((gbox (gensym)))
    `(let ((,gbox ,box))
       (let ((,left (left ,gbox))
             (,bot (bot ,gbox))
             (,right (right ,gbox))
             (,top (top ,gbox)))
         (declare (ignorable ,left ,bot ,right ,top))
         ,@body))))

(defun center (box)
  (point (+ (left box) (/ (width box) 2))
         (+ (bot box) (/ (height box) 2))))

(defgeneric box-contains-p (box thing))

(defmethod box-contains-p (box (point point))
  (and (<= (left box) (point-x point) (right box))
       (<= (bot box) (point-y point) (top box))))

(defmethod box-contains-p (box-a (box-b quad-box))
  (with-box-sides (la ba ra ta) box-a
    (with-box-sides (lb bb rb tb) box-b
      (and (<= la lb rb ra)
           (<= ba bb tb ta)))))

(defmethod box-intersects-p (box-a (box-b quad-box))
  (with-box-sides (la ba ra ta) box-a
    (with-box-sides (lb bb rb tb) box-b
      (not (or (< rb la)
               (< ra lb)
               (< ta bb)
               (< tb ba))))))

;;; Returns an array of length 4, with a box corresponding in order
;;; to the NW, NE, SW, SE bounding boxes of each quadrant of BOX
(defun box-divide-bounding-boxes (box)
  (with-box-sides (left bot right top) box
    (let* ((center (center box))
           (cx (point-x center))
           (cy (point-y center))
           (w1 (- cx left))
           (w2 (- right cx))
           (h1 (- cy bot))
           (h2 (- top cy))
           (quadrants (make-array 4)))
      (setf (aref quadrants 0) (make-box left cy w1 h2)
            (aref quadrants 1) (make-box cx cy w2 h2)
            (aref quadrants 2) (make-box left bot w1 h1)
            (aref quadrants 3) (make-box cx bot w2 h1))
      quadrants)))

;;; Quad-trees
(defvar *max-qtree-objects* 10)
(defvar *max-qtree-level* 7)

(defclass quad-tree ()
  ((level
    :reader level
    :initarg :level)
   (nodes
    :accessor nodes
    :initform nil
    :documentation "In order, the NW, NE, SW, and SE sub-nodes of the quad-tree")
   (objects
    :accessor objects
    :initarg :objects)
   (bounds
    :accessor bounds
    :initarg :bounds
    :initform (error "A bounding box needs to be specified")
    :type quad-box))
  (:default-initargs
   :objects (make-hash-table :test #'eq :size (1+ *max-qtree-objects*))))

(defun qtree-objects-count (tree)
  (hash-table-count (objects tree)))

(defun qtree-objects-add (tree obj)
  "Adds OBJ to the objects of TREE without any verification"
  (setf (gethash obj (objects tree)) t))

(defun qtree-objects-remove (tree obj)
  (remhash obj (objects tree)))

(defmacro do-qtree-objects ((obj tree) &body body)
  `(maphash (lambda (,obj v)
              (declare (ignore v))
              ,@body)
            (objects ,tree)))

(defun qtree-node (tree node)
  (aref (nodes tree) node))

(defun (setf qtree-node) (val tree node)
  (unless (nodes tree)
    (setf (nodes tree) (make-array 4)))
  (setf (aref (nodes tree) node) val))

(defun make-qtree (level bounds)
  (make-instance 'quad-tree :level level :bounds bounds))

(defun make-qtree-from-list (list bounds)
  (let ((tree (make-qtree 0 bounds)))
    (dolist (obj list)
      (qtree-insert tree obj))
    tree))

;;; Garbage collection -> isn't it enough to just set all 4 nodes to NIL
;;; instead of performing recursive calls ?
(defun qtree-clear (tree)
  (when tree
    (setf (objects tree) nil)
    (when (nodes tree)
      (loop :with nodes = (nodes tree)
            :for i :below 4
            :do (qtree-clear (qtree-node tree i))
                (setf (nodes tree) nil)))))

(defun qtree-split (tree)
  (loop :with new-level = (1+ (level tree))
        :for i :below 4
        :for quad-box :across (box-divide-bounding-boxes (bounds tree))
        :do (setf (qtree-node tree i) (make-qtree new-level quad-box))))

;;; This assumes that box is contained (= can fit) in the tree
(defun qtree-containing-node (tree box)
  (with-accessors ((nodes nodes))
      tree
    (loop :with quadrants = (if nodes
                                (map 'vector #'bounds nodes)
                                (box-divide-bounding-boxes (bounds tree)))
          :for i :below 4
          :for quad-box :across quadrants
          :when (box-contains-p quad-box box)
            :do (return i)
          :finally (return -1))))

(defun qtree-move-objects-to-subnodes (tree)
  (do-qtree-objects (obj tree)
    (let ((index (qtree-containing-node tree (make-bounding-box obj))))
      (when (/= index -1)
        (qtree-objects-remove tree obj)
        (qtree-insert (qtree-node tree index) obj)))))

(defun qtree-insert (tree object)
  (let ((obj-bounds (make-bounding-box object)))
    (assert (box-contains-p (bounds tree) obj-bounds)
            (object) "~A cannot fit in the quad-tree ~A~%" object tree)
    (let ((index (qtree-containing-node tree obj-bounds)))
      (cond
        ;; Object can't fit in a subnode, so we add it here
        ((= index -1)
         (qtree-objects-add tree object))
        ;; If we are at the lowest possible level, we can't split any
        ;; further so we simply add the object here
        ((= (level tree) *max-qtree-level*)
         (qtree-objects-add tree object))
        ;; Otherwise, if there are already sub-nodes, we add it in the
        ;; correct one
        ((nodes tree)
         (qtree-insert (qtree-node tree index) object))
        ;; If there are no subnodes we add it here
        (t
         (qtree-objects-add tree object)))
      ;; We now need to fix the fact that the current node might be contain
      ;; too many objects.
      ;; We need to split & move things to the right place afterwards
      (when (> (qtree-objects-count tree) *max-qtree-objects*)
        (unless (nodes tree) (qtree-split tree))
        (qtree-move-objects-to-subnodes tree)))))

(defun qtree-traverse (tree function &optional visit-node-p)
  "FUNCTION is a function of two arguments, OBJ and NODE. It is called
on each object of the tree with its bouding node, starting from the root,
and then recursively (DFS) on the nodes, in order NW/NE/SW/SE
If VISIT-NODE-P is non-NIL, it is called on each node. A NIL return value
means that this node is not recursively explored.
Return the value of FUNCTION on the last object"
  (let (res)
    (do-qtree-objects (obj-tree tree)
      (setf res (funcall function obj-tree tree)))
    (unless (null (nodes tree))
      (loop :for i :below 4
            :for node = (qtree-node tree i)
            :when (or (null visit-node-p)
                      (funcall visit-node-p node))
              :do (setf res (qtree-traverse node function))))
    res))

(defun qtree-helper-intersectp (obj obj-tree box-obj predicate)
  "Filter un-necessary computations: BOX is already supposed to be the
bounding box of OBJ, and the function returns T only if:
- OBJ and OBJ-TREE are distinct objects
- PREDICATE is either NIL or (PREDICATE OBJ OBJ-TREE) is non-NIL
- BOX and the bouding box of OBJ-TREE are intersecting"
  (and (not (eq obj obj-tree))
       (or (null predicate)
           (funcall predicate obj obj-tree))
       (box-intersects-p (make-bounding-box obj-tree) box-obj)))

(defun qtree-intersect-list (tree object &optional predicate)
  "List of all the objects that intersect OBJECT contained in TREE
If PREDICATE is non-NIL, only the objects OBJ-TREE such that the function
PREDICATE returns a non-NIL value on OBJ-TREE and OBJECT are considered"
  (let ((box (make-bounding-box object))
        acc)
    (flet ((add-intersect-to-acc (obj-tree node)
             (declare (ignore node))
             (when (qtree-helper-intersectp object obj-tree box predicate)
               (push obj-tree acc)))
           (visit-node-p (node)
             (box-intersects-p box (bounds node))))
      (qtree-traverse tree #'add-intersect-to-acc #'visit-node-p)
      acc)))

(defun qtree-intersect-some (tree object &optional predicate)
  "Return as soon as OBJECT intersects some object of TREE.
See qtree-intersect-list and qtree-helper-intersectp for the role of
PREDICATE"
  (let ((box (make-bounding-box object)))
    (flet ((some-intersect-p (obj-tree node)
             (declare (ignore node))
             (when (qtree-helper-intersectp object obj-tree box predicate)
               (return-from qtree-intersect-some obj-tree)))
           (visit-node-p (node)
             (box-intersects-p box (bounds node))))
      (qtree-traverse tree #'some-intersect-p #'visit-node-p))))

(defun qtree-valid-p (tree)
  (flet ((validp (obj node)
           (box-contains-p (bounds node) (make-bounding-box obj))))
    (qtree-traverse tree #'validp))
  t)

;; (defun qtree-self-intersect (tree &optional predicate))

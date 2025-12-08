;; numbra
;; Union-find

(in-package #:org.numbra.perso.ds.uf)

;; Union-find
(defclass uf-wrapper ()
  ((uf-item :accessor uf-item :initarg :item)
   (uf-parent :accessor uf-parent :initarg :parent)
   (uf-rank :accessor uf-rank :initform 0)
   (uf-size :accessor uf-size :initform 1)
   (uf-children :accessor uf-children :initform nil))
  (:documentation "Wrapper around arbitrary objects.
Contains the slots UF-ITEM, UF-PARENT, UF-SIZE and UF-RANK, and UF-CHILDREN:

- UF-ITEM is the content of this wrapper
- UF-PARENT is the representative of the equivalence class of the item
- UF-SIZE and UF-RANK are used to speed-up the data structure maintenance,
UF-SIZE being also useful to know the size of the connected component (this
is only valid for representatives)
- UF-CHILDREN is the list of elements that point towards this node. This is
useful to build the equivalence class of a given element.

NOTE : the rank is meaningless in the case of a union-by-size !"))

(defclass uf-partition ()
  ((test :reader partition-test
         :initarg :test)
   (elements :accessor partition-elements
             :initarg :elements
             :documentation "A hash-table mapping actual items to their wrapped version used in the
Union-Find algorithm")
   (representatives :accessor uf-representatives
                    :initarg :representatives
                    :type hash-table
                    :documentation "The current set of representatives of equivalence classes")
   (strategy :reader uf-strategy
             :initarg :strategy
             :type (or :rank :size)))
  (:default-initargs
   :strategy :size))

(defun wrap-uf-object (x)
  (let ((wrapper (make-instance 'uf-wrapper
                                :item x)))
    (setf (uf-parent wrapper) wrapper)
    wrapper))

(defun make-partition (test &key (strategy :size))
  (let ((elements (make-hash-table :test test))
        (repr (make-hash-table :test test)))
    (make-instance 'uf-partition :test test
                                 :elements elements
                                 :strategy strategy
                                 :representatives repr)))

(defun in-partition-p (partition x)
  (gethash x (partition-elements partition)))

(defun %add-representative (partition wx)
  (setf (gethash wx (uf-representatives partition)) t))

(defun %del-representative (partition wx)
  (remhash wx (uf-representatives partition)))

(defun add-to-partition (partition x)
  (let ((wrapped (wrap-uf-object x)))
    (setf (gethash x (partition-elements partition)) wrapped)
    (%add-representative partition wrapped)))

(defun uf-make-set (partition x)
  "Add X to PARTITION, or return its representative instead if it is
already present"
  (let ((wrapped-x (in-partition-p partition x)))
    (if wrapped-x
        wrapped-x
        (add-to-partition partition x))))

(defun uf-initialize (items &key (test #'eql) (strategy :size))
  (let ((partition (make-partition test :strategy strategy)))
    (etypecase items
      (sequence (map nil
                     (lambda (item)
                       (add-to-partition partition item))
                     items))
      (hash-table (utils:do-hashkeys (item items)
                    (add-to-partition partition item))))
    partition))

;; TODO: maintain the uf-children part of the data structure
(defun %uf-find (partition wx)          ; wx is a wrapped version
  (unless (eq wx (uf-parent wx))
    ;; recursive version, might require a lot of memory in theory
    (setf (uf-parent wx) (%uf-find partition (uf-parent wx)))
    (%del-representative partition wx)
    (push wx (uf-children (uf-parent wx))))
  (uf-parent wx))

(defun uf-find (partition x)
  "Find the representative for X in PARTITION"
  (let ((wrapped (in-partition-p partition x)))
    (when wrapped
      (%uf-find partition wrapped))))

(defun %uf-union-rank (partition wx wy)
  (unless (eq wx wy)
    (when (< (uf-rank wx) (uf-rank wy))
      (rotatef wx wy))
    (setf (uf-parent wy) wx)
    (when (= (uf-rank wx) (uf-rank wy))
      (incf (uf-rank wx)))
    (incf (uf-size wx) (uf-size wy))
    (%del-representative partition wy)
    (push wy (uf-children wx))))

(defun %uf-union-size (partition wx wy)
  (unless (eq wx wy)
    (when (< (uf-size wx) (uf-size wy))
      (rotatef wx wy))
    (setf (uf-parent wy) wx)
    (incf (uf-size wx) (uf-size wy))
    (%del-representative partition wy)
    (push wy (uf-children wx))))

(defun uf-union (partition x y)
  (let ((rep-x (uf-find partition x))
        (rep-y (uf-find partition y)))
    (unless (eq rep-x rep-y)
      (case (uf-strategy partition)
        (:rank (%uf-union-rank partition rep-x rep-y))
        (:size (%uf-union-size partition rep-x rep-y))
        (t (error "Invalid partition strategy for ~A: ~A" partition (uf-strategy partition)))))))

;; False ???
#|
(equivalence-classes (range 10)
                     '((1 . 3)
                       (1 . 5)
                       (5 . 7)
                       (8 . 9)
                       (4 . 6)
                       (9 . 6)))
|#

(defun uf-equivalence-class-of (partition x)
  (let ((wx (uf-find partition x)))
    (when wx
      ())))

(defun equivalence-classes (items base &key (test #'eql))
  "Compute the reflexive and transitive closure of BASE in
ITEMS x ITEMS.

 BASE is a list of CONS, containing pairs of elements belonging to the
same equivalence class.

 Return a hash-table with keys the representatives of the equivalences
classes, and values a list of all the elements of the class, including
the representative itself.

In particular, HASH-TABLE-COUNT of the result is the number of
distinct equivalence classes in ITEMS."
  (let ((partition (uf-initialize items :test test))
        (result (make-hash-table :test test)))
    (loop :for (x . y) :in base
          :do (uf-union partition x y))
    (utils:do-hash (x wrapped-x (partition-elements partition))
      (let ((repr (uf-item (uf-parent wrapped-x))))
        (if (gethash repr result)
            (push x (gethash repr result))
            (setf (gethash repr result) (list x)))))
    result))

;; numbra
;; Union-find

(in-package #:org.numbra.perso.ds.uf)

;; Union-find
(defclass uf-wrapper ()
  ((uf-item :accessor uf-item :initarg :item)
   (uf-parent :accessor uf-parent :initarg :parent)
   (uf-rank :accessor uf-rank :initarg :rank))
  (:documentation "Wrapper around arbitrary objects.
Contains the slots UF-ITEM, UF-PARENT, and UF-RANK"))

(defclass uf-partition ()
  ((test :reader partition-test :initarg :test)
   (elements :accessor partition-elements :initarg :elements
             :documentation "A hash-table mapping actual items to their wrapped version used in the
Union-Find algorithm")))

(defun wrap-uf-object (x)
  (let ((wrapper (make-instance 'uf-wrapper
                                :item x
                                :rank 0)))
    (setf (uf-parent wrapper) wrapper)
    wrapper))

(defun make-partition (test)
  (let ((table (make-hash-table :test test)))
    (make-instance 'uf-partition :test test :elements table)))

(defun in-partition-p (partition x)
  (gethash x (partition-elements partition)))

(defun add-to-partition (partition x)
  (let ((wrapped (wrap-uf-object x)))
    (setf (gethash x (partition-elements partition)) wrapped)))

(defun uf-make-set (partition x)
  "Add X to PARTITION, or return its representative instead if it is
already present"
  (let ((wrapped-x (in-partition-p partition x)))
    (if wrapped-x
        wrapped-x
        (add-to-partition partition x))))

(defun uf-initialize (items &key (test #'eql))
  (let ((partition (make-partition test)))
    (etypecase items
      (sequence (map nil
                     (lambda (item)
                       (add-to-partition partition item))
                     items))
      (hash-table (utils:do-hashkeys (item items)
                    (add-to-partition partition item))))
    partition))

(defun %uf-find (partition x)           ; x is a wrapped version
  (unless (eq x (uf-parent x))
    ;; recursive version, might require a lot of memory in theory
    (setf (uf-parent x) (%uf-find partition (uf-parent x))))
  (uf-parent x))

(defun uf-find (partition x)
  "Find the representative for X in PARTITION"
  (let ((wrapped (in-partition-p partition x)))
    (when wrapped
      (%uf-find partition wrapped))))

(defun %uf-union (x y)
  (unless (eq x y)
    (when (< (uf-rank x) (uf-rank y))
      (rotatef x y))
    (setf (uf-parent y) x)
    (when (= (uf-rank x) (uf-rank y))
      (incf (uf-rank x)))))

(defun uf-union (partition x y)
  (%uf-union (uf-find partition x) (uf-find partition y)))

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

;;; numbra:
;;; Utilities

(in-package #:org.numbra.perso.utils)

(defun flip (x y &optional (comp 'equal) (keep-others t))
  "Return a function of one argument which swaps X and Y
If KEEP-OTHERS is non-NIL, this function acts like `identity' on other elements
Otherwise, elements that are not X nor Y are mapped to Y"
  (lambda (z)
    (cond
      ((funcall comp x z) y)
      ((funcall comp y z) x)
      (keep-others z)
      (t y))))

(defun range (m &optional n (step 1))
  (assert (not (zerop step)) () "STEP cannot be 0")
  (let ((start (if n m 0))
        (end (if n n m))
        (test (if (plusp step) #'>= #'<=)))
    (loop :for i = start :then (+ i step)
          :until (funcall test i end)
          :collect i)))

(defun lazy-range (m &optional n (step 1))
  (assert (not (zerop step)) () "STEP cannot be 0")
  (let ((i (if n m 0))
        (end (or n m))
        (test (if (plusp step) #'>= #'<=)))
    (lambda (default)
      (if (funcall test i end)
          default
          (prog1 i (incf i step))))))

(defun permutations (n)
  "List of all the permutations of the integers between 0 and n included"
  (if (zerop n)
      (list '(0))
      (loop :with perms = (permutations (1- n))
            :for perm :in perms
            :append
            (loop :repeat (1+ n)
                  :for (beg end) = (list nil perm)
                    :then (list (cons (car end) beg)
                                (cdr end))
                  :collect (append beg (list n) end)))))

(defun sublists-length (list n)
  "List of all the sublists of LIST of length N"
  (cond
    ((= 1 n) (mapcar 'list list))
    ((null list) nil)
    (t
     (append (sublists-length (cdr list) n)
             (mapcar (lambda (x)
                       (cons (car list) x))
                     (sublists-length (cdr list) (1- n)))))))

(defun neighbours (i j array &key diagonal self)
  "List of positions of the neighbours of (I J) in ARRAY
If DIAGONAL is non-nil, includes the diagonally adjacent neighbours
If SELF is non-nil, include (I J) too"
  (let (neighbours)
    (dotimes (x 3)
      (dotimes (y 3)
        (let ((next-x (1- (+ x i)))
              (next-y (1- (+ y j))))
          (when (and (or (not (= x y 1)) self)
                     (and (or diagonal
                              (= next-x i)
                              (= next-y j)))
                     (array-in-bounds-p array next-x next-y))
            (push (list next-x next-y)
                  neighbours)))))
    neighbours))

(defgeneric argmax (sequence &key test start end key)
  (:documentation "Returns OBJ maximizing TEST in SEQUENCE between START and END.
If END is NIL, search it until the end of the sequence.
If KEY is non NIL, applies it to each element instead of using them
directly.
Additionally, the position of the element is returned as a secondary value,
as a list, as SEQUENCE might be a multi-dimensional object, and its value
for KEY as a third value.
If SEQUENCE is empty, return NIL for all three values"))

(defmethod argmax ((sequence list) &key (test #'<) (start 0) end key)
  (setf sequence (nthcdr start sequence))
  (cond
    ((and sequence
          (or (not end)
              (> end start)))
     (loop :with max-idx = start
           :with max-elt = (first sequence)
           :with max-val = (if key
                               (funcall key max-elt)
                               max-elt)
           :for elt :in (cdr sequence)
           :for idx :from start
           :while (or (not end) (< idx end))
           :for val = (if key (funcall key elt) elt)
           :when (funcall test max-val val)
             :do (setf max-elt elt
                       max-val val
                       max-idx idx)
           :finally
              (return (values max-elt (list max-idx) max-val))))
    (t (values nil nil nil))))

(defmethod argmax ((sequence vector) &key (test #'<) (start 0) end key)
  (let ((length (length sequence)))
    (cond
      ((and (< start length)
            (or (not end)
                (> end start)))
       (loop :with max-idx = start
             :with max-elt = (aref sequence start)
             :with max-val = (if key
                                 (funcall key max-elt)
                                 max-elt)
             :for idx :from (1+ start)
             :while (and (< idx length)
                         (or (not end)
                             (< idx end)))
             :for elt = (aref sequence idx)
             :for val = (if key (funcall key elt) elt)
             :when (funcall test max-val val)
               :do (setf max-elt elt
                         max-val val
                         max-idx idx)
             :finally
                (return (values max-elt (list max-idx) max-val))))
      (t (values nil nil nil)))))

(defmethod argmax ((sequence hash-table) &key (test #'<) start end key)
  (declare (ignore start end))
  (loop :for x :being :the :hash-keys :of sequence
        :for val = (if key (funcall key x) x)
        :for replace-max-p = nil :then (funcall test max-val val)
        :for max-val = val :then (if replace-max-p val max-val)
        :for max-elt = x :then (if replace-max-p x max-elt)
        :finally (return (values max-elt nil max-val))))

(defun %array-inverse-row-major-index (array index)
  (let ((dims (reverse (array-dimensions array)))
        (res ())
        rem)
    (dolist (d dims)
      (multiple-value-setq (index rem) (truncate index d))
      (push rem res))
    res))

(defmethod argmax ((sequence array) &key (test #'<) (start 0) end key)
  (assert (and (or (null start) (zerop start))
               (null end))
          ()
          "ARGMAX cannot be called on a subarray of a multimensional array~@
START has to be NIL or 0 instead of ~S~@
and END has to be NIL instead of ~S~%"
          start end)
  (loop :with length = (array-total-size sequence)
        :with max-idx = (make-list (array-rank sequence) :initial-element 0)
        :with max-elt = (row-major-aref sequence 0)
        :with max-val = (if key
                            (funcall key max-elt)
                            max-elt)
        :for idx :from 1
        :while (< idx length)
        :for elt = (row-major-aref sequence idx)
        :for val = (if key (funcall key elt) elt)
        :when (funcall test max-val val)
          :do (setf max-elt elt
                    max-val val
                    max-idx idx)
        :finally
           (return (values max-elt
                           (%array-inverse-row-major-index sequence max-idx)
                           max-val))))

(defgeneric deepcopy (thing)
  (:documentation "Recursively creates a copy of THING.")
  (:method ((thing t)) thing))

(defmethod deepcopy ((thing list))
  (mapcar #'deepcopy thing))

(defmethod deepcopy ((thing array))
  (loop :with new = (make-array (array-dimensions thing)
                                :element-type (array-element-type thing)
                                :adjustable (adjustable-array-p thing)
                                :fill-pointer (and (array-has-fill-pointer-p thing)
                                                   (fill-pointer thing)))
        :for i :below (array-total-size thing) :do
          (setf (row-major-aref new i)
                (deepcopy (row-major-aref thing i)))
        :finally (return new)))

(defmethod deepcopy ((thing hash-table))
  (let ((table (make-hash-table :size (hash-table-size thing)
                                :test (hash-table-test thing))))
    (maphash (lambda (k v)
               (setf (gethash (deepcopy k) table)
                     (deepcopy v)))
             thing)
    table))

(defun %shuffle (list len acc)
  (declare (type list list acc)
           (type fixnum len)
           (optimize (speed 3)))
  (if (endp list)
      acc
      (let* ((rand (random (+ len 1)))
             (new-acc (append (butlast acc rand)
                              (list (car list))
                              (last acc rand))))
        (%shuffle (cdr list) (+ len 1) new-acc))))

(defun shuffle (list)
  (%shuffle list 0 nil))

(defun ensure-list (obj)
  (if (listp obj) obj (list obj)))

(defun substitute-assoc (alist sequence &key (test #'eql) key (start 0) end)
  (flet ((subs (seq old-new)
           (substitute (cdr old-new) (car old-new) seq
                       :test test
                       :key key
                       :start start
                       :end end)))
    (reduce #'subs alist
            :initial-value sequence)))

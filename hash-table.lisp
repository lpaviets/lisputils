;;; numbra:
;;; Hash-table

(in-package #:org.numbra.perso.ds)

(defun ht-create (&rest args)
  "Create a hash-table using ARGS as keys and values.
If ARGS is of even length, the even-indexed elements are used as keys,
and the odd-indexed ones as values, alternatively.
If ARGS is of odd length, its first element is used as the :TEST argument
to MAKE-HASH-TABLE"
  (let* ((test (when (oddp (length args)) (pop args)))
         (table (make-hash-table :test (or test 'eql))))
    (loop :for (key val) :on args :by #'cddr
          :do (setf (gethash key table) val))
    table))

(defun ht-pop (table)
  "Return a random key from TABLE, and remove it from TABLE. As a
secondary return value, also return the value associated to it in
TABLE."
  (let (rk rv)
    (block nil
      (maphash (lambda (k v) (setf rk k rv v)) table)
      (return))
    (remhash rk table)
    (values rk rv)))

(defun ht-count (item table &key key (test 'eql) value)
  "Return the number of entries in TABLE satisfying a test with ITEM,
which defaults to EQL.
If VALUE is non-nil, compare the table values instead of its keys."
  (let ((counter 0))
    (maphash (lambda (k v)
               (let ((x (if value v k)))
                 (when (funcall test item (if key (funcall key x) x))
                   (incf counter))))
             table)
    counter))

(defun ht-count-if (predicate table &key key value)
  "Count the elements of TABLE satisfying PREDICATE.

If VALUE is NIL, iterate over the keys. Otherwise, iterate over the
values of TABLE.

Predicate is called with each element (key or value) directly, or with
(FUNCALL KEY ELEMENT) if KEY is non-NIL."
  (let ((counter 0))
    (maphash (lambda (k v)
               (let ((x (if value v k)))
                 (when (funcall predicate (if key (funcall key x) x))
                   (incf counter))))
             table)
    counter))

(defun ht-from-sequence (seq &key (test 'eql) key (start 0) end value)
  "Create a hash-table from sequence SEQ. The hash-table test is set to
TEST.

The keys of the hash-table are the elements of SEQ between positions
START and END, or (FUNCALL KEY ELEMENT) if KEY is non-NIL.

If VALUE is NIL, all the keys of the hash-table are set to T.
Otherwise, the value associated to ELEMENT is (FUNCALL VALUE ELEMENT)."
  (let ((table (make-hash-table :test test)))
    (reduce (lambda (ign x)
              (declare (ignore ign))
              (setf (gethash (if key (funcall key x) x) table)
                    (if value (funcall value x) t)))
            seq
            :start start
            :end end
            :initial-value t)
    table))

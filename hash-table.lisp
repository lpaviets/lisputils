;;; numbra:
;;; Hash-table

(in-package #:org.numbra.perso.ds.ht)

(defun ht-create (&rest args)
  "Create a hash-table using ARGS as keys and values.

If ARGS is of even length, the even-indexed elements are used as keys,
and the odd-indexed ones as values, alternatively.

If ARGS is of odd length, its first element is used as the :TEST argument
to MAKE-HASH-TABLE.

See also `ht-from-plist' for another way to create hash-tables from a
variable number of pairs."
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
      ;; Weird hack: we access any element using maphash but we stop
      ;; the mapping at the first iteration using a RETURN /outside/
      ;; the MAP form.
      (maphash (lambda (k v)
                 (setf rk k
                       rv v))
               table)
      (return))
    (remhash rk table)
    (values rk rv)))

(defun ht-merge (value-test hash-table &rest hash-tables)
  "Returns a fresh hash-table containing all the keys of
all the HASH-TABLES given as arguments, including HASH-TABLE. All the
arguments must have the same hash-table-test.

If a key appears in two or more hash-tables, it must be associated to
the same value, as tested by VALUE-TEST."
  (loop :with new-table = (make-hash-table :test (hash-table-test hash-table)
                                           :size (hash-table-size hash-table))
        :with test = (hash-table-test new-table)
        :for table :in (cons hash-table hash-tables)
        :unless (eq test (hash-table-test table))
          :do (error "Hash-tables ~A, ~A have different HASH-TABLE-TEST"
                     hash-table table)
        :do (utils:do-hash (x v table)
              (multiple-value-bind (prev found)
                  (gethash x new-table)
                (cond
                  ((not found) (setf (gethash x new-table) v))
                  ((and found (funcall value-test prev v)) t)
                  (t (error "Key ~A is associated to different values ~A and ~A"
                            x prev v)))))
        :finally (return new-table)))

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

(defun ht-from-plist (list &key (test 'eql) key)
  "Create a hash-table from LIST. The hash-table test is set to TEST.

LIST is understood to be a property-list: elements are grouped by
pairs, the first one being the key and the second one being the value.

For example, (HT-FROM-PLIST '(x 3 y 4)) will return a hash-table of
count 2, where the keys X and Y are respectively associated to the
values 3 and 4.

When KEY is non-NIL, it is a function of one argument, taking the keys
of the hash-table as input. In that case, the result of calling KEY on
the keys of the plist LIST are inserted in the hash-table, rather than
the elements themselves.

For example,

(HT-FROM-PLIST '(x 3 y 4) :test #'equalp :key 'symbol-name)

will return a hash-table of count 2, where the keys are now the
string\"X\" and \"Y\" rather than symbols. In particular, the TEST
can not be meaningfully left to EQL.

See also `ht-create' for another way to create hash-tables from a
variable number of pairs."

  (loop :with table = (make-hash-table :test test)
        :for (k v) :on list :by #'cddr
        :do (setf (gethash (if key (funcall key k) k) table) v)
        :finally (return table)))

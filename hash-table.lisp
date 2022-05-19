;;; numbra:
;;; Hash-table

(in-package #:numbra)

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
  (let ((counter 0))
    (maphash (lambda (k v)
               (let ((x (if value v k)))
                (when (funcall predicate (if key (funcall key x) x))
                  (incf counter))))
             table)
    counter))

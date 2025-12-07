;;; numbra
;;; Printing

(in-package #:org.numbra.perso.io)

(defun print-array (array &key separator (width 0))
  (loop :for i :below (array-dimension array 0) :do
    (loop :for j :below (array-dimension array 1) :do
      (format t "~v@A~@[~A~]" width (aref array i j) separator))
    (format t "~%"))
  array)

(defun print-hash (object &key (stream t))
  (format stream "#HASH{~{~{(~S : ~S)~}~%~^ ~}}"
          (loop :for key :being :the :hash-keys :of object
                  :using (:hash-value value)
                :collect (list key value))))

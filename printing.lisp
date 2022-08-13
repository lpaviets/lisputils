;;; numbra
;;; Printing

(in-package #:org.numbra.perso.io)

(defun print-array (array &optional sep)
  (loop :for i :below (array-dimension array 0) :do
    (loop :for j :below (array-dimension array 1) :do
      (format t "~A~@[~A~]" (aref array i j) sep))
    (format t "~%"))
  array)

(defun print-hash (object &optional (stream t))
  (format stream "#HASH{~{~{(~S : ~S)~}~%~^ ~}}"
          (loop :for key :being :the :hash-keys :of object
                  :using (:hash-value value)
                :collect (list key value))))

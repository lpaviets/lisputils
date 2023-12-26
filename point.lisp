(in-package #:org.numbra.perso.ds.point)

(defclass point ()
  ((x :accessor point-x :initarg :x)
   (y :accessor point-y :initarg :y)))

(declaim (inline point))
(defun point (x y)
  (make-instance 'point :x x :y y))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "X: ~S Y: ~S" (point-x obj) (point-y obj))))

(defmacro with-point ((x y) point &body body)
  `(with-accessors ((,x point-x)
                    (,y point-y))
       ,point
     ,@body))

(defun add-point (pt-a pt-b)
  (point (+ (point-x pt-a) (point-x pt-b))
         (+ (point-y pt-a) (point-y pt-b))))

(defun sub-point (pt-a pt-b)
  (point (- (point-x pt-a) (point-x pt-b))
         (- (point-y pt-a) (point-y pt-b))))

;;; numbra:
;;; Interval

(in-package #:org.numbra.perso.ds)

;;;; FIXME BUG probably in with-gensym ?
;;;; Nested calls to with-interval-bounds use twice the same symbol
;;;; for "ginterval", which should not happen.

(defclass interval ()
  ((start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (includedp :initarg :includedp :reader includedp
              :documentation "A cons cell (A . B) where A, B are either T or NIL.
If A (resp. B) is T, then START (resp. END) is considered to be
included in the interval."))
  (:documentation "A simple class representing an interval, bounded by START and END."))

(defun make-interval (start end &optional (includedp '(t . t)))
  (make-instance 'interval
                 :start start
                 :end end
                 :includedp includedp))

(defmacro with-interval-bounds ((start end &optional start-included-p end-included-p)
                                interval &body body)
  (utils:with-gensyms ((ginterval interval))
    `(with-accessors ((,start start)
                      (,end end))
         ,ginterval
       (symbol-macrolet ,(let ((res nil))
                           (if start-included-p
                               (push (list start-included-p `(car (includedp ,ginterval))) res))
                           (if end-included-p
                               (push (list end-included-p `(cdr (includedp ,ginterval))) res))
                           res)
         ,@body))))

(defmethod print-object ((obj interval) stream)
  (with-interval-bounds (s e si ei) obj
    (print-unreadable-object (obj stream :type t)
      (format stream "~:[(~;[~]~A, ~A~:[)~;]~]"
              si s e ei))))


(defun interval-from-number (n)
  (make-interval n n '(t . t)))

;;; Set operations
(defun interval-contains-p (interval x)
  (with-interval-bounds (s e si ei) interval
    (and (if si (<= s x) (< s x))
         (if ei (<= x e) (< x e)))))

(defun interval-empty-p (interval)
  (with-interval-bounds (s e si ei) interval
    (or (< e s)
        (and (= e s) (not (and si ei))))))

(defun interval-equal (interval-1 interval-2)
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (and (= s1 s2)
           (= e1 e2)
           (eq si1 si2)
           (eq ei1 ei2)))))

(defun interval-cardinal (interval)
  "Returns the number of integers contained in the interval."
  (with-interval-bounds (s1 e1 si1 ei1) interval
    (let ((low (if si1
                   (ceiling s1)
                   (floor (1+ s1))))
          (high (if ei1
                    (floor e1)
                    (ceiling (1- e1)))))
      (1+ (- high low)))))

(defun interval-intersect-p (interval-1 interval-2)
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (not (or (if (and ei1 si2) (< e1 s2) (<= e1 s2))
               (if (and ei2 si1) (< e2 s1) (<= e2 s1)))))))

(defun %interval-intersection (interval-1 interval-2)
  (when (and interval-1 interval-2) ; allow to write (reduce 'interval-intersection list-of-interval)
    (with-interval-bounds (s1 e1 si1 ei1) interval-1
      (with-interval-bounds (s2 e2 si2 ei2) interval-2
        (when (interval-intersect-p interval-1 interval-2)
          (let (low-val
                low-incl
                high-val
                high-incl)
            (cond
              ((< s1 s2) (setf low-val s2
                               low-incl si2))
              ((< s2 s1) (setf low-val s1
                               low-incl si1))
              (t (setf low-val s1
                       low-incl (and si1 si2))))
            (cond
              ((< e2 e1) (setf high-val e2
                               high-incl ei2))
              ((< e1 e2) (setf high-val e1
                               high-incl ei1))
              (t (setf high-val e1
                       high-incl (and ei1 ei2))))
            (make-interval low-val high-val (cons low-incl high-incl))))))))

(defun interval-intersection (interval &rest intervals)
  (loop :with intersection = interval
        :for int :in intervals
        :while intersection
        :do (setf intersection (%interval-intersection intersection int))
        :finally (return intersection)))

(defun %interval-union (interval-1 interval-2)
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (make-interval (min s1 s2) (max e1 e2) (cons (or si1 si2) (or ei1 ei2))))))

(defun interval-union (interval &rest intervals)
  (let ((all-intervals (sort (cons interval intervals) '< :key 'start)))
    (loop :with union = (list (car all-intervals))
          :for int-1 :in (cdr all-intervals)
          :do (setf union (loop :for (int-2 . rest) :on union
                                :if (interval-intersect-p int-1 int-2)
                                  :collect (%interval-union int-1 int-2) :into new-union
                                  :and :nconc rest :into new-union
                                  :and :do (return new-union)
                                :else
                                  :collect int-2 :into new-union
                                :finally (return (cons int-1 new-union))))
          :finally (return union))))

;; To test !
(defun interval-complement (interval-1 interval-2)
  "Return a list of intervals containing exactly the set of points
corresponding to the complement of INTERVAL-1 within INTERVAL-2."
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (let ((low  (make-interval s2 s1 (cons si2 (not si1))))
            (high (make-interval e1 e2 (cons (not ei1) ei2))))
        (remove-if 'interval-empty-p (list
                                      (%interval-intersection interval-2 low)
                                      (%interval-intersection interval-2 high)))))))

;;; Arithmetic operations: add, sub ... and their destructive versions
(defmethod interval-add ((interval interval) (x real))
  (with-interval-bounds (s e si ei) interval
    (make-interval (+ s x) (+ e x) (cons si ei))))

(defmethod interval-add ((interval-1 interval) (interval-2 interval))
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (make-interval (+ s1 s2) (+ e1 e2) (cons (and si1 si2) (and ei1 ei2))))))

(defmethod interval-addf ((interval interval) (x real))
  (with-interval-bounds (s e) interval
    (setf s (+ s x)
          e (+ e x))
    interval))

(defmethod interval-addf ((interval-1 interval) (interval-2 interval))
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (setf s1 (+ s1 s2)
            e1 (+ e1 e2)
            si1 (and si1 si2)
            ei1 (and ei1 ei2))))
  interval-1)

(defmethod interval-sub ((interval interval) (x real))
  (with-interval-bounds (s e si ei) interval
    (make-interval (- s x) (- e x) (cons si ei))))

(defmethod interval-sub ((interval-1 interval) (interval-2 interval))
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (make-interval (- s1 e2) (- e1 s2) (cons (and si1 ei2) (and ei1 si2))))))


(defmethod interval-subf ((interval interval) (x real))
  (with-interval-bounds (s e) interval
    (setf s (- s x)
          e (- e x))
    interval))

(defmethod interval-subf ((interval-1 interval) (interval-2 interval))
  (with-interval-bounds (s1 e1 si1 ei1) interval-1
    (with-interval-bounds (s2 e2 si2 ei2) interval-2
      (setf s1 (- s1 e2)
            e1 (- e1 s2)
            si1 (and si1 ei2)
            ei1 (and ei1 si2))))
  interval-1)

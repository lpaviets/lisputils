(in-package #:org.numbra.perso.geometry)

(defun on-segment-p (x p q)
  "Returns T if the point X lies on the closed segment from P to Q, NIL
otherwise.

X, P and Q are all lists of length 2."
  (let ((min0 (car p))
        (max0 (car q))
        (min1 (cadr p))
        (max1 (cadr q)))
    (when (< max0 min0)
      (rotatef min0 max0))
    (when (< max1 min1)
      (rotatef min1 max1))
    (and (<= min0 (car x) max0)
         (<= min1 (cadr x) max1))))

(defun orientation (p q r)
  "Computes the relative orientation of P, Q and R, three points given as lists of
the form (X Y).

Return value:

0  if P, Q, R are colinear
1  if they are in \"clockwise order\"
-1 if they are in \"counterclockwise order\""
  (destructuring-bind (p0 p1) p
    (destructuring-bind (q0 q1) q
      (destructuring-bind (r0 r1) r
        (let* ((one (* (- q1 p1) (- r0 q0)))
               (two (* (- q0 p0) (- r1 q1))))
          (signum (- two one)))))))


(defun segment-intersect-p (p1 q1 p2 q2)
  "Return T if the segments (P1, Q1) and (P2, Q2) intersect.

All the arguments are two-dimensional points, i.e. lists of lengths 2."
  (let ((o1 (orientation p1 q1 p2))
        (o2 (orientation p1 q1 q2))
        (o3 (orientation p2 q2 p1))
        (o4 (orientation p2 q2 q1)))
    (or
     ;; General case
     (and (/= o1 o2) (/= o3 o4))
     ;; p2 on p1->q1
     (and (zerop o1) (on-segment-p p2 p1 q1))
     ;; q2 on p1->q1
     (and (zerop o2) (on-segment-p q2 p1 q1))
     ;; p1 on p2->q2
     (and (zerop o3) (on-segment-p p1 p2 q2))
     ;; q1 on p2->q2
     (and (zerop o4) (on-segment-p q1 p2 q2)))))

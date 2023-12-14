(in-package #:org.numbra.perso.algo)

(defun find-cycle-dynamical-system (start function &key (test 'eql) key (size 1000))
  "Find the least K and N such that
FUNCTION^(N+K)(START) == FUNCTION^K(START)

The equality test is made using TEST, a valid hash-table test. If KEY
is non-nil, it is a function of one argument and is applied to the
left and right-hand-side of the previous test, before comparing them
with TEST.

Returns three values:

- The period N

- The length of the pre-period K

- An array A of length at least N+K such that A[I] = FUNCTION^i(START)

SIZE is an estimate of the period beforehand. It is only used for
performance reason, and can be left unchanged if the period is
completely unknown a priori."
  (loop :with key = (or key #'identity)
        :with cache = (make-hash-table :test test :size size)
        :with step->val = (make-array size :adjustable t
                                           :fill-pointer 0
                                           :initial-element start)
        :for obj = start :then (funcall function obj)
        :for keyed-obj = (funcall key obj)
        :for i :from 0
        :for foundp = (gethash keyed-obj cache)
        :do (vector-push-extend obj step->val)
        :if foundp
          :do (return (values (- i foundp) foundp step->val))
        :do (setf (gethash keyed-obj cache) i)))

(defun iterate-dynamical-system (start function n &key (test 'eql) (size (isqrt n)))
  "Returns the N-th iterate of FUNCTION on input START.

Written differently: returns FUNCTION^N(START)

This is especially useful when computing FUNCTION is costly or N is
large, but we hope that it is periodic and that we can skip most of it
by finding the period.

SIZE is an estimate of the period a priori. It can be left unchanged."
  (multiple-value-bind (period pre-period fun-values)
      (find-cycle-dynamical-system start function :test test :size size)
    (let* ((rem (mod (- n pre-period) period))
           (result (aref fun-values (+ rem pre-period))))
      result)))

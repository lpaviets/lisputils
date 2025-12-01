;;; numbra:
;;; arithmetic

(in-package #:org.numbra.perso.utils)

(defun factorial (n)
  (assert (<= 0 n))
  (loop :with i = 1
        :for k :from 2 :to n
        :do (setf i (* i k))
        :finally (return i)))

(defun binomial (k n)
  (assert (<= 0 k))
  (assert (<= 0 n))
  (if (< n k)
      0
      (let ((res 1))
        (loop :for i :from 1 :to (min k (- n k))
              :do (setf res (* res
                               (/ (+ n 1 (- i))
                                  i))))
        res)))

(defun extended-gcd (a b)
  "Return three values G, U, V, such that:

- G = GCD(A, B)
- G = AU + BV
- G is positive")

(defun crt (mod &rest equations)
  "Compute a solution to a Chinese-Remainder problem.

EQUATIONS is a list of pairs (AI MI), where the MI are assumed to be coprimes.

 Returns the value X such that X = AI (mod MI) for all I, when it exists.")

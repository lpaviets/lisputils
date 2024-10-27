;;; numbra:
;;; Bit manipulation

;; TODO: allow the pattern to specify repetitions:
;; "10[*]{6}" is equivalent to "10******"
;; Might allow nesting to do complex matches:
;; "10[*[01]{3}]{3}" would be "10[*010101]{3}" and so "10*010101*010101*010101"
;; This allows simpler writing for matching some specific part of a long integer.
;;
;; Need to implement a function which does the string replacement.

(in-package #:org.numbra.perso.bits)

(defun validate-pattern (pattern)
  (handler-case (string pattern)
    (simple-type-error () nil)
    (:no-error (spattern)
      (loop :for c :across spattern
            :always (find c "_01*()[]{}")))))

(defun normalize-pattern (pattern)
  (assert (validate-pattern pattern) nil
          "Invalid pattern ~A: should be a string designator containing 0/1/* only"
          pattern)
  (setf pattern (remove #\_ (string pattern)))
  (let ((parens (get-parenthesis pattern)))
    (values (remove-if (lambda (c) (find c "()")) pattern)
            parens)))

(defun compute-mask (pattern)
  (let* ((rev-pattern (reverse pattern))
         (mask 0)
         (val 0))
    (dotimes (i (length rev-pattern))
      (let ((bit (digit-char-p (char rev-pattern i))))
        (setf (logbitp i mask) bit
              (logbitp i val) (eql bit 1))))
    (values mask val)))

(defun get-parenthesis (pattern)
  "Returns a list of cons cells (START . END), where START is the position of the
first parenthesis, and END the corresponding closing parenthesis.

>>> (get-parenthesis \"ab(c((de)f))g(h))\")
((2 . 6) (3 . 5) (3 . 6) (7 . 8))"
  (let ((start-parens ())
        (res ()))
    (loop :with cur = 0
          :for c :across pattern
          :if (char= #\( c)
            :do (push cur start-parens)
          :else
            :if (char= #\) c)
              :do (push (cons (pop start-parens) cur) res)
          :else
            :do (incf cur))
    (assert (null start-parens) () "Pattern ~A is not balanced" pattern)
    (stable-sort res '< :key 'car)))

(declaim (ftype (function (integer (or string symbol character) &key (:msb t)) (or t nil)) match-bits-p))
(defun match-bits-p (n pattern &key (msb nil))
  "N should be an integer, and PATTERN a string-designator containing only
0, 1, and * characters. Underscores _ and parenthesis can additionally be used
but have no meaning.

Returns T if for evey non-* character B in PATTERN in position I, the i-th bit
of N is equal to B.

The bits of PATTERN are always matched against the least significant bits of N,
unless MSB is a non-nil value:

>>> (match-bits-p #b10010101 :1**1**01)
T

>>> (match-bits-p #b10010101 :1**0**01)
NIL

>>> (match-bits-p #b10010101 :01)
T

>>> (match-bits-p #b10010101 :1*01 :msb t)
T

>>> (match-bits-p #b10010101 :1*00 :msb t)
NIL"
  (let ((spattern (normalize-pattern pattern)))
    (multiple-value-bind (mask val) (compute-mask spattern)
      (when msb
        (let ((size-mask (length spattern))
              (size-n (integer-length n)))
          (setf mask (ash mask (- size-n size-mask))
                val (ash val (- size-n size-mask)))))
      (let ((mask-n (logand mask n)))
        (zerop (logxor mask-n val))))))

(defun compute-matches-from-parenthesis (n pattern parens &key msb)
  (let ((len (length pattern))
        (n-len (integer-length n)))
    (loop :for (start . end) :in parens
          :for size = (- end start)
          :for pos = (if msb (- n-len end) (- len end))
          :for byte = (byte size pos)
          :collect (ldb byte n))))

;; Maybe perform more at comptime ? Already a few things done when possible: if
;; PATTERN is a string/symbol, we compute the mask/template/parenthesizing at
;; compile time
(defmacro with-match-bits ((&rest vars) (pattern n &key msb) &body body)
  (utils:with-gensyms ((gn n) (gmsb msb) gpattern parens)
    `(,@(if (validate-pattern pattern)
            `(destructuring-bind (,gpattern ,parens) ',(multiple-value-list (normalize-pattern pattern)))
            `(multiple-value-bind (,gpattern ,parens) (normalize-pattern ,pattern)))
      (when (match-bits-p ,gn ,gpattern :msb ,gmsb)
        (destructuring-bind ,vars
            (compute-matches-from-parenthesis ,gn ,gpattern ,parens :msb ,gmsb)
          ,@body)))))

(type-i:define-inference-rule match-bits (test)
  (trivia:match test
    ((list* 'match-bits-p _)
     `((integerp type-i:?)))))

(trivia:defpattern bits (bitpattern &rest subpatterns)
                   "Match subpatterns to corresponding bit fields of the value being matched.
BITPATTERN can be a simple pattern, or a compound form of the form
(PATTERN :MSB MSB)"
  (destructuring-bind (pattern &key msb) (utils:ensure-list bitpattern)
    (multiple-value-bind (pattern parens) (normalize-pattern pattern)
      (let ((it (gensym "IT")))
        `(trivia:guard1 (,it :type integer)
                        (match-bits-p ,it
                                      ,pattern
                                      :msb ,msb)
                        (apply 'vector
                               (compute-matches-from-parenthesis ,it
                                                                 ,pattern
                                                                 ',parens
                                                                 :msb ,msb))
                        (trivia:simple-vector* ,@subpatterns))))))

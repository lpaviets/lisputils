;;; numbra:
;;; Subsets

(in-package #:org.numbra.perso.utils)

(defun %number-to-subset-string (n seq)
  (let ((res (make-string (logcount n))))
    (loop :with j = 0
          :for i :below (integer-length n)
          :if (logbitp i n)
            :do (setf (char res j) (char seq i))
                (incf j))
    res))

(defun %number-to-subset-vector (n seq)
  (let ((res (make-array (logcount n))))
    (loop :with j = 0
          :for i :below (integer-length n)
          :if (logbitp i n)
            :do (setf (aref res j) (aref seq i))
                (incf j))
    res))

(defun %number-to-subset-list (n seq)
  (do* ((i 0 (1+ i))
        (list seq (cdr list))
        (x (car seq) (car list))
        (res nil))
       ((endp list) (nreverse res))
    (when (logbitp i n)
      (push x res))))


(defun number-to-subset-sequence (n seq)
  (etypecase seq
    (list (%number-to-subset-list n seq))
    (string (%number-to-subset-string n seq))
    (vector (%number-to-subset-vector n seq))))

(defun number-to-subset-number (n-subset n-source)
  (let ((res 0))
    (loop :for i :below (integer-length n-source)
          :for bitp = (logbitp i n-subset)
          :if bitp
            :do (setf res (+ (if (logbitp i n-source) 1 0)
                             (ash res 1))))
    res))


(defmacro do-subsets ((x (k n &optional result) &optional bits-to-subset-fun) &body body)
  "Iterate over subsets of size K of a set of N elements.

In BODY, X is bound to an integer, whose non-zero bits correspond to
the chosen K elements of the set of size N.

BITS-TO-SUBSET-FUN should be a function of one argument. If non-NIL, X
is bound to the result of this called on the previously-explained
number instead.

Uses Gosper's hack internally. See e.g.
https://stackoverflow.com/questions/15932237/iterating-over-all-subsets-of-a-given-size"
  (with-gensyms ((bts-fun bits-to-subset-fun)
                 (gk k)
                 (gn n)
                 a b c max)
    `(progn
       (assert (<= ,gk ,gn))
       (let ((,c (1- (ash 1 ,gk)))
             (,max (ash 1 ,gn)))
         (loop :while (< ,c ,max)
               :for ,a = (logand ,c (- ,c))
               :for ,b = (+ ,c ,a)
               :for ,x = (if ,bts-fun
                             (funcall ,bts-fun ,c)
                             ,c)
               :do (progn ,@body)
                   (setf ,c (logior ,b
                                    (truncate (logxor ,c ,b)
                                              (* 4 ,a)))))
         ,result))))

(defmacro do-sequence-subsets ((x (k sequence &optional result)) &body body)
  "Simple wrapper around `DO-SUBSETS', which see.

SEQUENCE is the set from which we take subsets."

  (with-gensyms ((gseq sequence) len n fun)
    `(let ((,len (length ,gseq))
           (,fun (lambda (,n)
                   (number-to-subset-sequence ,n ,gseq))))
       (do-subsets (,x (,k ,len ,result) ,fun)
         ,@body))))

;;; WIP: generate perfect hash-tables
(defun pad (sequence length)
  (let ((type (etypecase sequence
                (string 'string)
                (vector 'vector)
                (list 'list))))
    (concatenate type sequence (make-sequence type (- length (length sequence))))))

(defun all-distinct-p (list)
  (let ((table (make-hash-table :test 'equalp))
        (result '()))
    (dolist (x list)
      (incf (gethash x table 0)))
    (maphash #'(lambda (key count)
                 (unless (eql 1 count)
                   (push (cons key count) result)))
             table)
    (not result)))

(defun find-smallest-subset-all-distinct (set &optional (number-to-subset 'number-to-subset-sequence))
  "SET is a list of elements (typically, sequences or integers).

NUMBER-TO-SUBSET is a function of two elements, an integer denoting a
subset, and the element from which to take this subset.

Returns 2 values:

- An integer denoting a subset - the non-zeros bits are elements of
  the subset.

- A list of the corresponding subsets for all the elements of SET.

Note that the elements are all padded. It is then possible that some
elements of the subsets are obtained from the padded part."
  (let* ((max-len (reduce 'max set :key 'length))
         (padded-set (mapcar (lambda (x) (pad x max-len)) set)))
    (loop :for k :from 1 :to max-len
          :do (do-subsets (x (k max-len))
                (let ((x-subsets (mapcar (lambda (elt)
                                           (number-to-subset-sequence x elt))
                                         padded-set)))
                  (when (all-distinct-p x-subsets)
                    (return-from find-smallest-subset-all-distinct
                      (values x
                              x-subsets))))))))

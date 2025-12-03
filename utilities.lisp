;;; numbra:
;;; Utilities

(in-package #:org.numbra.perso.utils)

(defun gethash-rec (table &rest keys)
  (flet ((next (table key)
           (multiple-value-bind (next-table foundp)
               (gethash key table)
             (if foundp
                 next-table
                 (return-from gethash-rec nil)))))
    (reduce #'next keys :initial-value table)))

;; (defun (setf gethash-rec) (val table &rest keys)
;;   (flet ((next (table key)
;;            (multiple-value-bind (next-table foundp)
;;                (gethash key table)
;;              (if foundp
;;                  next-table
;;                  (return-from gethash-rec nil)))))
;;     (reduce #'next keys :initial-value table)))

(defun flip (x y &optional (comp 'equal) (keep-others t))
  "Return a function of one argument which swaps X and Y
If KEEP-OTHERS is non-NIL, this function acts like `identity' on other elements
Otherwise, elements that are not X nor Y are mapped to Y"
  (lambda (z)
    (cond
      ((funcall comp x z) y)
      ((funcall comp y z) x)
      (keep-others z)
      (t y))))

(defun range (m &optional n (step 1))
  (assert (not (zerop step)) () "STEP cannot be 0")
  (let ((start (if n m 0))
        (end (if n n m))
        (test (if (plusp step) #'>= #'<=)))
    (loop :for i = start :then (+ i step)
          :until (funcall test i end)
          :collect i)))

(defun lazy-range (m &optional n (step 1))
  (assert (not (zerop step)) () "STEP cannot be 0")
  (let ((i (if n m 0))
        (end (or n m))
        (test (if (plusp step) #'>= #'<=)))
    (lambda (default)
      (if (funcall test i end)
          default
          (prog1 i (incf i step))))))

(defun cumulative (function sequence &rest args)
  (let (running)
    (flet ((wrapper (x y)
             (let ((res (funcall function x y)))
               (push res running)
               res)))
      (apply 'reduce #'wrapper sequence args)
      (nreverse running))))

(defun horner (coefficients x)
  (reduce (lambda (res coeff)
            (+ (* res x) coeff))
          coefficients))

(defun concat-numbers (numbers &key (same-length-p nil))
  (if same-length-p
      (horner numbers (expt 10 (integer-length-base (car numbers))))
      (loop :for num :in numbers
            :for pow = 1 :then (expt 10 (integer-length-base num))
            :for res = num :then (+ (* res pow) num)
            :finally (return res))))

(defun manhattan-distance (x y)
  "X and Y are lists, representing positions."
  (loop :for i :in x
        :for j :in y
        :sum (abs (- i j))))

(defun permutations (n)
  "List of all the permutations of the integers between 0 and n included"
  (if (zerop n)
      (list '(0))
      (loop :with perms = (permutations (1- n))
            :for perm :in perms
            :append
            (loop :repeat (1+ n)
                  :for (beg end) = (list nil perm)
                    :then (list (cons (car end) beg)
                                (cdr end))
                  :collect (append beg (list n) end)))))

(defun integer-length-base (integer &optional (base 10))
  (setf integer (abs integer))
  (loop :for n = 1 :then (* n base)
        :while (<= n integer)
        :count 1))

(defun sublists-length (list n)
  "List of all the sublists of LIST of length N"
  (cond
    ((= 1 n) (mapcar 'list list))
    ((null list) nil)
    (t
     (append (sublists-length (cdr list) n)
             (mapcar (lambda (x)
                       (cons (car list) x))
                     (sublists-length (cdr list) (1- n)))))))

(defgeneric argmax (sequence &key test start end key exclude-null)
  (:documentation "Returns OBJ maximizing TEST in SEQUENCE between START and END.
If END is NIL, search it until the end of the sequence.
If KEY is non NIL, applies it to each element instead of using them
directly.
If EXCLUDE-NULL is non-NIL, the null values (either themselves, of after
being applied KEY) are ignored.
Additionally, the position of the element is returned as a secondary value,
as a list, as SEQUENCE might be a multi-dimensional object, and its value
for KEY as a third value.
If SEQUENCE is empty, return NIL for all three values"))

(defmethod argmax ((sequence list) &key (test #'<) (start 0) end key exclude-null)
  (setf sequence (nthcdr start sequence))
  (cond
    ((and sequence
          (or (not end)
              (> end start)))
     (loop :with max-idx = start
           :with max-elt = (first sequence)
           :with max-val = (if key
                               (funcall key max-elt)
                               max-elt)
           :for elt :in (cdr sequence)
           :for idx :from (1+ start)
           :while (or (not end) (< idx end))
           :for val = (if key (funcall key elt) elt)
           :when (cond
                   ((not exclude-null) (funcall test max-val val))
                   ((and exclude-null val (not max-val)) t)
                   ((and exclude-null val max-val) (funcall test max-val val))
                   (t nil))
             :do (setf max-elt elt
                       max-val val
                       max-idx idx)
           :finally
              (return (values max-elt (list max-idx) max-val))))
    (t (values nil nil nil))))

(defmethod argmax ((sequence vector) &key (test #'<) (start 0) end key exclude-null)
  (let ((length (length sequence)))
    (cond
      ((and (< start length)
            (or (not end)
                (> end start)))
       (loop :with max-idx = start
             :with max-elt = (aref sequence start)
             :with max-val = (if key
                                 (funcall key max-elt)
                                 max-elt)
             :for idx :from (1+ start)
             :while (and (< idx length)
                         (or (not end)
                             (< idx end)))
             :for elt = (aref sequence idx)
             :for val = (if key (funcall key elt) elt)
             :when (cond
                     ((not exclude-null) (funcall test max-val val))
                     ((and exclude-null val (not max-val)) t)
                     ((and exclude-null val max-val) (funcall test max-val val))
                     (t nil))
               :do (setf max-elt elt
                         max-val val
                         max-idx idx)
             :finally
                (return (values max-elt (list max-idx) max-val))))
      (t (values nil nil nil)))))

(defmethod argmax ((sequence hash-table) &key (test #'<) start end key exclude-null)
  (declare (ignore start end))
  (loop :for x :being :the :hash-keys :of sequence
        :for val = (if key (funcall key x) x)
        :for replace-max-p = nil
          :then (cond
                  ((not exclude-null) (funcall test max-val val))
                  ((and exclude-null val (not max-val)) t)
                  ((and exclude-null val max-val) (funcall test max-val val))
                  (t nil))
        :for max-val = val :then (if replace-max-p val max-val)
        :for max-elt = x :then (if replace-max-p x max-elt)
        :finally (return (values max-elt nil max-val))))

;; From Serapeum
(defun array-index-row-major (array row-major-index)
  "The inverse of ARRAY-ROW-MAJOR-INDEX.

Given an array and a row-major index, return a list of subscripts.

     (apply #'aref (array-index-row-major i))
     ≡ (array-row-major-aref i)"
  (labels ((rec (subs dims)
             (if (null dims) subs
                 (multiple-value-bind (q r)
                     (truncate (car subs)
                               (the (integer 0 #.array-dimension-limit)
                                    (car dims)))
                   (rec (cons q (rplaca subs r))
                        (cdr dims))))))
    (rec (list row-major-index) (reverse (rest (array-dimensions array))))))

(defmethod argmax ((sequence array) &key (test #'<) (start 0) end key exclude-null)
  (assert (and (or (null start) (zerop start))
               (null end))
          ()
          "ARGMAX cannot be called on a subarray of a multimensional array~@
START has to be NIL or 0 instead of ~S~@
and END has to be NIL instead of ~S~%"
          start end)
  (loop :with length = (array-total-size sequence)
        :with max-idx = 0
        :with max-elt = (row-major-aref sequence 0)
        :with max-val = (if key
                            (funcall key max-elt)
                            max-elt)
        :for idx :from 1
        :while (< idx length)
        :for elt = (row-major-aref sequence idx)
        :for val = (if key (funcall key elt) elt)
        :when (cond
                ((not exclude-null) (funcall test max-val val))
                ((and exclude-null val (not max-val)) t)
                ((and exclude-null val max-val) (funcall test max-val val))
                (t nil))
          :do (setf max-elt elt
                    max-val val
                    max-idx idx)
        :finally
           (return (values max-elt
                           (array-index-row-major sequence max-idx)
                           max-val))))

(defun argmin (sequence &key (test #'<) (start 0) end key exclude-null)
  "Returns OBJ minimizing TEST in SEQUENCE between START and END.
For an explanation of the parameters and return values, see `argmax'.
The function works by calling `argmax' with [argmax]TEST bound to
(LAMBDA (X Y) (FUNCALL [argmin]TEST Y X)."
  (argmax sequence
          :start start
          :end end
          :key key
          :test (lambda (x y) (funcall test y x))
          :exclude-null exclude-null))

(defgeneric deepcopy (thing)
  (:documentation "Recursively creates a copy of THING.")
  (:method ((thing t)) thing))

(defmethod deepcopy ((thing list))
  (mapcar #'deepcopy thing))

(defmethod deepcopy ((thing array))
  (loop :with new = (make-array (array-dimensions thing)
                                :element-type (array-element-type thing)
                                :adjustable (adjustable-array-p thing)
                                :fill-pointer (and (array-has-fill-pointer-p thing)
                                                   (fill-pointer thing)))
        :for i :below (array-total-size thing) :do
          (setf (row-major-aref new i)
                (deepcopy (row-major-aref thing i)))
        :finally (return new)))

(defmethod deepcopy ((thing hash-table))
  (let ((table (make-hash-table :size (hash-table-size thing)
                                :test (hash-table-test thing))))
    (maphash (lambda (k v)
               (setf (gethash (deepcopy k) table)
                     (deepcopy v)))
             thing)
    table))

(defun %shuffle (list len acc)
  (declare (type list list acc)
           (type fixnum len)
           (optimize (speed 3)))
  (if (endp list)
      acc
      (let* ((rand (random (+ len 1)))
             (new-acc (append (butlast acc rand)
                              (list (car list))
                              (last acc rand))))
        (%shuffle (cdr list) (+ len 1) new-acc))))

(defun shuffle (list)
  (%shuffle list 0 nil))

(defun ensure-list (obj)
  (if (listp obj) obj (list obj)))

(defun substitute-assoc (alist sequence &key (test #'eql) key (start 0) end)
  (flet ((subs (seq old-new)
           (substitute (cdr old-new) (car old-new) seq
                       :test test
                       :key key
                       :start start
                       :end end)))
    (reduce #'subs alist
            :initial-value sequence)))

(defun lexicographic< (seq1 seq2 &optional (predicate #'<))
  (let ((seq1 (coerce seq1 'list))
        (seq2 (coerce seq2 'list)))
    (loop :for (a . rest1) :on seq1
          :for (b . rest2) :on seq2
          :when (funcall predicate b a)
            :do (return nil)
          :when (funcall predicate a b)
            :do (return t)
          :finally (return (if rest2 t nil)))))


(defun group-by (sorted-list &key key (test 'eql))
  "Group the elements of SORTED-LIST.

Returns a list of lists. Each sublist contains the largest subsequence
of SORTED-LIST for which all elements are equal under TEST.

If KEY is non-NIL, it is called on each element before comparing with
TEST.

The relative order of the elements is preserved. In particular, we have

SORTED-LIST == (reduce 'append (group-by SORTED-LIST))

under EQUALP, regardless of KEY and TEST."
  (loop :with acc = (list (car sorted-list))
        :for (p1 p2) :on sorted-list
        :for e1 = (if key (funcall key p1) p1)
        :for e2 = (when p2 (if key (funcall key p2) p2))
        :while p2
        :if (funcall test e1 e2)
          :do (push p2 acc)
        :else
          :collect (nreverse acc)
          :and :do (setf acc (list p2))
        :unless p2
          :collect acc))

;;; Parenthesis and matching pairs
(defparameter *parenthesis-pairs* '((#\( . #\))
                                    (#\[ . #\])
                                    (#\{ . #\})))

(defun get-closing-parens (x &optional (pairs *parenthesis-pairs*))
  (cdr (assoc x pairs)))

(defun get-opening-parens (x &optional (pairs *parenthesis-pairs*))
  (car (rassoc x pairs)))

(defun is-opening-parens-p (x &optional (pairs *parenthesis-pairs*) )
  (member x pairs :key 'car :test 'char=))

(defun is-closing-parens-p (x &optional (pairs *parenthesis-pairs*) )
  (member x pairs :key 'cdr :test 'char=))

(defun compute-parenthesis (pattern &optional (pairs *parenthesis-pairs*))
  "Returns a list of lists of the form (TYPE (START . END) (S . E)), where:

- TYPE is the type of opening parenthesis of the pair

- START is the position of the first parenthesis in the fully unparenthesized
  word

- END the position of the corresponding closing parenthesis

- S and E are the position of the same pair, but in the initial PATTERN, with
  parenthesis still included.

>>> (compute-parenthesis \"ab[c((de)f)]g{h}\")
((#\[ (2 . 6) (2 . 11))
 (#\( (3 . 6) (4 . 10))
 (#\( (3 . 5) (5 . 8))
 (#\{ (7 . 8) (13 . 15)))

PAIRS is a list of cons cells (OP . CL) where OP and CL are characters
corresponding to an opening and a respectively closing parenthesis. By default,
it contains the pairs (), [], {} but any character pair can be included."
  (let ((start-parens ())
        (res ()))
    (loop :with pos-clean = 0
          :for pos :from 0
          :for c :across pattern
          :for (last spos-clean spos) = (car start-parens)
          :for closingp = (is-closing-parens-p c pairs)
          :do (cond
                ((is-opening-parens-p c pairs)
                 (push (list c pos-clean pos) start-parens))
                ((and closingp (not (characterp last)))
                 (error "Pattern ~A is not balanced: ~C in position ~A has no opening parenthesis"
                        pattern c pos))
                (closingp
                 (if (char= (get-opening-parens c pairs) last)
                     (progn
                       (push (list last (cons spos-clean pos-clean) (cons spos pos))
                             res)
                       (pop start-parens))
                     (error "Pattern ~A is not balanced: ~C in position ~A closes a ~C"
                            pattern c pos last)))
                (t (incf pos-clean))))
    (assert (null start-parens)
            ()
            "Pattern ~A is not balanced: some opening parenthesis were not closed"
            pattern)
    (stable-sort res '< :key 'caadr)))

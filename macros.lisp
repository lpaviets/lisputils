;;; numbra:
;;; Macros

(in-package #:org.numbra.perso.utils)

(defmacro with-gensyms (gensyms &body body)
  "GENSYMS is a list of symbols SYM, or of pairs (SYM VAL).
Binds the symbols SYM to an uninterned symbol, as if using gensym.
For each pair (SYM VAL), the value of VAL will be evaluated once before
BODY, and bound to the gensym associated to SYM.
Example:
(defmacro double (x)
  (with-gensyms ((gx x))
    `(+ ,gx ,gx)))"
  (loop :for sym :in gensyms
        :if (symbolp sym)
          :collect `(,sym (gensym ,(symbol-name sym))) :into gensym-list
        :else
          :collect `(,(first sym) (gensym ,(symbol-name (first sym))))
            :into gensym-list
            :and
              :collect `(list ,(first sym) ,(second sym)) :into eval-list
        :finally
           (return `(let ,gensym-list
                      `(let (,,@eval-list)
                         ,,@body)))))

(defmacro do-array ((i j x array &optional return) &body body)
  "Iterate over a 2D array.
In the BODY:
I, J are respectively bound to the first and second coordinate at each step
X is bound the array[i][j] := (aref array i j)"
  (with-gensyms ((garray array))
    `(progn
       (loop :for ,i :below (array-dimension ,garray 0)
             :do
                (loop :for ,j :below (array-dimension ,garray 1)
                      :for ,x = (aref ,garray ,i ,j)
                      :do ,@body))
       ,return)))

(defmacro do-line ((i j &key (step 1)) (x1 y1) (x2 y2) &body body)
  "Iterate over a line, given as a pair of coordinates. Both extremal
points are *included* in the loop.

In BODY, I and J are bound to the horizontal and vertical coordinate
of each step respectively. They are incremented by STEP at each step,
in the right direction.

Some caveats:

- Only works if the line is \"straight\" or diagonal. Otherwise, loop
  infinitely.

- Loop infinitely if the STEP does not 'divide' the distance between
the starting point and the ending point."
  (with-gensyms ((gx1 x1)
                 (gx2 x2)
                 (gy1 y1)
                 (gy2 y2)
                 (gstep step)
                 inc-x inc-y)
    `(let* ((,inc-x (signum (* ,gstep (- ,gx2 ,gx1))))
            (,inc-y (signum (* ,gstep (- ,gy2 ,gy1)))))
       (do ((,i ,gx1 (+ ,i ,inc-x))
            (,j ,gy1 (+ ,j ,inc-y)))
           ((and (= ,i ,gx2)
                 (= ,j ,gy2)
                 (progn
                   ,@body)))
         ,@body))))

(defmacro do-line* ((x start end &key (step 1)) &body body)
  "Iterate over the multi-dimensional line between START and END (included).

X is bound to the successive positions, as a list. X is destructively
modified at each step, and you should not modify it.

This assumes that there is only one difference between START and END.

STEP will be the amount by which we increase X in that direction. It can
be given as an absolute value, the sign will be automatically changed
depending on the values of START and END at runtime."
  ;; Could simplify implementation using MISMATCH/NTHCDR, at the cost
  ;; of a minor performance slowdown.
  (with-gensyms (y rest rest-x end-val delta
                   (gstep step) (gstart start) (gend end))
    `(progn
       (assert (= (length ,gstart) (length ,gend))
               nil
               "~A and ~A are not of the same length." ,gstart ,gend)
       (let* ((,x (copy-seq ,gstart)))
         (multiple-value-bind (,rest ,end-val ,delta)
             (do ((,rest-x ,x   (cdr ,rest-x))
                  (,y      ,gend (cdr ,y)))
                 ((or (endp ,rest-x)
                      (/= (car ,rest-x) (car ,y)))
                  (values ,rest-x
                          (car ,y)
                          (if ,rest-x
                              (signum (- (car ,y) (car ,rest-x)))
                              0))))
           (setf ,gstep (* ,gstep ,delta))
           (if ,rest
               (do ()
                   ((if (= ,delta 1)
                        (< ,end-val (car ,rest))
                        (< (car ,rest) ,end-val)))
                 ,@body
                 (incf (car ,rest) ,gstep))
               ,@body))))))

(defmacro do-hash ((x v hash-table) &body body)
  `(block nil
     (maphash (lambda (,x ,v)
                ,@body)
              ,hash-table)))

(defmacro do-hashkeys ((x hash-table) &body body)
  (let ((val (gensym)))
    `(block nil
       (maphash (lambda (,x ,val)
                  (declare (ignore ,val))
                  ,@body)
                ,hash-table))))

(defmacro do-hashvalues ((x hash-table) &body body)
  (let ((key (gensym)))
    `(block nil
       (maphash (lambda (,key ,x)
                  (declare (ignore ,key))
                  ,@body)
                ,hash-table))))

(defmacro dotimes-product (((variable &rest range) &rest vars-ranges) &body body)
  (destructuring-bind (start end step)
      (ecase (length range)
        (1 (list 0 (first range) 1))
        (2 (list (first range) (second range) 1))
        (3 range))
    (with-gensyms ((gstart start) (gend end) (gstep step))
      `(loop :for ,variable :from ,gstart :below ,gend :by ,gstep
             :do ,(if vars-ranges
                      `(dotimes-product (,(first vars-ranges)
                                         ,@(rest vars-ranges))
                         ,@body)
                      (cons 'progn body))))))

;; TODO: difference between two possible behaviours:
;; - Make a cache per "toplevel function call": useful to compute e.g. Fibonacci
;; - Make a shared cache for *all* toplevel function calls
;; (defmacro defun-memoized (name (&key cache (test 'eql)) lambda-list &body body)
;;   (with-gensyms (memo (name-cached name))
;;     `(defun ,name ,lambda-list
;;        (let ((,memo (make-hash-table :test ,test)))
;;          (labels ((,name-cached ))
;;            (,name-cached ))))))

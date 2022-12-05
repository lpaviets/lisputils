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
        :for gensym = (gensym (if (symbolp sym)
                                  (symbol-name sym)
                                  (symbol-name (first sym))))
        :if (symbolp sym)
          :collect `(,sym ',gensym) :into gensym-list
        :else
          :collect `(,(first sym) ',gensym) :into gensym-list
          :and
            :collect `(list ,(first sym) ,(second sym)) :into eval-list
        :finally
           (return `(let ,gensym-list
                      (list 'let (list ,@eval-list)
                            ,@body)))))

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
  "Iterate over a line, given as a pair of coordinates
In BODY, I and J are bound to the horizontal and vertical coordinate of each
step respectively. They are incremented by STEP at each step, in the right
direction.
Some caveats:
- Only works if the line is \"straight\" or diagonal. Otherwise, loop infinitely.
- Loop infinitely if the STEP does not 'divide' the distance between the starting
point and the ending point."
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

;;; numbra:
;;; Macros

(in-package #:numbra)

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

;;; Whereas macro
;;; From Erik Naggum, briefly described here:
;;; http://groups.google.nl/group/comp.lang.lisp/msg/bc7772aa5ab1f3e4
;;; The idea is to successively and conditionnally bind variables
;;; Further bindings (and thus, the body) are processed if and only if the
;;; previous values are non-NIL
;;; If the case of an expression returning several values (e.g. gethash),
;;; all the returned values can be bound in the same expression. In that case,
;;; if the (optional) type declarations are of the form (or null ...) for
;;; the first bound variable, the condition will be on the next value instead
;;; of this one. This rule also applies, by induction, when there are three
;;; or more returned values.
;;;
;;;Example: (assuming table is hash-table)
;;; (whereas (((x presentp) (gethash 'hello table))
;;;           (y (+ (or x 0) 5) integer))
;;;        (print y))
;;;
;;; will bind (and therefore, print) y only if x is non-nil, that is, only
;;; if 'hello is a key AND (gethash 'hello table) is non-nil.
;;; On the other hand:
;;;
;;; (whereas (((x presentp) (gethash 'hello table) ((or null integer)))
;;;           (y (+ (or x 0) 5) integer))
;;;       (print y))
;;;
;;; will bind y if 'hello is a valid key for table, REGARDLESS of whether
;;; (gethash 'hello table) is NIL or not.

(defun whereas-test-variable (vars decls)
  (cond
    ((symbolp vars) vars)
    ((and (listp vars) (endp (cdr vars)))
     (error "Variables must be symbols or list of length at least 2: ~s" vars))
    (t
     (progn
       (assert (listp decls)
               (decls)
               (format nil "Type declarations ~s must be a list to match the variable list ~s"
                       decls vars))
       (loop :for (decl . rest-decls) :on decls
             :for (var . rest-vars) :on vars
             :while (and (< 2 (list-length decl))
                         (eq (first decl) 'or)
                         (eq (second decl) 'null))
             :finally (return (if rest-vars
                                  (car rest-vars)
                                  t)))))))

(defun whereas-parse-declarations (vars decls)
  (if (symbolp vars)
      (list (list 'type decls vars))
      (when decls
        (assert (<= (list-length decls) (list-length vars))
                ()
                (format nil "Not enough variables ~s to match the type declarations ~s~%"
                        vars decls))
        (loop :for var :in vars
              :for decl :in decls
              :collect `(type ,decl ,var)))))

(defun whereas-parse (bindings body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (vars values &optional decls) (car bindings)
        (let* ((symbol-var-p (symbolp vars))
               (binder (if symbol-var-p
                           `(let ((,vars ,values)))
                           `(multiple-value-bind ,vars ,values)))
               (test-var (whereas-test-variable vars decls))
               (next-bindings `(when ,test-var
                                 ,(whereas-parse (cdr bindings) body)))
               (declarations (whereas-parse-declarations vars decls)))
          (append binder
                  (list (if decls
                            `(locally (declare ,@declarations)
                               ,next-bindings)
                            next-bindings)))))))

(defmacro whereas (bindings &body body)
  "A version of Erik Naggum's idea in
http://groups.google.nl/group/comp.lang.lisp/msg/bc7772aa5ab1f3e4"
  (whereas-parse bindings body))

(in-package #:org.numbra.perso.utils)

(defmacro for ((var-or-vars sequence &rest args) &body body)
  "Iterate through SEQUENCE, binding VAR-OR-VARS to its successive
elements. ARGS can be used to give extra instructions on how to iterate.

If VAR-OR-VARS is a symbol, it is bound as if by LET to those
elements.

If it is a (possibly nested) list, VAR-OR-VARS is bound as if by
DESTRUCTURING-BIND.

BODY is then executed at each step, with VAR-OR-VARS bound as
described.

Within BODY, the keyword BREAK stops the execution of the loop, and
the keyword CONTINUE stops this iteration and continues with the next
one. Note that BREAK and CONTINUE are to be used as /variables/, not
as functions:

(for (x '(a b c d e))
  (case x
    (b continue)
    (d break))
  (print x))

  will print

a
c

FOR constructs can be nested. In that case, CONTINUE and BREAK will
always refer to the innermost construct."
  (with-gensyms (val next-p iterable cleanup block-name loop-name)
    `(symbol-macrolet ((break (return-from ,loop-name))
                       (continue (return-from ,block-name)))
       (multiple-value-bind (,iterable ,cleanup)
           (make-iterable ,sequence ,@args)
         (unwind-protect
              (loop :named ,loop-name
                    :do (block ,block-name
                          (multiple-value-bind (,val ,next-p)
                              (funcall ,iterable)
                            (unless ,next-p break)
                            ,(if (symbolp var-or-vars)
                                 `(let ((,var-or-vars ,val))
                                    ,@body)
                                 `(destructuring-bind ,var-or-vars
                                      ,val
                                    ,@body)))))
           (when ,cleanup
             (funcall ,cleanup)))))))

(defgeneric make-iterable (obj &rest args)
  (:documentation "Return an ITERABLE object, that is, a function that can be
repeatedly called to return the elements of OBJ. Additionnally, it may return a
secondary value, which is a function to be called at the end of the iteration.
It is used to \"cleanup\" in case the iteration stops abnormally, e.g. by a
`return-from' or raises an exception. This can be used to close file descriptors
or perform similar cleanup and safety operations.


If appropriate, any object that can be FUNCALLed can be returned by a method
specializing on MAKE-ITERABLE instead.

This function should take no argument, and return two values, as GETHASH:
- the first value is the (possible) element obtained at the current step, and
- the second value is T or NIL; it is NIL if and only if we are at the end of
the iterable.

The effects are undefined if OBJ is destructively modified by the calls to
ITERABLE"))

(defmethod make-iterable ((list list) &rest args)
  (declare (ignore args))
  (let ((cache list))
    (lambda ()
      (if (endp cache)
          (values nil nil)
          (values (pop cache) t)))))

(defmethod make-iterable ((vec vector) &rest args)
  (declare (ignore args))
  (let ((length (length vec))
        (index 0))
    (lambda ()
      (if (>= index length)
          (values nil nil)
          (values (aref vec (1- (incf index))) t)))))

(defmethod make-iterable ((array array) &rest args)
  (declare (ignore args))
  (let ((size (array-total-size array))
        (index 0))
    (lambda ()
      (if (>= index size)
          (values nil nil)
          (values (row-major-aref array (1- (incf index))) t)))))

(defmethod make-iterable ((hash hash-table) &rest args)
  (declare (ignore args))
  (with-hash-table-iterator (next-entry hash)
    (lambda ()
      (multiple-value-bind (more key value) (next-entry)
        (values (list key value) more)))))

(defmethod make-iterable ((stream stream) &rest args)
  (let ((read-fun (ecase (getf args :type)
                    ((:line nil) 'read-line)
                    ((:byte) 'read-byte)
                    ((:char) 'read-char)
                    ((:lisp) 'read)))
        (sym (gensym)))
    (lambda ()
      (let ((line (funcall read-fun stream nil sym)))
        (if (eq line sym)
            (values nil nil)
            (values line t))))))

(defmethod make-iterable ((path pathname) &rest args)
  (let* ((args (copy-list args))
         (type (getf args :type)))
    (remf args :type)
    (let ((file (apply 'open path args)))
      (values
       (make-iterable file :type type)  ; this is now a stream
       (lambda ()
         (close file))))))

;;; FUN is a function of ONE argument, DEFAULT. It returns either some 'new'
;;; elements, or DEFAULT is it reaches the 'end'
;;; This method is used to cast those functions into the MAKE-ITERABLE/FOR
;;; framework.
;;; It is useful to deal with functions that are already able to keep cache,
;;; state ... by themselves.
(defmethod make-iterable ((fun function) &rest args)
  (declare (ignore args))
  (let ((default (gensym)))
    (lambda ()
      (let ((res (funcall fun default)))
        (if (eq res default)
            (values nil nil)
            (values res t))))))

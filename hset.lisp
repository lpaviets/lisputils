;;; numbra:
;;; Hash-tables as sets

(in-package #:org.numbra.perso.ds.hset)

(defmacro define-hset-op (op (result key value already) (&body body) &body finally)
  "Defines a binary operation on hsets.

It iterates in order over all values of all the sets considered and
constructs a fresh hset RESULT. In BODY, KEY, VALUE and ALREADY will
respectively be bound to:

- the current KEY being considered for addition of removal into
  RESULT.

- The current VALUE that KEY has /within RESULT/.

- Whether KEY was present in RESULT or not already.

Before returning RESULT, execute FINALLY once."
  (let ((op-sym (read-from-string (format nil "hset-~(~A~)" op))))
    (utils:with-gensyms (test set)
      `(defun ,op-sym (hset &rest hsets)
         (loop :with ,result = (make-hash-table :test (hash-table-test hset)
                                                :size (hash-table-size hset))
               :with ,test = (hash-table-test ,result)
               :for ,set :in (cons hset hsets)
               :unless (eq ,test (hash-table-test ,set))
                 :do (error "HSets ~A, ~A use different equality tests"
                            hset ,set)
               :do (utils:do-hashkeys (,key ,set)
                     (multiple-value-bind (,value ,already)
                         (gethash ,key ,result)
                       (declare (ignorable ,value ,already))
                       ,@body))
               :finally ,@finally
                        (return ,result))))))


(define-hset-op union (result k v already)
    ((setf (gethash k result) t)))

(define-hset-op intersection (result k v already)
    ((incf (gethash k result 0)))
  (utils:do-hash (k v result)
    (if (< 1 v)
        (setf (gethash k result) t)
        (remhash k result))))

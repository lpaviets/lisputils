;;; numbra:
;;; Queue

(in-package #:org.numbra.perso.ds)

(defclass queue ()
  ((head :initarg :head :accessor queue-head)
   (tail :initarg :tail :accessor queue-tail)
   (size :initarg :size :accessor queue-size)))

(defun make-queue (&rest initial-elements)
  "Create a new queue with INITIAL-ELEMENTS inserted in the order of appearance."
  (make-instance 'queue :head initial-elements
                        :tail (last initial-elements)
                        :size (length initial-elements)))

;;; More efficient than doing one element at a time.
(defun queue-push (queue &rest elt)
  "Push ELTs onto the tail of QUEUE in the order of appearance."
  (assert (typep queue 'queue)
          ()
          "Trying to push on ~S: not a QUEUE" queue)
  (when elt
    (with-accessors ((tail queue-tail)
                     (size queue-size))
        queue
      (if tail
          (setf (cdr tail) elt
                tail (last tail))
          (setf (queue-head queue) elt
                tail (last elt)))
      (incf size (length elt))))
  queue)

(defun queue-empty-p (queue)
  (zerop (queue-size queue)))

(defun queue-pop (queue)
  "Pop from the front of QUEUE. Raises an error if QUEUE is empty."
  (with-accessors ((head queue-head)
                   (size queue-size))
      queue
    (let (return-value)
      (cond ((queue-empty-p queue) (error "Trying to pop from the empty queue ~S" queue))
            ((null (cdr head)) ; queue of length 1
             (setf return-value (car head)
                   head (cdr head)
                   (queue-tail queue) head))
            (t
             (setf return-value (car head)
                   head (cdr head))))
      (decf size)
      return-value)))

(defun queue-to-list (queue &optional (num-elements (queue-size queue)))
  (subseq (queue-head queue)
          0
          (min (queue-size queue) num-elements)))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (format stream "HEAD ~D" (first (queue-head queue)))))

(in-package :lifo-utils)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *overflow-flag* :OVERFLOW-6D15C000-009B-11C5-8CB8-000C299C8335)
  (defvar *underflow-flag* :UNDERFLOW-1FDDC000-E776-11C4-8FB4-000C299C8335))


(defmacro define-speedy-function (name args &body body)
  `(progn (declaim (inline ,name))
          (defun ,name ,args
            (declare (optimize (speed 3) (safety 0) (debug 0)))
            ,@body)))


;;; Queue Condition API

(define-condition queue-condition (error)
  ((queue :reader queue-condition-queue :initarg :queue))
  (:report (lambda (c s)
             (format s "Queue error in queue ~S"
                     (queue-condition-queue c)))))

(define-condition queue-length-error (queue-condition)
  ((attempted-length :reader queue-error-attempted-length :initarg :attempted-length))
  (:report (lambda (c s)
             (format s "Queue created with invalid length: ~S"
                     (queue-error-attempted-length c)))))

(define-condition queue-overflow-error (queue-condition)
  ((item :reader queue-overflow-extra-item :initarg :item))
  (:report (lambda (c s)
             (format s "Queue ~S is full, and can't have ~S stuffed into it"
                     (queue-condition-queue c) (queue-overflow-extra-item c)))))

(define-condition queue-underflow-error (queue-condition) ()
  (:report (lambda (c s)
             (format s "Queue ~S is empty, and can't be dequeued anymore"
                     (queue-condition-queue c)))))

(in-package #:cl-speedy-lifo-safe)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dummy* '!dummy!))


(define-speedy-function %make-queue (length)
  "Creates a new queue of maximum size LENGTH"
  (when (typep length 'fixnum)
    (locally (declare (fixnum length))
      (when (plusp length)
        (let ((queue (make-array (the fixnum (+ 1 length)) :initial-element nil)))
          ;; the 0th place, the queue's entry pointer,
          ;; stores the index of the top of the lifo queue, and (= (svref queue 0) 0) shows an empty queue
          (setf (svref queue 0) 0)
          (return-from %make-queue queue)))))
  (error 'queue-length-error :attempted-length length))

;;; Do we need a compiler macro for the above when LENGTH is constant so that we
;;;   don't add 2 at runtime? That's not very high on the priority list, although
;;;   it'll probably take less time to write than this comment did. -- Adlai

(define-speedy-function %queue-length (queue)
  "Returns QUEUE's maximum length"
  (the fixnum (- (length (the simple-vector queue)) 1)))

(define-speedy-function queuep (x)
  "If this returns NIL, X is not a queue"
  (when (simple-vector-p x)
    (let ((length (length x))
          (head (svref x 0)))
      (and (typep head 'fixnum)
           (< 1 head length)))))

(define-speedy-function %queue-out (queue)
  "QUEUE's exit pointer"
  (the fixnum (svref queue 0)))

(define-speedy-function %queue-in (queue)
  "QUEUE's entry pointer" ; overflow will be checked when this function is called
  (the fixnum (1+ (the fixnum (svref queue 0)))))

(define-speedy-function %queue-peek (queue)
  "Dereference QUEUE's exit pointer" ; underflow will be checked when this function is called
  (svref queue (%queue-out queue)))

#+:ignore
(define-speedy-function %queue-zero-p (queue)
  "Checks whether QUEUE's theoretical length is zero"
  (= (the fixnum (svref queue 0)) 0))

(define-speedy-function %queue-empty-p (queue)
  "Checks whether QUEUE is effectively empty"
  (= (the fixnum (svref queue 0)) 0))

(define-speedy-function %queue-full-p (queue)
  "Checks whether QUEUE is effectively full"
  (= (the fixnum (svref queue 0)) (1- (length queue))))

(define-speedy-function %queue-count (queue)
  "Returns QUEUE's effective length"
  (if (= (the fixnum (svref queue 0)) 0)
      0
      (the fixnum (svref queue 0))))

(define-speedy-function %next-index (current-index)
  (declare (fixnum current-index))
  (the fixnum (1+ current-index)))

(define-speedy-function %enqueue (object queue &aux (in (%queue-in queue)))
  (declare (fixnum in))
  "Enqueue OBJECT and increment QUEUE's entry pointer"
  (if (< in (the fixnum (length queue))) ;(/= in (the fixnum (length queue))) ; 02/24
      (prog1 (setf (svref queue in) object)
        (incf (the fixnum (svref queue 0))))
      ;;(error 'queue-overflow-error :queue queue :item object)
      #.*overflow-flag*))

(define-speedy-function %dequeue (queue keep-in-queue-p &aux (out (%queue-out queue)))
  "DEQUEUE, decrements QUEUE's entry pointer, and returns the previous top ref"
  (declare (fixnum out))
  (declare (boolean keep-in-queue-p))
  (if (/= (the fixnum (svref queue 0)) 0)
      (prog1 (svref queue out)
        (unless keep-in-queue-p (setf (svref queue out) nil))
        (decf (the fixnum (svref queue 0))))
      ;;(error 'queue-underflow-error :queue queue)
      #.*underflow-flag*))

(define-speedy-function %queue-flush (queue)
  "Returns QUEUE's effective length"
  (setf (svref queue 0) 0)
  queue)

;;; Now that all the backend functions are defined, we can define the API:

(defun make-queue (size)
  "Makes a queue of maximum size SIZE"
  (declare (fixnum size))
  (%make-queue size))

(defun queue-count (queue)
  "Returns the current size of QUEUE"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-count queue))

(defun queue-length (queue)
  "Returns the maximum size of QUEUE"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-length queue))

(defun queue-peek (queue)
  "Returns the next item that would be dequeued without dequeueing it."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (eq (svref queue 0) 0)
      (values nil nil)
      (values (%queue-peek queue) t)))

(defun queue-full-p (queue)
  "Returns NIL if more items can be enqueued."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-full-p queue))

(defun queue-empty-p (queue)
  "Tests whether QUEUE is empty"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-empty-p queue))

(defun queue-to-list (queue)
  "Convert a LIFO queue to a list, with the popping order kept."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-vector queue))
  (reverse (coerce (subseq queue 1 (1+ (the fixnum (svref queue 0)))) 'list)))

(defun list-to-queue (lst)
  "Convert a list to a LIFO queue, with the popping order kept."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list lst))
  (let ((queue (make-lifo (length lst))))
    (dolist (item (reverse lst))
      (%enqueue item queue))))

(defun enqueue (object queue)
  "Enqueues OBJECT in QUEUE. Return `object' instead of the whole `queue'"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%enqueue object queue))

(defun dequeue (queue &optional (keep-in-queue-p t))
  "Dequeues QUEUE. Return the element dequed.

When `keep-in-queue-p' sets to nil, the dequeued val will no longer keep an ref in the queue,
this is useful when the queue holds very big objects.

Making `keep-in-queue-p' as an optional parameter bring some performance penalty,
about 5 seconds slower than that as a non-optional parameter in a 10^9 pushes+pop operations test,
but it's still very fast."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%dequeue queue keep-in-queue-p))

(defun queue-find (item queue &key (key #'identity) (test #'eql))
  "Find `item' in `queue'"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function key test))
  (if (queue-empty-p queue)
      nil
      (let ((in (%queue-in queue)))
        (find item queue :start 1 :end in :key key :test test))))

(defun queue-flush (queue)
  "Make `queue' empty"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-flush queue))


;; ------- thread safe version -------

(define-speedy-function %enqueue-safe (object queue)
  "Enqueue OBJECT and increment QUEUE's entry pointer, thread safe."
  (let ((len (length (the simple-array queue))))
    (loop (let* ((entry (the fixnum (svref queue 0)))
                 (new-entry (1+ entry)))
            (if (< new-entry len)
                (when (compare-and-swap (svref queue 0) entry new-entry)
                  (setf (svref queue new-entry) object)
                  (return object))
                (return #.*overflow-flag*))))))

(define-speedy-function %dequeue-safe (queue keep-in-queue-p)
  "DEQUEUE, decrements QUEUE's entry pointer, and returns the previous top ref, thread safe."
  (loop (let* ((out (the fixnum (svref queue 0)))
               (new-out (1- out)))
          (if (/= out 0)
              (when (compare-and-swap (svref queue 0) out new-out)
                (if keep-in-queue-p
                    (return (svref queue out))
                    (let ((out-val nil))
                      (rotatef (svref queue out) out-val)
                      (return out-val))))
              (return #.*underflow-flag*)))))

(defun enqueue-safe (object queue)
  "Thread safe version of enqueue."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%enqueue-safe object queue))

(defun dequeue-safe (queue &optional (keep-in-queue-p t))
  "Thread safe version of dequeue."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%dequeue-safe queue keep-in-queue-p))

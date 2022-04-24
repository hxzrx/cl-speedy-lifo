(in-package #:cl-speedy-lifo-safe)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *dummy* '!dummy!)
  (defconstant *max-queue-length* (expt 10 10)))


(define-speedy-function %make-queue (length)
  "Creates a new queue of maximum size LENGTH"
  (declare (fixnum length))
  (let ((queue (make-array (the fixnum (+ 1 length)) :initial-element '#.*dummy*)))
    ;; the 0th place, the queue's entry pointer,
    ;; stores the index of the top of the lifo queue, and (= (svref queue 0) 0) shows an empty queue
    (setf (svref queue 0) 0)
    queue))

(define-speedy-function %queue-length (queue)
  "Returns QUEUE's maximum length"
  (the fixnum (- (length (the simple-vector queue)) 1)))

(define-speedy-function queuep (x)
  "If this returns NIL, X is not a queue"
  (when (simple-vector-p x)
    (let ((length (the fixnum (length x)))
          (head (the fixnum (svref x 0))))
      (< -1 head length #.(1+ *max-queue-length*)))))

(define-speedy-function %queue-out (queue)
  "QUEUE's exit pointer"
  (declare (simple-vector queue))
  (the fixnum (svref queue 0)))

(define-speedy-function %queue-in (queue)
  "QUEUE's entry pointer" ; overflow will be checked when this function is called
  (declare (simple-vector queue))
  (the fixnum (1+ (the fixnum (svref queue 0)))))

(define-speedy-function %queue-peek (queue)
  "Dereference QUEUE's exit pointer" ; underflow will be checked when this function is called
  (svref queue (%queue-out queue)))

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

(define-speedy-function %enqueue (object queue)
  "Enqueue OBJECT and increment QUEUE's entry pointer, thread safe."
  (let ((len (the fixnum (length (the simple-array queue)))))
    (loop (let* ((entry (the fixnum (svref queue 0)))
                 (new-entry (1+ entry)))
            (if (< new-entry len)
                ;; the dummy checking here is necessary,
                ;; as that place may not have been written by the dequeue operation at the current time.
                (when (and (eq (svref queue new-entry) '#.*dummy*)
                           (atomics:cas (svref queue 0) entry new-entry))
                  ;; the following setf will not overwrite for a fifo type and thus it will always success,
                  ;; but the action may be taken later
                  (return (setf (svref queue new-entry) object)))
                ;; full, enqueue will overflow, cas to make sure it's really full
                (when (atomics:cas (svref queue 0) entry entry) ; enqueue to a full queue is idempotent
                  (return #.*overflow-flag*)))))))

(define-speedy-function %dequeue (queue)
  "DEQUEUE, decrements QUEUE's entry pointer, and returns the previous top ref, thread safe."
  (loop (let* ((out (the fixnum (svref queue 0)))
               (new-out (1- out)))
          (if (= out 0)
              ;; empty, dequeue will underflow, cas to make sure it's really empty
              (when (atomics:cas (svref queue 0) out out) ; out == 0, dequeue to an empty queue is idempotent
                (return #.*underflow-flag*))
              ;; since the setf action for this place may be taken future in some time,
              ;; the svref action should make sure this place has been setf successfully,
              ;; so, the dummy checking here is necessary,
              ;; as that place may not have been written by the enqueue operation at the current time.
              (when (and (null (eq (svref queue out) '#.*dummy*))
                         (atomics:cas (svref queue 0) out new-out))
                (let ((res '#.*dummy*))
                  (rotatef (svref queue out) res)
                  (return res)))))))

(define-speedy-function %queue-flush (queue)
  (loop for i = (%dequeue queue)
        until (%queue-empty-p queue)
        finally (return queue)))

;;; Now that all the backend functions are defined, we can define the API:

(defun make-queue (size)
  "Makes a queue of maximum size SIZE"
  (assert (integerp size))
  (if (<= 2 size #.*max-queue-length*)
      (%make-queue size)
      (if (> size #.*max-queue-length*)
          (progn (warn "The size ~d exceeds the limit, will make a new queue with max size ~d" size #.*max-queue-length*)
                 (%make-queue #.*max-queue-length*))
          (error 'queue-length-error :attempted-length size))))


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
  (let ((queue (make-queue (length lst))))
    (dolist (item (reverse lst))
      (%enqueue item queue))))

(defun enqueue (object queue)
  "Enqueues OBJECT in QUEUE. Return `object' instead of the whole `queue'"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%enqueue object queue))

(defun dequeue (queue &optional keep-in-queue-p)
  "Dequeues QUEUE. Return the element dequed."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore keep-in-queue-p))
  (%dequeue queue))

(defun queue-find (item queue &key (key #'identity) (test #'eql))
  "Find `item' in `queue'"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function key test))
  (if (queue-empty-p queue)
      nil
      (let ((in (%queue-in queue)))
        (find item queue :start 1 :end in :key key :test test))))

(defun queue-flush (queue)
  "Make `queue' empty, ONLY use it when no other threads are modifying the `queue'."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-flush queue))

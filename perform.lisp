(ql:quickload :cl-speedy-lifo)
(ql:quickload :local-time)
(ql:quickload :bordeaux-threads)


(defparameter *times* (loop for i from 3 to 9
                            collect (expt 10 i)))
(defparameter *enq-count* (list 0))
(defparameter *deq-count* (list 0))
(defparameter *max-times* (expt 10 5))
(defparameter *threads-num* '(1 2 3 4 5 6 7 8))
(defparameter *counter* (list 0))
(defparameter *queue* nil)


;; ------- unsafe version -------

;; 13.112 seconds, 10^9, sbcl
(dolist (num *times*)
  (format t "LIFO queue, push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((lifo-queue (cl-speedy-lifo:make-queue num)))
    (time (progn
            (dotimes (i num)
              (cl-speedy-lifo:enqueue i lifo-queue))
            (dotimes (i num)
              (cl-speedy-lifo:dequeue lifo-queue nil))))))

;; 15.244 seconds, 10^9, sbcl
(dolist (num *times*)
  (format t "LIFO queue, push+pop, with consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((lifo-queue (cl-speedy-lifo:make-queue num)))
          (dotimes (i num)
            (cl-speedy-lifo:enqueue i lifo-queue))
          (dotimes (i num)
            (cl-speedy-lifo:dequeue lifo-queue nil)))))


;; ------- safe version -------
#|
;; 26.836 seconds, 10^9, sbcl
(dolist (num *times*)
  (format t "LIFO queue, push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((lifo-queue (cl-speedy-lifo-safe:make-queue num)))
    (time (progn
            (dotimes (i num)
              (cl-speedy-lifo-safe:enqueue i lifo-queue))
            (dotimes (i num)
              (cl-speedy-lifo-safe:dequeue lifo-queue nil))))))

;; 29.468 seconds, 10^9, sbcl
(dolist (num *times*)
  (format t "LIFO queue, push+pop, with consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((lifo-queue (cl-speedy-lifo-safe:make-queue num)))
          (dotimes (i num)
            (cl-speedy-lifo-safe:enqueue i lifo-queue))
          (dotimes (i num)
            (cl-speedy-lifo-safe:dequeue lifo-queue nil)))))
|#

;; -------- cl list -------

;; 1.636 seconds, 10^8, sbcl. For 10^9, it failed to get a result.
(dolist (num *times*)
  (format t "LIST queue, push+pop: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((list-queue nil))
          (dotimes (i num)
            (cl:push i list-queue))
          (dotimes (i num)
            (cl:pop list-queue)))))


;; ------- threads -------
#|
(dolist (t-num *threads-num*)
  (format t "~&safe lifo, threads: ~d, ~d times.~%" t-num *max-times*)
  (sb-ext:gc :full t)
  (setf *counter* (list 0))
  (setf *enq-count* (list 0))
  (setf *deq-count* (list 0))
  (let* ((num-each-thread (truncate (/ *max-times* t-num)))
         (lst (make-list num-each-thread :initial-element 8))
         (ts1 (local-time:now))
         (ts2 nil))
    (setf *queue* (cl-speedy-lifo-safe:make-queue *max-times*))
    (dotimes (th t-num)
      (bt:make-thread #'(lambda ()
                          (dolist (item lst)
                            (cl-speedy-lifo-safe:enqueue item *queue*)
                            (sb-ext:atomic-incf (car *enq-count*)))
                          (sb-ext:atomic-incf (car *counter*)))))
    (dotimes (th t-num)
      (bt:make-thread #'(lambda ()
                          (dolist (item lst)
                            (cl-speedy-lifo-safe:dequeue *queue*)
                            (sb-ext:atomic-incf (car *deq-count*)))
                          (sb-ext:atomic-incf (car *counter*)))))
    (loop until (= (car *counter*) (* 2 t-num))
          do (progn (sleep 0.001) (bt:thread-yield)))
    (setf ts2 (local-time:now))
    (format t "~&Time cost: ~d.~%" (local-time:timestamp-difference ts2 ts1))))
|#

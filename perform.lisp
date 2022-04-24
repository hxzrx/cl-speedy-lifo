(ql:quickload :cl-speedy-lifo)


(defparameter *times* (loop for i from 3 to 9
                            collect (expt 10 i)))


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

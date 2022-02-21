(in-package :cl-speedy-lifo)


(defparameter *times* (loop for i from 3 to 9
                            collect (expt 10 i)))

(dolist (num *times*)
  (format t "LIFO queue, push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((lifo-queue (make-queue num)))
    (time (progn
            (dotimes (i num)
              (enqueue i lifo-queue))
            (dotimes (i num)
              (dequeue lifo-queue nil))))))

(dolist (num *times*)
  (format t "LIFO queue, push+pop, with consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((lifo-queue (make-queue num)))
          (dotimes (i num)
            (enqueue i lifo-queue))
          (dotimes (i num)
            (dequeue lifo-queue nil)))))

(dolist (num *times*)
  (format t "LIST queue, push+pop: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((list-queue nil))
          (dotimes (i num)
            (cl:push i list-queue))
          (dotimes (i num)
            (cl:pop list-queue)))))

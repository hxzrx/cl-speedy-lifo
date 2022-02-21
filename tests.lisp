
(defpackage #:cl-speedy-lifo-tests
  (:use #:cl #:fiveam #:cl-speedy-lifo)
  (:nicknames #:lifo-tests)
  (:export #:run!
           #:all-tests))


(in-package :cl-speedy-lifo-tests)

(def-suite all-tests
  :description "The master suite of all cl-speedy-lifo tests.")

(in-suite all-tests)

(defparameter *loop-times* 10000)

(defun make-random-list (len &optional (max 5))
  (loop for i below len
        collect (random max)))

(test make-queue
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 100)))
           (queue (make-queue n)))
      (loop for i below (1+ n)
            do (progn
                 ;;(is (= 0 (queue-count queue)))
                 ;;(is (= n (queue-length queue)))
                 (if (= i 0)
                     (is (= 0 (svref queue i)))
                     (is (eq nil (svref queue i)))))))))

(test queue-count/queue-length/enqueue/-num
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (queue-count queue)))
      (is (= n (queue-length queue)))
      (dotimes (j k)
        (let ((rnd (random 5)))
          (is (= rnd (enqueue rnd queue)))))
      (is (= k (queue-count queue)))
      (is (= n (queue-length queue))))))

(test queue-count/queue-length/enqueue/-symbol
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (queue-count queue)))
      (is (= n (queue-length queue)))
      (dotimes (j k)
        (let ((rnd (gensym)))
          (is (eq rnd (enqueue rnd queue)))))
      (is (= k (queue-count queue)))
      (is (= n (queue-length queue))))))

(test queue-count/queue-length/enqueue/-string
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (queue-count queue)))
      (is (= n (queue-length queue)))
      (dotimes (j k)
        (let ((rnd (string (gensym))))
          (is (equal rnd (enqueue rnd queue)))))
      (is (= k (queue-count queue)))
      (is (= n (queue-length queue))))))

(test enqueue
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random (+ n 5))) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            for c from 1
            ;;do (is (eql element (enqueue element queue))))
            do (is (eql (if (> c n)
                            *overflow-flag*
                            element)
                        (enqueue element queue)))))))

(test dequeue
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            do (enqueue element queue))
      (loop for element in (reverse lst)
            do (is (eql element
                        (dequeue queue))))
      (is (eql *underflow-flag*
               (dequeue queue))))))

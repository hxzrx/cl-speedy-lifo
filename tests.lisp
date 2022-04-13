
(defpackage #:cl-speedy-lifo-tests
  (:use #:cl #:fiveam #:cl-speedy-lifo)
  (:nicknames #:lifo-tests)
  (:export #:run!
           #:all-tests))


(in-package :cl-speedy-lifo-tests)

(def-suite all-tests
  :description "The master suite of all cl-speedy-lifo tests.")

(in-suite all-tests)

(defparameter *loop-times* 1000)

(defun make-random-list (len &optional (max 5))
  (loop for i below len
        collect (random max)))

(test make-queue
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
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
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
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
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
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
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
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

(test queue-to-list
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n))
           (lst nil))
      (dotimes (j k)
        (enqueue (random 5) queue))
      (setf lst (queue-to-list queue))
      (is (= k (length lst)))
      (dotimes (j k)
        (is (= (pop lst) (dequeue queue)))))))

(test enqueue
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
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
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
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

(test queue-flush
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (queue-flush queue)
      (is (eql t (queue-empty-p queue)))
      (loop for element in lst
            do (enqueue element queue))
      (queue-flush queue)
      (is (eql t (queue-empty-p queue)))
      (is (eql *underflow-flag*
               (dequeue queue))))))

(test enqueue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
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
                        (enqueue-safe element queue)))))))

(test dequeue-safe-keep-single-thread
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            do (enqueue-safe element queue))
      (loop for element in (reverse lst)
            do (is (eql element
                        (dequeue-safe queue t))))
      (is (eql *underflow-flag*
               (dequeue-safe queue t))))))

(test dequeue-safe-keep-nicht-single-thread
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k))
           (total (apply #'+ lst)))
      (loop for element in lst
            do (enqueue-safe element queue))
      (is (= total (apply #'+ (queue-to-list queue))))
      (loop for element in (reverse lst)
            do (is (eql element
                        (dequeue-safe queue nil))))
      (is (eql *underflow-flag*
               (dequeue-safe queue nil))))))

(test enqueue-safe-multi-threads
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k))
           (total (apply #'+ lst)))
      (dolist (element lst)
        (let ((ele element)) ; make a bind as the var of element will change and will affect among the threads
          (bt:make-thread #'(lambda ()
                              (enqueue-safe ele queue)))))
      (sleep 0.01)
      (unless (= k (queue-count queue))
        (sleep 1)
        (format t "~&Result: ~d~%" (= k (queue-count queue)))
        (format t "~&lst: ~d~%" lst)
        (format t "~&queue: ~d~%" queue)
        (format t "~&lst len: ~d, queue-count: ~d, k = ~d~%~%" (length lst) (queue-count queue) k) (force-output)
        (is (= k (queue-count queue))))
      (is (= total (apply #'+ (queue-to-list queue))))
      )))

(test dequeue-safe-keep-multi-threads
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (dolist (element lst)
        (let ((ele element))
          (bt:make-thread #'(lambda ()
                              (enqueue-safe ele queue)))))
      (sleep 0.01) ; sleep time should not be two small
      (dotimes (num  (1- k)) ; dequeue all but the last element
        (dequeue-safe queue t))
      (sleep 0.01)
      (when (> (length lst) 0)
        (is (eql nil (eql *underflow-flag* (dequeue-safe queue t)))))
      (is (eql *underflow-flag* (dequeue-safe queue t)))
      )))

(test dequeue-safe-keep-nicht-multi-threads
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (dolist (element lst)
        (let ((ele element))
          (bt:make-thread #'(lambda ()
                              (enqueue-safe ele queue)))))
      (sleep 0.01) ; sleep time should not be two small
      (dotimes (num  (1- k)) ; dequeue all but the last element
        (dequeue-safe queue nil))
      (sleep 0.01)
      (when (> (length lst) 0)
        (is (eql nil (eql *underflow-flag* (dequeue-safe queue t)))))
      (is (eql *underflow-flag* (dequeue-safe queue nil)))
      )))

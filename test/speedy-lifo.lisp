(in-package :cl-speedy-lifo-tests)


(def-suite unsafe-lifo-test
  :description "cl-speedy-lifo.lisp"
  ;;:in all-tests)
  :in test-suite)


(in-suite unsafe-lifo-test)


(test make-queue
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 100)))
           (queue (cl-speedy-lifo:make-queue n)))
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
           (queue (cl-speedy-lifo:make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (cl-speedy-lifo:queue-count queue)))
      (is (= n (cl-speedy-lifo:queue-length queue)))
      (dotimes (j k)
        (let ((rnd (random 5)))
          (is (= rnd (cl-speedy-lifo:enqueue rnd queue)))))
      (is (= k (cl-speedy-lifo:queue-count queue)))
      (is (= n (cl-speedy-lifo:queue-length queue))))))

(test queue-count/queue-length/enqueue/-symbol
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (cl-speedy-lifo:make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (cl-speedy-lifo:queue-count queue)))
      (is (= n (cl-speedy-lifo:queue-length queue)))
      (dotimes (j k)
        (let ((rnd (gensym)))
          (is (eq rnd (cl-speedy-lifo:enqueue rnd queue)))))
      (is (= k (cl-speedy-lifo:queue-count queue)))
      (is (= n (cl-speedy-lifo:queue-length queue))))))

(test queue-count/queue-length/enqueue/-string
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (cl-speedy-lifo:make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (cl-speedy-lifo:queue-count queue)))
      (is (= n (cl-speedy-lifo:queue-length queue)))
      (dotimes (j k)
        (let ((rnd (string (gensym))))
          (is (equal rnd (cl-speedy-lifo:enqueue rnd queue)))))
      (is (= k (cl-speedy-lifo:queue-count queue)))
      (is (= n (cl-speedy-lifo:queue-length queue))))))

(test queue-to-list
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (cl-speedy-lifo:make-queue n))
           (k (random n))
           (lst nil))
      (dotimes (j k)
        (cl-speedy-lifo:enqueue (random 5) queue))
      (setf lst (cl-speedy-lifo:queue-to-list queue))
      (is (= k (length lst)))
      (dotimes (j k)
        (is (= (pop lst) (cl-speedy-lifo:dequeue queue)))))))

(test enqueue
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (cl-speedy-lifo:make-queue n))
           (k (random (+ n 5))) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            for c from 1
            ;;do (is (eql element (cl-speedy-lifo:enqueue element queue))))
            do (is (eql (if (> c n)
                            cl-speedy-lifo:*overflow-flag*
                            element)
                        (cl-speedy-lifo:enqueue element queue)))))))

(test dequeue
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (cl-speedy-lifo:make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            do (cl-speedy-lifo:enqueue element queue))
      (loop for element in (reverse lst)
            do (is (eql element
                        (cl-speedy-lifo:dequeue queue))))
      (is (eql cl-speedy-lifo:*underflow-flag*
               (cl-speedy-lifo:dequeue queue))))))

(test queue-flush
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (1+ (random 20))) ; queue length
           (queue (cl-speedy-lifo:make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (cl-speedy-lifo:queue-flush queue)
      (is (eql t (cl-speedy-lifo:queue-empty-p queue)))
      (loop for element in lst
            do (cl-speedy-lifo:enqueue element queue))
      (cl-speedy-lifo:queue-flush queue)
      (is (eql t (cl-speedy-lifo:queue-empty-p queue)))
      (is (eql cl-speedy-lifo:*underflow-flag*
               (cl-speedy-lifo:dequeue queue))))))

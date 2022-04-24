(in-package :cl-speedy-lifo-tests)


(defparameter *enqueue-sum* (make-atomic 0))
(defparameter *dequeue-sum* (make-atomic 0))


(def-suite safe-lifo-test
  :description "cl-speedy-lifo-safe.lisp"
  :in test-suite)


(in-suite safe-lifo-test)


(test make-queue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (signals error (cl-speedy-lifo-safe:make-queue 1))
  (signals error (cl-speedy-lifo-safe:make-queue 0))
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 100)))
           (queue (finishes (cl-speedy-lifo-safe:make-queue n))))
      (loop for i below (1+ n)
            do (progn
                 ;;(is (= 0 (queue-count queue)))
                 ;;(is (= n (queue-length queue)))
                 (if (= i 0)
                     (is (= 0 (svref queue i)))
                     (is (eq cl-speedy-lifo-safe::*dummy* (svref queue i)))))))))

(test queue-count/queue-length/enqueue/-num-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (cl-speedy-lifo-safe:queue-count queue)))
      (is (= n (cl-speedy-lifo-safe:queue-length queue)))
      (dotimes (j k)
        (let ((rnd (random 5)))
          (is (= rnd (cl-speedy-lifo-safe:enqueue rnd queue)))))
      (is (= k (cl-speedy-lifo-safe:queue-count queue)))
      (is (= n (cl-speedy-lifo-safe:queue-length queue))))))

(test queue-count/queue-length/enqueue/-symbol-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (cl-speedy-lifo-safe:queue-count queue)))
      (is (= n (cl-speedy-lifo-safe:queue-length queue)))
      (dotimes (j k)
        (let ((rnd (gensym)))
          (is (eq rnd (cl-speedy-lifo-safe:enqueue rnd queue)))))
      (is (= k (cl-speedy-lifo-safe:queue-count queue)))
      (is (= n (cl-speedy-lifo-safe:queue-length queue))))))

(test queue-count/queue-length/enqueue/-string-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n))) ; fill num
      (is (= 0 (cl-speedy-lifo-safe:queue-count queue)))
      (is (= n (cl-speedy-lifo-safe:queue-length queue)))
      (dotimes (j k)
        (let ((rnd (string (gensym))))
          (is (equal rnd (cl-speedy-lifo-safe:enqueue rnd queue)))))
      (is (= k (cl-speedy-lifo-safe:queue-count queue)))
      (is (= n (cl-speedy-lifo-safe:queue-length queue))))))

(test queue-to-list-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n))
           (lst nil))
      (dotimes (j k)
        (cl-speedy-lifo-safe:enqueue (random 5) queue))
      (setf lst (cl-speedy-lifo-safe:queue-to-list queue))
      (is (= k (length lst)))
      (dotimes (j k)
        (is (= (pop lst) (cl-speedy-lifo-safe:dequeue queue)))))))

(test enqueue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random (+ n 5))) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            for c from 1
            ;;do (is (eql element (cl-speedy-lifo-safe:enqueue element queue))))
            do (is (eql (if (> c n)
                            cl-speedy-lifo-safe:*overflow-flag*
                            element)
                        (cl-speedy-lifo-safe:enqueue element queue)))))))

(test dequeue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            do (cl-speedy-lifo-safe:enqueue element queue))
      (loop for element in (reverse lst)
            do (is (eql element
                        (cl-speedy-lifo-safe:dequeue queue))))
      (is (eql cl-speedy-lifo-safe:*underflow-flag*
               (cl-speedy-lifo-safe:dequeue queue))))))

(test queue-flush-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (cl-speedy-lifo-safe:queue-flush queue)
      (is (eql t (cl-speedy-lifo-safe:queue-empty-p queue)))
      (loop for element in lst
            do (cl-speedy-lifo-safe:enqueue element queue))
      (cl-speedy-lifo-safe:queue-flush queue)
      (is (eql t (cl-speedy-lifo-safe:queue-empty-p queue)))
      (is (eql cl-speedy-lifo-safe:*underflow-flag*
               (cl-speedy-lifo-safe:dequeue queue))))))

(test enqueue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random (+ n 5))) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            for c from 1
            ;;do (is (eql element (enqueue element queue))))
            do (is (eql (if (> c n)
                            cl-speedy-lifo-safe:*overflow-flag*
                            element)
                        (cl-speedy-lifo-safe:enqueue element queue)))))))

(test dequeue-safe-keep-single-thread
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k)))
      (loop for element in lst
            do (cl-speedy-lifo-safe:enqueue element queue))
      (loop for element in (reverse lst)
            do (is (eql element
                        (cl-speedy-lifo-safe:dequeue queue t))))
      (is (eql cl-speedy-lifo-safe:*underflow-flag*
               (cl-speedy-lifo-safe:dequeue queue t))))))

(test dequeue-safe-keep-nicht-single-thread
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k))
           (total (apply #'+ lst)))
      (loop for element in lst
            do (cl-speedy-lifo-safe:enqueue element queue))
      (is (= total (apply #'+ (cl-speedy-lifo-safe:queue-to-list queue))))
      (loop for element in (reverse lst)
            do (is (eql element
                        (cl-speedy-lifo-safe:dequeue queue nil))))
      (is (eql cl-speedy-lifo-safe:*underflow-flag*
               (cl-speedy-lifo-safe:dequeue queue nil))))))

(test enqueue-safe-multi-threads
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n)) ; fill num
           (lst (make-random-list k))
           (threads nil)
           (total (apply #'+ lst)))
      (dolist (element lst)
        (let ((ele element)) ; make a bind as the var of element will change and will affect among the threads
          (push (bt:make-thread #'(lambda ()
                                    (cl-speedy-lifo-safe:enqueue ele queue)))
                threads)))
      (dolist (th threads)
        (bt:join-thread th))
      (unless (= k (cl-speedy-lifo-safe:queue-count queue))
        (sleep 1)
        (format t "~&Result: ~d~%" (= k (cl-speedy-lifo-safe:queue-count queue)))
        (format t "~&lst: ~d~%" lst)
        (format t "~&queue: ~d~%" queue)
        (format t "~&lst len: ~d, queue-count: ~d, k = ~d~%~%" (length lst) (cl-speedy-lifo-safe:queue-count queue) k) (force-output)
        (is (= k (cl-speedy-lifo-safe:queue-count queue))))
      (is (= total (apply #'+ (cl-speedy-lifo-safe:queue-to-list queue))))
      )))

(test dequeue-safe-multi-threads
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (k (random n)) ; fill num
           (threads nil)
           (lst (make-random-list k)))
      (dolist (element lst)
        (let ((ele element))
          (push (bt:make-thread #'(lambda ()
                                    (cl-speedy-lifo-safe:enqueue ele queue)))
                threads)))
      (dolist (th threads)
        (bt:join-thread th))
      (dotimes (num  (1- k)) ; dequeue all but the last element
        (cl-speedy-lifo-safe:dequeue queue t))
      (when (> (length lst) 0)
        (is (eql nil (eql cl-speedy-lifo-safe:*underflow-flag* (cl-speedy-lifo-safe:dequeue queue t)))))
      (is (eql cl-speedy-lifo-safe:*underflow-flag* (cl-speedy-lifo-safe:dequeue queue t)))
      )))

(test dequeue-enqueue-mixed-threads
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i 10000);*loop-test-times*)
    (when (mod (1+ i) 1000) #+sbcl (sb-ext:gc :full t) #+ccl (ccl:gc))
    (setf *enqueue-sum* (make-atomic 0))
    (setf *dequeue-sum* (make-atomic 0))
    (let* ((n (+ 10 (random 10))) ; queue length
           (queue (cl-speedy-lifo-safe:make-queue n))
           (push-threads nil)
           (pop-threads nil)
           (k (random n)) ; fill num
           (lst (make-random-list k))
           (total (apply #'+ lst)))
      (assert (<= (length lst) (cl-speedy-lifo-safe:queue-length queue)))
      (dolist (element lst)
        (let ((ele element)) ; make a bind as the var of element will change and will affect among the threads
          (push (bt:make-thread #'(lambda ()
                                    (let ((res (cl-speedy-lifo-safe:enqueue ele queue)))
                                      (if (integerp res)
                                          (atomic-incf (atomic-place *enqueue-sum*) res)
                                          (format t "~&To enqueue: ~d, ret: ~d, queue: ~d~%" ele res queue)))))
                push-threads)))

      (dolist (element lst)
        (push (bt:make-thread #'(lambda ()
                                  (let ((res (cl-speedy-lifo-safe:dequeue queue t)))
                                    (if (integerp res)
                                        (atomic-incf (atomic-place *dequeue-sum*) res)
                                        (format t "~&Dequeue return: ~d, queue: ~d~%" res queue)))))
              pop-threads))

      (dolist (thread push-threads)
        (bt:join-thread thread))
      (dolist (thread pop-threads)
        (bt:join-thread thread))

      (is (= total (atomic-place *enqueue-sum*)))
      (is (= total (+ (atomic-place *dequeue-sum*)
                      (apply #'+ (cl-speedy-lifo-safe:queue-to-list queue)))))

      (unless (cl-speedy-lifo-safe:queue-empty-p queue)
        (format t "~&queue not empty: ~d~%elements: ~d~%" queue (cl-speedy-lifo-safe:queue-to-list queue)))
      (unless (= total (+ (atomic-place *dequeue-sum*)
                          (apply #'+ (cl-speedy-lifo-safe:queue-to-list queue))))
        (format t "~&~%List: ~d~%" lst)
        (format t "~&items num: ~d, enqueue sum: ~d, expect sum: ~d~%" k (atomic-place *enqueue-sum*) total)
        (format t "~&Queue count: ~d~%" (cl-speedy-lifo-safe:queue-count queue))
        (format t "~&Queue: ~d~%~%" queue))
      )))

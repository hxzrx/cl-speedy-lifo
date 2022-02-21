
(asdf:defsystem #:cl-speedy-lifo
  :author "He Xiangzhi <hexiangzhi@gmail.com>"
  :licence "MIT"
  :description "cl-speedy-lifo is a portable, non-consing, optimized LIFO queue (stack) implementation,
which is inspired by cl-speedy-queue"
  :serial t
  :in-order-to ((test-op (test-op "cl-speedy-lifo/tests")))
  :components ((:file "cl-speedy-lifo")))

(defsystem "cl-speedy-lifo/tests"
  :author "He Xiang-zhi"
  :license "MIT"
  :serial t
  :depends-on (:cl-speedy-lifo
               :fiveam)
  :components ((:file "tests"))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run!
                                      (find-symbol* 'all-tests 'cl-speedy-lifo-tests))))

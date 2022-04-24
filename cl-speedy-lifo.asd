(asdf:defsystem #:cl-speedy-lifo
  :author "He Xiangzhi <hexiangzhi@gmail.com>"
  :licence "MIT"
  :description "cl-speedy-lifo is a portable, non-consing, optimized LIFO queue (stack) implementation,
which is inspired by cl-speedy-queue."
  :depends-on (:atomics)
  :serial t
  :in-order-to ((test-op (test-op "cl-speedy-lifo/tests")))
  :components ((:file "packages")
               (:file "utils")
               (:file "cl-speedy-lifo")
               (:file "cl-speedy-lifo-safe")))

(defsystem "cl-speedy-lifo/tests"
  :author "He Xiang-zhi"
  :license "MIT"
  :serial t
  :depends-on (:cl-speedy-lifo
               :bordeaux-threads
               :fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "speedy-lifo")
                             (:file "speedy-lifo-safe"))))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run!
                                      (find-symbol* 'test-suite 'cl-speedy-lifo-tests))))

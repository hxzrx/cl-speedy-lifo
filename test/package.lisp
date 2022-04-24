(defpackage #:cl-speedy-lifo-tests
  (:use #:cl #:fiveam); #:cl-speedy-lifo)
  (:nicknames #:lifo-tests)
  (:export #:run!
           #:test-suite
           ))

(in-package :cl-speedy-lifo-tests)

(def-suite test-suite
  :description "Root of the tests.")

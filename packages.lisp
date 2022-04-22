(defpackage #:utils
  (:use :cl)
  (:export
   :define-speedy-function
   :queue-condition
   :queue-length-error
   :queue-overflow-error
   :queue-underflow-error
   :*underflow-flag*
   :*overflow-flag*))

(defpackage #:cl-speedy-lifo
  (:use :cl :utils)
  (:nicknames :lifo)
  (:export
   :make-queue
   :queue-to-list
   :list-to-queue
   :queue-count
   :queue-length
   :queue-peek
   :queue-full-p
   :queue-empty-p
   :enqueue
   :dequeue
   :queue-find
   :queue-flush
   :make-lifo
   :*overflow-flag*
   :*underflow-flag*))

(defpackage #:cl-speedy-lifo-safe
  (:use :cl :utils)
  (:nicknames :safe-lifo)
  (:export
   :make-queue
   :queue-to-list
   :list-to-queue
   :queue-count
   :queue-length
   :queue-peek
   :queue-full-p
   :queue-empty-p
   :enqueue
   :dequeue
   :enqueue-safe
   :dequeue-safe
   :queue-find
   :queue-flush
   :make-lifo
   :*overflow-flag*
   :*underflow-flag*))

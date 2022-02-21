# About

cl-speedy-lifo is a portable, non-consing, optimized lifo queue (stack) implementation,
which is inspired by cl-speedy-queue <https://github.com/zkat/cl-speedy-queue>.
# API

*[function]* `make-queue size`

  Creates a new queue of SIZE.

*[function]* `enqueue object queue`

  Enqueues OBJECT in QUEUE.

*[function]* `dequeue queue keep-in-queue`

  Dequeues QUEUE. If keep-in-queue sets to nil, the dequeued val will no longer keep an ref in the queue.

*[function]* `queue-count queue`

  Returns the current size of QUEUE.

*[function]* `queue-length queue`

  Returns the maximum size of QUEUE.

*[function]* `queue-peek queue`

  Returns the next item that would be dequeued without dequeueing it.

*[function]* `queue-full-p queue`

  Returns NIL if more items can be enqueued.

*[function]* `queue-empty-p queue`

  Returns NIL if there are still items in the queue.

*[function]* `make-lifo size`
  Alias for make-queue.

*[function]* `lifo-count queue`
  Alias for queue-count.

*[function]* `lifo-length queue`
  Alias for queue-length.

*[function]* `push object queue`
  Alias for enqueue.

*[function]* `pop queue`
  Alias for dequeue.

*[function]* `peek queue`
  Alias for queue-peek.


# Performance

| 10^n | LIFO Queue (without consing timed) | LIFO Queue(with consing timed) |            List Queue             |
| :--: | :--------------------------------: | :----------------------------: | :-------------------------------: |
|  3   |               0.000                |             0.000              |               0.000               |
|  4   |               0.000                |             0.000              |               0.000               |
|  5   |               0.000                |             0.000              |               0.000               |
|  6   |               0.016                |             0.016              |               0.012               |
|  7   |               0.128                |             0.140              |               0.136               |
|  8   |               1.248                |             1.476              |               2.548               |
|  9   |               12.560               |             16.376             | sbcl failed without any feedbacks |

Time costed compared cl-speedy-lifo queues with list lifo queues in seconds.



```commonlisp
(dolist (num *times*)
  (format t "LIFO queue, push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((lifo-queue (make-queue num)))
    (time (progn
            (dotimes (i num)
              (enqueue i lifo-queue))
            (dotimes (i num)
              (dequeue lifo-queue nil))))))
```

```commonlisp
(dolist (num *times*)
  (format t "LIFO queue, push+pop, with consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((lifo-queue (make-queue num)))
          (dotimes (i num)
            (enqueue i lifo-queue))
          (dotimes (i num)
            (dequeue lifo-queue nil)))))
```

```commonlisp
(dolist (num *times*)
  (format t "LIST queue, push+pop: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((list-queue nil))
          (dotimes (i num)
            (cl:push i list-queue))
          (dotimes (i num)
            (cl:pop list-queue)))))
```

# About

cl-speedy-lifo is a portable, non-consing, optimized lifo queue (stack) implementation,
which is inspired by cl-speedy-queue <https://github.com/zkat/cl-speedy-queue>.

# API

*[function]* `make-queue size`

  Creates a new queue of SIZE.

*[function]* `enqueue object queue`

  Enqueues OBJECT in QUEUE.

*[function]* `enqueue-safe object queue`

  Thread safe version of `enqueue`.

*[function]* `dequeue queue keep-in-queue`

  Dequeues QUEUE. If keep-in-queue sets to nil, the dequeued items will no longer keep an ref in the queue.

*[function]* `dequeue-safe queue keep-in-queue`

  Thread safe version of `dequeue`

*[function]* `queue-count queue`

  Returns the current size of QUEUE.

*[function]* `queue-length queue`

  Returns the maximum size of QUEUE.

*[function]* `queue-peek queue`

  Returns the next item that would be dequeued without dequeuing it.

*[function]* `queue-full-p queue`

  Returns NIL if more items can be enqueued.

*[function]* `queue-empty-p queue`

  Returns NIL if there are still items in the queue.

*[function]* `queue-to-list queue`

 Convert a LIFO queue to a list, with the popping order kept.

*[function]* `list-to-queue list`


# Performance

Now let's compared the time costs of push+pop operations, for this speedy-lifo and the build-in list.

| 10^n | Unsafe LIFO Queue | LIST Queue |
| :--: | :---------------: | :--------: |
|  3   |       0.000       |   0.000    |
|  4   |       0.000       |   0.000    |
|  5   |       0.000       |   0.000    |
|  6   |       0.020       |   0.011    |
|  7   |       0.164       |   0.124    |
|  8   |       1.476       |   1.636    |
|  9   |      16.668       |     -      |

All time units are in seconds.
Note that when timing the push+pop operations with LIST Queue for 10^9 times, sbcl's heap exhausted during garbage collection.


```commonlisp
(dolist (num *times*)
  (format t "Unsafe LIFO queue, push+pop, with consing timed: 10^~d times.~%" (log num 10))
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

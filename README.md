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

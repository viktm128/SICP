; Queue representation as a pair of pointers.

; Need error check on front only because that is a acceptable access point
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))
(define (empty-queue? q) (null? (front-ptr q)))
(define (make-queue) (cons '() '()))
(define (front-queue q) 
  (if (empty-queue? q)
    (error "Error: FRONT called on empty queue")
    (car (front-ptr q))
  )
)
(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? q)
        (set-front-ptr! q new-pair)
        (set-rear-ptr! q new-pair)
        q 
      )
      (else
        (set-cdr! (rear-ptr q) new-pair)
        (set-rear-ptr! q new-pair)
        q
      )
    )
  )
)
(define (delete-queue! q)
  (if (empty-queue? q) 
    (error "Error: DELETE was called on an empty queue.")
    (begin
      (set-front-ptr! q (cdr (front-ptr q)))
      q
    )
  )
)

"Exercise 3-21"
; Ben Biddle is running into this problem, because when we delete items from a single item queue,
; we don't update the rear pointer. The rear pointer technically still points to something that is 
; outside of the queue, but our interface will never allow us to act on it because insert-queue
; will check that the front pointer is empty, and reset both pointers upon next insertion.
(define (print-queue q)
  (front-ptr q)
)

"Exercise 3-22"
(define (make-queue-proc)
  (let 
    (
      (front-ptr '())
      (rear-ptr '())
    )
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "Error: Cannot find front item of an empty queue. FRONT-QUEUE")
        (car front-ptr)
      )
    )
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond
          ((empty-queue?)
            (set-front-ptr! new-pair)
            (set-rear-ptr! new-pair)
          )
          (else
            (set-cdr! rear-ptr new-pair)
            (set-rear-ptr! new-pair)
          )
        )
      )
    )
    (define (delete-queue!)
      (if (empty-queue?)
        (error "Error: DELETE-QUEUE was called on am empty queue.")
        (set-front-ptr! (cdr front-ptr))
      )
    )
    
    (define (dispatch m)
      (cond
        ((eq? m 'front-ptr) front-ptr)
        ((eq? m 'rear-ptr) rear-ptr)
        ((eq? m 'set-front-ptr!) set-front-ptr!)
        ((eq? m 'set-rear-ptr!) set-rear-ptr!)
        ((eq? m 'empty-queue?) empty-queue?)
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) delete-queue!)
        ((eq? m 'front-queue) front-queue)
        (else (error "Error: Undefined operation on MAKE-QUEUE" m))
      )
    )
    dispatch
  )
)

(define (front-queue-proc q) ((q 'front-queue)))
(define (insert-queue-proc! q item) ((q 'insert-queue!) item))
(define (delete-queue-proc! q) ((q 'delete-queue!)))

; I continue to not understand in which contexts a procedural implementtion is better


"Exercise 3-23"
; Implement as doubly linked list. O(n) space but bigger overhead. 

; Node Implementation
; node = (val next prev)
(define (make-node val) (cons val (cons '() '())))
(define (val node) (car node))
(define (next node) (cadr node))
(define (prev node) (cddr node))
(define (set-val! node new-val) (set-car! node new-val))
(define (set-next! node new-next) (set-car! (cdr node) new-next))
(define (set-prev! node new-prev) (set-cdr! (cdr node) new-prev))

; Deque Implementation on top of Node
(define (make-deque) (cons '() '()))
(define (empty-deque? d) (null? (front-ptr d)))
(define (front-deque d) ()
  (if (empty-deque? d)
    (error "Error: Tried to get front item from empty deque.")
    (val (front-ptr d))
  )
)
(define (rear-deque d) 
  (if (empty-deque? d)
    (error "Error: Tried to get rear item from empty deque.")
    (val (rear-ptr d))
  )
)
(define (front-insert-deque! d item)
  (let ((new-node (make-node item)))
    (cond
      ((empty-deque? d)
        (set-front-ptr! d new-node)
        (set-rear-ptr! d new-node)
      )
      (else
        (set-next! new-node (front-ptr d))
        (set-prev! (front-ptr d) new-node)
        (set-front-ptr! d new-node)
      )
    )
  )
)
(define (rear-insert-deque! d item)
  (let ((new-node (make-node item)))
    (cond
      ((empty-deque? d) 
        (set-front-ptr! d new-node)
        (set-rear-ptr! d new-node)
      )
      (else
        (set-next! (rear-ptr d) new-node)
        (set-prev! new-node (rear-ptr d))
        (set-rear-ptr! d new-node)
      )
    )
  )
)
(define (front-delete-deque! d) 
  (cond
    ((empty-deque? d) (error "Error: FRONT-DELETE was called on an empty deque."))
    ((eq? (front-ptr d) (rear-ptr d)) 
      (set-front-ptr! d '())
      (set-rear-ptr! d '())
    )
    (else
      (set-prev! (next (front-ptr d)) '())
      (set-front-ptr! d (next (front-ptr d)))
    )
  )
)
(define (rear-delete-deque! d) 
  (cond 
    ((empty-deque? d) (error "Error: REAR-DELETE was called on an empty deque."))
    ((eq? (front-ptr d) (rear-ptr d)) 
      (set-front-ptr! d '())
      (set-rear-ptr! d '())
    )
    (else
      (set-next! (prev (rear-ptr d)) '())
      (set-rear-ptr! d (prev (rear-ptr d)))
    )
  )
)
(define (print-deque d)
  (define (print node)
    (if (null? (next node))
      (begin 
        (display (val node))
        (display ")")
        (newline)
      )
      (begin
        (display (val node))
        (display " ")
        (print (next node))
      )
    )
  )
  (display "(")
  (print (front-ptr d))
)

(define d1 (make-deque))
(front-insert-deque! d1 'a)
(rear-insert-deque! d1 'b)
(rear-insert-deque! d1 'c)

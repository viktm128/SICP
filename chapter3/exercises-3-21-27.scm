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


; -----------------------------------------------------------------------------------------
; Implementing Tables

(define (make-table same-key?)
  (define (assoc-proc key records)
    (cond
      ((null? records) #f)
      ((same-key? key (caar records)) (car records))
      (else (assoc-proc key (cdr records)))
    )
  )

  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc-proc key1 (cdr local-table))))
        (if subtable
          (let ((record assoc-proc key2 (cdr subtable)))
            (if record
              (cdr record)
              #f
            )
          )
          #f
        )
      )
    )
    
    (define (insert! key1 key2 value)
      (let ((subtable (assoc-proc key1 (cdr local-table))))
        (if subtable
          (let ((record assoc-proc key2 (cdr subtable)))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key2 value) (cdr subtable)))
            )
          )
          (set-cdr! local-table (cons (list key1 (cons key2 value)) (cdr local-table)))
        )
      )
      'ok
    )
    
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation: TABLE" m))
      )
    )

    dispatch
  )
)

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

"Exercise 3-24"
; Easy to fix, just add an argument to make-table, and it can be called by the 
; definition of assoc-proc

"Exercise 3-25"
(define (make-table-n same-key?)
  (define (assoc-proc key records)
    (cond
      ((null? records) #f)
      ((same-key? key (caar records)) (car records))
      (else (assoc-proc key (cdr records)))
    )
  )

  (define (lookup-helper keys curr-table)
    (let ((search-result (assoc-proc (car keys) (cdr curr-table))))
      (cond
        ((not search-result) #f)
        ((null? (cdr keys)) (cdr serach-result))  ; Since its the last key, this is the record
        (else (lookup-helper (cdr keys) (search-result)))  ; search-result is a subtable
      )
    )
  )

  (define (insert!-helper keys value curr-table)
    (let ((search-result (assoc-proc (car keys) (cdr curr-table))))
      (cond
        ((and search-result (not (null? (cdr keys))))
          (insert!-helper (cdr keys) value search-result)
          (set-cdr! curr-table (cons () (cdr curr-table)))
        )
        ((and search-result (null? (cdr keys))) 
          (set-cdr! search-result value)
          'ok
        )
        ((null? (cdr keys)) 
          (set-cdr! curr-table (cons (cons (car key) value) (cdr curr-table)))
          'ok
        )
        (else
          (set-cdr! curr-table (cons (list (car keys)) (cdr curr-table)))
          (insert!-helper (cdr keys) value (cadr curr-table))  ; apply procedure on newly created table
        )
      )
    )
  )

  (let ((local-table (list '*table*)))
    (define (lookup keys) (lookup-helper keys local-table))
    (define (insert! keys) (insert!-helper keys value local-table))
 
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation: TABLE" m))
      )
    )

    dispatch
  )
)



"Exercise 3-26"
; If you want to change the organization structure of an n-dim table, you will need to 
; adapt the lookup and insert procedure to search pairs by binary tree. Not much should change.
; Potentially, you could abstract the way the backbone of the table works that way you could
; play with different implementations.

"Exercise 3-27"
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result ((table 'lookup-proc) x)))
        (if previously-computed-result
          previously-computed-result
          (let ((result (f x)))
            ((table 'insert-proc!) result)
            result
          )
        )
      )
    )
  )
)

(define (memo-fib n)
  (memoize
    (lambda (n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))
      )
    )
  )
)

; See environment diagram in notebook.

(define l (list 1 2 3 4 5))
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9))


"Exercise 2-17"
(define (last-pair-slow l)
  (list-ref l (- (length l) 1))
)

(define (last-pair l)
  (if (null? (cdr l))
    (car l)
    (last-pair (cdr l))
  )
)

"Exercise 2-18"
(define (reverse l)
  (if (null? l)
    l
    (append (reverse (cdr l)) (list (car l)))
  )
)


"Exercise 2-19"
; First let's re-examine the old method of making change
(define (count-change-old amount)
  (define (cc amount kinds-of-coins)
    (cond
      ((= amount 0) 1)
      ((or (< amount 0) (= 0 kinds-of-coins)) 0)
      (else 
        (+ (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins) (cc amount (- kinds-of-coins 1)))
      )
    )
  )
  (define (first-denomination kinds-of-coins)
    (cond
      ((= kinds-of-coins 1) 1)
      ((= kinds-of-coins 2) 5)
      ((= kinds-of-coins 3) 10)
      ((= kinds-of-coins 4) 25)
      ((= kinds-of-coins 5) 50)
    )
  )
  (cc amount 5)
)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (null? coin-values)) 0)
    (else
      (+ (cc amount (cdr coin-values)) (cc (- amount (car coin-values)) coin-values))
    )
  )
)
; You can make way way more combinations with UK coins 

"Exercise 2-20"
(define (same-parity x . y)
  (define (iter x y results)
    (cond 
      ((null? y) results)
      ((= (remainder x 2) (remainder (car y) 2)) (iter x (cdr y) (cons (car y) results)))
      (else (iter x (cdr y) results))    
    )
  )
  (reverse (iter x y (list x)))
)

"Exercise 2-21"
(define (square-list items)
  (if (null? items)
    ()
    (cons (square (car items)) (square-list (cdr items)))
  )
)

(define (square-list-2 items)
  (map square items)
)

"Exercise 2-22"
; Louis's iterative method prints in reverse order because cons
; appends new items to the "front" of the list as printed. In different
; words, when we cons an item to a list, it moves the head pointer attached
; to the list object to the newest item
;
; Switching the order of cons and the term to be added won't work as well.
; As he adds new items, they will be contained in a single-item list.
; ( (), (), (), (), etc)


"Exercise 2-23"
(define proc (lambda (x) (newline) (display x)))
(define (for-each- proc items)
  (cond 
    ((null? items) #t)
    (else 
      (proc (car items)) 
      (for-each- proc (cdr items))
    )
  )
)

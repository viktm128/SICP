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


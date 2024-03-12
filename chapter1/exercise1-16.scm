; Exercises 1-16
(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((= 0 (remainder n 2)) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1)))))
)

(define (fast-exp-iter a b n)
  (cond 
    ((= n 0) a)
    ((= 0 (remainder n 2)) (fast-exp-iter a (square b) (/ n 2))) 
    (else (fast-exp-iter (* a b) b (- n 1)))
  )
)

(define (fast-exp-new b n) (fast-exp-iter 1 b n))


; Exercsies 1-17 and 1-18
(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (mult-iter temp a b) (
  cond 
    ((= b 0) temp)
    ((= (remainder b 2) 0) (mult-iter temp (double a) (halve b)))
    (else (mult-iter (+ temp a) a (- b 1)))
))

(define (mult a b) (
  if (< b 0)
    (* -1 (mult-iter 0 a (abs b)))
    (mult-iter 0 a b)
))

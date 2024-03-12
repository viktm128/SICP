(define (fact-iter prod counter max_count)
  (if (> counter max_count)
    prod
    (fact-iter (* prod counter) (+ counter 1) max_count)
  )
)

(define (factorial n) (fact-iter 1 1 n))


(define (simple-binom n k) (
    / (factorial n) (* (factorial k) (factorial (- n k)) ) 
))

(define (binom n k)
  (
  if (> k (- n k))
    (binom-iter 1 1 (- n k) n)
    (binom-iter 1 1 k n)
  )
)

(define (binom-iter prod iter max_count n) (
  if (> iter max_count)
    prod
    (binom-iter (/ (* prod (+ (- n iter) 1)) iter) (+ iter 1) max_count n)
  )
)

(factorial 7)


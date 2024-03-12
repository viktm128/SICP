(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))
  )
)


(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (identity x) x)
(define (pi-term x) (/ 1 (* x (+ x 2))))
(define (pi-next a) (+ a 4))

(sum cube 1 inc 10)
(sum identity 1 inc 10)
(* 8.0 (sum pi-term 1 pi-next 1000))


(define (integral f a b dx) 
  (define (add-dx x) 
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
)

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)


"Exercise 1-29"
(define (simpson-recursive f a b n)
  (assert (> b a))
  (assert (even? n))

  (define h (/ (- b a) n))
  (define (f-sequence k) 
    (cond ((= k 0) (f (+ a (* h k))))
      ((= k n) (f (+ a (* h k))))
      ((even? k) (* 2 (f (+ a (* h k)))))
      ((odd? k) (* 4 (f (+ a (* h k)))))
  ))

  (* (/ h 3.0) (sum f-sequence 0 inc n))
)

(simpson-recursive cube 0 1 100)
(simpson-recursive cube 0 1 1000)

"Exercise 1-30"
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))
    ))
  (iter a 0)
)

(sum-iter cube 1 inc 10)
(sum-iter identity 1 inc 10)


"Exercise 1-31"
(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))
  ))
  (iter a 1)
)

(define (factorial n) (prod-iter identity 1 inc n))

; compute pi using specific sequence
(define (plus-two x) (+ x 2))
(define (prod-pi-term n) (/ (* (+ n 1) (- n 1)) (square n)))

; New appromation of pi
(* 4.0 (prod-iter prod-pi-term 3 plus-two 2001))

"Exercise 1-32"
(define (accumulate combiner null term a next b)
  (define (ac-iter a result)
    (if (> a b)
      result
      (ac-iter (next a) (combiner result (term a))))
  )
  (ac-iter a null)
)

(define (sum-a term a next b) (accumulate + 0 term a next b))
(define (prod-a term a next b) (accumulate * 1 term a next b))

;Test our results
(sum-a cube 1 inc 10)
(* 4.0 (prod-a prod-pi-term 3 plus-two 2001))


"Exercise 1-33"

(define (gcd a b) (
  if (= b 0)
    a
    (gcd b (remainder a b))
))
(define (relatively-prime a b) (= 1 (gcd a b)))

; Primality Setup - Functions from the Section
(define (find-next-divisor n test) (
  cond
    ((> (square test) n) n)
    ((= 0 (remainder n test)) test)
    (else (find-next-divisor n (+ 1 test)))
))

(define (find-smallest-divisor n) (find-next-divisor n 2))
(define (prime-basic n) (
  if (= n 1)
    #f
    (= n (find-smallest-divisor n))
))

(define (filtered-accumulate filter combiner null term a next b)
  (define (fa-iter a result)
    (cond ((> a b) result)
      ((filter a) (fa-iter (next a) (combiner result (term a))))
      (else (fa-iter (next a) (combiner result null)))
    )
  )
  (fa-iter a null)
)

; Sum of squares of all primes between 1 and 10 inclusive
(filtered-accumulate prime-basic + 0 square 1 inc 10)

; Product of all integers less than n relatively prime with n
(define (rel-prime-divsor-product n)
  (define (rel-prime-n a) (relatively-prime a n))
  (filtered-accumulate rel-prime-n * 1 identity 2 inc n)
)

(rel-prime-divsor-product 10)

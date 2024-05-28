; Implementing Streams in Terms of Force and Delay
;(define (cons-stream a b) (cons a (delay b))) --> the actual implementation of this changes how
;                                                   evaluation is done so that delay is called on 
;                                                   b before handing it to cons. Otherwise, if
;                                                   implemented as written, no computation will
;                                                   be delayed.
;(define (stream-car s) (car s))
;(define (stream-cdr s) (force (cdr s)))
;(define (stream-null? s) (or (null? s) (null? (cdr s))))
;(define the-empty-stream '())




; Implementing Stream Operations
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map (cons proc (map stream-cdr argstreams)))
    )
  )
)
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s))
    )
  )
)
(define (display-stream s)
  (stream-for-each display-line s)
)
(define (display-line x) 
  (newline) (display x)
)

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low (stream-enumerate-interval (+ low 1) high))
  )
)
(define (stream-filter pred s)
  (cond
    ((stream-null? s) the-empty-stream)
    ((pred (stream-car s))
      (cons-stream (stream-car s) (stream-filter pred (stream-cdr s)))
    )
    (else (stream-filter pred (stream-cdr s)))
  )
)


;----------------------------------------------------------------------------------------
; Infinite Streams in Action
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1)))
)
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens 
  (stream-filter 
    (lambda (x) (not (divisible? x 7)))
    integers
  )
)

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve s)
  (cons-stream
    (stream-car s)
    (sieve 
      (stream-filter 
        (lambda (x) (not (divisible? x (stream-car s)))) 
        (stream-cdr s)
      )
    )
  )
)
(define primes (sieve (integers-starting-from 2)))

; Implicit Definitions
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define alt-integers (cons-stream 1 (add-streams ones alt-integers)))
(define alt-fibs (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr alt-fibs) alt-fibs))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s)
)
(define double (cons-stream 1 (scale-stream double 2)))

(define alt-primes (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond
      ((> (square (stream-car ps)) n) #t)
      ((divisible? n (stream-car ps)) #f)
      (else (iter (stream-cdr ps)))
    )
  )
  (iter alt-primes)
)

;-----------------------------------------------------------------------------------------

"Exercise 3-50"
; See updated stream-map definition

"Exercise 3-51"
(define (show x)
  (display-line x)
  x
)

; (define x (stream-map show (stream-enumerate-interval 0 10)))
; 0
; Value --> x
;
; (stream-ref 5)
; 1
; 2
; 3
; 4
; 5 
; Value --> 5 
;
; (stream-ref 7)
; 6
; 7
; Value --> 7

"Exercise 3-52"
; The value of sum starts at 0. When seq is defined, sum will become the first value in the 
; enumerate interval but will not continue computing yet.
;
; Once the filter line is called sum will be updated until the first value of y can be computed
; which is 6 as the first even above 0. 
;
; In the next filter line, we will update sum up to the next number divisible by 5 which is 
; 10.
;
; y is defined to be a filtered stream of all the even values. So y will have values
; 6, 10, 28, 36, 66, 78, 120, 136, 190, 210
;
; z is defined to be a filter stream of all the values which are 0 mod 5. So z will have values
; 10, 15, 45, 55, 105, 120, 190, 210
;
; (stream-ref y 7) will yield 136. At this point, sum will also be updated up to 136
;
; (display-stream z) will yield
; 10 
; 15 
; 45
; 55
; 105
; 120
; 190
; 210
;
; which will force computation and sum will end at 210.

"Exercise 3-53"
; (define s (cons-stream 1 (add-streams s s))) --> this stream should behave just like double

"Exercise 3-54"
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

"Exercise 3-55"
(define (partial-sums s) 
  (cons-stream 
    (stream-car s) 
    (add-streams (partial-sums s) (stream-cdr s))
  )
)
; I am confused how this works. Should draw it out at some point.

"Exercise 3-56"
(define (merge-streams s1 s2)
  (cond 
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let 
        (
          (s1car (stream-car s1))
          (s2car (stream-car s2))
        )
        (cond
          ((< s1car s2car) (cons-stream s1car (merge-streams (stream-cdr s1) s2)))
          ((> s1car s2car) (cons-stream s2car (merge-streams s1 (stream-cdr s2))))
          (else (cons-stream s1car (merge-streams (stream-cdr s1) (stream-cdr s2))))
        )
      )
    )
  )
)
(define hamming-stream 
  (cons-stream 
    1 
    (merge-streams 
      (scale-stream hamming-stream 2) 
      (merge-streams (scale-stream hamming-stream 3) (scale-stream hamming-stream 5))
    )
  )
)
; Also don't really know how this works lmao.

"Exercise 3-57"
; Currently as memoized, after the first two, each additional term requires 1 addition as the previous two terms simply
; need to be looked up as opposed to computed. Therefore, for the nth term, we need max(0, n - 1) additions.
;
; To compute ___, we would need ___ additions
; Fib(0) --> 0
; Fib(1) --> 0
; Fib(2) --> 1 
; Fib(3) --> 2 
; Fib(4) --> 3
; Fib(5) --> 6
; Fib(6) --> 10
; Fib(7) --> 17
;
; In general to compute Fib(n), you will need the number additions to compute Fib(n - 1) + Fib(n - 2) + 1 additions. Probably
; by master theorem, this will grow exponentially.

"Exercise 3-58"
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (* num radix) den radix)
  )
)
; This procedure creates a stream which computes the numerical expansion of num/den in base radix. For example
; (expand 1 7 10) procudes 14285714... and 1/7 is 0.14285714...
; (expand 3 8 10) produces 37500000 and 3/8 is 0.3750000
;
; This may only work in general for num < den, but not sure. When I experiment with non base 10 things, it doesn't work as well.

"Exercise 3-59"
; a) 
(define (reciprocal-stream s)
  (cons-stream (/ 1 (stream-car s)) (reciprocal-stream (stream-cdr s)))
)
(define (integrate-series s)
  (mul-streams s (reciprocal-stream integers))
)

;b)
(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

"Exercise 3-60"
(define (mul-series s1 s2)
  (cons-stream 
    (* (stream-car s1) (stream-car s2))
    (add-streams 
      (scale-stream (stream-cdr s2) (stream-car s1))
      (mul-series (stream-cdr s1) s2)
    )
  )
)

(define id (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))


"Exercise 3-61"

(define (invert-unit-series s)
  (define inverted-series
    (cons-stream
      1 
      (scale-stream (mul-series (stream-cdr s) inverted-series) -1)
    )
  )
  inverted-series
)

(define invc (invert-unit-series cosine-series))


"Exercise 3-62"
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "Denominator Power Series has a constant term starting with 0" s2)
    (mul-series 
      s1
      (invert-unit-series (scale-stream s2 (/ 1 (stream-car s2))))
    )
  )
)

(define tan-series (div-series sine-series cosine-series))
; What the hell is going on really?

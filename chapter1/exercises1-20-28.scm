; All coding exercises in the GCD and primality testing sections


; Exercise 1-20
; Clever --> if a < b, the first call will simply switch the order
(define (gcd a b) (
  if (= b 0)
    a
    (gcd b (remainder a b))
))


; Primality Setup - Functions from the Section
(define (find-next-divisor n test) (
  cond
    ((> (square test) n) n)
    ((= 0 (remainder n test)) test)
    (else (find-next-divisor n (+ 1 test)))
))

(define (find-smallest-divisor n) (find-next-divisor n 2))

(define (prime-basic n) (= n (find-smallest-divisor n)))


; Exercise 1-21
(display "input   :   smallest divisor")(newline)
(display "199     :   199")(newline)
(display "1999    :   1999")(newline)
(display "19999   :   7")(newline)


; Exercise 1-22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock))
)
(define (start-prime-test n start-time) (
  if (prime-basic n)
    (report-prime (- (real-time-clock) start-time))
    (display " nothing")
))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time )
)

(define (search-for-primes count guess) (
  if (= count 0)
    (display "Done")
    ((search-for-primes (- count 1) (+ guess 2)) (timed-prime-test guess))
))

; Exercise 1-23
(define (next input) (
  cond ((= input 2) 3)
    (else (+ input 2))
))

(define (find-next-divisor-improved n test) (
  cond
    ((> (square test) n) n)
    ((= 0 (remainder n test)) test)
    (else (find-next-divisor-improved n (next test)))
))

(define (find-smallest-divisor-improved n) (find-next-divisor-improved n 2))

(define (prime-basic-improved n) (= n (find-smallest-divisor-improved n)))

(define (timed-prime-test-improved n) (
  (newline)
  (display n)
  define x (real-time-clock)
  define bool (prime-basic-improved n)
  define y (real-time-clock)
  if (bool)
    (display (- y x))
    (display "nothing")
))



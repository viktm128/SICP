; Need to load chapter3/exercises3-50-62.scm for stream functions.

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2.0)
)

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)
    )
  )
  guesses
)


; Pi Approximation
(define (pi-summands n)
  (cons-stream
    (/ 1.0 n)
    (stream-map - (pi-summands (+ n 2)))
  )
)

(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4.0))

(define (euler-transform s)
  (let 
    (
      (s0 (stream-ref s 0))
      (s1 (stream-ref s 1))
      (s2 (stream-ref s 2))
    )
    (cons-stream 
      (- s2 (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s))
    )
  )
)

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s)))
)

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s))
)

(define super-pi-stream (accelerated-sequence euler-transform pi-stream))


(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))
  )
)
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t))
    )
  )
)



; Integrals and Signal Processing
(define (integral integrand initial-value dt)
  (define int 
    (cons-stream
      initial-value
      (add-streams (scale-stream integrand dt) int)
    )
  )
  int
)

;-------------------------------------------------------------------------------------------------

"Exercise 3-63"
(define (inefficient-sqrt-stream x)
  (cons-stream
    1.0
    (stream-map (lambda (guess) (sqrt-improve guess x)) (inefficient-sqrt-stream x))
  )
)
; This version of a sqrt-stream function is currently inefficient because Louis is creating 
; many anonymous copies of the "same" stream in different frames. Thus, when the intepreter
; tries to optimize past calls to the "same" stream, it does not recognize these copies as 
; the same object and must recompute many previous terms. 
;
; If our implementation of delay did not include memoization, then, the initial procedure would
; be equally inefficient. Storying previous computation in guesses would not make the procedure 
; speed up searches for previous values --> it would just recompute the same values every time.
; It may have marginal memory advantages, but no increased inefficiency in speed.


"Exercise 3-64"
(define (stream-limit s tol)
  (let 
    (
      (s0 (stream-ref s 0))
      (s1 (stream-ref s 1))
    )
    (if (< (abs (- s1 s0)) tol)
      s1
      (stream-limit (stream-cdr s) tol)
    )
  )
)

(define (sqrt-x x tolerance)
  (stream-limit (sqrt-stream x) tolerance)
)

"Exercise 3-65"
(define (ln-2-summands n)
  (cons-stream 
    (/ 1.0 n)
    (stream-map - (ln-2-summands (+ n 1)))
  )
)

(define ln-2-basic (partial-sums (ln-2-summands 1)))
; (stream-limit ln-2-basic 0.001) --> 1001 terms computed

(define ln-2-intermediate (euler-transform ln-2-basic))
; (stream-limit ln-2-intermediate 0.001) --> 7 terms computed
; (stream-limit ln-2-intermediate 0.00001) --> 29 terms computed

(define ln-2-advanced (accelerated-sequence euler-transform ln-2-basic))
; (stream-limit ln-2-advanced 0.001) --> 4 terms computed
; (stream-limit ln-2-advanced 0.00001) --> 5 terms computed
; (stream-limit ln-2-advanced 0.0000001) --> 6 terms computed


"Exercise 3-66"
; (1, 100) should occur in position 197
; (100, 100) should occur in position 2^100 - 2 
; (99, 100) will occur in position 2^99 - 2 + 2^98
;
; In general, there is a pretty nice piecewise formula which can be proved with induction.
; See notebook for details.

"Exercise 3-67"
(define (flip-unique-pairs s)
  (if (= (car (stream-car s)) (cadr (stream-car s)))
    (cons-stream (stream-car s) (flip-unique-pairs (stream-cdr s))) 
    (cons-stream
      (stream-car s)
      (cons-stream
        (list (cadr (stream-car s)) (car (stream-car s)))
        (flip-unique-pairs (stream-cdr s))
      )
    )
  )
)
(define all-integer-pairs (flip-unique-pairs (pairs integers integers)))

; Alternate Approach
(define (all-pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave 
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (all-pairs (stream-cdr s) t)
    )
  )
)

"Exercise 3-68"
(define (louis-pairs s t)
  (interleave 
    (stream-map (lambda (x) (list (stream-car s) x) t))
    (louis-pairs (stream-cdr s) (stream-cdr t))
  )
)
; According to our evaluation order, as we try to call (louis-pairs integers integers), before we 
; enter the body of interleave, we will try to call (louis-pairs (stream-cdr s) (stream-cdr t)).
; Again, this is because we wanted to evaluate the arguments of our (interleave...) call before
; we enter a frame for interleave and call (cons-stream) which forces the delay. So the evaluation
; never gets delayed, and you enter an infinite recursion where nothing is computed.

"Exercise 3-69"
; TODO: Currently pairs-tu is computed a lot. Would only compute once
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map 
        (lambda (x) (cons (stream-car s) x)) 
        (stream-cdr (pairs t u))
      )
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
    )
  )
)
(define trips (triples integers integers integers))

(define pythagorean-triples
  (stream-filter 
    (lambda (x) (= (+ (square (car x)) (square (cadr x))) (square (caddr x))))
    trips
  )
)
; This is incredibly slow.


"Exercise 3-70"
(define (merge-weighted s1 s2 weight)
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
          ((< (weight s1car) (weight s2car)) 
            (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
          ((> (weight s1car) (weight s2car)) 
            (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
          (else 
            (cons-stream 
              s1car 
              (cons-stream 
                s2car 
                (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)
              )
            )
          )
        )
      )
    )
  )
)

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight
    )
  )
)

; a)
(define part-a (weighted-pairs integers integers (lambda (P) (+ (car P) (cadr P)))))
(define part-b 
  (let 
    ((non-divisible 
      (stream-filter 
        (lambda (i) 
          (and 
            (> (remainder i 2) 0) 
            (> (remainder i 3) 0) 
            (> (remainder i 5) 0)
          )
        )
        integers
      )
    ))
    (weighted-pairs 
      non-divisible 
      non-divisible 
      (lambda (P) (+ (* 2 (car P)) (* 3 (cadr P)) (* 5 (car P) (cadr P))))
    )
  )
)


"Exercise 3-71"
(define (cube x) (* x x x))
(define (sum-of-cubes P) (+ (cube (car P)) (cube (cadr P))))
(define (find-repititions-stream s proc)
  (let 
    (
      (w0 (proc (stream-car s)))
      (w1 (proc (stream-car (stream-cdr s))))
    )
    (if (= w0 w1) 
      (cons-stream 
        (cons (stream-car s) w0)
        (cons-stream 
          (cons (stream-car (stream-cdr s)) w1)
          (find-repititions-stream (stream-cdr s) proc)
        )
      )
      (find-repititions-stream (stream-cdr s) proc)
    )
  )
)
(define ramanujan-numbers
  (find-repititions-stream (weighted-pairs integers integers sum-of-cubes) sum-of-cubes)
)

; The first 6 ramanujan numbers and the pairs that determine them are 
; ((1 12) . 1729) ((9 10) . 1729) 
; ((2 16) . 4104) ((9 15) . 4104) 
; ((2 24) . 13832) ((18 20) . 13832) 
; ((10 27) . 20683) ((19 24) . 20683) 
; ((4 32) . 32832) ((18 30) . 32832) 
; ((2 34) . 39312) ((15 33) . 39312)

"Exercise 3-72"
(define (sum-of-squares P) (+ (square (car P)) (square (cadr P))))
(define (find-triple-repeats-stream s proc)
  (let 
    (
      (w0 (proc (stream-ref s 0)))
      (w1 (proc (stream-ref s 1)))
      (w2 (proc (stream-ref s 2)))
    )
    (if (= w0 w1 w2)
      (cons-stream
        (cons (stream-ref s 0) w0)
        (cons-stream
          (cons (stream-ref s 1) w1)
          (cons-stream 
            (cons (stream-ref s 2) w2)
            (find-triple-repeats-stream (stream-cdr s) proc)
          )
        )
      )
      (find-triple-repeats-stream (stream-cdr s) proc)
    )
  )
)
(define triple-sum-of-squares
  (find-triple-repeats-stream (weighted-pairs integers integers sum-of-squares) sum-of-squares)
)

; The first 6 numbers which can be written in 3 different ways as sums of squares are
; ((1 18) . 325) ((6 17) . 325) ((10 15) . 325)
; ((5 20) . 425) ((8 19) . 425) ((13 16) . 425)
; ((5 25) . 650) ((11 23) . 650) ((17 19) . 650)
; ((7 26) . 725) ((10 25) . 725) ((14 23) . 725)
; ((2 29) . 845) ((13 26) . 845) ((19 22) . 845)
; ((3 29) . 850) ((11 27) . 850) ((15 25) . 850)

"Exercise 3-73"
(define (RC R C dt)
  (define (new-circuit i v0)
    (add-streams 
      (scale-stream (integral i v0 dt) (/ 1.0 C))
      (scale-stream i R)
    )
  )
  new-circuit
)

"Exercise 3-74"
(define (zero-crossings sense-data)
  (stream-map 
    sign-change-detector
    sense-data
    (cons-stream 0 sense-data)  ; alternatively (stream-cdr sense data) --> just depends on initial behavior
  )
)

"Exercise 3-75"
(define (make-zero-crossings-louis input-stream prev-value prev-avg)
  (let ((avpt (/ (+ (stream-car input-stream) prev-value) 2)))
    (cons-stream
      (sign-change-detector avpt prev-avg)
      (make-zero-crossings-louis (stream-cdr input-stream) (stream-car input-stream) avpt)
    )
  )
)

"Exercise 3-76"
(define (avg x y) (/ (+ x y) 2.0))
(define (smooth-stream s)
  (let 
    (
      (s0 (stream-ref s 0))
      (s1 (stream-ref s 1))
    )
    (cons-stream (avg s0 s1) (smooth-stream (stream-cdr s)))
  )
)

(define (smoothed-zero-crossings data)
  (zero-crossings (smooth-stream data))
)

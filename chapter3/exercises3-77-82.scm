(define (integral delayed-integrand initial-value dt)
  (define int 
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int)
      )
    )
  )
  int
)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y
)

(define solve-stream (solve (lambda (x) x) 1 0.001))

;------------------------------------------------------------------------------------------------
(define (random-number-generator) 
  (cons-stream (random 1000000) (random-number-generator))
)
(define random-numbers (random-number-generator))

(define (map-successive-pairs f s)
  (cons-stream 
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))
  )
)

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1)) random-numbers)
)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo (stream-cdr experiment-stream) passed failed)
    )
  )
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))
  )
)
(define pi 
  (stream-map (lambda (p) (sqrt (/ 6.0 p))) (monte-carlo cesaro-stream 0 0))
)

;------------------------------------------------------------------------------------------------

"Exercise 3-77"
(define (integral-explicit integrand initial-value dt)
  (cons-stream
    initial-value
    (if (and (promise? integrand) (not (promise-forced? integrand)))
      (integral-explicit (force integrand) initial-value dt)  
      (integral-explicit
        (stream-cdr integrand)
        (+ (* dt (stream-car integrand)) initial-value)
        dt
      )
    )
  )
)
(define (solve-explicit f y0 dt)
  (define y (integral-explicit (delay dy) y0 dt))
  (define dy (stream-map f y))
  y
)
(define solve-stream-explicit (solve-explicit (lambda (x) x) 1 0.001))


"Exercise 3-78"
(define (solve-second-linear a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay d2y) dy0 dt))
  (define d2y (stream-map (lambda (z1 z2) (+ (* a z1) (* b z2))) dy y))
  y
)

"Exercise 3-79"
(define (solve-second-general f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay d2y) dy0 dt))
  (define d2y (stream-map f dy y))
  y
)

"Exercise 3-80"
(define (RLC R L C dt)
  (define (new-circuit vc0 il0)
    (define vc (integral (delay (scale-stream il (/ -1.0 C))) vc0 dt))  ; vc stream 
    (define il
      (integral
        (delay
          (add-streams
            (scale-stream vc (/ 1.0 L))
            (scale-stream il (/ (- R) L))
          )
        )
        il0
        dt
      )
    )  ; il stream
    (list vc il)
  )
  new-circuit
)

(define vcil-pair ((RLC 1.0 1.0 0.2 0.1) 10.0 0.0))


"Exercise 3-81"
(define (random-number-tester requests)
  (define random-numbers (random-number-generator))
  (define (iter requests rns)
    (cons-stream 
      (stream-car rns)
      (cond 
        ((eq? (stream-car requests) 'generate) (iter (stream-cdr requests) (stream-cdr rns)))
        ((eq? (stream-car requests) 'reset) (iter (stream-cdr requests) random-numbers))
        (else (error "Unknown argument in random-number-test" (stream-car requests)))
      )             
    )
  )
  (iter requests random-numbers)
)

(define test-rqs (cons-stream 'generate (cons-stream 'generate (cons-stream 'reset (cons-stream 'generate 'generate)))))


"Exercise 3-82"
(define (random-in-range-stream low high)
  (cons-stream 
    (+ low (random (- high low)))
    (random-in-range-stream low high)
  )
)
(define (estimate-integral P x1 x2 y1 y2)
  (define x-stream (random-in-range-stream x1 x2))
  (define y-stream (random-in-range-stream y1 y2))
  (define P-stream (stream-map P x-stream y-stream))
  (scale-stream (monte-carlo P-stream 0 0) (* (- x2 x1) (- y2 y1)))
  
)


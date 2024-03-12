; Fixed Point function from Book

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))
    )
    (display guess) (newline)
  )
  (try first-guess)
)

; Fixed point of cos(x)
(fixed-point cos 1.0)

; Fixed point of sin(x) + cos(x)
(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)

; sqrt function with average damping
(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0)
)

"Exercise 1-35"
; The golden ratio is a fixed point of x --> 1 + 1/x
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

"Exercise 1-36"
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 2.0)
; Average damping speeds it up quite a bit 


"Exercise 1-37"
(define (cont-frac n d k) 
  (define (iter j result)
    (if (< j 1)
      result
      (iter (- j 1) (/ (n j) (+ (d j) result)))
    )
  )
  (iter k 0)
)

(/ 1.0 (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 100))
; Takes 12 terms to be accurate up to 4 decimal places


"Exercise 1-38"
(define (d i) 
  (if (= 0 (remainder (+ i 1) 3))
    (* (/ (+ i 1) 3) 2.0)
    1.0
  )
)
(+ 2 (cont-frac (lambda (x) 1.0) d 100))


"Exercise 1-39"
(define (tan-cf x k)
  (define (n i)
    (if (= i 1.0)
      x
      (- (square x))
    )
  )
  (cont-frac n (lambda (i) (- (* 2.0 i) 1)) k)
)

(tan-cf (/ 3.14159 4) 100)

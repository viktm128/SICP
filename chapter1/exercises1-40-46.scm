(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))
    )
  )
  (try first-guess)
)

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2))
)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
)


(define dx 0.00001)
(define (deriv g) 
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)

(define (newton-transform g)
  (lambda(x) (- x (/ (g x) ((deriv g) x))))
)

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess)
)


"Exercise 1-40"
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
)

"Exercise 1-41"
(define (inc x) (+ x 1))
(define (double f)
  (lambda (x) (f (f x)))
)
(((double (double double)) inc) 5)


"Exercise 1-42"
(define (compose f g)
  (lambda (x) (f (g x)))
)
((compose square inc) 6)

"Exercise 1-43"
(define (repeated f n)
  (define (iter n result)
    (if (= n 1)
      result
      (iter (- n 1) (compose f result)))
  )
  (iter n f)
)

"Exercise 1-44"
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))
)
(define (n-fold-smoothed f n)
  ((repeated smooth n) f)
)


"Exercise 1-45"
(define (log-2 x) (/ (log x) (log 2)))
(define (power x n)
  (define (iter result n)
    (if (< n 1)
      result
      (iter (* result x) (- n 1)))
  )
  (iter 1 n)
)

(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (log-2 n))) (lambda (y) (/ x (power y (- n 1))))) 1.0)
)


"Exercise 1-46"
(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
        guess
        (try next)
      )
    )
  )
  (lambda (x) (try x))
)
(define (good-enough? x1 x2) (< (abs (- x1 x2)) 0.001))
(define (new-fp f first) ((iterative-improve good-enough? f) first))

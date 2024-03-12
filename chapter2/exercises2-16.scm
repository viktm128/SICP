(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))
)
(define (mul-rat x y )
  (make-rat (* (numer x) (numer y) ) (* (denom x) (denom y)))
)
(define (-rat x) (make-rat (- (numer x)) (denom x)))
(define (sub-rat x y) (add-rat x (-rat y)))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y)))
)
(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (denom x) (numer y)))
)

(define (gcd a b) (
  if (= b 0)
  (abs a)
  (gcd b (remainder a b))
))

(define (make-rat-helper n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))
  )
)

(define (make-rat n d)
  (cond 
    ((< (* n d) 0) (make-rat-helper  (- (abs n)) (abs d)))
    (else (make-rat-helper (abs n) (abs d)))
  )
)

(define (numer x) (car x))
(define (denom y) (cdr y))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)



"Exercise 2-1"
; See updated definition of make-rat and make-rat-helper


"Exercise 2-2"
(define (make-point x y) (cons x y))
(define (x-coord p) (car p))
(define (y-coord p) (cdr p))

; This does not identify segments with "flipped" endpoints
; This does not prevent a segment from having the same two endpoints
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))

(define (midpoint line)
  (make-point 
    (/ (+ (x-coord (start-segment line)) (x-coord (end-segment line))) 2)
    (/ (+ (y-coord (start-segment line)) (y-coord (end-segment line))) 2)
  )
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-coord p))
  (display ", ")
  (display (y-coord p))
  (display ")")
)

"Exercise 2-3"
(define (sqrt x) (exp (* 0.5 (log x))))
(define (length segment)
  (sqrt(+
    (square (- (x-coord (start-segment segment)) (x-coord (end-segment segment)))) 
    (square (- (y-coord (start-segment segment)) (y-coord (end-segment segment)))) 
  ))
)
(define (orthogonal? v1 v2)
  (= (+ (* (x-coord v1) (x-coord v2)) (* (y-coord v1) (y-coord v2))) 0)
)

; p is a basepoint and v1,v2 is a spanning vector set
(define (make-rect p v1 v2) 
  (assert (> (length (make-segment (make-point 0 0) v1))))
  (assert (> (length (make-segment (make-point 0 0) v2))))
  (assert (orthogonal? v1 v2))
  (cons p (cons v1 v2))
)
(define (get-base rect) (car rect))
(define (get-v1 rect) (car (cdr rect)))
(define (get-v2 rect) (cdr (cdr rect)))
(define (perimeter rect)
  (+ 
    (* 2 (length (make-segment (make-point 0 0) (get-v1 rect))))
    (* 2 (length (make-segment (make-point 0 0) (get-v2 rect))))
  )
)

(define (area rect)
  (* (length (make-segment (make-point 0 0) (get-v1 rect)))
     (length (make-segment (make-point 0 0) (get-v2 rect)))
  )
)
; Too bored to make another representation, but would want functions which
; compute all 4 points of the rectangle and all 4 segements to fully complete this suite

"Exercise 2-4"
(define (newcons x y) (lambda (m) (m x y)))
(define (newcar z) (z (lambda (p q) p)))
(define (newcdr z) (z (lambda (p q) q)))


"Exercise 2-5"
(define (power x n)
  (define (iter n result)
    (if (< n 1)
      result
      (iter (- n 1) (* result x))
    )
  )
  (iter n 1)
)
(define (count-factor num factor)
  (define (iter num result)
    (if (> (remainder num factor) 0)
      result
      (iter (/ num factor) (+ result 1))
    )
  )
  (iter num 0)
)

; Assume a,b > 0 integers
(define (prodcons a b) (* (power 2 a) (power 3 b)))
(define (prodcar z) (count-factor z 2))
(define (prodcdr z) (count-factor z 3))


"Exercise 2-6"
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x))))
)

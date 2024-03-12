"Given setup"
(define (add-int x y)
  (make-int
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))
  )
)

(define (mult-int x y)
  (let (
    (p1 (* (lower-bound x) (lower-bound y)))
    (p2 (* (lower-bound x) (upper-bound y)))
    (p3 (* (upper-bound x) (lower-bound y)))
    (p4 (* (upper-bound x) (upper-bound y)))
  )
    (make-int (min p1 p2 p3 p4) (max p1 p2 p3 p4))
  )
)

(define (div-int x y)
  (assert (not (and
    (or (= (lower-bound y) 0) (< (lower-bound y) 0))
    (or (= (upper-bound y) 0) (> (upper-bound y) 0))
  )))
  (mult-int x (make-int (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))
)

"Helper Function"
(define (print-int i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]")
)

"Exercise 2-7"
(define (make-int a b) (assert (or (< a b) (= a b)))(cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

"Exercise 2-8"
(define (sub-int x y)
  (make-int 
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))
  )
)

"Exercise 2-9"
(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))
; Proof that the width of a sum or difference is the sum of the widths always
; found in notebook
; 
; Example that it it won't work with multiplication:
; [2, 4] * [2, 4] = [4, 16] but [2, 4] [10, 12] = [20, 48]
; Division follows immediately because you can multiply by reciprocal to show same problem

"Exercise 2-10"
; Implemented with assert in div-int

"Exercise 2-11"
; More to come
; This problem sucks --> not interesting at all


"Exercise 2-12"
(define (make-center-width c w)
  (make-int (- c w) (+ c w))
)
(define (center i) (/ (+ (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p 0.01))
)
(define (percent i) (* (/ (width i) (center i)) 100))

"Exercise 2-13"
; When p1, p2 close to 0, the percentage of the product is approx p1 + p2

"Exercise 2-14"
(define (par1 r1 r2)
  (div-int (mult-int r1 r2) (add-int r1 r2))
)
(define (par2 r1 r2)
  (let ((one (make-int 1 1)))
    (div-int one (add-int (div-int one r1) (div-int one r2)))
  )
)
; With i1 = (make-center-percent 6 3)
; (par1 i1 i1) = (2.74, 3.28)
; (par2 i2 i2) = (2.91 3.09)


"Exercise 2-15"
; Assuming the intervals are all fully positive (non-negative resistances)
; Eva Lu Ator is indeed correct that par2 produces tighter intervals. See notebook
; for proof. However, it is unclear and unlikely tht all formulations with repeated variables
; created wider intervals.


"Exercise 2-16"
; Currently as defined, subtraction is not an inverse operation of addition
; Similarly, division is not an inverse operation of multiplication
; Gut instinct says this is impossible. I am doubtful there is a consistent way
; to define multiplicative inverses in a way which is compatible with our algebraic
; notion of division of real numbers. Best way to estimate the error is with a diffeq.

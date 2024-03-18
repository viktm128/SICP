; Some helper functions

(define (filter predicate sequence)
  (cond
    ((null? sequence) ())
    ((predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))
    )
    (else (filter predicate (cdr sequence)))
  )
)

(define (accumulate op initial sequence)
  (if (null? sequence) 
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))
  )
)

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))
  )
)

(define (enumerate-tree tree)
  (cond
    ((null? tree) ())
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))
  )
)

"Exercise 2-33"
(define (new-map p sequence) (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(define (new-append seq1 seq2) (accumulate cons seq2 seq1))
(define (new-len sequence) (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


"Exercise 2-34"
(define (horners-eval x coeffs)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff)) 0 coeffs)
)

"Exercise 2-35"
(define (count-leaves t) 
  (accumulate (lambda (x y) (+ 1 y)) 0 (enumerate-tree t))
)

"Exercise 2-36"
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map car seqs)) (accumulate-n op init (map cdr seqs)))
  )
)

"Exercise 2-37"
(define v1 (list 1 2 3 4))
(define v2 (list 4 5 6 6))
(define v3 (list 6 7 8 9))
(define m (list v1 v2 v3))
(define (dot-product v w)
  (accumulate + 0 (map * v w))
)
(define (matvec m v)
  (map (lambda (x) (dot-product x v)) m)
)
(define (transpose m)
  (accumulate-n cons () m)
)
(define (matmul m n)
  (let ((cols (transpose n)))
    (map (lambda(x) (matvec cols x)) m)
  )
)

"Exercise 2-38"
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))
    )
  )
  (iter init seq)
)
; fold-left and accumulate will agree if the operation is commutative and associativeGa


"Exercise 2-39"
(define (reverse-r l)
  (accumulate (lambda (x y) (append y (list x) )) () l)
)
(define (reverse-l l)
  (fold-left (lambda (x y) (cons y x)) () l)
)


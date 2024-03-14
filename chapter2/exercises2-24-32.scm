"Exercise 2-24"
;(1 (2 (3 4)))

"Exercise 2-25"
; (1 3 (5 7) 9) --> (car (cdr (car (cdr (cdr x)))))
; ((7)) --> (car (car x))
; (1 (2 ( 3 (4 (5 (6 7)))))) --> (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

"Exercise 2-26"
; (define x (list 1 2 3))
; (define y (list 4 5 6))
; (append x y) --> (1 2 3 4 5 6)
; (cons x y) --> ((1 2 3) 4 5 6)
; (list x y) --> ((1 2 3) (4 5 6))

"Exercise 2-27"
(define x (list (list 1 2) (list 3 4)))
(define (deep-reverse l)
  (cond
    ((null? l) l)
    ((pair? (car l)) (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
    (else (append (deep-reverse (cdr l)) (list (car l)))) 
  )
)

"Exercise 2-28"
(define (fringe l)
  (cond
    ((null? l) l)
    ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
    (else (cons (car l) (fringe (cdr l))))
  )
)

"Exercise 2-29"
(define (make-mobile left right) (list left right))
(define (make-branch len structure) (list len structure))

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))
(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))
(define (mobile? struct) (pair? struct))


(define lb (make-branch 3 8))
(define rb (make-branch 4 6))
(define sm (make-mobile lb rb))
(define (total-weight m)
  (cond
    ((null? m) 0)
    ((mobile? m) (+
                 (total-weight (branch-structure (left-branch m)))
                 (total-weight (branch-structure (right-branch m)))
    ))
    (else m)
  )
)
(define (balanced? m)
  (cond
    ((pair? m) (and 
      (balanced? (branch-structure (left-branch m)))
      (balanced? (branch-structure (right-branch m)))
      (= 
        (* (branch-length (left-branch m)) (total-weight (branch-structure (left-branch m)))) 
        (* (branch-length (right-branch m)) (total-weight (branch-structure (right-branch m))))
      )
    ))
    (else #t)
  )
)

; To change the program if the implemntation of make-mobile and make-branch
; changed to use cons, the only step required would be to adapt the selectors

"Exercise 2-30"
(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (square-tree tree)
  (map
    (lambda (sub-tree)
      (if (pair? sub-tree)
        (square-tree sub-tree)
        (square sub-tree)
      )
    )
    tree
  )
)

"Exercise 2-31"
(define (tree-map f tree)
  (map
    (lambda (sub-tree)
      (if (pair? sub-tree)
        (tree-map f sub-tree)
        (f sub-tree)
      )
    )
    tree
  )
)
(define (cube-tree tree) (tree-map (lambda (x) (* x x x)) tree))

"Exercise 2-32"
(define (subsets s)
  (if (null? s) 
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest))
    )
  )
)
; The function starts by cdr-ing all the way down to null in s 
; In the null case, we create a list with an empty list inside
; Then as we collapse back up, we take the current element, 
; make a copy of all of the subsets we've found so far and add
; the element into these duplicated subsets. In essence, we start with
; the empty subset and create new subsets by adding an element to it. 
; Then we take to the two subsets we have, and add an element to each of those.
; And so on.

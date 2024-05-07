"Exercise 3-12"
; (define x '(a b))
; (define y '(c d))
; (define z (append x y))
; (cdr x) --> (b)
; (define w (append! x y))
; (cdr x) --> (b c d)

"Exercise 3-13"
(define (last-pair x) (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x
)
; (define z (make-cycle '(a b c)))
; (last-pair z) --> infinite loop

"Exercise 3-14"
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x)
      )
    )
  )
  (loop x '())
)
; This has the effect of reversing x and returning this reversed copy, but it does ruin x,
; leaving behind (list (car x))
; See notebook for diagrams.

"Exercise 3-15"
; See diagram in notebook.

"Exercise 3-16"
(define (count-pairs-biddle x)
  (if (not (pair? x))
    0
    (+ (count-pairs-biddle (car x)) (count-pairs-biddle (cdr x)) 1)
  )
)
; See diagrams in notebooks. Will need some sort of set behavior to create these lists.
; (define x '(a b c))
; (count-pairs-biddle x) --> 3
;
; (set-car! x (cdr (cdr x))) --> ((c) b c)
; (count-pairs-biddle x) --> 4
; 
; (set-car! x 'a) --> (a b c)
; (make-cycle x)
; (count-pairs-biddle x) --> "maximum recursion depth reach"

"Exercise 3-17"
(define (visited elt container)
  (cond 
    ((null? container) #f)
    ((eq? elt (car container)) #t)
    (else (visited elt (cdr container)))
  )
)

(define (count-pairs x)
  (define y '())
  (define (traverse-counter x)
    (cond
      ((not (pair? x)) 0)
      ((visited x y) 0)
      (else 
        (set! y (cons x y))
        (+ (traverse-counter (car x)) (traverse-counter (cdr x)) 1)
      )
    )
  )
  (traverse-counter x)
)


"Exercise 3-18"
(define (contains-cycle? x)
  (define y '())
  (define (iter x)
    (cond
      ((not (pair? x)) #f)
      ((or (visited (cdr x) y) (visited (car x) y)) #t)
      (else
        (set! y (cons x y))
        (or (iter (cdr x)) (iter (car x)))
      )
    )
  )
  (iter x)
)

; Note this exercises assumes the data is in a list. So it will not find cycles if they
; are stored in sub-lists
"Exercise 3-19"
(define (contains-cycle-improved? x)
  (define (safe-cdr lst)
    (if (pair? lst)
      (cdr lst)
      '()
    )
  )
  (define (iter a b)
    (cond
      ((or (not (pair? b)) (not (pair? a))) #f)
      ((eq? a b) #t)
      ((eq? a (safe-cdr b)) #t)
      (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))
    )
  )
  (iter (safe-cdr x) (safe-cdr (safe-cdr x)))
)

(define x '(a b c))

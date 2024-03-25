; Symbols and automatic differenttiation

"Exercise 2-53"
; (list 'a 'b 'c) --> (a b c)
; (list (list 'george)) --> ((george))
; (cdr '((x1 x2) (y1 y2))) --> '((y1 y2))'
; (cadr '((x1 x2) (y1 y2))) --> (y1 y2)
; (pair? (car '(a short list) )) --> #f
; (memq 'red '((red shoes) (blue socks))) --> #f
; (memq 'red '(red shoes blue socks)) --> (red shoes blue socks)

"Exercise 2-54"
(define (equal-mine? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((null? l1) #f)
    ((null? l2) #f)
    ((and (pair? (car l1)) (pair? (car l2))) (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))
    ((pair? (car l1)) #f)
    ((pair? (car l2)) #f)
    (else (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))
  )
)

"Exercise 2-55"
; (car ''abracadabra) --> (car (quote 'abracadabra)) --> (car (quote (quote abracadabra)))
; The outer list evaluates the car function on the list argument in front of it
; LISP tries to evaluate said list which is (quote (quote abracadabra))
; Since scheme evaluates in normal order (lazy), it applies the first quote immediately
; the first quote makes (quote abracadabra) a list of two symbols 
; So now our expression is equivalent to (car (list 'quote 'abracadabra))
; car selects the head element which is 'quote which is quote


; Symbolic Differenttiation
(define (variable? e) (symbol? e))
(define (same-variable? e1 e2) 
  (and (variable? e1) (variable? e2) (eq? e1 e2))
)

(define (deriv expr var)
  (cond
    ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1 0))
    ((sum? expr) (make-sum (deriv (addend expr) var) (deriv (augend expr) var)))
    ((product? expr)
      (make-sum 
        (make-product (multiplier expr) (deriv (multiplicand expr) var)) 
        (make-product (multiplicand expr) (deriv (multiplier expr) var))
      )
    )
    ((exponentiation? expr) 
      (make-product (make-product
        (exponent expr) 
        (make-exponentiation (base expr) (- (exponent expr) 1))
      ) (deriv (base expr) var) ) 
    )
    (else (error "unknown expression type: DERIV" exp))
  )
)

"Exercise 2-56"
(define (exponentiation? expr) (and (pair? expr) (eq? (car expr) '**)))
(define (base expr) (cadr expr))
(define (exponent expr) (caddr expr))
(define (make-exponentiation base exponent)
  (cond
    ((symbol? exponent) (error "Symbolic differentiator cannot handle exponential expressions yet." ))
    ((= exponent 0) 1)
    ((= exponent 1) base)
    ((and (number? base) (= base 1)) 1)
    ((and (number? base) (number? exponent)) (power base exponent))
    (else (list '** base exponent))
  )
)
(define (power x n)
  (define (iter result n)
    (if (< n 1)
      result
      (iter (* result x) (- n 1)))
  )
  (iter 1 n)
)

"Exercise 2-57"
(define (sum? e) (and (pair? e) (eq? (car e) '+)))
(define (addend e) (cadr e))
(define (augend e) 
  (if (null? (cdddr e))
    (caddr e)
    (cons '+ (cddr e))
  )
)
(define (make-sum e1 e2) 
  (cond
    ((and (number? e1) (= e1 0)) e2)
    ((and (number? e2) (= e2 0)) e1)
    ((and (number? e1) (number? e2)) (+ e1 e2))
    (else (list '+ e1 e2))
  )
)

(define (product? e) (and (pair? e) (eq? (car e) '*)))
(define (multiplier e) (cadr e))
(define (multiplicand e) 
  (if (null? (cdddr e))
    (caddr e)
    (cons '* (cddr e))
  )
)
(define (make-product m1 m2) 
  (cond
    ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
    ((and (number? m1) (= m1 1)) m2)
    ((and (number? m2) (= m2 1)) m1)
    (else (list '* m1 m2))
  )
)

"Exercise 2-58"
; Redo the sum and multiplication forms with infix notation

; a) Assume that operations come paired and with appropriate parenthesis
(define (sum? expr)
  (and (pair? expr) (not (null? (cdr expr))) (eq? '+ (cadr expr)))
)
(define (addend expr) (car expr))
(define (augend expr) (caddr expr))
(define (make-sum e1 e2)
 (cond
    ((and (number? e1) (= e1 0)) e2)
    ((and (number? e2) (= e2 0)) e1)
    ((and (number? e1) (number? e2)) (+ e1 e2))
    (else (list e1 '+ e2))
  )
)

(define (product? expr) 
  (and (pair? expr) (not (null? (cdr expr))) (eq? (cadr expr) '*))
)
(define (multiplier expr) (car expr))
(define (multiplicand expr) (caddr expr))
(define (make-product m1 m2) 
  (cond
    ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
    ((and (number? m1) (= m1 1)) m2)
    ((and (number? m2) (= m2 1)) m1)
    (else (list m1 '* m2))
  )
)

; b) Write the appropriate selectors so that it works with regular algebraic notation 
(define (sum? expr)
  (and (pair? expr) (memq '+ expr))
)
(define (addend expr) 
  (if (eq? '+ (cadr expr)) 
    (car expr)
    (pre-memq '+ expr)
  )
)
(define (augend expr) 
  (let ((rest (cdr (memq '+ expr))))
    (if (null? (cdr rest))
      (car rest)
      rest
    )
  )
)
(define (make-sum e1 e2)
 (cond
    ((and (number? e1) (= e1 0)) e2)
    ((and (number? e2) (= e2 0)) e1)
    ((and (number? e1) (number? e2)) (+ e1 e2))
    (else (list e1 '+ e2))
  )
)

(define (product? expr) 
  (and (pair? expr) (not (null? (cdr expr))) (eq? (cadr expr) '*))
)
(define (multiplier expr) (car expr))
(define (multiplicand expr) 
  (if (null? (cdddr expr)) 
    (caddr expr)
    (cddr expr)
  )
)
(define (make-product m1 m2) 
  (cond
    ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
    ((and (number? m1) (= m1 1)) m2)
    ((and (number? m2) (= m2 1)) m1)
    (else (list m1 '* m2))
  )
)

(define (pre-memq item x)
  (cond
    ((null? (cdr x)) #f)
    ((eq? item (car x)) ())
    (else (cons (car x) (pre-memq item (cdr x))))
  )
)

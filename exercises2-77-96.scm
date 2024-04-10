; mit-scheme does not come equipped with get and put
; the functions below emulate having an operation table with get and put
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))



; Set up for creating tagged data and generic operations
(define (attach-tag tag contents) 
  (if (eq? tag 'scheme-number)
    contents
    (cons tag contents)
  )
)
(define (type-tag datum) 
  (cond
    ((number? datum) 'scheme-number)
    ((pair? datum) (car datum))
    (else (error "Bad tagged data: TYPE-TAG" datum))
  )
)
(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "Bad tagged data: CONTENTS" datum))
  )
)

; Create Ordinary Number Package
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'neg '(scheme-number) (lambda (x) (tag (- x))))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done
)

; Create Rational Number Package
(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x))

  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))
    )
  )

  (define (add-rat x y)
    (make-rat
      (+ 
        (* (numer x) (denom y)) 
        (* (numer y) (denom x))
      ) 
      (* (denom x) (denom y))
    )
  )
  (define (sub-rat x y)
    (make-rat
      (- 
        (* (numer x) (denom y)) 
        (* (numer y) (denom x))
      ) 
      (* (denom x) (denom y))
    )
  )
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) (* (denom x) (denom y)))
  )
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y)) (* (numer y) (denom x)))
  )
  (define (neg-rat x)
    (make-rat (- (numer x)) (denom x))
  )

  (define (equal-rat? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x)))
  )

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'neg '(rational) (lambda (x) (tag (neg-rat x))))
  (put 'equ? '(rational rational) (lambda (x y) (equal-rat? x y)))
  (put '=zero? '(rational) (lambda (x) (= 0 (numer x))))
  (put 'numer '(rational) (lambda (x) (numer x)))
  (put 'denom '(rational) (lambda (x) (denom x)))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
)


; Install Complex Packages (including Polar and Rectangular)
(define (install-rectangular-package)
  (define (tag x) (attach-tag'rectangular x))

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) 
    (atan (imag-part z) (real-part z))
  )
  
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-real-imag r a))))  
)
(define (install-polar-package)
  (define (tag x) (attach-tag 'polar x))

  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (make-from-real-imag x y) ((sqrt (+ (square x) (square y))) (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
)

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  (define (make-from-real-imag x y) 
    ((get 'make-from-real-imag 'rectangular) x y)
  )
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)
  )

  (define (tag x) (attach-tag 'complex x))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2)))
  )
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2)))
  )
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2)))
  )
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2)))
  )
  (define (neg-complex z)
    (make-from-real-imag (- (real-part z)) (- (imag-part z)))
  )
  (define (equal-complex? z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))
  )

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'neg '(complex) (lambda (z) (tag (neg-complex z))))
  (put 'equ? '(complex complex) (lambda (z1 z2) (equal-complex? z1 z2)))
  (put '=zero? '(complex) (lambda (z) (= 0 (magnitude z))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
)


; Install Packages and Define User Interface
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg x) (apply-generic 'neg x))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))
(define (real z) (apply-generic 'real-part z))
(define (imag z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-scheme-number x) ((get 'make 'scheme-number) x))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang x y) ((get 'make-from-mag-ang 'complex) x y))

; Test variables
(define x (make-complex-from-real-imag 3 4))
(define i (make-complex-from-real-imag 0 1))
(define y (make-complex-from-mag-ang 1 (/ 3.14159 4)))

"Exercise 2-77"
; Currently, I think you also have to make sure end definitions for magnitude using
; apply generic. Similar to having add, sub, mul, div
; 
; As currently written, when you do (magnitude x) as a user. We start by calling apply generic
; to select the complex magnitude funciton.
;
; However, looking into that package, we call apply generic to define the local magnitude
; procedure. Then looking in the body of that function for rectangular parts. We call local
; real and imaginary parts to actually compute the answer.
;
; So in total, there are 2 calls.

"Exercise 2-78"
; When creating an ordinary number, need to check if the tag is scheme number and then 
; only return the value.
; When finding the type of a plain number without a tag, check if the datum is a singular number
; (unpaired) and return the 'scheme-number tag
; When finding the contents, check if it is a plain number without a tag (unpaired) and return
; the value. See updated definitions above.

"Exercise 2-79"
; See internal definitions added to each package. Very susceptible to floating point 
; equality errors. For example (equ? x (mul x (1 + 0i))) need not be true
(define (equ? x y) (apply-generic 'equ? x y))

"Exercise 2-80"
; See internal definitions added to each package.
(define (=zero? x) (apply-generic '=zero? x))


; Coercion and Casting
;
; mit-scheme does not come equipped with get-coercion and put-coercion
; the functions below emulate having an operation table with get and put
(define coercion-array '())

(define (put-coercion op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! coercion-array (put-helper (list op type) coercion-array)))

(define (get-coercion op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) coercion-array))

; Move apply-generic down here and re-write to include recursion attempts
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let 
            (
              (type1 (car type-tags)) 
              (type2 (cadr type-tags)) 
              (a1 (car args)) 
              (a2 (cadr args))
            )
            (if (not (eq? type1 type2))
              (let
                (
                  (t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1))
                )
                (cond
                  (t1->t2 (apply-generic op (t1->t2 a1) a2))
                  (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                  ((and (get-coercion type1 'RAISE) (get-coercion type2 'RAISE))
                    (let 
                      (
                        (way1 (tower-coercion a1 a2))
                        (way2 (tower-coercion a2 a1))
                      )
                      (cond
                        (way1 (apply-generic op (car way1) (cadr way1)))
                        (way2 (apply-generic op (car way2) (cadr way2)))
                      )
                    )
                  )
                  (else (error "No method listed for these types 1: APPLY-GENERIC" (list op type-tags)))
                )
              )
              (error "No method listed for these types 2: APPLY-GENERIC" (list op type-tags))
            )
          )  
          (error "No method listed for these types 3: APPLY-GENERIC" (list op type-tags))
        )
      )
    )
  )
)

"Exercise 2-81"
; a) The procedure first tries to lookup an exp procedure
; with two complex arguments. It however, cannot find one
; in the procedure table. So it attempts to try casting
; It will then get the two types and look for functions which
; cast them into each other. If Louis has added a coercion 
; for complex to complex, then it will successfully cast the 
; first element two itself as a complex number. It will then call
; apply-generic again with the arguments unchanged. So it will get
; caught in a recursion loop forever.

; b) Without having identity conversions in our coercion table
; apply-generic will correctly identify that there is no way to 
; to run the operation with these arguments. However, it will
; waste time looking up these identity coercions. So the output
; of the procedure is fine; however, it is inefficient.

; c) See updated definition.

"Exercise 2-82"
; There is no clean way of treating a generic list of objects. In general, if you have
; n arguments, with k_j different possible conversions of argument j based on its type,
; then you are required to search all prod_1^n k_j combinations. You can use some nice
; strategies to try and trim this search, but it will will be on the order of k^n. You also
; need to maintain internal lists of what type each other type can be converted into.


"Exercise 2-83"
(define (raise-scheme n)
  (if (exact? n)
    (make-rational n 1)
    (make-complex-from-real-imag n 0)
  )
)
(define (raise-rational r) (inexact (/ (numer r) (denom r))))
(put-coercion 'scheme-number 'RAISE raise-scheme)
(put-coercion 'rational 'RAISE raise-rational)
(put-coercion 'complex 'RAISE (lambda (x) x))  ; Identity raise for top of the tower

(define (raise x)
  (let ((type (type-tag x)))
    (let ((proc (get-coercion type 'RAISE)))
      (if proc
        (proc x)
        (error "Type not member of tower type structure. RAISE" (list x type))
      )
    )
  )
)

"Exercise 2-84"
(define (tower-coercion x y)
  (let ((new (raise x)))
    (cond
      ((eq? (type-tag new) (type-tag x)) #f)
      ((eq? (type-tag new) (type-tag y)) (list new y))
      (else (tower-coercion new y))
    )
  )
)
; Please see (apply-generic) definition for the rest of the implementation



"Exercise 2-85"
(define (project-complex z) (inexact (real z)))
(define (project-real x)
  (if (exact? x)
    x
    (make-rational (round (* 10 10 10 10 10 10 x)) (* 10 10 10 10 10 10))
  )
)
(define (project-rational x) (exact (round (/ (numer x) (denom x)))))
(put-coercion 'complex 'PROJECT project-complex)
(put-coercion 'scheme-number 'PROJECT project-real)
(put-coercion 'rational 'PROJECT project-rational)

(define (project x)
  (let ((type (type-tag x)))
    (let ((proc (get-coercion type 'PROJECT)))
      (if proc
        (proc x)
        (error "Type not member of tower structure. PROJECT" (list x type))
      )
    )
  )
)

(define (drop x)
  (let ((p (project x)))
    (if (equ? x (raise p))
      (drop p)
      x
    )
  )
)

; Very easy to install drop because it only needs to be used in the apply line.

"Exercise 2-86"
; To update the complex package, in the definitions, all +, * need to be replaced with add and mul
; Additionally, you would need to define generic trig functions which take type T to type T.
; This is not guaranteed by any mathematical implementation. So functionally, you would have to tag every coefficient
; unless it happened to be a real number. When you tag these coefficients, you might write (sine 1) as the imaginary 
; part of some number. I will not be installing this change because I think it reduces the beauty of this program.
; If you want to do something like this, well, you should define clear parameters on when you want sine to approximate a value
; and when you want to represent the number abstractly.


;---------------------------------------------------------------------------------------------------------------------------

; Term Representation
;(define (install-term-package)
;  (define (tag x) (attach-tag 'term x))
;
;  (define (make-term order coeff) (list order coeff))
;  (define (order term) (car term))
;  (define (coeff term) (cadr term))
;
;  (put 'make '(term) (lambda (o c) (tag (make-term o c))))
;  (put 'order '(term) order)
;  (put 'coeff '(term) coeff)
;)

; Dense Representation
(define (install-dense-package)
  (define (tag x) (attach-tag 'dense x))
 
  ; Term getters
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (first-term tl) (list (- (length tl) 1) (car tl)))
  (define (rest-terms tl) (cdr tl))
  (define (adjoin-term term tl) 
    (cond 
      ((=zero? (coeff term)) tl)
      ((= 0 (length tl)) (list (coeff term)))
      ((= (order term) (+ (order (first-term tl)) 1)) (cons (coeff term) tl))
      (else (adjoin-term term (cons 0 tl)))
    )
  )

  (put 'empty-term-list? '(dense) (lambda (tl) (null? tl)))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (lambda (tl) (tag (rest-terms tl))))
  (put 'adjoin-term '(term dense) (lambda (term tl) (tag (adjoin-term term tl))))
  (put 'make '(dense) (lambda (tl) (tag tl)))
)

; Sparse Package
(define (install-sparse-package)
  (define (tag x) (attach-tag 'sparse x))

  ; Term getters
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (first-term tl) (car tl))
  (define (rest-terms tl) (cdr tl))
  (define (adjoin-term term tl)
    (if (=zero? (coeff term))
      tl
      (cons term tl)
    )
  )
  
  (put 'empty-term-list? '(sparse) (lambda (tl) (null? tl)))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) (lambda (tl) (tag (rest-terms tl))))
  (put 'adjoin-term '(term sparse) (lambda (term tl) (tag (adjoin-term term tl))))
  (put 'make '(sparse) (lambda (tl) (tag tl)))
)

; Polynomial Representation
(define (install-polynomial-package)
  (define (tag x) (attach-tag 'polynomial x))

  ; Install Term List Representations
  ;(install-term-package)
  (define (make-term o c) (attach-tag 'term (list o c)))  ; This should only be called by adjoin-term which will strip the tag
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ; Note: internal term definitions to the term-list packages will refer to the definitions
  ; above because they aren't defined in the package body, and therefore search upward by 
  ; lexical scoping.
  (install-sparse-package)
  (install-dense-package)
  (define (the-empty-term-list) '(sparse))
  (define (empty-term-list? tl) (apply-generic 'empty-term-list? tl))
  (define (first-term tl) (apply-generic 'first-term tl))
  (define (rest-terms tl) (apply-generic 'rest-terms tl))
  (define (adjoin-term term tl) (apply-generic 'adjoin-term term tl)) 
  
  ; Polynomial Selectors and Constructors 
  (define (variable? x) (symbol? x))
  (define (same-variable? x1 x2) (and (variable? x1) (variable? x2) (eq? x1 x2)))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))
  ;(define (make-poly variable tagged-tl) (list variable (apply-generic 'make tagged-tl)))
  (define (make-poly variable tagged-tl) (list variable tagged-tl))
  (define (make-from-sparse variable tl) (list variable ((get 'make '(sparse)) tl)))
  (define (make-from-dense variable tl) (list variable ((get 'make '(dense)) tl)))

  ; Polynomial Arithmetic
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in the same variable: ADD-POLY" (list p1 p2))
    )
  )
  (define (add-terms L1 L2)
    (cond
      ((empty-term-list? L1) L2)
      ((empty-term-list? L2) L1)
      (else
        (let 
          (
            (t1 (first-term L1))
            (t2 (first-term L2))
          )
          (cond
            ((> (order t1) (order t2)) (adjoin-term (attach-tag 'term t1) (add-terms (rest-terms L1) L2)))
            ((< (order t1) (order t2)) (adjoin-term (attach-tag 'term t2) (add-terms L1 (rest-terms L2))))
            (else 
              (adjoin-term 
                    (make-term (order t1) (add (coeff t1) (coeff t2))) 
                    (add-terms (rest-terms L1) (rest-terms L2))
              )
            )
          )
        )
      )
    )
  )

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in the same variable: ADD-POLY" (list p1 p2))
    )
  )
  (define (mul-terms L1 L2)
    (if (empty-term-list? L1)
      (the-empty-term-list)
      (add-terms 
        (mul-term-by-all-terms (first-term L1) L2)
        (mul-terms (rest-terms L1) L2)
      )
    )
  )
  (define (mul-term-by-all-terms t1 L)
    (if (empty-term-list? L)
      (the-empty-term-list)
      (adjoin-term 
        (make-term (+ (order t1) (order (first-term L))) (mul (coeff t1) (coeff (first-term L)))) 
        (mul-term-by-all-terms t1 (rest-terms L))
      )
    )
  )
  (define (neg-poly p)
    (make-poly (variable p) (negate-terms (term-list p)))
  )
  (define (negate-terms tl)
    (if (empty-term-list? tl)
      (the-empty-term-list)
      (let ((ft (first-term tl)))
        (adjoin-term (make-term (order ft) (neg (coeff ft))) (negate-terms (rest-terms tl)))
      )
    )
  )
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2))
  )
  (define (div-poly p1 p2)
    (cond
      ((zero-poly? p2) (error "Divide by zero-poly: DIV-POLY" (list p1 p2)))
      ((not (same-variable? (variable p1) (variable p2))) (error "Polys not in the same variable: DIV-POLY" (list p1 p2)))
      (else (make-poly (variable p1) (car (div-terms (term-list p1) (term-list p2)))))
    )
  )
  (define (div-terms L1 L2)
    (if (empty-term-list? L1)
      (list (the-empty-term-list) (the-empty-term-list))
      (let 
        (
          (t1 (first-term L1))
          (t2 (first-term L2))
        )
        (if (> (order t2) (order t1))
          (list (the-empty-term-list) L1)
          (let 
            (
              (new-c (div (coeff t1) (coeff t2)))
              (new-o (- (order t1) (order t2)))
            )
            (let ((product (mul-term-by-all-terms (list new-o new-c) L2)))
              (let ((new_L1 (add-terms L1 (negate-terms product))))
                (let ((rest-of-result (div-terms new_L1 L2)))
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                )
              )
            )
          )
        )
      )
    )
  )

  (define (zero-poly? p) (empty-term-list? (term-list p)))

  ; Upload to system
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make-from-sparse 'polynomial (lambda (var terms) (tag (make-from-sparse var terms))))
  (put 'make-from-dense 'polynomial (lambda (var terms) (tag (make-from-dense var terms))))
  (put '=zero? '(polynomial) zero-poly?)
)

(install-polynomial-package)
(define (make-polynomial-from-sparse var terms) ((get 'make-from-sparse 'polynomial) var terms))
(define (make-polynomial-from-dense var terms) ((get 'make-from-dense 'polynomial) var terms))
; Coercions - constant polynomials
(define (constant-polynomial num) 
  (if (=zero? num)
    (make-polynomial-from-sparse 'x ())
    (make-polynomial-from-sparse 'x (list (list 0 num)))
  )
)
; TODO: still creates polynomials tied to x - shouldn't matter what the "variable" is
(put-coercion 'scheme-number 'polynomial constant-polynomial)
(put-coercion 'rational 'polynomial constant-polynomial)
(put-coercion 'complex 'polynomial constant-polynomial)


; Used for testing
(define sp (make-polynomial-from-sparse 'x (list (list 100 1) (list 2 2) (list 0 1))))
(define dp (make-polynomial-from-dense 'x (list 1 1)))


"Exercise 2-87"
; Please see package for implementation

"Exercise 2-88"
; This will entail creating negation for all numbers --> see packages for implementation details
; Now see polynomial package for subtraction details

"Exercise 2-89"
; Need to separate our term list representations. A lot should be similar

"Exercise 2-90"
; The hard decision is to decide when we should be creating a sparse or dense polynomial representation 
; Additionally, we need to be able to cast between different types

"Exercise 2-91"
; Please see the Polynomial package for the div-poly and div-term procedure definitions.

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
    (cond ((null? array) )
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

  (define (equal-rat? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x)))
  )

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) (lambda (x y) (equal-rat? x y)))
  (put '=zero? '(rational) (lambda (x) (= 0 (numer x))))
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
  (define (equal-complex? z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))
  )

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method listed for these types: APPLY-GENERIC" (list op type-tags))
      )
    )
  )
)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
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

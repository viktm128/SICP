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


"Exercise 2-73"

; a) The derivative function was reworked so that when taking the derivative
; of any compound expressions, it will lookup the appropriate expressions in 
; an operation table of some sorts. In particular, it looks up how to handle expressions
; which start with + or *. These operators function as tags on the expression data.
;
; We can't include number and variable because they are based off primitives number? 
; and symbol? and we would then have to take every number and variable in our expression.
(define (variable? e) (symbol? e))
(define (same-variable? e1 e2) 
  (and (variable? e1) (variable? e2) (eq? e1 e2))
)

(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (deriv expr var)
  (cond
    ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1 0))
    (else 
      (let ((op (get 'deriv (operator expr))) (args (operands expr)))
        (if op
          (op args var)
          (error "unknown expression type: DERIV" exp)
        )
      )
    )
    ;((product? expr)
    ;  
    ;)
    
    ;(else (error "unknown expression type: DERIV" exp))
  )
)


; b) Install sums and Products

(define (install-sums)
  (define (addend e) (car e))
  (define (augend e) 
    (if (null? (cddr e))
      (cadr e)
      (cons '+ (cdr e))
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

  (define (derive-sum expr var)
    (make-sum (deriv (addend expr) var) (deriv (augend expr) var))
  )

  (display "installing sums...") (newline)
  (put 'make '+ make-sum)
  (put 'deriv '+ derive-sum)
  'done
)


(define (install-products)
  (define ms (get 'make '+))
  (define (multiplier e) (car e))
  (define (multiplicand e) 
    (if (null? (cddr e))
      (cadr e)
      (cons '* (cdr e))
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

  (define (derive-product expr var)
    (ms
      (make-product (multiplier expr) (deriv (multiplicand expr) var)) 
      (make-product (multiplicand expr) (deriv (multiplier expr) var))
    )
  )

  (display "installing products...") (newline)
  (put 'make '* make-product)
  (put 'deriv '* derive-product)
  'done!
)

; c) Install exponentiation

(define (install-monomial)
  
  (define mp (get 'make '*))
  (define (base expr) (car expr))
  (define (exponent expr) (cadr expr))
  (define (make-monomial base exponent)
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

  (define (derive-monomial expr var)
    (mp
      (mp
        (exponent expr) 
        (make-monomial (base expr) (- (exponent expr) 1))
      ) (deriv (base expr) var) 
    ) 
  )
 
  (display "installing monomials...") (newline)
  (put 'make '** make-monomial)
  (put 'deriv '** derive-monomial)
  'done!
)

; d) The only thing that would need to change are put and get calls throughout the program
; - this is a little annoying because everyone would have to make minor refactors to their code


"Exercise 2-74"
; a) Lets assume that divisions all create a method called get-record specific to their file
; which takes in an employee name and returns the record. If the record doesn't exist, it 
; should return false.
; Divisions should "put" the procedure in the operation table under the tag 'get record and 
; some specific unique division tag
; Additionally, all division files need to be tagged with this identifier
(define (get-record e_name division_file)
  (define dgr (get (tag-type division_file) 'get-record))
  (dgr e_name division_file)
)

; b) All of the records from a specific division file should be tagged with the 
; same division identifier as above. Additionally, each division should put a 
; procedure which takes in a division specific record and outputs the salary

(define (get-salary e_record)
  ((get (tag-type e_record) 'get_salary) e_record)
)

; c) Assume files is an unordered list. 
(define (find-employee-record e_name files)
  (if (null? files) 
    #f
    (let ((flag (get-record e_name (car files))))
      (if flag
        flag
        (find-employee-record e_name (cdr files))
      )
    )
  )
)

;d) The acquired company must tag all of their personnel records and their 
; personnel file with a unique tag. Additionally, they must add a 'get-salary
; and 'get-record procedure into the central operation table.
; Lastly, central office, must add their division to their central list of files


"Exercise 2-75"
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond
      ((eq? op 'real) (* r (cos a)))
      ((eq? op 'imag) (* r (sin a)))
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      (else (error "Uknown op: MAKE-FROM-MAG-ANG" op))
    )
  )
  dispatch
)

"Exercise 2-76"
; Generic Operations with Explicit Dispatch: 
; When we add new operations, we simply need to write an operation which works 
; for each data type. This can all be done in one place
; To add a new data type, you need to go to each previous operation and define
; how it will work on the new data type
;
; Data Driven: If you add a new data object, you need to write an install-package method
; in that method, you need you will need to define how procedures hand this object.
; This is all contained to this one package. To add a new operation, you will have to update
; each data's install-package procedure
;
; Message Passing: If you want to add a new operation, you need to edit every data objects 
; representation because you'll need to add a new condition to the internal dispatch definition
; To add a new data object, you need to write a single procedure which creates a dispatch based
; on the different operations.
;
; If you are going to be adding a lot of data objects, choose data driven or message passing.
; If you are going to be adding a lot of operations, you should use generic operations with 
; explicit dispatch.



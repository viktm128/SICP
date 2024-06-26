; Model the Celsius to Fahrenheit Conversion Equation

; Representing Connectors in Terms of Primatives
(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value new-val setter)
      (cond
        ((not (has-value? me))
          (set! value new-val)
          (set! informant setter)
          (for-each-except setter inform-about-value constraints)
        )
        ((not (= value new-val)) (error "Contradiction" (list value new-val)))
        (else 'ignored)
      )
    )
    (define (forget-my-value rectractor)
      (if (eq? rectractor informant)
        (begin
          (set! informant #f)
          (for-each-except rectractor inform-about-no-value constraints)
        )
        'ignored
      )
    )
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints))
      )
      (if (has-value? me)
        (inform-about-value new-constraint)
      )
      'done
    )

    (define (me request)
      (cond
        ((eq? request 'has-value?) (if informant #t #f))
        ((eq? request 'value) value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else (error "Unknown operation: CONNECTOR" request))
      )
    )
    me
  )
)


(define (for-each-except exception proc items)
  (define (loop things)
    (cond
      ((null? things) 'done)
      ((eq? exception (car things)) (loop (cdr things)))
      (else 
        (proc (car things))
        (loop (cdr things))
      )
    )
  )
  (loop items)
)

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant) ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))


; Representing Constraints in Terms of Connectors
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
        (set-value! sum (+ (get-value a1) (get-value a2)) me)
      )
      ((and (has-value? a1) (has-value? sum))
        (set-value! a2 (- (get-value sum) (get-value a1)) me)
      )
      ((and (has-value? a2) (has-value? sum))
        (set-value! a1 (- (get-value sum) (get-value a2)) me)
      )
    )
  )

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value)
  )

  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: ADDER" request))
    )
  )

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me
)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      ((or (and (has-value? m1) (= (get-value m1) 0)) (and (has-value? m2) (= (get-value m2) 0)))
        (set-value! product 0 me)
      )
      ((and (has-value? m1) (has-value? m2))
        (set-value! product (* (get-value m1) (get-value m2)) me)
      )
      ((and (has-value? m1) (has-value? product))
        (set-value! m2 (/ (get-value product) (get-value m1)) me)
      )
      ((and (has-value? m2) (has-value? product))
        (set-value! m1 (/ (get-value product) (get-value m2)) me)
      )
    )
  )

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value)
  )

  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: MULTIPLIER" request))
    )
  )

  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me
)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request)
  )
  (connect connector me)
  (set-value! connector value me)
  me
)

(define (inform-about-value constraint) (constraint 'I-have-a-value))
(define (inform-about-no-value constraint) (constraint 'I-lost-my-value))


; Representing Simulation in Terms of Constraints
(define C (make-connector))
(define F (make-connector))
(define (celsius-fahrenheit-converter c f)
  (let 
    (
      (u (make-connector))
      (v (make-connector))
      (x (make-connector))
      (y (make-connector))
      (w (make-connector))
    )
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok
  )
)
(celsius-fahrenheit-converter C F)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value)
  )
  (define (process-new-value)
    (print-probe (get-value connector))
  )
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: PROBE" request))
    )
  )
  (connect connector me)
  me
)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)



"Exercise 3-33"
(define (averager a b c)
  (let 
    (
      (s (make-connector))
      (const (make-connector))
    )
    (adder a b s)
    (multiplier s const c)
    (constant 0.5 const)
    'ok
  )
)

"Exercise 3-34"
; If you try to define squarer that way, suppose you set the value of b. You should expect the value of a to automatically
; update as it is the square root of b and there is only one unknown in this situation. However, it can't because multiplier
; believes there are two unique positional unknowns, neither of which have values, even though they need to be the same value.
; Multiplier is not equipped to compute the square root of b.

"Exercise 3-35"
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0: SQUARER" (get-value b))
        (set-value! a (sqrt (get-value b)) me)
      )
      (if (has-value? a)
        (set-value! b (square (get-value a)) me)
      )
    )
  )
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value)
  )
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: SQUARER" request))
    )
  )

  (connect a me)
  (connect b me)
  me
)

"Exercise 3-36"
; See diagram in notebook.

"Exercise 3-37"
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z
  )
)
(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z
  )
)
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z
  )
)
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z
  )
)
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z
  )
)

(define (new-celsius-fahrenheit-converter x)
  (c+ 
    (c* 
      (c/ 
        (cv 9) 
        (cv 5)
      )
      x
    )
    (cv 32)
  )
)
(define C-new (make-connector))
(define F-new (new-celsius-fahrenheit-converter C-new))

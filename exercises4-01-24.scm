"Exercise 4-1"
(define (list-of-values-l-to-r exprs env)
  (if (no-operands? exprs) 
    '()
    (let ((first (eval-me (first-operand exprs) env)))
      (cons first (list-of-values-l-to-r (rest-operands exprs) env))
    )
  )
)

(define (list-of-values-r-to-l exprs env)
  (if (no-operands? exprs)
    '()
    (let ((rest (list-of-values-r-to-l (rest-operands exprs) env)))
      (cons (eval-me (first-operand exprs) env) rest)
    )
  )
)


"Exercise 4-2"
; a) If Louis moves the clause for procedure application to the beginning (above assignment 
; and definition), it will trigger before it checks if there is a specific key word which
; needs special processing. In particular, it will think 'define' is a procedure command and look 
; to apply the procedure named "define" to the subsequent arguments --> some of which do not 
; exist yet.
;
; b) The only thing you need to change is the application helpers. In particular 

(define (application? expr) (tagged-list? expr 'call))
(define (operator expr) (cadr expr))
(define (operands expr) (cddr expr))

; The operands procedures do not need to be rewritten.

"Exercise 4-3"
; Please see metacircular_evaluator.scm --> The definitions will all be changed. Please see old 
; definition below.
(define (eval-me expr env)
  (cond 
    ((self-evaluating? expr) expr)
    ((variable? expr) (lookup-variable-value expr env))
    ((quoted? expr) (text-of-quotation expr))
    ((assignment? expr) (eval-assignment expr env))
    ((definition? expr) (eval-definition expr env))
    ((if? expr) (eval-if expr env))
    ((lambda? expr) (make-procedure (lambda-parameters expr) (lambda-body expr) env))
    ((begin? expr) (eval-sequence (begin-actions expr) env))
    ((cond? expr) (eval-me (cond->if expr) env))
    ((application? expr) 
     (apply-me (eval-me (operator expr) env) (list-of-values (operands expr) env))
    )
    (else (error "Unknown expression type." expr))
  )
)

"Exercise 4-4"
; Please see logical packages in metacircular_evaluator

"Exercise 4-5"
; Please see updated cond package definition 

"Exercise 4-6"
; Please see let package in metacircular_evaluator

"Exercise 4-7"
; Please see updated let package definition

"Exercise 4-8"
; Please see updated let package definition. This is so messy. Will definitely need debugging.

"Exercise 4-9"
; Please see loops package. Only implementing a basic while loop for now.
; Currently, it is not actually written in a way in which tail recursion happens. So needs to be adjusted at some point

"Exercise 4-10"
; Skip!


"Exercise 4-11"
; Re represent frames and environments where frames contain lists of name-value pairs
(define (make-frame variables vals) (map cons variables vals))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (cons frame (cons var val))
)

; The basic environment procedures do not need to change
; as they are about the structure of frames within an environment
; which is not changing here. They are omitted, but can be found
; in the metacircular_evaluator for reference.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond 
        ((null? frame) (env-loop (enclosing-environment env)))
        ((eq? var (car (first-binding frame))) (cdr (first-binding frame)))
        (else (scan (rest-bindings frame)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (first-frame env))
    )
  )
  (env-loop env)
)

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond 
        ((null? frame) (env-loop (enclosing-environment env)))
        ((eq? var (car (first-binding frame))) (set-car! frame (cons var val)))
        (else (scan (rest-bindings frame)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (scan (first-frame env))
    )
  )
  (env-loop env)
)

(define (define-variable! var val env)
  (define (scan frame)
    (cond
      ((null? frame) (add-binding-to-frame! var val frame))
      ((eq? var (car (first-binding frame))) (set-car! frame (cons var val)))
      (else (scan (rest-bindings frame)))
    )
  )
  (scan (first-frame env))
)


"Exercise 4-12"
; Old definitions are given here.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
        ((null? vars) (env-loop (enclosing-environment env)))
        ((eq? var (car vars)) (car vals))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame) (frame-values frame))
      )
    )
  )
  (env-loop env)
)

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
        ((null? vars) (env-loop (enclosing-environment env)))
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame) (frame-values frame))
      )
    )
  )
  (env-loop env)
)

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond 
        ((null? vars) (add-binding-to-frame! var val frame))
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (scan (frame-variables frame) (frame-values frame))
  )
)


"Exercise 4-13"
; Probably never going to be used, but see metacircular_evaluator.

"Exercise 4-14"
; If you write a version of map within the metacircular_evaluator, the argument to map which is itself a procedure can
; be both primitive and compound. However, if you try to install the native map directly, when it is called on a user defined
; function, the native map will try to "apply" an object created by 'make-procedure' in the underlying lisp. However, native map 
; probably doesn't assume that procedures are stored the way we have them stored in our metacircular_evaluator so it doesn't know 
; what to do with such a an object.

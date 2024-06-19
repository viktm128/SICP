; Get and Put Operations Table for Evaluator
(define eval-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! eval-array (put-helper (list op type) eval-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) eval-array))


; Represent a Metacircular Evaluator of Scheme
(define (eval-me expr env)
  (cond
    ((self-evaluating? expr) expr)
    ((variable? expr) (lookup-variable-value expr env))
    (else 
      (let ((op (get 'eval (car expr))))
        (if op 
          (op expr env)
          (if (pair? expr)
            (apply-me (eval-me (operator expr) env) (list-of-values (operands expr) env))
            (error "Unknown expression type." expr)
          )
        )
      )
    )
  )
)


(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (list-of-values exprs env)
  (if (no-operands? exprs) 
    '()
    (cons (eval-me (first-operand exprs) env) (list-of-values (rest-operands exprs) env))
  )
)
(define (apply-me proc args)
  (cond 
    ((primitive-procedure? proc) (apply-primitive-procedure proc args))
    ((compound-procedure? proc) 
      (eval-me
        ((get 'make 'begin) (procedure-body proc)) 
        (extend-environment 
          (procedure-paramters proc) 
          args 
          (procedure-environment proc)
        )
      )
    )
    (else (error "Unknown procedure type." proc))
  )
)

; Install Packages --------------------------------------------------------------------------------------------------------

; Quotes Package
(define (install-quote-package)
  (define (text-of-quotation expr env) (cadr expr))

  (put 'eval 'quote text-of-quotation)
  'ok
)

; Assignment Package
(define (install-assignment-package)
  (define (assignment-variable expr) (cadr expr))
  (define (assignment-value expr) (caddr expr))
  (define (eval-assignment expr env)
    (set-variable-value! (assignment-variable expr) (eval-me (assignment-value expr) env) env)
    'ok
  )
  
  (put 'eval 'set! eval-assignment)
  'ok
)

; If Package 
(define (install-if-package)
  (define (if-predicate expr) (cadr expr))
  (define (if-consequent expr) (caddr expr))
  (define (if-alternative expr)
    (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false
    )
  )
  
  (define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))
  (define (eval-if expr env)
    (if (true? (eval-me (if-predicate expr) env))
      (eval-me (if-consequent expr) env)
      (eval-me (if-alternative expr) env)
    )
  )

  (put 'eval 'if eval-if)
  (put 'make 'if make-if)
  'ok
)

; Lambda Package
(define (install-lambda-package)
  (define (lambda-parameters expr) (cadr expr))
  (define (lambda-body expr) (cddr expr))

  (define (make-lambda params body) (list 'lambda params body))
  (define (eval-lambda expr env)
    (make-procedure (lambda-parameters expr) (lambda-body expr) env)
  )

  (put 'eval 'lambda eval-lambda)
  (put 'make 'lambda make-lambda)
  'ok
)

; Definition Package (requires lambda package installed first)
(define (install-definition-package)
  (define (definition-variable expr)
    (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)
    )
  )
  (define (definition-value expr)
    (if (symbol? (cadr expr))
      (caddr expr)
      ((get 'make 'lambda) (cdadr expr) (caddr expr))  ; TODO: confirm this is a error in the book and that this is well behaved.
    )
  )
  (define (eval-definition expr env)
    (define-variable! (definition-variable expr) (eval-me (definition-value expr) env) env)
    'ok
  )

  (define (make-definition var expr) (list 'define var expr))
 
  (put 'eval 'define eval-definition)
  (put 'make 'define make-definition)
  'ok
)


; Sequence Package
(define (install-sequence-package)
  (define (actions expr) (cdr expr))
  (define (last-action? seq) (null? (cdr seq)))
  (define (first-action seq) (car seq))
  (define (rest-actions seq) (cdr seq))

  ;TODO big problem here - need to switch exprs with (actions exprs) or something like that
  (define (eval-actions exprs env)
    (cond 
      ((last-action? exprs) (eval-me (first-action exprs) env))
      (else 
        (eval-me (first-action exprs) env)
        (eval-actions (rest-actions exprs) env)
      )
    )
  )
  (define (eval-begin expr env) (eval-actions (actions expr) env))
  (define (make-begin seq) (cons 'begin seq))

  (put 'eval 'begin eval-begin)
  (put 'make 'begin make-begin)
)


; Conditions Package
(define (install-cond-pacakge)
  (define (cond-clauses expr) (cdr expr))
  (define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
  (define (clause-type-test? clause) (eq? (cadr clause) '=>))
  (define (cond-test clause) (car clause))
  (define (cond-test-proc clause) (caddr clause))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (cond->if expr) (expand-clauses (cond-clauses expr)))
  (define (expand-clauses clauses)
    (if (null? clauses)
      'false
      (let 
        (
          (first (car clauses))
          (rest (cdr clauses))
          (sequence->expr (get 'make 'begin))
          (make-if (get 'make 'if))
        )
        (if (cond-else-clause? first)
          (if (null? rest)
            (sequence->expr (cond-actions first))
            (error "Else clause is not the last option.")
          )
          (if (clause-type-test? first)
            (make-if 
              (cond-test first)
              (list (cond-test-proc first) (cond-test first))  ; create expression which looks like (proc condition-argument)
              (expand-clauses rest)
            )
            (make-if 
              (cond-predicate first)
              (sequence->expr (cond-actions first))
              (expand-clauses rest)
            )
          )
        )
      )
    )
  )

  (define (eval-cond expr env) (eval-me (cond->if expr) env))
  (put 'eval 'cond eval-cond)
)


; Logical Operators
(define (install-logical-package)
  (define (predicates expr) (cdr expr))
  (define (first-predicate preds) (car preds))
  (define (rest-predicates preds) (cdr preds))
  (define (last-pred? preds) (null? (cdr preds)))


  (define (eval-and expr env)
    (if (null? (predicates expr))
      (error "Logical Expression given without predicates" expr)
      (eval-and-preds (predicates expr) env)
    )
  )
  (define (eval-and-preds preds env)
    (if (true? (eval-me (first-predicate preds) env))
      (if (last-pred? preds)
        'true
        (eval-and-preds (rest-predicates preds) env)
      )
      'false
    )
  )

  (define (eval-or expr env)
    (if (null? (predicates expr))
      (error "Logical Expression given without predicates" expr)
      (eval-or-preds (predicates expr) env)
    )
  )
  (define (eval-or-preds preds env)
    (if (true? (eval-me (first-predicate preds) env))
      'true
      (if (last-pred? preds)
        'false
        (eval-or-preds (rest-predicates preds) env)
      )
    )
  )
  

  (define (eval-not expr env)
    (if (true? (eval-me (first-predicate (predicates expr)) env))
      'false
      'true
    )
  )

  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or)
  (put 'eval 'not eval-not)
)


; Let Package
(define (install-let-package)
  (define (named? expr) (symbol? (cadr expr)))
  (define (name expr) (cadr expr))
  (define (named-bindings expr) (caddr expr))
  (define (named-body expr) (cadddr expr))
  (define (bindings expr) (cadr expr))
  (define (first-binding bindings) (car bindings))
  (define (last-binding? bindings) (null? (cdr bindings)))
  (define (rest-bindings bindings) (cdr bindings))
  (define (body expr) (caddr expr))

  (define (params bindings)
    (if (null? bindings)
      '()
      (cons (caar bindings) (params (cdr bindings)))
    )
  )
  (define (binding-vals bindings)
    (if (null? bindings)
      '()
      (cons (cadar bindings) (binding-vals (cdr bindings)))
    )
  )
  (define (make-let bindings body) (list 'let (list bindings) body))

  (define (let->combination expr)
    (if (named? expr)
      ((get 'make 'begin) 
        (list 
          ((get 'make 'define) (cons (name expr) (params (named-bindings expr))) (named-body expr))
          (cons (name expr) (binding-vals (named-bindings expr)))
        )
      ) 
      (cons
        ((get 'make 'lambda) (params (bindings expr)) (body expr)) 
        (binding-vals (bindings expr))
      )
    )
  )

  (define (let*->nested-let expr)
    (expand-bindings (bindings expr) (body expr))
  )

  (define (expand-bindings bindings body)
      (if (last-binding? bindings)
        (make-let (first-binding bindings) body)
        (make-let (first-binding bindings) (expand-bindings (rest-bindings bindings) body))
      )
  )

  (define (eval-let expr env)
    (eval-me
      (let->combination expr)
      env
    )
  )

  (define (eval-let* expr env)
    (eval-me (let*->nested-let expr) env)
  )

  (put 'eval 'let eval-let)
  (put 'eval 'let* eval-let*)
)


; Loops Package
(define (install-loops-package)
  (define (condition expr) (cadr expr))
  (define (body expr) (caddr expr))

  ; this isn't actually iterative (i.e. tail recursion doesn't happen)
  (define (eval-while expr env)
    (eval-me 
      (
       (get 'make 'if) 
          (condition expr) 
          ((get 'make 'begin) (list (body expr) expr))
          'false
      )
      env
    )
  )

  (put 'eval 'while eval-while)
)

;---------------------------------------------------------------------------------------------
; Representing Expressions

(define (self-evaluating? expr)
  (cond 
    ((number? expr) #t)
    ((string? expr) #t)
    (else #f)
  )
)

(define (variable? expr) (symbol? expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
    (eq? (car expr) tag)
    #f
  )
)

(define (sequence->expr seq)
  (cond 
    ((null? seq) seq)
    ((last-action? seq) (first-action seq))
    (else (make-begin seq))
  )
)



;---------------------------------------------------------------------------------------------
; Evaluator Data Structures

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-paramters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (make-frame variables vals) (cons variables vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))
)

(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too few variables supplied." vars vals)
      (error "Too few values supplied." vars vals)
    )
  )
)


(define (env-search var env success-proc exit_proc error_message)
  (define (scan-frame vars vals frame)
    (cond
      ((null? vars) 
        (if exit_proc
          (exit_proc frame)
          (env-search var (enclosing-environment env) success-proc exit_proc error_message)
        )
      )
      ((eq? var (car vars)) (success-proc vars vals))
      (else (scan-frame (cdr vars) (cdr vals) frame))
    )
  )
  (if (eq? env the-empty-environment)
    (error error_message var)
    (let ((frame (first-frame env)))
        (scan-frame (frame-variables frame) (frame-values frame) frame)
    )
  )
)

(define (lookup-variable-value var env)
  (env-search var env (lambda (vars vals) (car vals)) #f "Unbound variable.")
)

(define (set-variable-value! var val env)
  (env-search var env (lambda (vars vals) (set-car! vals val)) #f "Unbound variable: SET!")
)

(define (define-variable! var val env)
  (env-search var env (lambda (vars vals) (set-car! vals val)) (lambda (frame) (add-binding-to-frame! var val frame)) "No error message")
)

(define (make-unbound! var env)
  (env-search var env (lambda (vars vals) (begin (set! vars (cdr vars)) (set! vals (cdr vals)))) #f "Variable already not bound")
)



; The Global Environment and Drivers
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures 
  (list 
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'list list)
    (list 'null? null?)
    (list 'cadr cadr)
    (list 'cdar cdar)
    (list 'caar caar)
    (list 'cddr cddr)
    (list 'assoc assoc)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '= =)
    (list '< <)
    (list '> >)
  )
)

(define (primitive-procecure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) 
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)
)
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args)
)


(define input-prompt  ";;; M-Eval input: ")
(define output-prompt ";;; M-Eval value: ")

(define (prompt-for-input str) (newline) (newline) (display str) (newline))
(define (announce-output str) (newline) (display str) (newline))
(define (user-print object)
  (if (compound-procedure? object)
    (display 
      (list 
        'compound-procedure 
        (procedure-paramters object)
        (procedure-body object)
        '<procedure-env>
      )
    )
    (display object)
  )
)

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read))) 
    (let ((output (eval-me input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (driver-loop)
)

(define (setup-environment)
  (let 
    (
      (initial-env 
        (extend-environment 
          (primitive-procecure-names) 
          (primitive-procedure-objects) 
          the-empty-environment
        )
      )
    )
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env
  )
)

(install-quote-package)
(install-assignment-package)
(install-if-package)
(install-lambda-package)
(install-definition-package)
(install-sequence-package)
(install-cond-pacakge)
(install-logical-package)
(install-let-package)
(install-loops-package)

(define the-global-environment (setup-environment))
(driver-loop)


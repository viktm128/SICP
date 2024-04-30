(define balance 100)
(define (withdraw amount)
    (if (>= (- balance amount) 0)
      (begin
        (set! balance (- balance amount))
        balance
      )
      "Insufficient funds."
    )
)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= (- balance amount) 0)
        (begin
          (set! balance (- balance amount))
          balance
        )
        "Insufficient Funds"
      )
    )
  )
)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= (- balance amount) 0)
      (begin
        (set! balance (- balance amount))
        balance
      )
      "Insufficient Funds"
    )
  )
)

(define (make-account balance password)
  ; Withdraw procedure
  (define (withdraw amount)
    (if (>= (- balance amount) 0)
      (begin
        (set! balance (- balance amount))
        balance
      )
      "Insufficent Funds"
    )
  )

  ; Deposit procedure
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  
  ; Message dispatcher
  (define (dispatch pass-attempt m)
    (cond
      ((not (eq? pass-attempt password)) (lambda (x) "Incorrect Password")) 
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "ERROR: unknown request in MAKE-ACOUNT"))
    )
  )

  ; Return value
  dispatch
)


"Exercise 3-1"
(define (make-accumulator total)
  (lambda (addend)
    (set! total (+ total addend))
    total
  )
)

"Exercise 3-2"
(define (make-monitored f)
  (define counter 0)
  (lambda (input)
    (if (eq? input 'how-many-calls?)
      counter
      (begin
        (set! counter (+ counter 1))
        (f input)
      )
    )
  )
)

"Exercise 3-3"
; See updated make-account definition with passwords

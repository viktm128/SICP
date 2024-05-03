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

(define (make-account balance . password)
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

  ; Add Member
  (define (add-member newp) 
    (set! password (cons newp password))
    "New Member Successfully Joined"
  )
  
  ; Security Variables
  (define incorrect-counter 0)

  ; Message dispatcher
  (define (dispatch pass-attempt m)
    (cond
      ((null? password) (error "Error: This account must be password protected." password))
      ((not (elt-of pass-attempt password)) 
        (begin
          (set! incorrect-counter (+ incorrect-counter 1))
          (if (> incorrect-counter 6)
            (lambda (x) (call-the-cops))
            (lambda (x) "Incorrect Password")
          )
        )
      )
      ((eq? m 'joint) add-member)
      ((eq? m 'withdraw) 
        (begin
          (set! incorrect-counter 0)
          withdraw
        )
      )
      ((eq? m 'deposit) 
        (begin
          (set! incorrect-counter 0)
          deposit
        )
      )
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

"Exercise 3-4"
(define (call-the-cops)
  "Too many incorrect attemps... account locked."
)


(define (rand-bigint)
  (random 100000000)
)

(define (estimate-pi-common-factors trials)
  (sqrt (/ 6.0 (monte-carlo trials cesaro-test)))
)
(define (cesaro-test) (= (gcd (rand-bigint) (rand-bigint)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else (iter (- trials-remaining 1) trials-passed))
    )
  )
  (iter trials 0)
)

"Exercise 3-5"
(define (random-in-range low high)
  (+ low (random (- high low)))
)

(define (circle-pred-test r)
  (let 
    (
     (x (random-in-range (- r) r))
     (y (random-in-range (- r) r))
    )
    (< (+ (square x) (square y)) (square r)) 
  )
)

(define (estimate-integral P x1 x2 y1 y2 trials) 
  (* (- x2 x1) (- y2 y1) (monte-carlo trials P))
)

(define (estimate-pi-circle r trials)
  (define P (lambda () (circle-pred-test r)))
  (inexact (/ (estimate-integral P (- r) r (- r) r trials) (square r)))
)

"Exercise 3-6"
(define (rand-test argument)
  (define x 101123)
  (cond
    ((eq? argument 'generate) (begin (set! x (rand-update x)) x))
    ((eq? argument 'reset) (lambda (y) (set! x y)))
    (else (error "Bad command given to RAND-TEST." argument))
  )
)

"Exercise 3-7"
(define (elt-of x L)
  (cond 
    ((null? L) #f)
    ((eq? x (car L)) #t)
    (else (elt-of x (cdr L)))
  )
)
(define (make-joint acc pass1 pass2)
  ((acc pass1 'joint) pass2)
  acc
)


"Exercise 3-8"
(define f 
  (let ((init 0))
    (lambda (x)
      (set! init (- x init))
      (- x init)
    )
  )
)
; This is horrible... by the way addition evaluates (right to left????)

"Exercises 3-9 to 3-11 can all be found in a notebook as they are drawing diagrams."

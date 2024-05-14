(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (seralized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val
        )
      )
      serialized-p
    )
  )
)

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond 
        ((eq? m 'acquire)
          (if (test-and-set! cell)
            (the-mutex 'acquire)
          )
        )
        ((eq? m 'release) (clear! cell))
        ((eq? m 'cell) cell)
        (else (error "Unknown message received: MAKE-MUTEX" m))
      )
    )
    the-mutex
  )
)
(define (clear! cell) (set-car! cell #f))
(define (test-and-set! cell)
  (if (car cell)
    #t 
    (begin 
      (set-car! cell #t)
      #f 
    )
  )
)
; Note test-and-set! only works if you insist on test-and-set! being called atomically.
; To do this, you will need to understand the system architechture better.

"Exercise 3-38"
; a) We expect final balances of $35, $40, $45, or $50. See notebook for details.
; b) We could potentially see balances of $110 or $80 which are new.

"Exercise 3-39"
; x could be 100, 101, or 121. See notebook for details.

"Exercise 3-40"
; The possibilities for x are 10^2, 10^3, 10^4, 10^5, or 10^6. If we insist on serialization,
; the only possibility is 10^6.

"Exercise 3-41"
; I disagree with Ben. Protecting balance does not affect the behavior of the program.
; Accesses to balance do not change the state of the account ever, so it cannot create anomolies.
; It can however, lead to confusing consumer behavior as you might check the balance on your account
; and get some number that does not reflect the value of the account after some operation which is 
; about to happen. However, protecting access to balance does not substantively prevent this problem.

"Exercise 3-42"
; In general, this change looks safe to make. Each account will have one serializer which is created
; when (make-account is called). This serializer is now the object that represents the account.
; When it is called, it will created protected versions of the withdraw and deposit, and then 
; create a dispatch which calls these things. However, in the old version, the same serializer created
; new protected procedures each type dispatch was called. I don't believe there are any changes to 
; concurrency.

"Exercise 3-43"
; If exchanges are properly serialized, then exchanging them in any order should lead to
; permutations of the 3 account balances. Since permutations preserve the set of values, 
; the account balances will be exactly $10, $20, or $30 in this scenario.
;
; If we were using the unserialized, original version of the exchange procedure, but with 
; accounts which serialize deposits and withdrawals, the following mistakes could happen.
; Suppose the accounts have $40 and $50. The exchange function computes the difference as $10, 
; but then account 1 ges a deposit of $20. So when the exchange happens, we would expect
; to see the new balances are $50 and $60. However, as the deposit hits, the balances become $60
; and $50, followed by a $10 deposit into account 1 and a $10 withdrawal into account 2, leading
; to balances of $70 and $40. Notice that $50 + $60 = $40 + $60 + $20 = $70 + $40.
;
; In general with this implementation, we should expect the total sum of money in the system
; to stay constant. This is because we have either serialized functions which simultaneously access
; and update an account value or separated out the access and update. In initial exchange function,
; we compute a difference simply using access. Now a problem might occur that those balances may 
; be updated, but since we use seralized deposit and withdraw procedures to actually update the 
; balances, our new account balances represent a string of deposits and withdrawls which are serialized
; but potentially in the wrong order. So the total amount of money in the system cannot change.
;
; If we don't serialized deposits and withdrawals, we have already seen that even without exchange
; money does not have to be conserved.

"Exercise 3-44"
; I believe Ben Bitdiddle is right. The key difference between transferring and exchanging balances
; is that the amount of transfer is pre-determined and not directly related to the account balances
; of our two accounts. This prevents the need to access the balances of the two accounts before 
; making the relevant deposits and withdrawals. We can simply add two procedures to the serialized
; procedure queue and keep on our way.

"Exercise 3-45"
; If dispatchs to deposit and withdraw are called using the same serializer we give to exchange, 
; the exchange procedure will be unable to actually call deposit and withdraw because the exchange
; procedure itself will have locked the respective serializers. This is avoided in the complicated
; implementation because in exchange, unserialized versions of deposit and withdraw are called because
; they are happening in the context of a larger procedure which has been serialized to different 
; deposits and withdrawals.

"Exercise 3-46"
; See notebook for drawing

"Exercise 3-47"
(define (create-n proc n)
  (define (iter output-list n)
    (if (< n 1)
      output-list
      (iter (cons (proc) output-list) (- n 1))
    )
  )
  (iter '() n)
)
(define (traverse-mutexes m-list proc)
  (cond
    ((null? m-list) #f)
    ((proc (car m-list)) (traverse-mutexes (cdr m-list proc)))
  )
)
(define (semaphore-1 n)
  (let ((mutexes (create-n make-mutex n)))
    (define (the-semaphore m)
      (cond
        ((eq? m 'acquire)
          (traverse-mutexes mutexes (lambda (mut) (test-and-set! (mut 'cell))))
          (the-semaphore 'acquire)
        )
        ((eq? m 'release)
          (traverse-mutexes mutexes (lambda (mut) (if (car (mut 'cell)) (clear! (mut 'cell)))))
          (the-semaphore 'release)
        )
        (else (error "Unknown message: SEMAPHORE" m))
      )
    )
    the-semaphore
  )
)

(define (semaphore-2 n)
  (let ((proc-count (list 0)))
    (define (the-semaphore m)
      (cond 
        ((eq? m 'acquire)
          (if (test-and-set-semaphore! proc-count n)
            (the-semaphore 'acquire) 
          )
        )
        ((eq? m 'release) (set-car! proc-count (- (car proc-count) 1)))
        (else (error "Unknown message: SEMAPHORE" m))
      )
    )
    the-semaphore
  )
)
(define (test-and-set-semaphore! proc-count n)
  (if (< proc-count n) (begin (set-car! proc-count (+ (car proc-count) 1)) #f) #t)
)

"Exercise 3-48"
(define (make-account-and-serializer balance account-num)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount)) 
        balance
      )
      "Insufficient funds"
    )
  )
  (define (deposit amount)
    (set! balance (+ balance amount)) 
    balance
  )
  
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond 
        ((eq? m 'withdraw) (balance-serializer withdraw))
        ((eq? m 'deposit) (balance-serializer deposit))
        ((eq? m 'balance) balance)
        ((eq? m 'number) account-num)
        ((eq? m 'serializer) balance-serializer)
        (else (error "Unknown request: MAKE-ACCOUNT-SERIALIZER" m))
      )
    )
    dispatch
  )
)

(define (bank-account-generator)
  (let ((next-num 0))
    (define (dispatch m)
      (cond
        ((eq? m 'make-account) 
          (lambda (balance)
            (set! next-num (+ next-num  1))
            (make-account-and-serializer balance next-num)
          )
        )
      )
    )
    dispatch
  )
)
(define bank-system (bank-account-generator))
(define (make-account bank-system balance)
  ((bank-system 'make-account) balance)
)


(define (deposit account amount)
  (let 
    (
      (s (account 'serializer))
      (d (account 'deposit))
    )
    ((s d) amount)
  )
)

(define (withdraw account amount)
  (let 
    (
      (s (account 'serializer))
      (w (account 'withdraw))
    )
    ((s w) amount)
  )
)

(define (serialized-exchange account1 account2)
  (let 
    (
      (serializer1 (account1 'serializer))
      (serializer2 (account2 'serializer))
    )
    (if (< (account1 'number) (account2 'number))
      ((serializer2 (serializer1 exchange))
        account1
        account2
      )
      ((serializer1 (serializer2 exchange))
        account1
        account2
      )
    )
  )
)

; This strategy works here because it priorities a global order on which arguments to 
; attempt to serialize first. In particular, it is impossible for different operations to 
; try to exchange account balances at the same time now because both procedures will try to 
; access the same account first.

"Exercise 3-49"
; Consider some process where we give some account a command to increase the balance of other 
; specified accounts by 10%.
;
; Else where in the world, one of the recipient accounts tries to do the same procedure. It lists
; one of the original accounts in this procedure as a recipient. The central planner cannot know 
; that each account will list the other or if their are other loops with other actors.
;
; So ahead of time, unless the planner knows that both plan on doing the procedure together, 
; the planner cannot lock out other account access. This can lead to deadlock.

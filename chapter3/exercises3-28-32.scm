; Representing Wires
(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures))
    )
  )
)

(define (get-signal wire) (wire 'get-signal))
(define (get-action-procedures wire) (wire 'get-action-procedures))
(define (set-signal! wire new-value) ( (wire 'set-signal!) new-value) )
(define (add-action! wire ac-proc) ((wire 'add-action!) ac-proc))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin
          (set! signal-value new-value)
          (call-each action-procedures)
        )
        'temp1
      )
    )

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc)
    )

    (define (dispatch m)
      (cond
        ((eq? m 'get-signal) signal-value)
        ((eq? m 'get-action-procedures) action-procedures)
        ((eq? m 'set-signal!) set-my-signal!)
        ((eq? m 'add-action!) accept-action-procedure!)
        (else (error "Unknown command passed to wire." m))
      )
    )
    dispatch
  )
)

; Representing the Agenda
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or 
      (null? segments) 
      (< time (segment-time (car segments)))
    )
  )
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)
    )
  )
  (define (add-to-segments! segments)
    (if (= time (segment-time (car segments))) 
      (insert-queue! (segment-queue (car segments)) action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr! segments (cons (make-new-time-segment time action) rest))
          (add-to-segments! rest)
        )
      )
    )
  )
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments! agenda (cons (make-new-time-segment time action) segments))
      (add-to-segments! segments)
    )
  )
)

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda))
    )
  )
)

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg))
    )
  )
)

; Representing Logical Operations
(define (logical-not s)
  (cond 
    ((= s 0) 1)
    ((= s 1) 0)
    (else (error "Invalid signal" s))
  )
)

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 
    1
    0
  )
)

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1))
    1
    0
  )
)

; Representing Primitive Gates
(define (inverter-gate input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))
    )
  )
  (add-action! input invert-input)
  'ok
)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-delay (lambda () (set-signal! output new-value)))
    )
  )
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok
)

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-delay (lambda () (set-signal! output new-value)))
    )
  )
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok
)

; Representing Compound Gates
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter-gate c e)
    (and-gate d e s)
    'ok
  )
)

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or c2 c1 c-out)
    'ok
  )
)


; Representing the Simulator
(define (after-delay dt action)
  (add-to-agenda! (+ dt (current-time the-agenda)) action the-agenda)
)
(define (propogate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propogate)
    )
  )
)
(define (probe name wire)
  (add-action! 
    wire
    (lambda ()
      (newline)
      (display name) (display " ")
      (display (current-time the-agenda))
      (display " New-value = ")
      (display (get-signal wire))
    )
  )
)
(define inverter-delay 2)
(define and-delay 3)
(define or-delay 5)
(define the-agenda (make-agenda))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propogate)
(set-signal! input-2 1)
(propogate)


"Exercise 3-28"
; See definitions implemented above

"Exercise 3-29"
(define (de-morgan-or o1 o2 out)
  (let ((c (make-wire)) (d (make-wire)) (e (make-wire)))
    (inverter-gate o1 c)
    (inverter-gate o2 d)
    (and-gate c d e)
    (inverter-gate e out)
    'ok
  )
)

"Exercise 3-30"
(define (ripple-carry-adder A-wires B-wires S-wires C)
  (define (iter A-wires B-wires S-wires c-in)
    (cond 
      ((null? (cdr S-wires))
        (full-adder (car A-wires) (car B-wires) c-in (car S-wires) C)
        'ok
      )
      (else
        (let ((new-c (make-wire)))
          (full-adder (car A-wires) (car B-wires) c-in (car S-wires) new-c)
          (iter (cdr A-wires) (cdr B-wires) (cdr S-wires) new-c)
        )
      )
    )
  )
)
; To find the delay constraints, first note that the delay on a half-adder, assuming
; that different wires count on the same clock (not sequentially) is max(and + not, or) + and
;
; The delay on an adder therefore is half-adder + half-adder + or-action-procedure
;
; So the delay on an ripple-carry-adder is
;
; n * full-adder = n (2 half-adder + or) = 2n * half-adder + n * or 
;                = 2n (max(and + not, or) + and) + n * or 
;                = 2n * max(and + not, or) + 2n * and + n * or 
;

"Exercise 3-31"
; The reason this matters is incredibly subtle in my opinion. Without running the action once when it is added 
; to a wire, the connection is not officially on the agenda UNTIL the value of the trigger of the action changes.
; However, when you run propogate, your wires are not guaranteed to change the values of all intermediate nodes, which 
; means some connections are still non-existent. This may effect subsequent runs of propogate.
;
; Being very concrete, suppose we consider our system up to the line (set-signal! input-1 1). This will add connections to 
; carry and intermediate wire d to our agenda. However, when we (propogate) and reach the carry action items, because carry 
; is not changing in our first propogation, the connections which are based on carry do not get called into existence.
;
; This means that e is not updated properly and then sum is not updated properly as those connections are not established either.

"Exercise 3-32"
; Consider an and-gate with inputs currently (0 1). In a constant time segment we change the first input to (1 1). It will
; append a new command to the agenda in 3 time units which says change output to 1. We then change the input to (1 0). 
; We append a new command to the agenda in 3 time units which says change output to 0. 
;
; If we handle these commands using queueing logic, the output going forward will be 0. If we handle these commands using
; stack logic, the output going forward will be 1.

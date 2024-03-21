(define (filter predicate sequence)
  (cond
    ((null? sequence) ())
    ((predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))
    )
    (else (filter predicate (cdr sequence)))
  )
)

(define (accumulate op initial sequence)
  (if (null? sequence) 
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))
  )
)

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))
  )
)

(define (flatmap proc seq) (accumulate append () (map proc seq)))
(define (distinct-pairs n) (flatmap (lambda (i) (map (lambda (j) (list j i)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))


; Primality Setup - Functions from the Section
(define (find-next-divisor n test) (
  cond
    ((> (square test) n) n)
    ((= 0 (remainder n test)) test)
    (else (find-next-divisor n (+ 1 test)))
))
(define (find-smallest-divisor n) (find-next-divisor n 2))
(define (prime-basic? n) (= n (find-smallest-divisor n)))

(define (prime-sum? pair) (prime-basic? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (filter prime-sum? (distinct-pairs n))
)

(define (remove item seq) (filter (lambda (x) (not (= x item))) seq))
(define (permutations s)
  (if (null? s)
    (list ())
    (flatmap (lambda (x) (map (lambda (p) (cons x p)) (permutations (remove x s)))) s)
  )
)

"Exercise 2-40"
; See distinct-pairs above.

"Exercise 2-41"
(define (distinct-triples n) 
  (flatmap
    (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list k j i)) (enumerate-interval 1 (- j 1))))
        (enumerate-interval 1 (- i 1))
      )
    )
    (enumerate-interval 1 n)
  )
)
(define (distinct-triple-sum n s)
  (filter (lambda (x) (= s (+ (car x) (cadr x) (caddr x)))) (distinct-triples n))
)

"Exercise 2-42"
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map
              (lambda (new-row) (adjoin-position new-row k rest-of-queens)) 
              (enumerate-interval 1 board-size)
            )
          )
          (queen-cols (- k 1))
        )
      )
    )
  )
  (queen-cols board-size)
)

; Represent the board with coordinates 1-n on row and col with (1,1) in top left
; Write a position as (row, col)
; Represent queen placement by a list of pairs 
(define empty-board ())
(define (make-pos x y) (cons x y))
(define (get-row p) (car p))
(define (get-col p) (cdr p))
(define (adjoin-position row col curr_board) (cons (make-pos row col) curr_board))

(define (row_safe? curr_queen curr_board)
  (cond 
    ((null? curr_board) #t)
    ((not (= (car curr_queen) (car (car curr_board)))) (row_safe? curr_queen (cdr curr_board)))
    (else #f)
  )
)

(define (diag_one_safe? curr_queen curr_board)
  (cond 
    ((null? curr_board) #t)
    ((not (= 
            (- (car curr_queen) (cdr curr_queen)) 
            (- (car (car curr_board)) (cdr (car curr_board))))) 
      (diag_one_safe? curr_queen (cdr curr_board)))
    (else #f)
  )
)

(define (diag_two_safe? curr_queen curr_board)
  (cond 
    ((null? curr_board) #t)
    ((not (= 
            (+ (car curr_queen) (cdr curr_queen)) 
            (+ (car (car curr_board)) (cdr (car curr_board)))))
      (diag_two_safe? curr_queen (cdr curr_board)))
    (else #f)
  )
)

(define (safe? k curr_board)
  (let ((curr_queen (car curr_board)))
    (and
      (row_safe? curr_queen (cdr curr_board))
      (diag_one_safe? curr_queen (cdr curr_board))
      (diag_two_safe? curr_queen (cdr curr_board))
    )
  )
)

"Exercise 2-43"
; The problem if you flip the order is that data from (queen-cols k) is not stored
; So every time flatmap takes a new-row number, it has to recall (queen-cols) which sets
; up a new giant recursion chain. In fact, at every level, you have to call the previous leve (board-size)
; more times. This means that the overall function will run in approximately
; (board-size)^(board-size)T time

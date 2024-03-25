"Exercise 2-44"
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split (- n 1))))
      (below painter (beside smaller smaller) )
    )
  )
)

"Exercise 2-45"
(define (split join-big join-small)
  (lambda (painter n)
    (if (= n 0)
      (let ((smaller ((split join-big join-small) painter (- n 1))))
        (join-big painter (join-small smaller smaller))
      )
    )
  )
)


"Exercise 2-46"
(define (make-vec x y) (cons x y))
(define (xcor v) (car v))
(define (ycor v) (cdr v))

(define (add-vec v1 v2) (make-vec (+ (xcor v1) (xcor v2)) (+ (ycor v1) (ycor v2))))
(define (sub-vec v1 v2) (make-vec (- (xcor v1) (xcor v2)) (- (ycor v1) (ycor v2))))
(define (scalar-mult c v) (make-vec (* c (xcor v)) (* c (ycor v))))

"Exercise 2-47"
(define (make-frame o e1 e2) (list o e1 e2))
(define (make-frame-alt o e1 e2) (cons o (cos e1 e2)))

(define (get-o f) (car f))
(define (get-e1 f) (cadr f))
(define (get-e2 f) (caddr f))

(define (get-o-alt f) (car f))
(define (get-e1-alt f) (cadr f))
(define (get-e2-alt f) (cddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vec 
      (get-o frame) 
      (add-vec (scalar-mult (xcor v) (get-e1 frame)) (scalar-mult (ycor v) (get-e2 frame)))
    )
  )
)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line ((frame-coord-map frame) (start-segment segment)) ((frame-coord-map frame) (end-segment segment)))
      )
      segment-list
    )
  )
)

"Exercise 2-48"
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

"Exercise 2-49"
(define outline-painter 
  (segments->painter 
    (list
      (make-segment (make-vec 0 0) (make-vec 0 1))
      (make-segment (make-vec 0 0) (make-vec 1 0))
      (make-segment (make-vec 1 0) (make-vec 1 1))
      (make-segment (make-vec 0 1) (make-vec 1 1))
    )
  )
)
(define x-painter
  (segments->painter
    (list
      (make-segment (make-vec 1 0) (make-vec 0 1))
      (make-segment (make-vec 0 0) (make-vec 1 1))
    )
  )
)
(define diamond-painter
  (segments->painter
    (list
      (make-segment (make-vec 0 0.5) (make-vec 0.5 1))
      (make-segment (make-vec 0.5 1) (make-vec 1 0.5))
      (make-segment (make-vec 1 0.5) (make-vec 0.5 0))
      (make-segment (make-vec 0.5 0) (make-vec 0 0.5))
    )
  )
)
; Making the wave painter would be boring so I am skipping for now

"Exercise 2-50"
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame
            new-origin
            (sub-vec (m corner1) new-origin)
            (sub-vec (m corner2) new-origin)
          )
        )
      )
    )
  )
)

(define (flip-horiz painter)
  (transform-painter 
    painter
    (make-vec 0 1)
    (make-vec 1 1)
    (make-vec 0 0)
  )
)
(define (rotate180 painter)
  (transform-painter
    painter
    (make-vec )
    (make-vec)
    (make-vec)
  )
)
(define (rotate270 painter)
  (transform-painter
    painter
    (make-vec 0 1)
    (make-vec 0 0)
    (make-vec 1 1)
  )
)

"Exercise 2-51"
(define (below painter1 painter2)
  (let ((split-point (make-vec 0 0.5)))
    (let (
          (paint-bottom (transform-painter
            painter1
            (make-vec 0 0)
            (make-vec 1 0)
            split-point
          )) 
          (paint-top (transform-painter
            painter2
            split-point
            (make-vec 1 0.5)
            (make-vec 0 1)
          ))
        )
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)
      )
    )
  )
)

(define (other-below painter1 painter2)
  (rotate90 (besides (rotate270 painter1) (rotate270 painter2)))
)

"Exercise 2-52"
; Skipping this exercise because it can be toyed with whenever to scratch a design itch

; Immplementing Set Operations in Different Data Structures

; Unordered Lists
(define (uo-element-of-set? x set)
  (cond
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (uo-element-of-set? x (cdr set)))
  )
)

(define (uo-adjoin-element x set)
  (if (uo-element-of-set? x set)
    set
    (cons x set)
  )
)

(define (uo-intersection set1 set2)
  (cond
    ((or (null? set1) (null? set2)) ())
    ((uo-element-of-set? (car set1) set2) 
     (cons (car set1) (uo-intersection (cdr set1) set2))
    )
    (else (uo-intersection (cdr set1) set2))
  )
)

"Exercise 2-59"
(define (uo-union set1 set2)
  (if (null? set1)
    set2
    (uo-adjoin-element (car set1) (uo-union (cdr set1) set2))
  )
)

"Exercise 2-60"
; Uonordered Lists Allowing for Duplicates
(define (duo-element-of-set? x set)
  (cond 
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (duo-element-of-set x (cdr set)))
  )
)

(define (duo-adjoin-element x set) (cons x set))
(define (duo-union set1 set2) (append set1 set2))
(define (duo-intersection set1 set2)
  (cond
    ((or (null? set1) (null? set2)) ())
    ((duo-element-of-set? (car set1) set2)
      (cons (car set1) (duo-intersection (cdr set1) set2))
    )
    (else (duo-intersection (cdr set1) set2))
  )
)

; Ordered Lists --> assume numbers for comparison operation
(define (o-element-of-set? x set)
  (cond
    ((null? set) #f)
    ((< x (car set)) #f)
    ((= x (car set)) #t)
    (else (o-element-of-set? x (cdr set)))
  )
)
(define (o-intersection set1 set2)
  (cond
    ((or (null? set1) (null? set2)) ())
    ((< (car set1) (car set2)) (o-intersection (cdr set1) set2))
    ((> (car set1) (car set2)) (o-intersection set1 (cdr set2)))
    (else (cons (car set1) (o-intersection (cdr set1) (cdr set2))))
  )
)

"Exercise 2.61"
(define (o-adjoin-element x set)
  (cond 
    ((null? set) (list x))
    ((< x (car set)) (cons x set))
    ((= x (car set)) set)
    (else (cons (car set) (o-adjoin-element x (cdr set))))
  )
)

"Exercise 2-62"
(define (o-union set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    ((< (car set1) (car set2)) (cons (car set1) (o-union (cdr set1) set2)))
    ((> (car set1) (car set2)) (cons (car set2) (o-union set1 (cdr set2))))
    (else (cons (car set1) (o-union (cdr set1) (cdr set2))))
  )
)

; Sets as binary trees (hoping to have them balanced)
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree val l r) (list val l r))

(define (bt-element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (entry (car set))) #t)
    ((< x (entry (car set))) (bt-element-of-set? x (left-branch set)))
    ((> x (entry (car set))) (bt-element-of-set? x (right-branch set)))
  )
)

(define (bt-adjoin-element x set)
  (cond
    ((null? set) (make-tree x () ()))
    ((= x (entry set)) set)
    ((< x (entry set)) 
     (make-tree (entry set) (bt-adjoin-element x (left-branch set)) (right-branch set)))
    ((> x (entry set)) 
     (make-tree (entry set) (left-branch set) (bt-adjoin-element x (right-branch set))))
  )
)

"Exercise 2-63"
; a) (tree->list-1) will go as far left as it can, add that element to one list
; It then takes the parent element and traverses the right child branch. It always looks left,
; grabs everything, and then appends that to a list with the parent and the right. It is
; Always joining the left half of the elements with the right half of the elements
;
; (tree->list-2) will always look as far right as it can to start
; It grabs the right child, adds the parent and then calls a helper to add smaller elements
; 
; The distinction should be most prominent on trees that are balanced.
;
; b) I believe the second algorithm will perform faster. 
; ; Both should be in the log n family while traversing, 
; but through the use of append in the first one, you spend 
; O(n/2) energy at each level combining the lists. So at the highest level,
; the first algorithm runs in O(n) time which is slower than O(log n)

(define tree-1 
  (make-tree 7 
    (make-tree 3 (make-tree 1 () ()) (make-tree 5 () ())) 
    (make-tree 9 () (make-tree 11 () ()))
  )
)
(define tree-2
  (make-tree 3
    (make-tree 1 () ())
    (make-tree 7 (make-tree 5 () ()) (make-tree 9 () (make-tree 11 () ())))
  )
)
(define tree-3
  (make-tree 5
    (make-tree 3 (make-tree 1 () ()) ())
    (make-tree 9 (make-tree 7 () ()) (make-tree 11 () ()))
  )
)
(define tree-4 (make-tree 1 () (make-tree 2 () (make-tree 3 () (make-tree 4 () (make-tree 5 () (make-tree 6 () (make-tree 7 () ()))))))))

(define (tree->list-1 tree)
  (if (null? tree)
    ()
    (append 
      (tree->list-1 (left-branch tree)) 
      (cons (entry tree) (tree->list-1 (right-branch tree)))
    )
  )
)

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
      result
      (copy-to-list
        (left-branch tree)
        (cons (entry tree) (copy-to-list (right-branch tree) result))
      )
    )
  )
  (copy-to-list tree ())
)

"Exercise 2-64"
; a) (list->tree) grabs the first element in a pair that should be the finished tree 
; and an empty list.
;
; Partial tree starts by saying if we need to join 0 elements to a tree, return
; an empty tree and say that the rest of the elements need to be made
;
; It finds half the size of the elements to add to the left (minus one to ensure
; there is an element for the parent)
;
; It then builds the left half of the tree using exactly that many elements
;
; It takes the remaining elements, and picks out the first one as the parent (this-entry)
; It takes the rest of the remaining elements and creates the right partial tree
;
; It then joins the left right and parent element and returns any elements that are remaining
; to be added to a different half of the tree.
;
; b) This should be O(n) as each element need only be added once and its not moved once
; added to a tree.

"Exercise 2-65"
(define (bt-union set1 set2)
  (let 
    ((list1 (tree->list-2 set1))
     (list2 (tree->list-2 set2)))

    (list->tree (o-union list1 list2))
  )
)

(define (bt-intersection set1 set2)
  (let 
    ((list1 (tree->list-2 set1))
     (list2 (tree->list-2 set2)))

    (list->tree (o-intersection list1 list2))
  )
)

; Each call to tree->list-2 is O(n1) and O(n2) respectively.
; o-union and o-intersection are O(n1 + n2) which is still linear time complexity
; list->tree runs in O(n) time as well

"Exercise 2-66"
(define (lookup given-key set-of-records)
  (cond
    ((null? set-of-records) #f)
    ((equal? given-key (key (entry set-of-records))) (entry set-of-records))
    ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
    ((> given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records)))
  )
)



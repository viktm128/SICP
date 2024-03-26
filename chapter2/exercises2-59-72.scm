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

; Huffman Encoding Trees
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? 'leaf (car object)))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list 
    left 
    right 
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))
  )
)
(define (h-left-branch tree) (car tree))
(define (h-right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)
  )
)
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)
  )
)

(define (choose-branch bit node)
  (cond
    ((= 0 bit) (h-left-branch node))
    ((= 1 bit) (h-right-branch node))
    (else (error "BAD MESSAGE: message not binary"))
  )
)
(define (decode bits tree)
  (define (decode-1 bits node)
    (if (null? bits)
      ()
      (let ((next-node (choose-branch (car bits) node)))
        (if (leaf? next-node)
          (cons (symbol-leaf next-node) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-node)
        )
      )
    )
  )
  (decode-1 bits tree)
)

(define (h-adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (h-adjoin-set x (cdr set))))
  )
)
(define (make-leaf-set pairs)
  (if (null? pairs)
    ()
    (h-adjoin-set (make-leaf (caar pairs) (cadar pairs)) (make-leaf-set (cdr pairs)))
  )
)

"Exercise 2-67"
(define sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)
      )
    )
  )
)
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; a d a b b c a 


"Exercise 2-68"
(define (encode message tree)
  (if (null? message)
    ()
    (append (encode-symbol (car message) tree) (encode (cdr message) tree))
  )
)

(define (encode-symbol x tree)
  (cond
    ((leaf? tree) ())
    ((uo-element-of-set? x (symbols (h-left-branch tree))) 
     (cons '0 (encode-symbol x (h-left-branch tree))))
    ((uo-element-of-set? x (symbols (h-right-branch tree)))
     (cons '1 (encode-symbol x (h-right-branch tree))))
    (else (error "ERROR: letter not contained in this Huffman Tree"))
  )
)

"Exercise 2-69"
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)
(define (successive-merge set)
  (if (null? (cdr set))
    (car set)
    (let ((node1 (car set)) (node2 (cadr set)))
      (successive-merge (h-adjoin-set (make-code-tree node1 node2) (cddr set)))
    )
  )
)

"Exercise 2-70"
(define pairs '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define song '(
  Get a job 
  Sha na na na na na na na na
  Get a job
  Sha na na na na na na na na
  Wah yip yip yip yip yip yip yip yip yip
  Sha boom 
))

(define length-huffman (length (encode song (generate-huffman-tree pairs))))
(define length-fixed 3 * (+ 2 2 3 1 1 2 16 9))

; The Huffman encoding takes 84 bits
; Since there are 8 tokens in our language, we need 3 bits per character in a 
; fixed representation. There are 36 words in our song so the total fixed length 
; encoding will take 108 bits 

"Exercise 2-71"
; Proof in journal

"Exercise 2-72"
; In the case above, the most frequent element will be the only left leaf from the top node
; So encoding it is O(1)
; The least frequent element will be (n-1) 1s and to encode it, we will need to make n-1 steps.
; So encoding it is O(n - 1) = O(n)

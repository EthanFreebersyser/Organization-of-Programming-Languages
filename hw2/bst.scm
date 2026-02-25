(#%require rackunit)

;;;Author: Ethan Freebersyser
;;;Date: 2/25/26
;;;Documentation: Provided by the asssignment instuctions written by Dr. Yang
;;;Hw3:To write Scheme functions that implement integer binary search trees.
;;;    To test Scheme code using a unit-testing framework. 

;;Return an empty binary tree.
(define null-binary-tree
  (lambda ()
    '()))

;;Given a nonempty binary tree node return the value. The behaviour when node
;;is not a nonempty binary tree is unspecified.
(define entry
  (lambda (node)
    (car node)))

;;Given a nonempty binary tree node return left subtree. The behaviour when node
;;is not a nonempty binary tree is unspecified.
(define left
  (lambda (node)
    (car (cdr node))))

;;Given a nonempty binary tree node return right subtree. The behaviour when node
;;is not a nonempty binary tree is unspecified.
(define right
  (lambda (node)
    (car (cdr (cdr node)))))

;;Return a new binary tree whose root node's value is element, left subtree is
;;left-subtree, and right subtree is right-subtree.
(define make-binary-tree
  (lambda (element left-subtree right-subtree)
    (list element left-subtree right-subtree)))

;;Return true (#t) if obj represents a valid binary tree, and false (#f) otherwise. 
(define binary-tree?
  (lambda (obj)
    (cond
      ((null? obj)                      #t) ; empty is a BT
      ((and (pair? obj)
            (= (length obj) 3) 
            (integer? (entry obj)) 
            (binary-tree? (left obj))   ; left tree must be BT
            (binary-tree? (right obj))) #t)
      (else                             #f)))) 

;;Return true if obj represents a valid binary search tree (BST), and false otherwise. 
(define bst?
  (lambda (obj)
    (and (binary-tree? obj)
         (cond
           ((null? obj) #t)
           (else
            (and (less? (left obj) (entry obj))
                 (greater? (right obj) (entry obj))
                 (bst? (left obj))
                 (bst? (right obj))))))))

;;Returns true if all values are less than the entry of obj, and false otherwise
(define less?
  (lambda (obj val)
    (cond
      ((null? obj)          #t)
      ((>= (entry obj) val) #f)
      (else
       (and (less? (left obj) val)
            (less? (right obj) val))))))

;;Returns true if all values are greater than the entry of obj, and false otherwise
(define greater?
  (lambda (obj val)
    (cond
      ((null? obj)          #t)
      ((<= (entry obj) val) #f)
      (else
       (and (greater? (left obj) val)
            (greater? (right obj) val))))))

;;Return true if obj represents an empty binary tree, and false otherwise.
(define null-binary-tree?
  (lambda (obj)
    (and (binary-tree? obj)
         (null? obj))))

;;Return #t if bst contains a node with value v, and #f otherwise.
(define member?
  (lambda (v bst)
    (cond
      ((null? bst)       #f)
      ((= v (entry bst)) #t)
      ((< v (entry bst)) (member? v (left bst)))
      (else              (member? v (right bst))))))

;;Return a list containing all values in bst in the order obtained from a preorder traversal.
(define preorder
  (lambda (bst)
    (cond
      ((null? bst) '())
      (else
       (append (list(entry bst))
               (preorder (left bst))
               (preorder (right bst)))))))

;;Return a list containing all values in bst in the order obtained from a inorder traversal.
(define inorder
  (lambda (bst)
    (cond
      ((null? bst) '())
      (else
       (append (inorder (left bst))
               (list (entry bst))
               (inorder (right bst)))))))

;;Return a list containing all values in bst in the order obtained from a preorder traversal.
(define postorder
  (lambda (bst)
    (cond
      ((null? bst) '())
      (else
       (append (postorder (left bst))
               (postorder (right bst))
               (list (entry bst)))))))

;;Return a new binary search tree identical to bst but with integer v appearing
;;in its proper location. Smaller values are inserted to the left of a node,
;;larger values to the right of a node. If v is already in the tree, just
;;return (a copy of) the original tree without changing it.
(define insert
  (lambda (v bst)
    (cond
      ((null? bst)
       (make-binary-tree v '() '()))
      ((= v (entry bst)) bst)
      ((< v (entry bst))
       (make-binary-tree (entry bst)
                         (insert v (left bst))
                         (right bst)))
      (else
       (make-binary-tree (entry bst)
                         (left bst)
                         (insert v (right bst)))))))
      
     
;; Tests

;null-binary-tree
(check-equal? (null-binary-tree) '())

;entry
(check-equal? (entry '(5 () ())) 5)                     
(check-equal? (entry '(10 (3 () ()) (15 () ()))) 10)    
(check-equal? (entry '(-5 () ())) -5)

;left
(check-equal? (left '(5 () ())) '())                   
(check-equal? (left '(5 (3 () ()) ())) '(3 () ()))
(check-equal? (left '(10 (5 (2 () ()) (7 () ())) (15 () ()))) 
              '(5 (2 () ()) (7 () ())))

;right
(check-equal? (right '(5 () ())) '())                  
(check-equal? (right '(5 () (7 () ()))) '(7 () ()))     
(check-equal? (right '(10 (5 () ()) (15 (12 () ()) (20 () ())))) 
              '(15 (12 () ()) (20 () ())))

;make-binary-tree
(check-equal? (make-binary-tree 5 '() '()) 
              '(5 () ()))
(check-equal? (make-binary-tree 10 '(5 () ()) '(15 () ()))
              '(10 (5 () ()) (15 () ())))
(check-equal? (make-binary-tree 1 (make-binary-tree 2 '() '()) '())
              '(1 (2 () ()) ()))

;null-binary-tree?
(check-equal? (null-binary-tree? '()) #t)
(check-equal? (null-binary-tree? (null-binary-tree)) #t)
(check-equal? (null-binary-tree? '(5 () ())) #f)
(check-equal? (null-binary-tree? '(1 (2 () ()) (3 () ()))) #f)

;binary-tree?
(check-equal? (binary-tree? '()) #t)
(check-equal? (binary-tree? '(5 () ())) #t)
(check-equal? (binary-tree? '(5 (3 () ()) ())) #t)
(check-equal? (binary-tree? 5) #f)
(check-equal? (binary-tree? '(5)) #f)
(check-equal? (binary-tree? '(5 ())) #f)

;bst?
(check-equal? (bst? '()) #t)
(check-equal? (bst? '(5 () ())) #t)
(check-equal? (bst? '(5 (3 () ()) ())) #t)
(check-equal? (bst? '(5 (7 () ()) ())) #f)
(check-equal? (bst? '(5 () (3 () ()))) #f)
(check-equal? (bst? '(5 (7 () ()) (3 () ()))) #f)

;memeber
(check-equal? (member? 5 '()) #f)
(check-equal? (member? 5 '(5 () ())) #t)
(check-equal? (member? 3 '(5 () ())) #f)
(check-equal? (member? 7 '(5 () ())) #f)

;preorder
(check-equal? (preorder '(5 () ())) '(5))
(check-equal? (preorder '(5 (3 () ()) ())) '(5 3))
(check-equal? (preorder '(5 () (7 () ()))) '(5 7))

;inorder
(check-equal? (inorder '(5 () ())) '(5))
(check-equal? (inorder '(5 (3 () ()) ())) '(3 5))
(check-equal? (inorder '(5 () (7 () ()))) '(5 7))

;post order
(check-equal? (postorder '(5 () ())) '(5))
(check-equal? (postorder '(5 (3 () ()) ())) '(3 5))
(check-equal? (postorder '(5 () (7 () ()))) '(7 5))

;insert
(check-equal? (insert 5 '()) '(5 () ()))
(check-equal? (insert 3 '(5 () ())) '(5 (3 () ()) ()))
(check-equal? (insert 7 '(5 () ())) '(5 () (7 () ())))
(check-equal? (insert 5 '(5 () ())) '(5 () ()))
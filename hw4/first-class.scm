(#%require rackunit)

;;;Author: Ethan Freebersyser
;;;Date: 2/25/26
;;;Documentation: Provided by the asssignment instuctions written by Dr. Yang
;;;Hw4: To not discriminate against functions and treat them as first-class citizens.

;;Takes a 2-parameter function f and returns a curried version of that function.
(define curry2
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (f x y)))))

(check-equal? (((curry2 +) 1) 2) 3)
(check-equal? (((curry2 -) 1) 2) -1)
(check-equal? (((curry2 *) 1) 2) 2)
(check-equal? (((curry2 /) 1) 2) 1/2)
(check-equal? (((curry2 cons) 1) '(2 3)) '(1 2 3))

;;Takes a curried 2-parameter function and returns a normal Scheme uncurried
;;version of that function.
(define uncurry2
  (lambda (f)
    (lambda (x y)
      ((f x) y))))

(check-equal? ((uncurry2 (lambda (a) (lambda (b) (+ a b)))) 1 2) 3)
(check-equal? ((uncurry2 (lambda (a) (lambda (b) (* a b)))) 1 2) 2)
(check-equal? ((uncurry2 (lambda (a) (lambda (b) (- a b)))) 1 2) -1)
(check-equal? ((uncurry2 (lambda (a) (lambda (b) (cons a b)))) 1 '(2 3)) '(1 2 3))

;;Multiplcation using curry2
(define mult
  (curry2 *))

(check-equal? ((mult 1) 2) 2)
(check-equal? ((mult 2) 3) 6)
(check-equal? ((mult -1) 2) -2)

;;akes two 1-parameter functions f and g and returns a function that is f
;;composed with g.
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(check-equal? ((compose car cdr) '(1 2 3)) 2)
(check-equal? ((compose not null?) '()) #f)

;;akes a 1-parameter predicate predicate and returns a predicate that is
;;the negation of predicate. 
(define negate
  (lambda (predicate)
    (lambda (x)
      (not (predicate x)))))

(check-equal? ((negate odd?) 4) #t)
(check-equal? ((negate odd?) 3) #f)
(check-equal? ((negate null?) '()) #f)

(check-equal? (((curry2 (uncurry2 (lambda (a) (lambda (b) (+ a b))))) 3) 4) 7)

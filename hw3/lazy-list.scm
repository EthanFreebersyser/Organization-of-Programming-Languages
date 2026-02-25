(#%require rackunit)

;;;Author: Ethan Freebersyser
;;;Date: 2/25/26
;;;Documentation: Provided by the asssignment instuctions written by Dr. Yang
;;;Hw3: To build a data structure to represent a (possibly infinite) sequence of elements. 

;;This function takes an integer and returns an integer lazy list containing
;;the infinite sequence of values first, first+1, ... 
(define lazy-infinite-range
  (lambda (first)
    (cons first (lambda () (lazy-infinite-range (+ first 1))))))

(check-equal? (car (lazy-infinite-range 0)) 0)
(check-equal? (car (lazy-infinite-range 5)) 5)

;;This function takes a lazy list llst and an integer n and returns an ordinary
;;Scheme list containing the first n values in the lazy list. If the lazy list
;;contains fewer than n values, then all the values in the lazy list are returned.
;;If the lazy list is empty, return an empty Scheme list.
(define first-n
  (lambda (llst n)
    (cond
      ((null? llst) '())
      ((= n 0)      '())
      (else         (cons (car llst)
                          (first-n ((cdr llst)) (- n 1)))))))

(check-equal? (first-n (lazy-infinite-range 0) 5) '(0 1 2 3 4))
(check-equal? (first-n (lazy-infinite-range 1) 1) '(1))
(check-equal? (first-n (lazy-infinite-range 3) 3) '(3 4 5))
(check-equal? (first-n '() 3)                     '())


;;This function takes a lazy list llst and an integer n and returns the n-th
;;value in the lazy list (counting from 1). If the lazy list contains fewer
;;than n values, then #f is returned.
(define nth
  (lambda (llst n)
    (cond
      ((null? llst) #f)
      ((= n 1)      (car llst))
      (else         (nth ((cdr llst)) (- n 1))))))

(check-equal? (nth (lazy-infinite-range 0) 1)   0)
(check-equal? (nth (lazy-infinite-range 0) 5)   4)
(check-equal? (nth (lazy-infinite-range 1) 10)  10)
(check-equal? (nth '() 1)                       #f)

;;This function takes two lazy lists llst1 and llst2 and returns the
;;coordinate-wise sum of the two lists as a lazy list. If llst1 and
;;llst2 have different lengths, imagine that the shorter list is
;;padded with zeros in the end so they have the same lengths. 
(define lazy-add
  (lambda (llst1 llst2)
    (cond
      ((and (null? llst1) (null? llst2)) '())
      ((null? llst1)
       (cons (car llst2)
             (lambda () (lazy-add '() ((cdr llst2))))))
      ((null? llst2)
       (cons (car llst1)
             (lambda () (lazy-add ((cdr llst1)) '()))))
      (else
       (cons (+ (car llst1) (car llst2))
             (lambda () (lazy-add ((cdr llst1)) ((cdr llst2)))))))))


(check-equal? (first-n (lazy-add (lazy-infinite-range 0)
                                 (lazy-infinite-range 0)) 5)  '(0 2 4 6 8))
(check-equal? (first-n (lazy-add (lazy-infinite-range 1)
                                 (lazy-infinite-range 1)) 4)  '(2 4 6 8))
(check-equal? (lazy-add '() '())                              '())  

;;This function takes a function predicate and a lazy list llst and returns
;;a new lazy list that represents (in order) precisely the elements elt of
;;llst such that (predicate elt) is true.
(define lazy-filter
  (lambda (predicate llst)
    (cond
      ((null? llst) '())
      ((predicate (car llst))
       (cons (car llst)
             (lambda () (lazy-filter predicate ((cdr llst))))))
      (else
       (lazy-filter predicate ((cdr llst)))))))

(check-equal? (first-n (lazy-filter odd?  (lazy-infinite-range 0)) 5)  '(1 3 5 7 9))
(check-equal? (first-n (lazy-filter even? (lazy-infinite-range 0)) 5)  '(0 2 4 6 8))
(check-equal? (first-n (lazy-filter odd?  (lazy-infinite-range 1)) 3)  '(1 3 5))
  
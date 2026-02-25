;;;Author: Ethan Freebersyser
;;;Date: 2/25/26
;;;Documentation: Provided by the asssignment instuctions written by Dr. Yang
;;;Hw5: To build an infinite list of primes using an ancient algorithm.

;;This function takes a function predicate and a lazy list llst and returns a
;;new lazy list that represents (in order) precisely the elements elt of llst
;;such that (predicate elt) is true. -- Dr. Yang Hw3
(define lazy-filter
  (lambda (predicate llst)
    (cond
      ((null? llst) '())
      ((predicate (car llst))
       (cons (car llst)
             (lambda () (lazy-filter predicate ((cdr llst))))))
      (else
       (lazy-filter predicate ((cdr llst)))))))

;;This function takes an integer and returns an integer lazy list containing
;;the infinite sequence of values first, first+1, ... -- Dr. Yang Hw3
(define lazy-infinite-range
  (lambda (first)
    (cons first (lambda () (lazy-infinite-range (+ first 1))))))

;;A predicate that says the integer n is not (evenly) divisible by
;;the integer d.
(define not-divisible?
  (lambda (d n)
    (not (= (remainder n d) 0))))

;;The curried version of not-divisible?. That is, a higher-order function,
;;so that (not-divisible-by d) is a predicate for non-divisibility by d.
(define not-divisible-by
  (lambda (d)
    (lambda (n)
      (not-divisible? d n))))

;;A function that takes a lazy list and "sieves" the first item from the rest,
;;returning a new lazy list. Depending on how you plan to use sieve, you may or
;;may not want to recursively sieve the rest of the lazy list.
(define sieve
  (lambda (llst)
    (cons (car llst)
          (lambda ()
            (sieve (lazy-filter (not-divisible-by (car llst))
                                ((cdr llst))))))))
;;A zero-parameter function that computes a lazy list containing all prime
;;numbers, starting at 2, using the Sieve of Eratosthenes.
(define primes
  (lambda ()
    (sieve (lazy-infinite-range 2))))

;;;Author: Ethan Freebersyser
;;;Date: 2/25/26
;;;Documentation: Provided by the asssignment instuctions written by Dr. Yang
;;;Hw1: To learn the basics of scheme

;; EXERCISE 1
;;Write sequences of cars and cdrs that will pick the symbol 'mary out of the
;;following expressions:
;;'(jane elizabeth mary kitty lydia)
;;'(((jane) (elizabeth) (mary) (kitty) (lydia)))
;;'(jane (elizabeth) ((mary)) (((kitty))) ((((lydia)))))

(car(cdr (cdr '(jane elizabeth mary kitty lydia))))
(car(car(cdr (cdr (car '(((jane) (elizabeth) (mary) (kitty) (lydia))))))))
(car (car (car (cdr (cdr '(jane (elizabeth) ((mary)) (((kitty))) ((((lydia))))))))))

;; EXERCISE 2
;;Write a recursive function named sum that adds up all the elements in a list.
(define sum
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst))))))

(sum '(5 5 1 1 2))

;; EXERCISE 3
;;Write a recursive function, keep-first-n, that returns a list of the first n
;;elements in a list.
(define keep-first-n
  (lambda (n lst)
    (cond ((< n 1) '())
          ((null? lst) '())
          (else
           (cons (car lst)
                 (keep-first-n (- n 1) (cdr lst)))))))

(keep-first-n 3 '(a b c d e f g h i))
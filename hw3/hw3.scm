(define range
  (lambda (i j)
    (if (> i j)
        '()
        (cons i (range (+ i 1) j)))))

(range 3 6)

(define list-ref
  (lambda (lst k)
    (if (null? lst)
        '()
        (if (= 0 k)
             (car lst)
             (list-ref (cdr lst) (- k 1))))))

(list-ref '(0 1 2 3 4 5 6 7) 4)

(define length
  (lambda (lst)
    (if (null? lst)
        0
        (+ (length (cdr lst)) 1))))

(length '(0 1 2 3 4 5 6 7))

(define append
  (lambda (lst1 lst2)
    (display ".")
    (if (null? lst1)
        lst2
        (cons (car lst1) (append (cdr lst1) lst2)))))

(append '(0 1 2 3) '(4 5 6 7))


(define reverse
  (lambda  (lst)
    (if (null? lst)
        '()
        (append (reverse (cdr lst)) (list (car lst))))))

(reverse '(0 1 2 3 4 5 6 7))

(define append
  (lambda lst
    (if (null? (car lst))
        '()
        (cons (car (car lst) (append (car (cdr lst)) (cdr lst)))))))
        
;;(append '(0 1 2 3) '(4 5 6 7) '(8 9 10 11) '(12 13 14 15))
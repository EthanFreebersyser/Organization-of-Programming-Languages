(define foldl
  (lambda (f init lst)
    (if (null? lst)
        init
        (foldl f (f (car lst) init) (cdr lst)))))

(foldl - 0 '(1 2 3 4))

(define foldr
  (lambda (f init lst)
    (if (null? lst)
        init
        (f (foldr f (car lst) (cdr lst)) init))))

(foldr - 0 '(1 2 3 4))
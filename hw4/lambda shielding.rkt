(define x 1)

(let ((x 2)) x)

((lambda (x) x) 3)

((lambda (y) x) 4)

(let ((x (+ x 1)))
  x)

(let* ((x (+ x 1)))
  x)

;;(letrec ((x (+ x 1)))
;;  x)

x

(((lambda (x y)         ;x=5 y=6
    (lambda (y z)       ;y=7 z=8
      (list x y z)))    ; 5 7 8
  5 6)
 7 8)

((((lambda (x y z)             ;x=1 y=2 z=3
     (lambda (y u v)           ;y=4 u=5 v=6
       (lambda (z v w)         ;z=7 v=8 w=9
         (list x y z u v w)))) ;1 4 7 5 8 9
   1 2 3)
  4 5 6)
 7 8 9)
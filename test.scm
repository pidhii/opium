
(define (unpack-pair x)
  (values (car x) (cdr x)))

(define-overload (fold-left f z xs)
  (if (pair? xs)
    (let*-values (((x xs2) (unpack-pair xs))
                  ((z2) (f z x)))
      (fold-left f z2 xs2))
    z))


(define-overload (add x y) (+ x y))
(define-overload (add s1 s2) (string-append s1 s2))

(define result0 (add 1 2))
(define result1 (add "one" "two"))
(define result2 (fold-left add 0 (list 1 2 3 4 5)))
(define result3 (fold-left add "" (list "a" "b" "c" "d" "e")))
(define result4
  (let ((fold-left-instance fold-left))
    (fold-left-instance add 0 (list 1 2 3 4 5))))


(define result_add3
  (let ((add3 (lambda (x y z) (add x (add y z)))))
    (add3 1 2 3)))


(define (unpack-pair x)
  (values (car x) (cdr x)))

(define (fold-left f z xs)
  (if (pair? xs)
    (let*-values (((x xs) (unpack-pair xs))
                  ((z) (f z x)))
      (fold-left f z xs))
    z))


(define-overload (add x y) (+ x y))
(define-overload (add s1 s2) (string-append s1 s2))

(define result1 (fold-left add 0 (list 1 2 3 4 5)))
(define result2 (fold-left add "" (list "a" "b" "c" "d" "e")))


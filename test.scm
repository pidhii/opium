
;(define-record-type (pair T U)
  ;(pair (:car T) (:cdr U))
  ;pair?
  ;(:car car set-car!)
  ;(:cdr cdr set-cdr!))

;(predicate (result-of (pair T U) (pair T U)))
;(predicate (result-of (pair? (pair _ _)) boolean))
;(predicate (result-of (car (pair T _)) T))
;(predicate (result-of (set-car! (pair T _) T) nil))
;(predicate (result-of (cdr (pair _ U)) U))
;(predicate (result-of (set-cdr! (pair _ U) U) nil))




;(define-variant-type (cons-list T)
  ;(cons T (cons-list T))
  ;(empty-list))

;(predicate (result-of (cons T (cons-list T)) (cons-list T)))
;(predicate (result-of (empty-list) (cons-list _)))
;(predicate (match-on (cons T (cons-list T)) (cons-list T)))
;(predicate (match-on (empty-list) (cons-list _)))


(define (add-lists xs ys)
  (cases (xs ys)
    (((cons x xs) (cons y ys))
     (cons (add x y) (add-lists xs ys)))
    ((a b) (empty-list))))

(define (fold-left f z xs)
  (cases (xs)
    (((cons x xs))
      (let ((z2 (f z x)))
        (fold-left f z2 xs)))
    (((empty-list)) z)))

;(define (test x)
  ;(cases x
    ;((foo (foo a b) c) d)
    ;((foo (bar e f) g) h)
    ;((bar (foo i j) k) l)
    ;((bar (bar (foo m1 m2) n) o) p)
    ;((bar (bar (bar q1 q2) r) s) t)))


(define-overload (add x y) (+ x y))
(define-overload (add s1 s2) (string-append s1 s2))

(define result0 (add 1 2))
(define result1 (add "one" "two"))

(define result2 (fold-left add 0 (list 1 2 3 4 5)))
(define result3 (fold-left add "" (list "a" "b" "c" "d" "e")))

(define result4
  (let ((fold-left-instance fold-left))
    (fold-left-instance add 0 (list 1 2 3 4 5))))

(define result_add-lists
  (add-lists (list 1 2 3) (list 4 5 6)))


(define result_add3
  (let ((add3 (lambda (x y z) (add x (add y z)))))
    (add3 1 2 3)))

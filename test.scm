
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scheme builtin functions
;;
(pragma prolog
  (ensure-loaded "prolog-std.scm")
  ;; builting arithmetics
  (predicate (result-of (+ num num) num))
  ;; builtin string functions
  (predicate (result-of (string-append . Strs) str) (all str Strs))
  ;; builtin functions for cons-lists
  (predicate (result-of (list . Ts) (cons-list T)) (all T Ts))
  (predicate (result-of (car (cons-list T)) T))
  (predicate (result-of (cdr (cons-list T)) (cons-list T)))
  ;; values
  (predicate (result-of (values . Values) Values))
  ;; misc
  (predicate (result-of (display _) void))
  (predicate (result-of (newline) void)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; type cons-list T = cons T (cons-list T)
;;                  | empty-list
;;
;; Note: we will simply use native pairs (pair?) of Scheme for the cons-list.
;;
(pragma prolog
  ;; constructor types
  (predicate (result-of (cons T (cons-list T)) (cons-list T)))
  (predicate (result-of (empty-list) (cons-list _)))
  ;; rules for type-cases
  (predicate (match-on (cons T (cons-list T)) (cons-list T)))
  (predicate (match-on (empty-list) (cons-list _))))
;;
(pragma scheme-translator
  ;; rules for translation of cases expressions with cons-list
  (cases-rule (cons _ _) (cons-list _) pair? unpack-pair)
  (cases-rule (empty-list) (cons-list _) null? <unused>) ;; nothing to unpack
  ;; inline auxiliary functions to avoid type-check and name mangling
  (inline
    (define (unpack-pair p) (values (car p) (cdr p)))
    (define (empty-list) '())))



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

(define (join sep strings)
  (cases (strings)
    (((cons first rest))
     (define (join s1 s2) (string-append s1 sep s2))
     (fold-left join first rest))
    ((_) "")))

(define-overload (add x y) (+ x y))
(define-overload (add s1 s2) (string-append s1 s2))

(display (add 1 2)) (newline)
(display (add "one" "two")) (newline)

(display (fold-left add 0 (list 1 2 3 4 5))) (newline)
(display (fold-left add "" (list "a" "b" "c" "d" "e"))) (newline)

(display
  (let ((fold-left-instance fold-left))
    (fold-left-instance add 0 (list 1 2 3 4 5))))
(newline)

(display (add-lists (list 1 2 3) (list 4 5 6))) (newline)

(display
  (let ((add3 (lambda (x y z) (add x (add y z)))))
    (add3 1 2 3)))
(newline)

(display (join ", " (list "one" "two" "three"))) (newline)


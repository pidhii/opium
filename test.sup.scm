(ensure-loaded "prolog-std.scm")


(predicate (result-of (pair? _) boolean))

;(predicate (result-of (cons X Y) (cons X Y)))
(predicate (result-of (cons T (cons-list T)) (cons-list T)))
(predicate (result-of (nil) (cons-list _)))

(predicate (result-of (unpack-tuple/2 (tuple/2 T U)) (T U)))
(predicate (result-of (unpack-pair (cons-list T)) (T (cons-list T))))

(predicate (result-of (<f> num num) (tuple/2 str num)))
(predicate (result-of (<xs>) (cons-list num)))



; x - y
(predicate (result-of (- num num) num))
; x > y
(predicate (result-of (> num num) boolean))


; and-expression
(predicate (result-of (and . Clauses) boolean) (all boolean Clauses))
(predicate (all X (X . Tail)) (all X Tail))
(predicate (all X ()))



; real_time ()
(predicate (result-of (real_time) num))
; poll thread
(predicate (result-of (poll Thread) Return)
  (result-of (Thread) Return))
; some?
(predicate (result-of (some? (maybe _)) boolean))
; values
(predicate (result-of (values . Values) (values . Values)))

(predicate (result-of (<thread>) (maybe num)))


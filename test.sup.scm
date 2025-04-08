(ensure-loaded "prolog-std.scm")


(predicate (result-of (pair? _) boolean))


;(predicate (result-of (cons X Y) (cons X Y)))
(predicate (result-of (cons T (cons-list T)) (cons-list T)))

(predicate (result-of (car (cons-list T)) T))
(predicate (result-of (cdr (cons-list T)) (cons-list T)))

(predicate (result-of (<xs>) (cons-list num)))

(predicate (result-of (list . Ts) (cons-list T))
  (all T Ts))


; x - y
(predicate (result-of (- num num) num))
; x + y
(predicate (result-of (+ num num) num))
; x > y
(predicate (result-of (> num num) boolean))



; real_time ()
(predicate (result-of (real_time) num))
; poll thread
(predicate (result-of (poll Thread) Return)
  (result-of (Thread) Return))
; some?
(predicate (result-of (some? (maybe _)) boolean))
; values
(predicate (result-of (values . Values) Values))

(predicate (result-of (<thread>) (maybe num)))

;(predicate (result-of (unpack-pair (cons-list T)) (T (cons-list T))))


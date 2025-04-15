(ensure-loaded "prolog-std.scm")


(predicate (result-of (pair? _) boolean))

;; ==========================
;;  create function template
;; ==========================
;; [Args, Result, Body]
;; -> (= Template (#dynamic-function-dispatch 'Args 'Result `Body))
;; -> reference the template via this variable (Template)

;; ===============================
;;  instantiate function template
;; ===============================
;; [Template]
;; -> (insert-cells Template <Specialization>)

;; =============================
;;  invoking the specialization
;; =============================
(predicate (result-of ((#dynamic-function-dispatch _ BindArgs BindResult Body) . Args) Result)
  (= BindArgs Args)
  (= BindResult Result)
  (call Body))


;(predicate (result-of (cons X Y) (cons X Y)))
(predicate (result-of (cons T (cons-list T)) (cons-list T)))

(predicate (result-of (car (cons-list T)) T))
(predicate (result-of (cdr (cons-list T)) (cons-list T)))

(predicate (result-of (<xs>) (cons-list num)))

(predicate (result-of (list . Ts) (cons-list T))
  (all T Ts))

(predicate (result-of (unpack-pair (cons-list T)) (T (cons-list T))))

(predicate (result-of (print _) nil))


; x - y
(predicate (result-of (- num num) num))
; x + y
(predicate (result-of (+ num num) num))
; x > y
(predicate (result-of (> num num) boolean))


(predicate (result-of (string-append str str) str))


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


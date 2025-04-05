(define-variant-type (cons-list T)
  (cons T (cons-list T))
  ;; produces:
  ;;   (predicate (result-of (cons T (cons-list T)) (cons-list T)))
  ;; add global binding:
  ;;   cons cons
  none
  ;; add global binding:
  ;;   none (cons-list _)
  )


(define (fold_map f z xs)
  (if (pair? xs)
    (let*-values (((x xs) (unpack-pair xs))
                  ((x z) (unpack-tuple/2 (f z x))))
      (cons x (fold_map f z xs)))
    none))

(define result (fold_map <f> <z> (<xs>)))


(define (runThread thread timeout)
    (let* ((starttime (real_time))
           (mayberesult (poll thread))
           (stoptime (real_time))
           (exectime (- stoptime starttime)))
      (if (and (some? mayberesult) (> exectime timeout))
        (values mayberesult exectime)
        (runThread thread (- timeout exectime)))))

(define result (runThread <thread> 0))

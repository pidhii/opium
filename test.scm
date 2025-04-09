(define (unpack-pair x)
  (values (car x) (cdr x)))


(define (fold-left f z xs)
  (if (pair? xs)
    (let*-values (((x xs) (unpack-pair xs))
                  ((z) (f z x)))
      (fold-left f z xs))
    z))


(define (add x y) (+ x y))
(define (add s1 s2) (string-append s1 s2))

(define result1 (fold-left add 0 (list 1 2 3 4 5)))
(define result2 (fold-left add "" (list "a" "b" "c" "d" "e")))

;type list(T) = cons(T, list T) | none

;fold_left(f, z, xs) =
  ;match xs in
  ;| cons(x, xs) -> fold_left(f, f(z, x), xs)
  ;| none -> z

  ;if cons(x, xs) = xs then
    ;fold_left(f, f(z, x), xs)
  ;else z

;getTempColor(temp) =
  ;cond
  ;| temp < 45 -> "#66ff66"
  ;| temp < 50 -> "#77ff00"
  ;| temp < 55 -> "#88ff00"
  ;| temp < 60 -> "#99ff00"
  ;| temp < 65 -> "#aaff00"
  ;| temp < 70 -> "#bbff00"
  ;| temp < 75 -> "#cccc00"
  ;| temp < 80 -> "#dd8800"
  ;| temp < 90 -> "#ee5500"
  ;| otherwize -> "#ff0000"

;makeMarkup(temp, color) =
  ;format(R"(<span color="%d">%dC</span>)", color, ...)

;data = popen("sneosrs -j 2>/dev/null", READ).parse_json()
;gpu_pci_name = list(data).map(first).find(\l -> l.match(/amdgpu-cpi-[0-9]{4}/))



;type list T = cons T (list T) | none

;def fold_left f z xs =
  ;match xs in
  ;| cons x xs -> fold_left f (f z x) xs
  ;| none -> z


;def getTempColor temp =
  ;cond
  ;| temp < 45 -> "#66ff66"
  ;| temp < 50 -> "#77ff00"
  ;| temp < 55 -> "#88ff00"
  ;| temp < 60 -> "#99ff00"
  ;| temp < 65 -> "#aaff00"
  ;| temp < 70 -> "#bbff00"
  ;| temp < 75 -> "#cccc00"
  ;| temp < 80 -> "#dd8800"
  ;| temp < 90 -> "#ee5500"
  ;| otherwize -> "#ff0000"

;def makeMarkup temp color =
  ;format R"(<span color="%d">%dC</span>)" color ...

;data = popen "sneosrs -j 2>/dev/null" READ |> parse_json()
;gpu_pci_name = list data |> map first |> find (\l -> match /amdgpu-cpi-[0-9]{4}/ l)


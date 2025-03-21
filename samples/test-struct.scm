(predicate (= X X))
(predicate (test (cons a b) c))
(predicate (test (cons a c) b))

(query (= x x))
(query (= x y))
(query (test (cons a X) c))


(predicate (cadr Cadr (cons Car (cons Cadr Cddr))))

(query
  (and
    (= L (cons a (cons b c)))
    (cadr Cadr L)))
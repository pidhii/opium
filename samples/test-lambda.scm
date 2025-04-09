(predicate (= X X))

(predicate (return-of ((lambda:1234 C1 C2 C3 ...) X1 X2 X3) Return)
  ...)

(query
  (and (= Foo (lambda:1234 FooX) FooX #t))
       (return-of (Foo 42) X)
       (return-of (Foo 24) Y)
      ))
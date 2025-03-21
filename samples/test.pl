loves(sam, glan).
loves(jill, bill).
loves(bob, _).
loves(ada, _).
loves(X, X).
loves(X, ivan) :- loves(X, bill).

loves(X, Y) :- loves(Y, X).

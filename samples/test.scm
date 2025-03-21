(predicate (loves sam glan))
(predicate (loves jill bill))
(predicate (loves bob X))
(predicate (loves ada X))
(predicate (loves X X))
(predicate (loves X ivan) (loves X bill))

(query (loves jill Q))

; Malicious code 
(predicate (loves X Y) (loves Y X))
(query (loves X ivan))
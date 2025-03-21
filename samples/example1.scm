; source: https://athena.ecs.csus.edu/~mei/logicp/prolog/programming-examples.html

; Example 1 : Below food table shows the facts, rules, goals and their english meanings.
;
; Facts
; food(burger). 	  // burger is a food
; food(sandwich). 	// sandwich is a food
; food(pizza).    	// pizza is a food
; lunch(sandwich).  // sandwich is a lunch
; dinner(pizza).   	// pizza is a dinner
; 
(predicate (food burger))
(predicate (food sandwich))
(predicate (food pizza))
(predicate (lunch sandwich))
(predicate (dinner pizza))

; Rules 	
; meal(X) :- food(X).  // Every food is a meal OR
;                      // Anything is a meal if it is a food
; 
(predicate (meal X) (food X))

; Queries / Goals 	
; ?- food(pizza).       // Is pizza a food?
; ?- meal(X), lunch(X). // Which food is meal and lunch? 
; ?- dinner(sandwich).  // Is sandwich a dinner?
(query (food pizza))
(query (and (meal X) (lunch X)))
(query (dinner sandwich))



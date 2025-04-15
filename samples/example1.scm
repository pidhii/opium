;; Opium - Ultimate static type system for type-annotation-free code
;; Copyright (C) 2025  Ivan Pidhurskyi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


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



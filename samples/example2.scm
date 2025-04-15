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

; Example 2 : Below student-professor relation table shows the facts, rules, goals and their english meanings.
; Facts                       English meanings 
; studies(charlie, csc135).  // charlie studies csc135
; studies(olivia, csc135).   // olivia studies csc135
; studies(jack, csc131).     // jack studies csc131
; studies(arthur, csc134).   // arthur studies csc134
;   
(predicate (studies charlie csc135))
(predicate (studies olivia csc135))
(predicate (studies jack csc131))
(predicate (studies arthur csc134))

; teaches(kirke, csc135).     // kirke teaches csc135
; teaches(collins, csc131).   // collins teaches csc131
; teaches(collins, csc171).   // collins teaches csc171
; teaches(juniper, csc134).   // juniper teaches csc134
;    
(predicate (teaches kirke csc135))
(predicate (teaches collins csc131))
(predicate (teaches collins csc171))
(predicate (teaches juniper csc134))

; Rules  
; professor(X, Y) :-
; teaches(X, C), studies(Y, C).  // X is a professor of Y if X teaches C and Y studies C.
; 
(predicate (professor X Y)
  (teaches X C)
  (studies Y C))

; Queries / Goals   
; ?- studies(charlie, What).      // charlie studies what? OR What does charlie study?
; ?- professor(kirke, Students).  // Who are the students of professor kirke.  
;
(query (studies charlie What))
(query (professor kirke Students))

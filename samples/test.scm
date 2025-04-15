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


(predicate (loves sam glan))
(predicate (loves jill bill))
(predicate (loves bob X))
(predicate (loves ada X))
(predicate (loves X X))
(predicate (loves X ivan) (loves X bill))

(query (loves jill Q))

; Malicious code 
(predicate (loves X Y) (loves Y X))

(query (loves ivan X))


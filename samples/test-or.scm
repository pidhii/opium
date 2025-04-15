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


(predicate (= X X))

(predicate (human-color Color)
  (or (= Color white)
      (= Color black)
      (= Color yellow)))

(predicate (origin-of white europe))
(predicate (origin-of black afrika))
(predicate (origin-of yellow asia))
(predicate (origin-of green mars))


(predicate (people-inhabit Place)
  (origin-of SkinColor Place)
  (human-color SkinColor))

(query
  (and (people-inhabit europe)
       (people-inhabit afrika)))

(query (people-inhabit mars))



(predicate (food pizza))
(predicate (drink water))
(query (food X))
(query (drink X))
(query (or (food X) (drink X)))


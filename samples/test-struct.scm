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
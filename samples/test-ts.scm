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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test #1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; real_time ()
(predicate (return-type (real_time) numeric))

; poll thread
(predicate (return-type (poll Thread) Return)
  (return-type (Thread) Return))

; x - y
(predicate (return-type (operator- numeric numeric) numeric))

; runThread thread timeout
(predicate (return-type (runThread Thread Timeout) Return)
  (return-type (real_time) StartTime)
  (return-type (poll Thread) MaybeResult)
  (return-type (real_time) StopTime)
  (return-type (operator- StopTime StartTime) ExecTime)
  (or (= Return (record MaybeResult ExecTime))
      (and (return-type (operator- Timeout ExecTime) Tmp)
           (return-type (runThread Thread Tmp) Return))))


(predicate (return-type (thread ) numeric))
(query (return-type (runThread thread numeric) T))
(query
  (and 
       (= Thread thread)
       (= Timeout numeric)
       (return-type (real_time) StartTime)
       (return-type (poll Thread) MaybeResult)
       (return-type (real_time) StopTime)
       (return-type (operator- StopTime StartTime) ExecTime)
       (or (= Return (record MaybeResult ExecTime))
           (and (return-type (operator- Timeout ExecTime) Tmp)
               (return-type (runThread Thread Tmp) Return)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test #2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fold_map
; --------
;
; (define (fold_map f z xs)
;   (if-match ((cons x xs) xs)
;     (match ((tuple/2 x z) (f z x))
;       (cons x (fold_map f z xs)))
;     nil))
;
; (define (fold_map f z xs)
;   (if (pair? xs)
;       (let-values (((x xs) (unpack-pair)))
;           (let-values (((x z) (unpack-tuple/2 (f z x))))
;               (cons x (fold_map f z xs))))
;       '()))
 
(predicate (return-type (fold_map F Z Xs) Return)
  (or (and (= Xs (cons X Xs_))
           (return-type (F Z X) (pair X_ Z_))
           (return-type (fold_map F Z_ Xs_) Tmp0)
           (= Return (cons X_ Tmp0)))
      (= Return nil)))

(predicate (return-type (f numeric numeric) (pair numeric numeric)))

(predicate (cons-list ValueType ListType)
  (or (= ListType (cons ValueType ListType))
      (= ListType nil)))
(query (cons-list numeric L))

(query
  (and (cons-list numeric NumericList)
       (return-type (fold_map f numeric NumericList) X)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test #3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; x == y
(predicate (return-type (operator== numeric numeric) boolean))

; def even x = if x == 0 then true else odd (x - 1)
(predicate (return-type (even X) Return)
  (return-type (operator== X numeric) boolean)
  (or (= Return boolean)
      (and (return-type (operator- X numeric) Tmp0)
           (return-type (odd Tmp0) Return))))

; def odd x = if x == 0 then false else even (x - 1)
(predicate (return-type (odd X) Return)
  (return-type (operator== X numeric) boolean)
  (or (= Return boolean)
      (and (return-type (operator- X numeric) Tmp0)
           (return-type (even Tmp0) Return))))

(query (return-type (operator== X numeric) boolean))
(query (return-type (even numeric) X))
(query
  (and (return-type (operator== X numeric) boolean)
       (or (= Return boolean)
           (and (return-type (operator- X numeric) Tmp0)
               (return-type (even Tmp0) Return)))))



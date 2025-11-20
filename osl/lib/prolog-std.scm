;;;;
;; maplist/2 Goal L1 L2
;;
;; Apply Goal to pairs of elements from lists L1 and L2.
;; Gloal must thus be a predicate of arity 2.
;; Fails if lists are not of the same length.
;;;;
(predicate (maplist/2 _ () ()))
(predicate (maplist/2 G (X . Xs) (Y . Ys))
  (call G X Y)
  (maplist/2 G Xs Ys))

;;;;
;; append HeadList TailList ResList
;;
;; Succeeds if ResList is a concatenation of lists HeadList and TailLists.
;;;;
(predicate (append () Y Y))
(predicate (append (X . Xs) Y (X . NewXs))
  (append Xs Y NewXs))

;;;;
;; xappend TailList HeadList ResList
;;
;; Same as append but with swapped order of the first two arguments.
;;;;
(predicate (xappend Tail List Result)
  (append List Tail Result))

;;;;
;; member Elt List
;;
;; Succeeds if Elt is present in list List
;;;;
;(predicate (member X (X . _)))
;(predicate (member X (_ . List))
  ;(member X List))
(predicate (member X (Y . Ys))
  (if (= X Y) #t (member X Ys)))

;;;;
;; member@ Elt List
;;
;; Succeeds if Elt equivalent (=@=) to an element in list List
;;;;
(predicate (member@ X (Y . _))
  (=@= X Y))
(predicate (member@ X (_ . List))
  (member@ X List))


;;;;
;; flatten ListOfLists List
;;;;
(predicate (flatten () ()))
(predicate (flatten (FirstList . OtherLists) List)
  (if (= OtherLists ())
      (= List FirstList)
      (and (flatten OtherLists MergedOtherLists)
           (append FirstList MergedOtherLists List))))

;;;;
;; fold
;;
;; (query (fold-left append () ((1 2) (3 4) (5 6)) Res))
;; Res = (1 2 3 4 5 6)
;; => yes
;;;;
(predicate (fold-left _ Z () Z))
(predicate (fold-left Op Z (X . Xs) Res)
  (call Op Z X NewZ)
  (fold-left Op NewZ Xs Res))


;;;;
;; set-union Set1 Set2 Set1-U-Set2
;;;;
(predicate (set-union () Set Set))
(predicate (set-union (X . Set1) Set2 Set3)
  (if (member X Set2)
      (set-union Set1 Set2 Set3)
      (set-union Set1 (X . Set2) Set3)))

;;;;
;; set-union@ Set1 Set2 Set1-U-Set2
;;;;
(predicate (set-union@ () Set Set))
(predicate (set-union@ (X . Set1) Set2 Set3)
  (if (member@ X Set2)
      (set-union@ Set1 Set2 Set3)
      (set-union@ Set1 (X . Set2) Set3)))

;;;;
;; all X List
;;;;
(predicate (all _ ()))
(predicate (all X (X . List))
  (all X List))


;;;;
;; subset SubSet Set
;;;;
(predicate (subset () _))
(predicate (subset (X . Xs) Ys)
  (member X Ys)
  (subset Xs Ys))

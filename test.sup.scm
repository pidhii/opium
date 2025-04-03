
(predicate (= X X))

; (result-of (f x1 ... (union a b) ... xn) Result)
; = (and (result-of (f x1 ... a ... xn) R1)
;        (result-of (f x1 ... b ... xn) R2)
;        (= Result (union R1 R2)))

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
(predicate (member X (X . _)))
(predicate (member X (_ . List))
  (member X List))

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



(predicate (append-to-variants SeqVariants X NewSeqVariants)
  (maplist/2 (xappend (X)) SeqVariants NewSeqVariants))

(predicate (lift-unions () SeqVariants SeqVariants))
(predicate (lift-unions (InHead . InTail) CurSeqVariants NewSeqVariants)
  (if (and (nonvar InHead) (= InHead (union . UnionVariants)))
      (and (maplist/2 (append-to-variants CurSeqVariants) UnionVariants VariantsOfUpdatedSeqVariants)
           (flatten VariantsOfUpdatedSeqVariants UpdatedSeqVariants))
      (maplist/2 (xappend (InHead)) CurSeqVariants UpdatedSeqVariants))
  (lift-unions InTail UpdatedSeqVariants NewSeqVariants))

(predicate (variants-of X Variants)
  (if (and (nonvar X) (= X (union . Variants)))
      (and)
      (= Variants (X))))

(predicate (unify X Y Result)
  (debug "X: " X ", Y: " Y)
  (variants-of X XVariants)
  (variants-of Y YVariants)
  (set-union XVariants YVariants XAndYVariants)
  (if (= XAndYVariants (Result))
      (and)
      (= Result (union . XAndYVariants))))

(predicate (result-of* Form Result)
  (lift-unions Form (()) FormVariants)
  (maplist/2 result-of FormVariants ResultVariants)
  (debug ResultVariants)
  (= ResultVariants (FirstResult . OtherResults))
  (fold-left unify FirstResult OtherResults Result))

(predicate (result-of (pair? _) boolean))

(predicate (result-of (cons X Y) (cons X Y)))
(predicate (result-of (unpack-tuple/2 (tuple/2 T U)) (T U)))

(predicate (result-of (unpack-pair (cons T U)) (T U)))

(predicate (result-of (<f> num num) (tuple/2 num num)))
;(predicate (result-of (<xs>) (union List nil)) (= List (cons num List)))
(predicate (result-of (<xs>) List) (= List (cons num List)))



; x - y
(predicate (result-of (- num num) num))
; x > y
(predicate (result-of (> num num) boolean))


; and-expression
(predicate (result-of (and . Clauses) boolean) (all boolean Clauses))
(predicate (all X (X . Tail)) (all X Tail))
(predicate (all X ()))



(predicate (maybe T (union (some T) none)))
;(predicate (maybe T (some T)))
;(predicate (maybe _ none))

; real_time ()
(predicate (result-of (real_time) num))
; poll thread
(predicate (result-of (poll Thread) Return)
  (result-of (Thread) Return))
; some?
(predicate (result-of (some? (some _)) boolean))
(predicate (result-of (some? none) boolean))
; values
(predicate (result-of (values . Values) (values . Values)))

(predicate (result-of (<thread>) Result)
  (maybe num Result))


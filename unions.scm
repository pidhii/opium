
(predicate (variant-of* X Variant)
  (if (and (nonvar X) (= X (#union . XVariants)))
      (and (member XVariant XVariants)
           (variant-of* XVariant Variant))
      (if (and (nonvar X) (= X (XHead . XTail)))
          (and (variant-of* XHead XHeadVariant)
               (variant-of* XTail XTailVariant)
               (= Variant (XHeadVariant . XTailVariant)))
          (= X Variant))))

(predicate (variants-of X Variants)
  (if (and (nonvar X) (= X (#union . Variants)))
      (and)
      (= Variants (X))))

(predicate (variants-of* X Variants)
  (query (variant-of* X) Variants))

(predicate (append-to-variants SeqVariants X NewSeqVariants)
  (maplist/2 (xappend (X)) SeqVariants NewSeqVariants))

(predicate (lift-unions () SeqVariants SeqVariants))
(predicate (lift-unions (InHead . InTail) CurSeqVariants NewSeqVariants)
  (if (and (nonvar InHead) (= InHead (#union . UnionVariants)))
      (and (maplist/2 (append-to-variants CurSeqVariants) UnionVariants VariantsOfUpdatedSeqVariants)
           (flatten VariantsOfUpdatedSeqVariants UpdatedSeqVariants))
      (maplist/2 (xappend (InHead)) CurSeqVariants UpdatedSeqVariants))
  (lift-unions InTail UpdatedSeqVariants NewSeqVariants))


(predicate (unify X Y Result)
  (variants-of X XVariants)
  (variants-of Y YVariants)
  (set-union@ XVariants YVariants XAndYVariants)
  (if (= XAndYVariants (Result))
      (and)
      (= Result (#union . XAndYVariants))))


(predicate (result-of* Form Result)
  (lift-unions Form (()) FormVariants)
  (variants-of* Form FormVariants)
  (debug FormVariants)
  (maplist/2 result-of FormVariants ResultVariants)
  (= ResultVariants (FirstResult . OtherResults))
  (fold-left unify FirstResult OtherResults Result))


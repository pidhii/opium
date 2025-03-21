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


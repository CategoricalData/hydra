(ns hydra.lib.pairs)

;; Pairs are 2-element lists: (list a b)

;; bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
(def hydra_lib_pairs_bimap
  "Map over both elements of a pair."
  (fn [f] (fn [g] (fn [p] (list (f (first p)) (g (second p)))))))

;; first :: Pair a b -> a
(def hydra_lib_pairs_first
  "Get the first element of a pair."
  (fn [p] (first p)))

;; second :: Pair a b -> b
(def hydra_lib_pairs_second
  "Get the second element of a pair."
  (fn [p] (second p)))

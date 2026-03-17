(ns hydra.lib.pairs)

;; Pairs are 2-element lists: (list a b)

;; bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
(def hydra_lib_pairs_bimap
  (fn [f] (fn [g] (fn [p] (list (f (first p)) (g (second p)))))))

;; first :: Pair a b -> a
(def hydra_lib_pairs_first
  (fn [p] (first p)))

;; second :: Pair a b -> b
(def hydra_lib_pairs_second
  (fn [p] (second p)))

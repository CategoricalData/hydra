(ns hydra.lib.sets
  (:require [hydra.lib.equality :refer [generic-compare]]))

;; Sets are Clojure hash sets for O(1) amortized insert/lookup/delete.
;; toList produces a sorted output (via generic-compare) for determinism.

(defn- to-hash-set
  "Ensure we have a hash set."
  [coll]
  (if (set? coll) coll (set coll)))

;; delete :: a -> Set a -> Set a
(def hydra_lib_sets_delete
  (fn [x] (fn [s] (disj (to-hash-set s) x))))

;; difference :: Set a -> Set a -> Set a
(def hydra_lib_sets_difference
  (fn [s1] (fn [s2]
    (let [s2-set (to-hash-set s2)]
      (reduce (fn [acc x] (if (contains? s2-set x) (disj acc x) acc))
              (to-hash-set s1)
              s1)))))

;; empty :: Set a
(def hydra_lib_sets_empty #{})

;; from_list :: [a] -> Set a
(def hydra_lib_sets_from_list
  (fn [xs] (set xs)))

;; insert :: a -> Set a -> Set a
(def hydra_lib_sets_insert
  (fn [x] (fn [s] (conj (to-hash-set s) x))))

;; intersection :: Set a -> Set a -> Set a
(def hydra_lib_sets_intersection
  (fn [s1] (fn [s2]
    (let [s2-set (to-hash-set s2)]
      (reduce (fn [acc x] (if (contains? s2-set x) acc (disj acc x)))
              (to-hash-set s1)
              s1)))))

;; map :: (a -> b) -> Set a -> Set b
(def hydra_lib_sets_map
  (fn [f] (fn [s]
    (set (map f s)))))

;; member :: a -> Set a -> Bool
(def hydra_lib_sets_member
  (fn [x] (fn [s] (contains? (to-hash-set s) x))))

;; null :: Set a -> Bool
(def hydra_lib_sets_null
  (fn [s] (empty? s)))

;; size :: Set a -> Int
(def hydra_lib_sets_size
  (fn [s] (count s)))

;; singleton :: a -> Set a
(def hydra_lib_sets_singleton
  (fn [x] #{x}))

;; to_list :: Set a -> [a]
;; Sort for deterministic output
(def hydra_lib_sets_to_list
  (fn [s] (sort generic-compare (seq s))))

;; union :: Set a -> Set a -> Set a
(def hydra_lib_sets_union
  (fn [s1] (fn [s2]
    (into (to-hash-set s1) s2))))

;; unions :: [Set a] -> Set a
(def hydra_lib_sets_unions
  (fn [ss]
    (reduce (fn [acc s] (into acc s))
            #{}
            ss)))

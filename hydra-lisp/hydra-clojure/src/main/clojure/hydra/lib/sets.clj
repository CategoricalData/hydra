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
  "Delete an element from a set."
  (fn [x] (fn [s] (disj (to-hash-set s) x))))

;; difference :: Set a -> Set a -> Set a
(def hydra_lib_sets_difference
  "Compute the difference of two sets."
  (fn [s1] (fn [s2]
    (let [s2-set (to-hash-set s2)]
      (reduce (fn [acc x] (if (contains? s2-set x) (disj acc x) acc))
              (to-hash-set s1)
              s1)))))

;; empty :: Set a
(def hydra_lib_sets_empty
  "Create an empty set."
  #{})

;; from_list :: [a] -> Set a
(def hydra_lib_sets_from_list
  "Create a set from a list."
  (fn [xs] (set xs)))

;; insert :: a -> Set a -> Set a
(def hydra_lib_sets_insert
  "Insert an element into a set."
  (fn [x] (fn [s] (conj (to-hash-set s) x))))

;; intersection :: Set a -> Set a -> Set a
(def hydra_lib_sets_intersection
  "Compute the intersection of two sets."
  (fn [s1] (fn [s2]
    (let [s2-set (to-hash-set s2)]
      (reduce (fn [acc x] (if (contains? s2-set x) acc (disj acc x)))
              (to-hash-set s1)
              s1)))))

;; map :: (a -> b) -> Set a -> Set b
(def hydra_lib_sets_map
  "Map a function over a set."
  (fn [f] (fn [s]
    (set (map f s)))))

;; member :: a -> Set a -> Bool
(def hydra_lib_sets_member
  "Check if an element is in a set."
  (fn [x] (fn [s] (contains? (to-hash-set s) x))))

;; null :: Set a -> Bool
(def hydra_lib_sets_null
  "Check if a set is empty."
  (fn [s] (empty? s)))

;; singleton :: a -> Set a
(def hydra_lib_sets_singleton
  "Create a singleton set."
  (fn [x] #{x}))

;; size :: Set a -> Int
(def hydra_lib_sets_size
  "Get the size of a set."
  (fn [s] (count s)))

;; to_list :: Set a -> [a]
;; Sort for deterministic output
(def hydra_lib_sets_to_list
  "Convert a set to a list."
  (fn [s] (sort generic-compare (seq s))))

;; union :: Set a -> Set a -> Set a
(def hydra_lib_sets_union
  "Compute the union of two sets."
  (fn [s1] (fn [s2]
    (into (to-hash-set s1) s2))))

;; unions :: [Set a] -> Set a
(def hydra_lib_sets_unions
  "Compute the union of multiple sets."
  (fn [ss]
    (reduce (fn [acc s] (into acc s))
            #{}
            ss)))

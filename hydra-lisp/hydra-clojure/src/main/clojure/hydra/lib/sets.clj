(ns hydra.lib.sets
  (:require [hydra.lib.equality :refer [generic-compare]]))

;; Sets are sorted lists with no duplicates

(defn set-insert [x s]
  (cond
    (empty? s) (list x)
    (= (generic-compare x (first s)) 0) s
    (< (generic-compare x (first s)) 0) (cons x s)
    :else (cons (first s) (set-insert x (rest s)))))

(defn set-member? [x s]
  (cond
    (empty? s) false
    (= x (first s)) true
    :else (recur x (rest s))))

;; delete :: a -> Set a -> Set a
(def hydra_lib_sets_delete
  (fn [x] (fn [s] (remove #(= x %) s))))

;; difference :: Set a -> Set a -> Set a
(def hydra_lib_sets_difference
  (fn [s1] (fn [s2]
    (filter (fn [x] (not (set-member? x s2))) s1))))

;; empty :: Set a
(def hydra_lib_sets_empty ())

;; from_list :: [a] -> Set a
(def hydra_lib_sets_from_list
  (fn [xs]
    (loop [rest_ (seq xs) acc ()]
      (if (nil? rest_)
        acc
        (recur (next rest_) (set-insert (first rest_) acc))))))

;; insert :: a -> Set a -> Set a
(def hydra_lib_sets_insert
  (fn [x] (fn [s] (set-insert x s))))

;; intersection :: Set a -> Set a -> Set a
(def hydra_lib_sets_intersection
  (fn [s1] (fn [s2]
    (filter (fn [x] (set-member? x s2)) s1))))

;; map :: (a -> b) -> Set a -> Set b
(def hydra_lib_sets_map
  (fn [f] (fn [s]
    (loop [rest_ (seq s) acc ()]
      (if (nil? rest_)
        acc
        (recur (next rest_) (set-insert (f (first rest_)) acc)))))))

;; member :: a -> Set a -> Bool
(def hydra_lib_sets_member
  (fn [x] (fn [s] (set-member? x s))))

;; null :: Set a -> Bool
(def hydra_lib_sets_null
  (fn [s] (empty? s)))

;; size :: Set a -> Int
(def hydra_lib_sets_size
  (fn [s] (count s)))

;; singleton :: a -> Set a
(def hydra_lib_sets_singleton
  (fn [x] (list x)))

;; to_list :: Set a -> [a]
(def hydra_lib_sets_to_list
  (fn [s] s))

;; union :: Set a -> Set a -> Set a
(def hydra_lib_sets_union
  (fn [s1] (fn [s2]
    (loop [rest_ (seq s2) acc s1]
      (if (nil? rest_)
        acc
        (recur (next rest_) (set-insert (first rest_) acc)))))))

;; unions :: [Set a] -> Set a
(def hydra_lib_sets_unions
  (fn [ss]
    (loop [rest_ (seq ss) acc ()]
      (if (nil? rest_)
        acc
        (recur (next rest_) ((hydra_lib_sets_union acc) (first rest_)))))))

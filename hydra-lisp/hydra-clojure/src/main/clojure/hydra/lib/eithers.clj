(ns hydra.lib.eithers
  (:require [hydra.lib.equality :refer [generic-compare]]))

;; Either representation: (list :left val) or (list :right val)

;; bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
(def hydra_lib_eithers_bimap
  (fn [f] (fn [g] (fn [e]
    (if (= (first e) :left)
      (list :left (f (second e)))
      (list :right (g (second e))))))))

;; bind :: Either a b -> (b -> Either a c) -> Either a c
(def hydra_lib_eithers_bind
  (fn [e] (fn [f]
    (if (= (first e) :left)
      e
      (f (second e))))))

;; either :: (a -> c) -> (b -> c) -> Either a b -> c
(def hydra_lib_eithers_either
  (fn [f] (fn [g] (fn [e]
    (if (= (first e) :left)
      (f (second e))
      (g (second e)))))))

;; map :: (b -> c) -> Either a b -> Either a c
(def hydra_lib_eithers_map
  (fn [f] (fn [e]
    (if (= (first e) :left)
      e
      (list :right (f (second e)))))))

;; map_list :: (a -> Either e b) -> [a] -> Either e [b]
(def hydra_lib_eithers_map_list
  (fn [f] (fn [xs]
    (loop [rest_ (seq xs) acc ()]
      (if (nil? rest_)
        (list :right (reverse acc))
        (let [result (f (first rest_))]
          (if (= (first result) :left)
            result
            (recur (next rest_) (cons (second result) acc)))))))))

;; map_maybe :: (a -> Either e b) -> Maybe a -> Either e (Maybe b)
(def hydra_lib_eithers_map_maybe
  (fn [f] (fn [m]
    (cond
      (or (nil? m)
          (and (sequential? m) (empty? m))
          (and (sequential? m) (= (first m) :nothing)))
      (list :right (list :nothing))

      (and (sequential? m) (= (first m) :just))
      (let [result (f (second m))]
        (if (= (first result) :left)
          result
          (list :right (list :just (second result)))))

      (and (sequential? m) (= (first m) :maybe)
           (>= (count m) 2) (not (nil? (second m))))
      (let [result (f (second m))]
        (if (= (first result) :left)
          result
          (list :right (list :maybe (second result)))))

      ;; bare value (term-level maybe: (:maybe val) where val is not wrapped)
      :else
      (let [result (f m)]
        (if (= (first result) :left)
          result
          (list :right (second result))))))))

;; foldl :: (a -> b -> Either c a) -> a -> [b] -> Either c a
(def hydra_lib_eithers_foldl
  (fn [f] (fn [init] (fn [xs]
    (loop [rest_ (seq xs) acc init]
      (if (nil? rest_)
        (list :right acc)
        (let [result ((f acc) (clojure.core/first rest_))]
          (if (= (clojure.core/first result) :left)
            result
            (recur (next rest_) (second result))))))))))

;; from_left :: a -> Either a b -> a
;; Thunk-aware: if def_ is a zero-arg fn, only called when Either is Right
(def hydra_lib_eithers_from_left
  (fn [def_] (fn [e]
    (if (= (first e) :left)
      (second e)
      (if (fn? def_) (def_) def_)))))

;; from_right :: b -> Either a b -> b
;; Thunk-aware: if def_ is a zero-arg fn, only called when Either is Left
(def hydra_lib_eithers_from_right
  (fn [def_] (fn [e]
    (if (= (first e) :right)
      (second e)
      (if (fn? def_) (def_) def_)))))

;; is_left :: Either a b -> Bool
(def hydra_lib_eithers_is_left
  (fn [e] (= (first e) :left)))

;; is_right :: Either a b -> Bool
(def hydra_lib_eithers_is_right
  (fn [e] (= (first e) :right)))

;; lefts :: [Either a b] -> [a]
(def hydra_lib_eithers_lefts
  (fn [es] (map second (filter #(= (first %) :left) es))))

;; partition_eithers :: [Either a b] -> ([a], [b])
(def hydra_lib_eithers_partition_eithers
  (fn [es]
    (let [lefts_  (vec (map second (filter #(= (first %) :left) es)))
          rights_ (vec (map second (filter #(= (first %) :right) es)))]
      (list lefts_ rights_))))

;; rights :: [Either a b] -> [b]
(def hydra_lib_eithers_rights
  (fn [es] (map second (filter #(= (first %) :right) es))))

;; map_set :: (a -> Either e b) -> Set a -> Either e (Set b)
(def hydra_lib_eithers_map_set
  (fn [f] (fn [s]
    (loop [rest_ (seq s) acc ()]
      (if (nil? rest_)
        (list :right (sort-by pr-str (distinct (reverse acc))))
        (let [result (f (first rest_))]
          (if (= (first result) :left)
            result
            (recur (next rest_) (cons (second result) acc)))))))))

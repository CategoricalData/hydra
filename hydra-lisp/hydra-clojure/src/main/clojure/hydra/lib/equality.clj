(ns hydra.lib.equality)

(defn generic-compare [a b]
  (cond
    (= a b) 0
    (and (number? a) (number? b)) (compare a b)
    (and (string? a) (string? b)) (compare a b)
    (and (char? a) (char? b)) (compare (int a) (int b))
    (and (keyword? a) (keyword? b)) (compare a b)
    (and (boolean? a) (boolean? b)) (compare a b)
    (and (sequential? a) (sequential? b))
    (let [len-a (count a) len-b (count b)]
      (if (not= len-a len-b)
        (compare len-a len-b)
        (loop [ra (seq a) rb (seq b)]
          (if (nil? ra) 0
              (let [c (generic-compare (clojure.core/first ra) (clojure.core/first rb))]
                (if (not= c 0) c (recur (next ra) (next rb))))))))
    (and (nil? a) (nil? b)) 0
    (nil? a) -1
    (nil? b) 1
    :else (compare (pr-str a) (pr-str b))))

;; compare :: a -> a -> Comparison
;; Returns a Comparison union: (list :less_than nil), (list :equal_to nil), (list :greater_than nil)
(def hydra_lib_equality_compare
  (fn [a] (fn [b]
    (let [c (generic-compare a b)]
      (cond
        (neg? c) (list :less_than nil)
        (zero? c) (list :equal_to nil)
        :else (list :greater_than nil))))))

;; equal :: a -> a -> Bool
(def hydra_lib_equality_equal
  (fn [a] (fn [b] (= a b))))

;; gt :: a -> a -> Bool
(def hydra_lib_equality_gt
  (fn [a] (fn [b] (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
(def hydra_lib_equality_gte
  (fn [a] (fn [b] (>= (generic-compare a b) 0))))

;; identity :: a -> a
(def hydra_lib_equality_identity
  (fn [x] x))

;; lt :: a -> a -> Bool
(def hydra_lib_equality_lt
  (fn [a] (fn [b] (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
(def hydra_lib_equality_lte
  (fn [a] (fn [b] (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
(def hydra_lib_equality_max
  (fn [a] (fn [b] (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
(def hydra_lib_equality_min
  (fn [a] (fn [b] (if (<= (generic-compare a b) 0) a b))))

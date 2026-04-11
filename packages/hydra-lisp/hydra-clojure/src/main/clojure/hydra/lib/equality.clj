(ns hydra.lib.equality)

(defn generic-compare
  "Generic comparison function for arbitrary Clojure values, returning -1, 0, or 1."
  [a b]
  (cond
    (identical? a b) 0
    (nil? a) (if (nil? b) 0 -1)
    (nil? b) 1
    (and (number? a) (number? b)) (compare a b)
    (and (string? a) (string? b)) (compare a b)
    (and (keyword? a) (keyword? b)) (compare a b)
    (and (boolean? a) (boolean? b)) (compare a b)
    (and (char? a) (char? b)) (compare (int a) (int b))
    ;; Records (defrecord instances): compare by type name first, then fields
    (instance? clojure.lang.IRecord a)
    (if (instance? clojure.lang.IRecord b)
      (let [ta (type a) tb (type b)]
        (if (= ta tb)
          ;; Same record type: compare field values in key order
          (let [ka (sort (keys a)) ;; record keys are always the same for same type
                ]
            (loop [ks (seq ka)]
              (if (nil? ks) 0
                (let [k (first ks)
                      c (generic-compare (get a k) (get b k))]
                  (if (not= c 0) c (recur (next ks)))))))
          (compare (str ta) (str tb))))
      (compare (str (type a)) (str (type b))))
    (map? a)
    (if (map? b)
      (let [ca (count a) cb (count b)]
        (if (not= ca cb)
          (compare ca cb)
          (loop [ra (seq a) rb (seq b)]
            (cond
              (nil? ra) 0
              :else
              (let [ea (first ra) eb (first rb)
                    ck (generic-compare (key ea) (key eb))]
                (if (not= ck 0) ck
                  (let [cv (generic-compare (val ea) (val eb))]
                    (if (not= cv 0) cv
                      (recur (next ra) (next rb))))))))))
      (compare (str (type a)) (str (type b))))
    (and (sequential? a) (sequential? b))
    (let [len-a (count a) len-b (count b)]
      (if (not= len-a len-b)
        (compare len-a len-b)
        (loop [ra (seq a) rb (seq b)]
          (if (nil? ra) 0
              (let [c (generic-compare (clojure.core/first ra) (clojure.core/first rb))]
                (if (not= c 0) c (recur (next ra) (next rb))))))))
    :else (compare (pr-str a) (pr-str b))))

;; compare :: a -> a -> Comparison
;; Returns a Comparison union: (list :less_than nil), (list :equal_to nil), (list :greater_than nil)
(def hydra_lib_equality_compare
  "Compare two values and return a Comparison."
  (fn [a] (fn [b]
    (let [c (generic-compare a b)]
      (cond
        (neg? c) (list :less_than nil)
        (zero? c) (list :equal_to nil)
        :else (list :greater_than nil))))))

;; equal :: a -> a -> Bool
(def hydra_lib_equality_equal
  "Check if two values are equal."
  (fn [a] (fn [b] (= a b))))

;; gt :: a -> a -> Bool
(def hydra_lib_equality_gt
  "Check if first value is greater than second."
  (fn [a] (fn [b] (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
(def hydra_lib_equality_gte
  "Check if first value is greater than or equal to second."
  (fn [a] (fn [b] (>= (generic-compare a b) 0))))

;; identity :: a -> a
(def hydra_lib_equality_identity
  "Return a value unchanged."
  (fn [x] x))

;; lt :: a -> a -> Bool
(def hydra_lib_equality_lt
  "Check if first value is less than second."
  (fn [a] (fn [b] (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
(def hydra_lib_equality_lte
  "Check if first value is less than or equal to second."
  (fn [a] (fn [b] (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
(def hydra_lib_equality_max
  "Return the maximum of two values."
  (fn [a] (fn [b] (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
(def hydra_lib_equality_min
  "Return the minimum of two values."
  (fn [a] (fn [b] (if (<= (generic-compare a b) 0) a b))))

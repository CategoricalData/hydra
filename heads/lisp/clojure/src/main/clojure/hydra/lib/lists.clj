(ns hydra.lib.lists
  (:require [hydra.lib.equality :refer [generic-compare]]))

;; concat :: [[a]] -> [a]
(def hydra_lib_lists_concat
  (fn [xss] (apply concat xss)))

;; concat2 :: [a] -> [a] -> [a]
(def hydra_lib_lists_concat2
  (fn [xs] (fn [ys] (concat xs ys))))

;; cons :: a -> [a] -> [a]
(def hydra_lib_lists_cons
  (fn [x] (fn [xs] (cons x xs))))

;; drop :: Int -> [a] -> [a]
(def hydra_lib_lists_drop
  (fn [n] (fn [xs] (drop n xs))))

;; drop_while :: (a -> Bool) -> [a] -> [a]
(def hydra_lib_lists_drop_while
  (fn [pred_] (fn [xs] (drop-while pred_ xs))))

;; elem :: a -> [a] -> Bool
(def hydra_lib_lists_elem
  (fn [x] (fn [xs]
    (loop [rest_ (seq xs)]
      (cond
        (nil? rest_) false
        (= x (first rest_)) true
        :else (recur (next rest_)))))))

;; filter :: (a -> Bool) -> [a] -> [a]
(def hydra_lib_lists_filter
  (fn [pred_] (fn [xs] (filter pred_ xs))))

;; find :: (a -> Bool) -> [a] -> Maybe a
(def hydra_lib_lists_find
  (fn [pred_] (fn [xs]
    (loop [rest_ (seq xs)]
      (cond
        (nil? rest_) (list :nothing)
        (pred_ (first rest_)) (list :just (first rest_))
        :else (recur (next rest_)))))))

;; foldl :: (b -> a -> b) -> b -> [a] -> b
(def hydra_lib_lists_foldl
  (fn [f] (fn [init_] (fn [xs]
    (loop [acc init_ rest_ (seq xs)]
      (if (nil? rest_)
        acc
        (recur ((f acc) (first rest_)) (next rest_))))))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(def hydra_lib_lists_foldr
  (fn [f] (fn [init_] (fn [xs]
    (reduce (fn [acc el] ((f el) acc)) init_ (reverse xs))))))

;; intercalate :: [a] -> [[a]] -> [a]
(def hydra_lib_lists_intercalate
  (fn [sep] (fn [xss]
    (if (empty? xss)
      ()
      (loop [rest_ (next (seq xss)) acc (list (first xss))]
        (if (nil? rest_)
          (apply concat (reverse acc))
          (recur (next rest_) (cons (first rest_) (cons sep acc)))))))))

;; intersperse :: a -> [a] -> [a]
(def hydra_lib_lists_intersperse
  (fn [sep] (fn [xs]
    (if (empty? xs)
      ()
      (loop [rest_ (next (seq xs)) acc (list (first xs))]
        (if (nil? rest_)
          (reverse acc)
          (recur (next rest_) (cons (first rest_) (cons sep acc)))))))))

;; length :: [a] -> Int
(def hydra_lib_lists_length
  (fn [xs] (count xs)))

;; map :: (a -> b) -> [a] -> [b]
(def hydra_lib_lists_map
  (fn [f] (fn [xs] (map f xs))))

;; maybe_at :: Int -> [a] -> Maybe a
(def hydra_lib_lists_maybe_at
  (fn [n] (fn [xs]
    (if (and (>= n 0) (< n (count xs)))
      (list :just (nth xs n))
      (list :nothing)))))

;; maybe_head :: [a] -> Maybe a
(def hydra_lib_lists_maybe_head
  (fn [xs]
    (if (empty? xs)
      (list :nothing)
      (list :just (first xs)))))

;; maybe_init :: [a] -> Maybe [a]
(def hydra_lib_lists_maybe_init
  (fn [xs]
    (if (empty? xs)
      (list :nothing)
      (list :just (or (butlast xs) ())))))

;; maybe_last :: [a] -> Maybe a
(def hydra_lib_lists_maybe_last
  (fn [xs]
    (if (empty? xs)
      (list :nothing)
      (list :just (last xs)))))

;; maybe_tail :: [a] -> Maybe [a]
(def hydra_lib_lists_maybe_tail
  (fn [xs]
    (if (empty? xs)
      (list :nothing)
      (list :just (rest xs)))))

;; nub :: [a] -> [a]  (remove duplicates, preserve order)
(def hydra_lib_lists_nub
  (fn [xs]
    (loop [rest_ (seq xs) seen #{} acc ()]
      (if (nil? rest_)
        (reverse acc)
        (let [x (first rest_)]
          (if (contains? seen x)
            (recur (next rest_) seen acc)
            (recur (next rest_) (conj seen x) (cons x acc))))))))

;; null :: [a] -> Bool
(def hydra_lib_lists_null
  (fn [xs] (empty? xs)))

;; partition :: (a -> Bool) -> [a] -> Pair [a] [a]
(def hydra_lib_lists_partition
  (fn [pred_] (fn [xs]
    (loop [rest_ (seq xs) yes () no ()]
      (if (nil? rest_)
        (list (reverse yes) (reverse no))
        (if (pred_ (first rest_))
          (recur (next rest_) (cons (first rest_) yes) no)
          (recur (next rest_) yes (cons (first rest_) no))))))))

;; pure :: a -> [a]
(def hydra_lib_lists_pure
  (fn [x] (list x)))

;; replicate :: Int -> a -> [a]
(def hydra_lib_lists_replicate
  (fn [n] (fn [x] (repeat n x))))

;; reverse :: [a] -> [a]
(def hydra_lib_lists_reverse
  (fn [xs] (reverse xs)))

;; singleton :: a -> [a]
(def hydra_lib_lists_singleton
  (fn [x] (list x)))

;; sort :: [a] -> [a]
(def hydra_lib_lists_sort
  (fn [xs] (sort-by identity generic-compare xs)))

;; sort_on :: (a -> b) -> [a] -> [a]
(def hydra_lib_lists_sort_on
  (fn [f] (fn [xs] (sort-by f generic-compare xs))))

;; span :: (a -> Bool) -> [a] -> Pair [a] [a]
(def hydra_lib_lists_span
  (fn [pred_] (fn [xs]
    (loop [rest_ (seq xs) acc ()]
      (cond
        (nil? rest_) (list (reverse acc) ())
        (pred_ (first rest_)) (recur (next rest_) (cons (first rest_) acc))
        :else (list (reverse acc) rest_))))))

;; take :: Int -> [a] -> [a]
(def hydra_lib_lists_take
  (fn [n] (fn [xs] (take n xs))))

;; transpose :: [[a]] -> [[a]]
;; Haskell semantics: handles ragged lists (takes what's available at each index)
(def hydra_lib_lists_transpose
  (fn [xss]
    (if (empty? xss)
      ()
      (let [max-len (apply max (map count xss))]
        (loop [i 0 acc ()]
          (if (>= i max-len)
            (reverse acc)
            (let [col (loop [rows (seq xss) col-acc ()]
                        (if (nil? rows)
                          (reverse col-acc)
                          (let [row (first rows)]
                            (if (> (count row) i)
                              (recur (next rows) (cons (nth row i) col-acc))
                              (recur (next rows) col-acc)))))]
              (if (empty? col)
                (reverse acc)
                (recur (inc i) (cons col acc))))))))))

;; uncons :: [a] -> Maybe (a, [a])
(def hydra_lib_lists_uncons
  (fn [xs]
    (if (empty? xs)
      (list :nothing)
      (list :just (list (first xs) (rest xs))))))

;; apply :: [a -> b] -> [a] -> [b]
(def hydra_lib_lists_apply
  (fn [fs] (fn [xs]
    (mapcat (fn [f] (map f xs)) fs))))

;; bind :: [a] -> (a -> [b]) -> [b]
(def hydra_lib_lists_bind
  (fn [xs] (fn [f] (mapcat f xs))))

;; group :: [a] -> [[a]]
(def hydra_lib_lists_group
  (fn [xs]
    (if (empty? xs)
      ()
      (loop [rest_ (next (seq xs)) curr (first xs) acc-group (list (first xs)) groups ()]
        (if (nil? rest_)
          (reverse (cons (reverse acc-group) groups))
          (let [x (first rest_)]
            (if (= x curr)
              (recur (next rest_) curr (cons x acc-group) groups)
              (recur (next rest_) x (list x) (cons (reverse acc-group) groups)))))))))

;; zip :: [a] -> [b] -> [Pair a b]
(def hydra_lib_lists_zip
  (fn [xs] (fn [ys] (map list xs ys))))

;; zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
(def hydra_lib_lists_zip_with
  (fn [f] (fn [xs] (fn [ys]
    (map (fn [x y] ((f x) y)) xs ys)))))

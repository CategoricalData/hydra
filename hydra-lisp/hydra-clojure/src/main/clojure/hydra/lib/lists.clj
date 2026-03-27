(ns hydra.lib.lists
  (:require [hydra.lib.equality :refer [generic-compare]]))

;; apply :: [a -> b] -> [a] -> [b]
(def hydra_lib_lists_apply
  "Apply a list of functions to a list of values (applicative style)."
  (fn [fs] (fn [xs]
    (mapcat (fn [f] (map f xs)) fs))))

;; at :: Int -> [a] -> a
(def hydra_lib_lists_at
  "Get the element at a specified index in a list."
  (fn [n] (fn [xs] (nth xs n))))

;; bind :: [a] -> (a -> [b]) -> [b]
(def hydra_lib_lists_bind
  "Apply a function that returns lists to each element and flatten results."
  (fn [xs] (fn [f] (mapcat f xs))))

;; concat :: [[a]] -> [a]
(def hydra_lib_lists_concat
  "Concatenate a list of lists."
  (fn [xss] (apply concat xss)))

;; concat2 :: [a] -> [a] -> [a]
(def hydra_lib_lists_concat2
  "Concatenate two lists."
  (fn [xs] (fn [ys] (concat xs ys))))

;; cons :: a -> [a] -> [a]
(def hydra_lib_lists_cons
  "Prepend a value to a list."
  (fn [x] (fn [xs] (cons x xs))))

;; drop :: Int -> [a] -> [a]
(def hydra_lib_lists_drop
  "Drop the first n elements from a list."
  (fn [n] (fn [xs] (drop n xs))))

;; drop_while :: (a -> Bool) -> [a] -> [a]
(def hydra_lib_lists_drop_while
  "Drop elements from the beginning of a list while predicate is true."
  (fn [pred_] (fn [xs] (drop-while pred_ xs))))

;; elem :: a -> [a] -> Bool
(def hydra_lib_lists_elem
  "Check if an element is in a list."
  (fn [x] (fn [xs]
    (loop [rest_ (seq xs)]
      (cond
        (nil? rest_) false
        (= x (first rest_)) true
        :else (recur (next rest_)))))))

;; filter :: (a -> Bool) -> [a] -> [a]
(def hydra_lib_lists_filter
  "Filter a list based on a predicate."
  (fn [pred_] (fn [xs] (filter pred_ xs))))

;; find :: (a -> Bool) -> [a] -> Maybe a
(def hydra_lib_lists_find
  "Find the first element matching a predicate."
  (fn [pred_] (fn [xs]
    (loop [rest_ (seq xs)]
      (cond
        (nil? rest_) (list :nothing)
        (pred_ (first rest_)) (list :just (first rest_))
        :else (recur (next rest_)))))))

;; foldl :: (b -> a -> b) -> b -> [a] -> b
(def hydra_lib_lists_foldl
  "Fold a list from the left."
  (fn [f] (fn [init_] (fn [xs]
    (loop [acc init_ rest_ (seq xs)]
      (if (nil? rest_)
        acc
        (recur ((f acc) (first rest_)) (next rest_))))))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(def hydra_lib_lists_foldr
  "Fold a list from the right."
  (fn [f] (fn [init_] (fn [xs]
    (reduce (fn [acc el] ((f el) acc)) init_ (reverse xs))))))

;; group :: [a] -> [[a]]
(def hydra_lib_lists_group
  "Group consecutive equal elements."
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

;; head :: [a] -> a
(def hydra_lib_lists_head
  "Get the first element of a list."
  (fn [xs] (first xs)))

;; init :: [a] -> [a]  (all but last)
(def hydra_lib_lists_init
  "Return all elements except the last one."
  (fn [xs] (or (butlast xs) ())))

;; intercalate :: [a] -> [[a]] -> [a]
(def hydra_lib_lists_intercalate
  "Intercalate a list of lists with a separator list between each."
  (fn [sep] (fn [xss]
    (if (empty? xss)
      ()
      (loop [rest_ (next (seq xss)) acc (list (first xss))]
        (if (nil? rest_)
          (apply concat (reverse acc))
          (recur (next rest_) (cons (first rest_) (cons sep acc)))))))))

;; intersperse :: a -> [a] -> [a]
(def hydra_lib_lists_intersperse
  "Intersperse a value between elements of a list."
  (fn [sep] (fn [xs]
    (if (empty? xs)
      ()
      (loop [rest_ (next (seq xs)) acc (list (first xs))]
        (if (nil? rest_)
          (reverse acc)
          (recur (next rest_) (cons (first rest_) (cons sep acc)))))))))

;; last :: [a] -> a
(def hydra_lib_lists_last
  "Get the last element of a list."
  (fn [xs] (last xs)))

;; length :: [a] -> Int
(def hydra_lib_lists_length
  "Get the length of a list."
  (fn [xs] (count xs)))

;; map :: (a -> b) -> [a] -> [b]
(def hydra_lib_lists_map
  "Map a function over a list."
  (fn [f] (fn [xs] (map f xs))))

;; nub :: [a] -> [a]  (remove duplicates, preserve order)
(def hydra_lib_lists_nub
  "Remove duplicate elements from a list."
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
  "Check if a list is empty."
  (fn [xs] (empty? xs)))

;; partition :: (a -> Bool) -> [a] -> Pair [a] [a]
(def hydra_lib_lists_partition
  "Partition a list into elements that satisfy a predicate and elements that do not."
  (fn [pred_] (fn [xs]
    (loop [rest_ (seq xs) yes () no ()]
      (if (nil? rest_)
        (list (reverse yes) (reverse no))
        (if (pred_ (first rest_))
          (recur (next rest_) (cons (first rest_) yes) no)
          (recur (next rest_) yes (cons (first rest_) no))))))))

;; pure :: a -> [a]
(def hydra_lib_lists_pure
  "Create a list with a single element."
  (fn [x] (list x)))

;; replicate :: Int -> a -> [a]
(def hydra_lib_lists_replicate
  "Create a list with n copies of a value."
  (fn [n] (fn [x] (repeat n x))))

;; reverse :: [a] -> [a]
(def hydra_lib_lists_reverse
  "Reverse a list."
  (fn [xs] (reverse xs)))

;; safe_head :: [a] -> Maybe a
(def hydra_lib_lists_safe_head
  "Get the first element of a list, returning Nothing if the list is empty."
  (fn [xs]
    (if (empty? xs)
      (list :nothing)
      (list :just (first xs)))))

;; singleton :: a -> [a]
(def hydra_lib_lists_singleton
  "Create a single-element list."
  (fn [x] (list x)))

;; sort :: [a] -> [a]
(def hydra_lib_lists_sort
  "Sort a list."
  (fn [xs] (sort-by identity generic-compare xs)))

;; sort_on :: (a -> b) -> [a] -> [a]
(def hydra_lib_lists_sort_on
  "Sort a list based on a key function."
  (fn [f] (fn [xs] (sort-by f generic-compare xs))))

;; span :: (a -> Bool) -> [a] -> Pair [a] [a]
(def hydra_lib_lists_span
  "Split a list at the first element where predicate fails."
  (fn [pred_] (fn [xs]
    (loop [rest_ (seq xs) acc ()]
      (cond
        (nil? rest_) (list (reverse acc) ())
        (pred_ (first rest_)) (recur (next rest_) (cons (first rest_) acc))
        :else (list (reverse acc) rest_))))))

;; tail :: [a] -> [a]
(def hydra_lib_lists_tail
  "Get all elements of a list except the first."
  (fn [xs] (rest xs)))

;; take :: Int -> [a] -> [a]
(def hydra_lib_lists_take
  "Take the first n elements from a list."
  (fn [n] (fn [xs] (take n xs))))

;; transpose :: [[a]] -> [[a]]
;; Haskell semantics: handles ragged lists (takes what's available at each index)
(def hydra_lib_lists_transpose
  "Transpose a list of lists."
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

;; zip :: [a] -> [b] -> [Pair a b]
(def hydra_lib_lists_zip
  "Zip two lists into pairs."
  (fn [xs] (fn [ys] (map list xs ys))))

;; zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
(def hydra_lib_lists_zip_with
  "Zip two lists with a combining function."
  (fn [f] (fn [xs] (fn [ys]
    (map (fn [x y] ((f x) y)) xs ys)))))

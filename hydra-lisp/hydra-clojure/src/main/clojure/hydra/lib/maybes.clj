(ns hydra.lib.maybes)

;; Maybe representation: nil, (:just val), or (:nothing) for native maybes.
;; IMPORTANT: Do NOT treat (:maybe ...) as a native maybe — that's
;; the Hydra Term.Maybe term constructor, not a maybe value.

(defn maybe-nothing? [m]
  (or (nil? m)
      (and (sequential? m) (empty? m))
      (and (sequential? m) (= (first m) :nothing))))

(defn maybe-value [m]
  (cond
    (and (sequential? m) (= (first m) :just)) (second m)
    :else m))

;; bind :: Maybe a -> (a -> Maybe b) -> Maybe b
(def hydra_lib_maybes_bind
  (fn [m] (fn [f]
    (if (maybe-nothing? m)
      (list :nothing)
      (f (maybe-value m))))))

;; cat :: [Maybe a] -> [a]
(def hydra_lib_maybes_cat
  (fn [ms]
    (loop [rest_ ms acc ()]
      (if (empty? rest_)
        (reverse acc)
        (let [m (first rest_)]
          (if (not (maybe-nothing? m))
            (recur (next rest_) (cons (maybe-value m) acc))
            (recur (next rest_) acc)))))))

;; from_just :: Maybe a -> a
(def hydra_lib_maybes_from_just
  (fn [m]
    (if (maybe-nothing? m)
      (throw (ex-info "fromJust: Nothing" {}))
      (maybe-value m))))

;; from_maybe :: a -> Maybe a -> a
(def hydra_lib_maybes_from_maybe
  (fn [def_] (fn [m]
    (if (maybe-nothing? m)
      def_
      (maybe-value m)))))

;; is_just :: Maybe a -> Bool
(def hydra_lib_maybes_is_just
  (fn [m] (not (maybe-nothing? m))))

;; is_nothing :: Maybe a -> Bool
(def hydra_lib_maybes_is_nothing
  (fn [m] (maybe-nothing? m)))

;; map :: (a -> b) -> Maybe a -> Maybe b
(def hydra_lib_maybes_map
  (fn [f] (fn [m]
    (if (maybe-nothing? m)
      (if (or (nil? m) (and (sequential? m) (empty? m)))
        nil  ;; preserve nil/empty representation for term-level
        (list :nothing))
      (let [result (f (maybe-value m))]
        (if (and (sequential? m) (= (first m) :just))
          (list :just result)
          result))))))

;; map_maybe :: (a -> Maybe b) -> [a] -> [b]
(def hydra_lib_maybes_map_maybe
  (fn [f] (fn [xs]
    (loop [rest_ (seq xs) acc ()]
      (if (nil? rest_)
        (reverse acc)
        (let [result (f (first rest_))]
          (if (not (maybe-nothing? result))
            (recur (next rest_) (cons (maybe-value result) acc))
            (recur (next rest_) acc))))))))

;; maybe :: b -> (a -> b) -> Maybe a -> b
(def hydra_lib_maybes_maybe
  (fn [def_] (fn [f] (fn [m]
    (if (maybe-nothing? m)
      def_
      (f (maybe-value m)))))))

;; apply :: Maybe (a -> b) -> Maybe a -> Maybe b
(def hydra_lib_maybes_apply
  (fn [mf] (fn [ma]
    (if (maybe-nothing? mf)
      (list :nothing)
      (if (maybe-nothing? ma)
        (list :nothing)
        (list :just ((maybe-value mf) (maybe-value ma))))))))

;; cases :: Maybe a -> b -> (a -> b) -> b
(def hydra_lib_maybes_cases
  (fn [m] (fn [nothing-val] (fn [just-fn]
    (if (maybe-nothing? m)
      nothing-val
      (just-fn (maybe-value m)))))))

;; compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
(def hydra_lib_maybes_compose
  (fn [f] (fn [g] (fn [x]
    (let [result (f x)]
      (if (maybe-nothing? result)
        (list :nothing)
        (g (maybe-value result))))))))

;; pure :: a -> Maybe a
(def hydra_lib_maybes_pure
  (fn [x] (list :just x)))

;; to_list :: Maybe a -> [a]
(def hydra_lib_maybes_to_list
  (fn [m]
    (if (maybe-nothing? m)
      ()
      (list (maybe-value m)))))

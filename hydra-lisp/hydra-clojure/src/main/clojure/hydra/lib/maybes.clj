(ns hydra.lib.maybes)

;; Maybe representation: nil, (:just val), or (:nothing) for native maybes.
;; IMPORTANT: Do NOT treat (:maybe ...) as a native maybe -- that's
;; the Hydra Term.Maybe term constructor, not a maybe value.

(defn maybe-nothing?
  "Check if a value represents Nothing in the Maybe encoding."
  [m]
  (or (nil? m)
      (and (sequential? m) (empty? m))
      (and (sequential? m) (= (first m) :nothing))))

(defn maybe-value
  "Extract the value from a Maybe encoding."
  [m]
  (cond
    (and (sequential? m) (= (first m) :just)) (second m)
    :else m))

;; apply :: Maybe (a -> b) -> Maybe a -> Maybe b
(def hydra_lib_maybes_apply
  "Apply a function to an argument (applicative)."
  (fn [mf] (fn [ma]
    (if (maybe-nothing? mf)
      (list :nothing)
      (if (maybe-nothing? ma)
        (list :nothing)
        (list :just ((maybe-value mf) (maybe-value ma))))))))

;; bind :: Maybe a -> (a -> Maybe b) -> Maybe b
(def hydra_lib_maybes_bind
  "Chain operations on optional values, handling Nothing cases automatically."
  (fn [m] (fn [f]
    (if (maybe-nothing? m)
      (list :nothing)
      (f (maybe-value m))))))

;; cases :: Maybe a -> b -> (a -> b) -> b
;; Thunk-aware: if nothing-val is a zero-arg fn, only called when Maybe is Nothing
(def hydra_lib_maybes_cases
  "Handle an optional value with the maybe value as the first argument."
  (fn [m] (fn [nothing-val] (fn [just-fn]
    (if (maybe-nothing? m)
      (if (fn? nothing-val) (nothing-val) nothing-val)
      (just-fn (maybe-value m)))))))

;; cat :: [Maybe a] -> [a]
(def hydra_lib_maybes_cat
  "Filter out Nothing values from a list."
  (fn [ms]
    (loop [rest_ ms acc ()]
      (if (empty? rest_)
        (reverse acc)
        (let [m (first rest_)]
          (if (not (maybe-nothing? m))
            (recur (next rest_) (cons (maybe-value m) acc))
            (recur (next rest_) acc)))))))

;; compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
(def hydra_lib_maybes_compose
  "Compose two Maybe-returning functions (Kleisli composition)."
  (fn [f] (fn [g] (fn [x]
    (let [result (f x)]
      (if (maybe-nothing? result)
        (list :nothing)
        (g (maybe-value result))))))))

;; from_just :: Maybe a -> a
(def hydra_lib_maybes_from_just
  "Extract value from a Just, or error on Nothing (partial function)."
  (fn [m]
    (if (maybe-nothing? m)
      (throw (ex-info "fromJust: Nothing" {}))
      (maybe-value m))))

;; from_maybe :: a -> Maybe a -> a
;; Thunk-aware: if def_ is a zero-arg fn, only called when Maybe is Nothing
(def hydra_lib_maybes_from_maybe
  "Get a value from an optional value, or return a default value."
  (fn [def_] (fn [m]
    (if (maybe-nothing? m)
      (if (fn? def_) (def_) def_)
      (maybe-value m)))))

;; is_just :: Maybe a -> Bool
(def hydra_lib_maybes_is_just
  "Check if a value is Just."
  (fn [m] (not (maybe-nothing? m))))

;; is_nothing :: Maybe a -> Bool
(def hydra_lib_maybes_is_nothing
  "Check if a value is Nothing."
  (fn [m] (maybe-nothing? m)))

;; map :: (a -> b) -> Maybe a -> Maybe b
(def hydra_lib_maybes_map
  "Map a function over an optional value."
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
  "Map a function over a list and collect Just results."
  (fn [f] (fn [xs]
    (loop [rest_ (seq xs) acc ()]
      (if (nil? rest_)
        (reverse acc)
        (let [result (f (first rest_))]
          (if (not (maybe-nothing? result))
            (recur (next rest_) (cons (maybe-value result) acc))
            (recur (next rest_) acc))))))))

;; maybe :: b -> (a -> b) -> Maybe a -> b
;; Thunk-aware: if def_ is a zero-arg fn, only called when Maybe is Nothing
(def hydra_lib_maybes_maybe
  "Eliminate an optional value with a default and a function."
  (fn [def_] (fn [f] (fn [m]
    (if (maybe-nothing? m)
      (if (fn? def_) (def_) def_)
      (f (maybe-value m)))))))

;; pure :: a -> Maybe a
(def hydra_lib_maybes_pure
  "Lift a value into the Maybe type."
  (fn [x] (list :just x)))

;; to_list :: Maybe a -> [a]
(def hydra_lib_maybes_to_list
  "Convert a Maybe to a list: Just x becomes [x], Nothing becomes []."
  (fn [m]
    (if (maybe-nothing? m)
      ()
      (list (maybe-value m)))))

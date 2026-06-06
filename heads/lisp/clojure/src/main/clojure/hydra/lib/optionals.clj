(ns hydra.lib.optionals)

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
(def hydra_lib_optionals_bind
  (fn [m] (fn [f]
    (if (maybe-nothing? m)
      (list :nothing)
      (f (maybe-value m))))))

;; cat :: [Maybe a] -> [a]
(def hydra_lib_optionals_cat
  (fn [ms]
    (loop [rest_ ms acc ()]
      (if (empty? rest_)
        (reverse acc)
        (let [m (first rest_)]
          (if (not (maybe-nothing? m))
            (recur (next rest_) (cons (maybe-value m) acc))
            (recur (next rest_) acc)))))))

;; from_optional :: a -> Maybe a -> a
;; Thunk-aware: if def_ is a zero-arg fn, only called when Maybe is Nothing
(def hydra_lib_optionals_from_optional
  (fn [def_] (fn [m]
    (if (maybe-nothing? m)
      (if (fn? def_) (def_) def_)
      (maybe-value m)))))

;; is_given :: Maybe a -> Bool
(def hydra_lib_optionals_is_given
  (fn [m] (not (maybe-nothing? m))))

;; is_none :: Maybe a -> Bool
(def hydra_lib_optionals_is_none
  (fn [m] (maybe-nothing? m)))

;; map :: (a -> b) -> Maybe a -> Maybe b
(def hydra_lib_optionals_map
  (fn [f] (fn [m]
    (if (maybe-nothing? m)
      (if (or (nil? m) (and (sequential? m) (empty? m)))
        nil  ;; preserve nil/empty representation for term-level
        (list :nothing))
      (let [result (f (maybe-value m))]
        (if (and (sequential? m) (= (first m) :just))
          (list :just result)
          result))))))

;; map_optional :: (a -> Maybe b) -> [a] -> [b]
(def hydra_lib_optionals_map_optional
  (fn [f] (fn [xs]
    (loop [rest_ (seq xs) acc ()]
      (if (nil? rest_)
        (reverse acc)
        (let [result (f (first rest_))]
          (if (not (maybe-nothing? result))
            (recur (next rest_) (cons (maybe-value result) acc))
            (recur (next rest_) acc))))))))

;; apply :: Maybe (a -> b) -> Maybe a -> Maybe b
(def hydra_lib_optionals_apply
  (fn [mf] (fn [ma]
    (if (maybe-nothing? mf)
      (list :nothing)
      (if (maybe-nothing? ma)
        (list :nothing)
        (list :just ((maybe-value mf) (maybe-value ma))))))))

;; cases :: Maybe a -> b -> (a -> b) -> b
;; Thunk-aware: if nothing-val is a zero-arg fn, only called when Maybe is Nothing
(def hydra_lib_optionals_cases
  (fn [m] (fn [nothing-val] (fn [just-fn]
    (if (maybe-nothing? m)
      (if (fn? nothing-val) (nothing-val) nothing-val)
      (just-fn (maybe-value m)))))))

;; compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
(def hydra_lib_optionals_compose
  (fn [f] (fn [g] (fn [x]
    (let [result (f x)]
      (if (maybe-nothing? result)
        (list :nothing)
        (g (maybe-value result))))))))

;; pure :: a -> Maybe a
(def hydra_lib_optionals_pure
  (fn [x] (list :just x)))

;; to_list :: Maybe a -> [a]
(def hydra_lib_optionals_to_list
  (fn [m]
    (if (maybe-nothing? m)
      ()
      (list (maybe-value m)))))

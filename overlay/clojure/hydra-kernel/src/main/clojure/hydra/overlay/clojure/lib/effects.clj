(ns hydra.overlay.clojure.lib.effects)

;; Clojure implementations of hydra.lib.effects primitives (#494).
;;
;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t): there is no
;; TypeVariantEffect in the target, so effectful programs are ordinary eager native code and
;; "running the effect" simply means forcing the value. These primitives therefore reduce to
;; ordinary applications. See the Haskell reference implementation in Hydra.Haskell.Lib.Effects
;; (where effect<t> = IO t). All functions are curried, matching the Clojure prim runtime style.
;;
;; Optional representation (term-level): (list :none) or (list :given val).

;; apply :: effect<x -> y> -> effect<x> -> effect<y>
(def hydra_lib_effects_apply
  "Applicative apply for effects. Since effects are transparent, this just applies ef to ex."
  (fn [ef] (fn [ex] (ef ex))))

;; bind :: effect<x> -> (x -> effect<y>) -> effect<y>
(def hydra_lib_effects_bind
  "Sequence two effectful computations. Since effects are transparent, this just applies f to a."
  (fn [a] (fn [f] (f a))))

;; compose :: (x -> effect<y>) -> (y -> effect<z>) -> x -> effect<z>
(def hydra_lib_effects_compose
  "Kleisli composition for effects: run f, then g on its result."
  (fn [f] (fn [g] (fn [x] (g (f x))))))

;; fold_list :: (x -> y -> effect<x>) -> x -> [y] -> effect<x>
(def hydra_lib_effects_fold_list
  "Left-fold over a list with an effect-returning function."
  (fn [f] (fn [acc] (fn [xs]
    (reduce (fn [a x] ((f a) x)) acc xs)))))

;; map :: (x -> y) -> effect<x> -> effect<y>
(def hydra_lib_effects_map
  "Map a pure function over the result of an effect. Since effects are transparent, just apply f."
  (fn [f] (fn [a] (f a))))

;; mapList :: (x -> effect<y>) -> [x] -> effect<[y]>
(def hydra_lib_effects_map_list
  "Map an effect-returning function over a list, collecting the results."
  (fn [f] (fn [xs] (mapv f xs))))

;; mapOptional :: (x -> effect<y>) -> Maybe x -> effect<Maybe y>
(def hydra_lib_effects_map_optional
  "Map an effect-returning function over an optional."
  (fn [f] (fn [m]
    (cond
      (or (nil? m)
          (and (sequential? m) (empty? m))
          (and (sequential? m) (= (first m) :none)))
      (list :none)

      (and (sequential? m) (= (first m) :given))
      (list :given (f (second m)))

      ;; bare value (term-level maybe)
      :else
      (list :given (f m))))))

;; pure :: x -> effect<x>
(def hydra_lib_effects_pure
  "Lift a pure value into an effect. Since effects are transparent, this is the identity."
  (fn [x] x))

(ns hydra.overlay.clojure.lib.functions)

;; absurd :: void -> x
(def hydra_overlay_clojure_lib_functions_absurd
  "Eliminate a value of the uninhabited void type. Unreachable in any well-typed program."
  (fn [_v] (throw (ex-info "hydra.lib.functions.absurd: void has no inhabitants" {}))))

;; identity :: a -> a
(def hydra_overlay_clojure_lib_functions_identity
  "Return a value unchanged."
  (fn [x] x))

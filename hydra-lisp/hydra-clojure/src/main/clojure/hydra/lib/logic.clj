(ns hydra.lib.logic)

;; and :: Bool -> Bool -> Bool
(def hydra_lib_logic_and
  "Compute the logical AND of two boolean values."
  (fn [a] (fn [b] (and a b))))

;; if_else :: Bool -> a -> a -> a
(def hydra_lib_logic_if_else
  "Compute a conditional expression."
  (fn [cond_] (fn [then_] (fn [else_] (if cond_ then_ else_)))))

;; not :: Bool -> Bool
(def hydra_lib_logic_not
  "Compute the logical NOT of a boolean value."
  (fn [a] (not a)))

;; or :: Bool -> Bool -> Bool
(def hydra_lib_logic_or
  "Compute the logical OR of two boolean values."
  (fn [a] (fn [b] (or a b))))

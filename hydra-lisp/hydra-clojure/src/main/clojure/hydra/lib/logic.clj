(ns hydra.lib.logic)

;; and :: Bool -> Bool -> Bool
(def hydra_lib_logic_and
  (fn [a] (fn [b] (and a b))))

;; if_else :: Bool -> a -> a -> a
(def hydra_lib_logic_if_else
  (fn [cond_] (fn [then_] (fn [else_] (if cond_ then_ else_)))))

;; not :: Bool -> Bool
(def hydra_lib_logic_not
  (fn [a] (not a)))

;; or :: Bool -> Bool -> Bool
(def hydra_lib_logic_or
  (fn [a] (fn [b] (or a b))))

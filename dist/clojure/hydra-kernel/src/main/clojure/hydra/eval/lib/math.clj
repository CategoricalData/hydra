(ns hydra.eval.lib.math
  (:require [hydra.core :refer :all]
))

(declare hydra_eval_lib_math_even hydra_eval_lib_math_odd)

(def hydra_eval_lib_math_even (fn [cx] (fn [g] (fn [x] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.equality.equal") (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.maybes.fromMaybe") (list :literal (list :integer (list :int32 0))))) (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.math.maybeMod") x)) (list :literal (list :integer (list :int32 2))))))))) (list :literal (list :integer (list :int32 0))))))))))

(def hydra_eval_lib_math_odd (fn [cx] (fn [g] (fn [x] (list :right (list :application (->hydra_core_application (list :variable "hydra.lib.logic.not") (list :application (->hydra_core_application (list :variable "hydra.lib.math.even") x)))))))))

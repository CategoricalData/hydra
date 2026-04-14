(ns hydra.eval.lib.math
  (:require [hydra.core :refer :all]
))

(declare hydra_eval_lib_math_even hydra_eval_lib_math_odd hydra_eval_lib_math_pred hydra_eval_lib_math_succ)

(def hydra_eval_lib_math_even (fn [cx] (fn [g] (fn [x] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.equality.equal") (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.math.mod") x)) (list :literal (list :integer (list :int32 2))))))) (list :literal (list :integer (list :int32 0))))))))))

(def hydra_eval_lib_math_odd (fn [cx] (fn [g] (fn [x] (list :right (list :application (->hydra_core_application (list :variable "hydra.lib.logic.not") (list :application (->hydra_core_application (list :variable "hydra.lib.math.even") x)))))))))

(def hydra_eval_lib_math_pred (fn [cx] (fn [g] (fn [x] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.math.sub") x)) (list :literal (list :integer (list :int32 1))))))))))

(def hydra_eval_lib_math_succ (fn [cx] (fn [g] (fn [x] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.math.add") x)) (list :literal (list :integer (list :int32 1))))))))))

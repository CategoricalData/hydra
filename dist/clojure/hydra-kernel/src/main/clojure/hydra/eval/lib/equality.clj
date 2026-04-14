(ns hydra.eval.lib.equality
  (:require [hydra.core :refer :all]
))

(declare hydra_eval_lib_equality_identity hydra_eval_lib_equality_max hydra_eval_lib_equality_min)

(def hydra_eval_lib_equality_identity (fn [cx] (fn [g] (fn [x] (list :right x)))))

(def hydra_eval_lib_equality_max (fn [cx] (fn [g] (fn [x] (fn [y] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.equality.gte") x)) y)))) x)) y))))))))

(def hydra_eval_lib_equality_min (fn [cx] (fn [g] (fn [x] (fn [y] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.equality.lte") x)) y)))) x)) y))))))))

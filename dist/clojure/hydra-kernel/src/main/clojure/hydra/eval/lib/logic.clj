(ns hydra.eval.lib.logic
  (:require [hydra.core :refer :all]
))

(declare hydra_eval_lib_logic_and hydra_eval_lib_logic_not hydra_eval_lib_logic_or)

(def hydra_eval_lib_logic_and (fn [cx] (fn [g] (fn [a] (fn [b] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) b)) (list :literal (list :boolean false))))))))))

(def hydra_eval_lib_logic_not (fn [cx] (fn [g] (fn [a] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) (list :literal (list :boolean false)))) (list :literal (list :boolean true)))))))))

(def hydra_eval_lib_logic_or (fn [cx] (fn [g] (fn [a] (fn [b] (list :right (list :application (->hydra_core_application (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) (list :literal (list :boolean true)))) b))))))))

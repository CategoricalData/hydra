(ns hydra.eval.lib.sets
  (:require [hydra.core :refer :all] [hydra.extract.core :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.sets :refer :all]
))

(declare hydra_eval_lib_sets_map)

(def hydra_eval_lib_sets_map (fn [cx] (fn [g] (fn [fun] (fn [set_term] ((hydra_lib_eithers_bind (((hydra_extract_core_set cx) g) set_term)) (fn [elements] (list :right (list :application (->hydra_core_application (list :function (list :primitive "hydra.lib.sets.fromList")) (list :list ((hydra_lib_lists_map (fn [el] (list :application (->hydra_core_application fun el)))) (hydra_lib_sets_to_list elements)))))))))))))

(ns hydra.encode.classes
  (:require [hydra.classes :refer :all] [hydra.core :refer :all]
))

(declare hydra_encode_classes_type_class)

(def hydra_encode_classes_type_class (fn [match_target] ((fn [match_value] (cond (= (first match_target) :equality) ((fn [y] (list :inject (->hydra_core_injection "hydra.classes.TypeClass" (->hydra_core_field "equality" ((fn [_] (list :unit nil)) y))))) match_value) (= (first match_target) :ordering) ((fn [y] (list :inject (->hydra_core_injection "hydra.classes.TypeClass" (->hydra_core_field "ordering" ((fn [_] (list :unit nil)) y))))) match_value))) (second match_target))))

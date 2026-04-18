(ns hydra.dsl.classes
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_classes_type_class_equality hydra_dsl_classes_type_class_ordering)

(def hydra_dsl_classes_type_class_equality (list :inject (->hydra_core_injection "hydra.classes.TypeClass" (->hydra_core_field "equality" (list :unit nil)))))

(def hydra_dsl_classes_type_class_ordering (list :inject (->hydra_core_injection "hydra.classes.TypeClass" (->hydra_core_field "ordering" (list :unit nil)))))

(ns hydra.dsl.util
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_util_case_convention_camel hydra_dsl_util_case_convention_lower_snake hydra_dsl_util_case_convention_pascal hydra_dsl_util_case_convention_upper_snake hydra_dsl_util_comparison_equal_to hydra_dsl_util_comparison_greater_than hydra_dsl_util_comparison_less_than hydra_dsl_util_precision_arbitrary hydra_dsl_util_precision_bits)

(def hydra_dsl_util_case_convention_camel (list :inject (->hydra_core_injection "hydra.util.CaseConvention" (->hydra_core_field "camel" (list :unit nil)))))

(def hydra_dsl_util_case_convention_lower_snake (list :inject (->hydra_core_injection "hydra.util.CaseConvention" (->hydra_core_field "lowerSnake" (list :unit nil)))))

(def hydra_dsl_util_case_convention_pascal (list :inject (->hydra_core_injection "hydra.util.CaseConvention" (->hydra_core_field "pascal" (list :unit nil)))))

(def hydra_dsl_util_case_convention_upper_snake (list :inject (->hydra_core_injection "hydra.util.CaseConvention" (->hydra_core_field "upperSnake" (list :unit nil)))))

(def hydra_dsl_util_comparison_equal_to (list :inject (->hydra_core_injection "hydra.util.Comparison" (->hydra_core_field "equalTo" (list :unit nil)))))

(def hydra_dsl_util_comparison_greater_than (list :inject (->hydra_core_injection "hydra.util.Comparison" (->hydra_core_field "greaterThan" (list :unit nil)))))

(def hydra_dsl_util_comparison_less_than (list :inject (->hydra_core_injection "hydra.util.Comparison" (->hydra_core_field "lessThan" (list :unit nil)))))

(def hydra_dsl_util_precision_arbitrary (list :inject (->hydra_core_injection "hydra.util.Precision" (->hydra_core_field "arbitrary" (list :unit nil)))))

(def hydra_dsl_util_precision_bits (fn [x] (list :inject (->hydra_core_injection "hydra.util.Precision" (->hydra_core_field "bits" ((fn [v] v) x))))))

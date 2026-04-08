(ns hydra.extract.util
  (:require [hydra.core :refer :all] [hydra.errors :refer :all] [hydra.extract.core :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.logic :refer :all] [hydra.util :refer :all]
))

(declare hydra_extract_util_comparison)

(def hydra_extract_util_comparison (fn [cx] (fn [graph] (fn [term] ((hydra_lib_eithers_bind (((hydra_extract_core_unit_variant "hydra.util.Comparison") graph) term)) (fn [fname] (if ((hydra_lib_equality_equal ((fn [v] v) fname)) "equalTo") (list :right (list :equal_to nil)) (if ((hydra_lib_equality_equal ((fn [v] v) fname)) "lessThan") (list :right (list :less_than nil)) (if ((hydra_lib_equality_equal ((fn [v] v) fname)) "greaterThan") (list :right (list :greater_than nil)) (list :left (list :extraction (list :unexpected_shape (->hydra_errors_unexpected_shape_error "comparison" ((fn [v] v) fname))))))))))))))

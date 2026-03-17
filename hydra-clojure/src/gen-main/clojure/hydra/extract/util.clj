(ns hydra.extract.util
  (:require [hydra.context :refer :all] [hydra.core :refer :all] [hydra.error :refer :all] [hydra.extract.core :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.strings :refer :all] [hydra.util :refer :all]
))

(declare hydra_extract_util_comparison)

(def hydra_extract_util_comparison (fn [cx] (fn [graph] (fn [term] ((hydra_lib_eithers_bind ((((hydra_extract_core_unit_variant cx) "hydra.util.Comparison") graph) term)) (fn [fname] (if ((hydra_lib_equality_equal ((fn [v] v) fname)) "equalTo") (list :right (list :equal_to nil)) (if ((hydra_lib_equality_equal ((fn [v] v) fname)) "lessThan") (list :right (list :less_than nil)) (if ((hydra_lib_equality_equal ((fn [v] v) fname)) "greaterThan") (list :right (list :greater_than nil)) (list :left (->hydra_context_in_context (list :other ((hydra_lib_strings_cat2 "expected comparison but found ") ((fn [v] v) fname))) cx)))))))))))

(ns hydra.error.core
  (:require [hydra.accessors :refer :all] [hydra.core :refer :all] [hydra.variants :refer :all]
))

(declare hydra_error_core_invalid_term_error-variants)

(defrecord hydra_error_core_duplicate_binding_error [location name])
(defn make-hydra_error_core_duplicate_binding_error [location name] (->hydra_error_core_duplicate_binding_error location name))

(defrecord hydra_error_core_duplicate_field_error [location name])
(defn make-hydra_error_core_duplicate_field_error [location name] (->hydra_error_core_duplicate_field_error location name))

(def hydra_error_core_invalid_term_error-variants (list :duplicate_binding :duplicate_field))

(defrecord hydra_error_core_undefined_field_error [field_name type_name])
(defn make-hydra_error_core_undefined_field_error [field_name type_name] (->hydra_error_core_undefined_field_error field_name type_name))

(defrecord hydra_error_core_undefined_term_error [name])
(defn make-hydra_error_core_undefined_term_error [name] (->hydra_error_core_undefined_term_error name))

(defrecord hydra_error_core_undefined_type_error [name])
(defn make-hydra_error_core_undefined_type_error [name] (->hydra_error_core_undefined_type_error name))

(defrecord hydra_error_core_unexpected_term_variant_error [expected_variant actual_term])
(defn make-hydra_error_core_unexpected_term_variant_error [expected_variant actual_term] (->hydra_error_core_unexpected_term_variant_error expected_variant actual_term))

(defrecord hydra_error_core_unexpected_type_variant_error [expected_variant actual_type])
(defn make-hydra_error_core_unexpected_type_variant_error [expected_variant actual_type] (->hydra_error_core_unexpected_type_variant_error expected_variant actual_type))

(ns hydra.errors
  (:require [hydra.core :refer :all] [hydra.error.checking :refer :all] [hydra.error.core :refer :all] [hydra.paths :refer :all]
))

(declare hydra_errors_error-variants hydra_errors_extraction_error-variants hydra_errors_inference_error-variants hydra_errors_resolution_error-variants)

(defrecord hydra_errors_decoding_error [value])
(defn make-hydra_errors_decoding_error [value] (->hydra_errors_decoding_error value))

(def hydra_errors_error-variants (list :checking :decoding :duplicate_binding :duplicate_field :extraction :inference :other :resolution :undefined_field :undefined_term_variable :untyped_term_variable :unexpected_term_variant :unexpected_type_variant :unification))

(def hydra_errors_extraction_error-variants (list :empty_list :multiple_bindings :multiple_fields :no_matching_field :no_such_binding :not_enough_cases :unexpected_shape))

(def hydra_errors_inference_error-variants (list :checking :other :unification))

(defrecord hydra_errors_multiple_bindings_error [name])
(defn make-hydra_errors_multiple_bindings_error [name] (->hydra_errors_multiple_bindings_error name))

(defrecord hydra_errors_multiple_fields_error [field_name])
(defn make-hydra_errors_multiple_fields_error [field_name] (->hydra_errors_multiple_fields_error field_name))

(defrecord hydra_errors_no_matching_field_error [field_name])
(defn make-hydra_errors_no_matching_field_error [field_name] (->hydra_errors_no_matching_field_error field_name))

(defrecord hydra_errors_no_such_binding_error [name])
(defn make-hydra_errors_no_such_binding_error [name] (->hydra_errors_no_such_binding_error name))

(defrecord hydra_errors_no_such_primitive_error [name])
(defn make-hydra_errors_no_such_primitive_error [name] (->hydra_errors_no_such_primitive_error name))

(defrecord hydra_errors_other_error [value])
(defn make-hydra_errors_other_error [value] (->hydra_errors_other_error value))

(defrecord hydra_errors_other_inference_error [path message])
(defn make-hydra_errors_other_inference_error [path message] (->hydra_errors_other_inference_error path message))

(defrecord hydra_errors_other_resolution_error [value])
(defn make-hydra_errors_other_resolution_error [value] (->hydra_errors_other_resolution_error value))

(def hydra_errors_resolution_error-variants (list :no_such_binding :no_such_primitive :no_matching_field :other :unexpected_shape))

(defrecord hydra_errors_unexpected_shape_error [expected actual])
(defn make-hydra_errors_unexpected_shape_error [expected actual] (->hydra_errors_unexpected_shape_error expected actual))

(defrecord hydra_errors_unification_error [left_type right_type message])
(defn make-hydra_errors_unification_error [left_type right_type message] (->hydra_errors_unification_error left_type right_type message))

(defrecord hydra_errors_unification_inference_error [path cause])
(defn make-hydra_errors_unification_inference_error [path cause] (->hydra_errors_unification_inference_error path cause))

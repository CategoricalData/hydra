(ns hydra.error.core
  (:require [hydra.accessors :refer :all] [hydra.core :refer :all] [hydra.variants :refer :all]
))

(declare hydra_error_core_invalid_term_error-variants hydra_error_core_invalid_type_error-variants)

(defrecord hydra_error_core_duplicate_binding_error [location name])
(defn make-hydra_error_core_duplicate_binding_error [location name] (->hydra_error_core_duplicate_binding_error location name))

(defrecord hydra_error_core_duplicate_field_error [location name])
(defn make-hydra_error_core_duplicate_field_error [location name] (->hydra_error_core_duplicate_field_error location name))

(defrecord hydra_error_core_undefined_field_error [field_name type_name])
(defn make-hydra_error_core_undefined_field_error [field_name type_name] (->hydra_error_core_undefined_field_error field_name type_name))

(defrecord hydra_error_core_unexpected_term_variant_error [expected_variant actual_term])
(defn make-hydra_error_core_unexpected_term_variant_error [expected_variant actual_term] (->hydra_error_core_unexpected_term_variant_error expected_variant actual_term))

(defrecord hydra_error_core_unexpected_type_variant_error [expected_variant actual_type])
(defn make-hydra_error_core_unexpected_type_variant_error [expected_variant actual_type] (->hydra_error_core_unexpected_type_variant_error expected_variant actual_type))

(defrecord hydra_error_core_constant_condition_error [location value])
(defn make-hydra_error_core_constant_condition_error [location value] (->hydra_error_core_constant_condition_error location value))

(defrecord hydra_error_core_empty_case_statement_error [location type_name])
(defn make-hydra_error_core_empty_case_statement_error [location type_name] (->hydra_error_core_empty_case_statement_error location type_name))

(defrecord hydra_error_core_empty_let_bindings_error [location])
(defn make-hydra_error_core_empty_let_bindings_error [location] (->hydra_error_core_empty_let_bindings_error location))

(defrecord hydra_error_core_empty_term_annotation_error [location])
(defn make-hydra_error_core_empty_term_annotation_error [location] (->hydra_error_core_empty_term_annotation_error location))

(defrecord hydra_error_core_empty_type_name_in_term_error [location])
(defn make-hydra_error_core_empty_type_name_in_term_error [location] (->hydra_error_core_empty_type_name_in_term_error location))

(defrecord hydra_error_core_invalid_lambda_parameter_name_error [location name])
(defn make-hydra_error_core_invalid_lambda_parameter_name_error [location name] (->hydra_error_core_invalid_lambda_parameter_name_error location name))

(defrecord hydra_error_core_invalid_let_binding_name_error [location name])
(defn make-hydra_error_core_invalid_let_binding_name_error [location name] (->hydra_error_core_invalid_let_binding_name_error location name))

(defrecord hydra_error_core_invalid_type_lambda_parameter_name_error [location name])
(defn make-hydra_error_core_invalid_type_lambda_parameter_name_error [location name] (->hydra_error_core_invalid_type_lambda_parameter_name_error location name))

(defrecord hydra_error_core_nested_term_annotation_error [location])
(defn make-hydra_error_core_nested_term_annotation_error [location] (->hydra_error_core_nested_term_annotation_error location))

(defrecord hydra_error_core_redundant_wrap_unwrap_error [location type_name])
(defn make-hydra_error_core_redundant_wrap_unwrap_error [location type_name] (->hydra_error_core_redundant_wrap_unwrap_error location type_name))

(defrecord hydra_error_core_self_application_error [location name])
(defn make-hydra_error_core_self_application_error [location name] (->hydra_error_core_self_application_error location name))

(defrecord hydra_error_core_term_variable_shadowing_error [location name])
(defn make-hydra_error_core_term_variable_shadowing_error [location name] (->hydra_error_core_term_variable_shadowing_error location name))

(defrecord hydra_error_core_type_variable_shadowing_in_type_lambda_error [location name])
(defn make-hydra_error_core_type_variable_shadowing_in_type_lambda_error [location name] (->hydra_error_core_type_variable_shadowing_in_type_lambda_error location name))

(defrecord hydra_error_core_undefined_term_variable_error [location name])
(defn make-hydra_error_core_undefined_term_variable_error [location name] (->hydra_error_core_undefined_term_variable_error location name))

(defrecord hydra_error_core_undefined_type_variable_in_binding_type_error [location name])
(defn make-hydra_error_core_undefined_type_variable_in_binding_type_error [location name] (->hydra_error_core_undefined_type_variable_in_binding_type_error location name))

(defrecord hydra_error_core_undefined_type_variable_in_lambda_domain_error [location name])
(defn make-hydra_error_core_undefined_type_variable_in_lambda_domain_error [location name] (->hydra_error_core_undefined_type_variable_in_lambda_domain_error location name))

(defrecord hydra_error_core_undefined_type_variable_in_type_application_error [location name])
(defn make-hydra_error_core_undefined_type_variable_in_type_application_error [location name] (->hydra_error_core_undefined_type_variable_in_type_application_error location name))

(defrecord hydra_error_core_unknown_primitive_name_error [location name])
(defn make-hydra_error_core_unknown_primitive_name_error [location name] (->hydra_error_core_unknown_primitive_name_error location name))

(defrecord hydra_error_core_unnecessary_identity_application_error [location])
(defn make-hydra_error_core_unnecessary_identity_application_error [location] (->hydra_error_core_unnecessary_identity_application_error location))

(defrecord hydra_error_core_untyped_term_variable_error [location name])
(defn make-hydra_error_core_untyped_term_variable_error [location name] (->hydra_error_core_untyped_term_variable_error location name))

(def hydra_error_core_invalid_term_error-variants (list :constant_condition :duplicate_binding :duplicate_field :empty_case_statement :empty_let_bindings :empty_term_annotation :empty_type_name_in_term :invalid_lambda_parameter_name :invalid_let_binding_name :invalid_type_lambda_parameter_name :nested_term_annotation :redundant_wrap_unwrap :self_application :term_variable_shadowing :type_variable_shadowing_in_type_lambda :undefined_term_variable :undefined_type_variable_in_binding_type :undefined_type_variable_in_lambda_domain :undefined_type_variable_in_type_application :unknown_primitive_name :unnecessary_identity_application :untyped_term_variable))

(defrecord hydra_error_core_duplicate_record_type_field_names_error [location name])
(defn make-hydra_error_core_duplicate_record_type_field_names_error [location name] (->hydra_error_core_duplicate_record_type_field_names_error location name))

(defrecord hydra_error_core_duplicate_union_type_field_names_error [location name])
(defn make-hydra_error_core_duplicate_union_type_field_names_error [location name] (->hydra_error_core_duplicate_union_type_field_names_error location name))

(defrecord hydra_error_core_empty_record_type_error [location])
(defn make-hydra_error_core_empty_record_type_error [location] (->hydra_error_core_empty_record_type_error location))

(defrecord hydra_error_core_empty_type_annotation_error [location])
(defn make-hydra_error_core_empty_type_annotation_error [location] (->hydra_error_core_empty_type_annotation_error location))

(defrecord hydra_error_core_empty_union_type_error [location])
(defn make-hydra_error_core_empty_union_type_error [location] (->hydra_error_core_empty_union_type_error location))

(defrecord hydra_error_core_invalid_forall_parameter_name_error [location name])
(defn make-hydra_error_core_invalid_forall_parameter_name_error [location name] (->hydra_error_core_invalid_forall_parameter_name_error location name))

(defrecord hydra_error_core_invalid_type_scheme_variable_name_error [location name])
(defn make-hydra_error_core_invalid_type_scheme_variable_name_error [location name] (->hydra_error_core_invalid_type_scheme_variable_name_error location name))

(defrecord hydra_error_core_nested_type_annotation_error [location])
(defn make-hydra_error_core_nested_type_annotation_error [location] (->hydra_error_core_nested_type_annotation_error location))

(defrecord hydra_error_core_non_comparable_map_key_type_error [location key_type])
(defn make-hydra_error_core_non_comparable_map_key_type_error [location key_type] (->hydra_error_core_non_comparable_map_key_type_error location key_type))

(defrecord hydra_error_core_non_comparable_set_element_type_error [location element_type])
(defn make-hydra_error_core_non_comparable_set_element_type_error [location element_type] (->hydra_error_core_non_comparable_set_element_type_error location element_type))

(defrecord hydra_error_core_single_variant_union_error [location field_name])
(defn make-hydra_error_core_single_variant_union_error [location field_name] (->hydra_error_core_single_variant_union_error location field_name))

(defrecord hydra_error_core_type_variable_shadowing_in_forall_error [location name])
(defn make-hydra_error_core_type_variable_shadowing_in_forall_error [location name] (->hydra_error_core_type_variable_shadowing_in_forall_error location name))

(defrecord hydra_error_core_undefined_type_variable_error [location name])
(defn make-hydra_error_core_undefined_type_variable_error [location name] (->hydra_error_core_undefined_type_variable_error location name))

(defrecord hydra_error_core_void_in_non_bottom_position_error [location])
(defn make-hydra_error_core_void_in_non_bottom_position_error [location] (->hydra_error_core_void_in_non_bottom_position_error location))

(def hydra_error_core_invalid_type_error-variants (list :duplicate_record_type_field_names :duplicate_union_type_field_names :empty_record_type :empty_type_annotation :empty_union_type :invalid_forall_parameter_name :invalid_type_scheme_variable_name :nested_type_annotation :non_comparable_map_key_type :non_comparable_set_element_type :single_variant_union :type_variable_shadowing_in_forall :undefined_type_variable :void_in_non_bottom_position))

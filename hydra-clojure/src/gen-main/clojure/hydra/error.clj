(ns hydra.error
  (:require [hydra.core :refer :all] [hydra.typing :refer :all] [hydra.variants :refer :all]
))

(declare hydra_error_checking_error-variants hydra_error_error-variants)

(def hydra_error_checking_error-variants (list :incorrect_unification :not_a_forall_type :not_a_function_type :type_arity_mismatch :type_mismatch :unbound_type_variables :unequal_types :unsupported_term_variant :untyped_lambda :untyped_let_binding))

(defrecord hydra_error_decoding_error [value])
(defn make-hydra_error_decoding_error [value] (->hydra_error_decoding_error value))

(defrecord hydra_error_duplicate_binding_error [name])
(defn make-hydra_error_duplicate_binding_error [name] (->hydra_error_duplicate_binding_error name))

(defrecord hydra_error_duplicate_field_error [name])
(defn make-hydra_error_duplicate_field_error [name] (->hydra_error_duplicate_field_error name))

(def hydra_error_error-variants (list :checking :decoding :duplicate_binding :duplicate_field :other :undefined_field :undefined_term :undefined_type :unexpected_term_variant :unexpected_type_variant :unification))

(defrecord hydra_error_incorrect_unification_error [substitution])
(defn make-hydra_error_incorrect_unification_error [substitution] (->hydra_error_incorrect_unification_error substitution))

(defrecord hydra_error_not_a_forall_type_error [type type_arguments])
(defn make-hydra_error_not_a_forall_type_error [type type_arguments] (->hydra_error_not_a_forall_type_error type type_arguments))

(defrecord hydra_error_not_a_function_type_error [type])
(defn make-hydra_error_not_a_function_type_error [type] (->hydra_error_not_a_function_type_error type))

(defrecord hydra_error_other_error [value])
(defn make-hydra_error_other_error [value] (->hydra_error_other_error value))

(defrecord hydra_error_type_arity_mismatch_error [type expected_arity actual_arity type_arguments])
(defn make-hydra_error_type_arity_mismatch_error [type expected_arity actual_arity type_arguments] (->hydra_error_type_arity_mismatch_error type expected_arity actual_arity type_arguments))

(defrecord hydra_error_type_mismatch_error [expected_type actual_type])
(defn make-hydra_error_type_mismatch_error [expected_type actual_type] (->hydra_error_type_mismatch_error expected_type actual_type))

(defrecord hydra_error_unbound_type_variables_error [variables type])
(defn make-hydra_error_unbound_type_variables_error [variables type] (->hydra_error_unbound_type_variables_error variables type))

(defrecord hydra_error_undefined_field_error [field_name type_name])
(defn make-hydra_error_undefined_field_error [field_name type_name] (->hydra_error_undefined_field_error field_name type_name))

(defrecord hydra_error_undefined_term_error [name])
(defn make-hydra_error_undefined_term_error [name] (->hydra_error_undefined_term_error name))

(defrecord hydra_error_undefined_type_error [name])
(defn make-hydra_error_undefined_type_error [name] (->hydra_error_undefined_type_error name))

(defrecord hydra_error_unequal_types_error [types description])
(defn make-hydra_error_unequal_types_error [types description] (->hydra_error_unequal_types_error types description))

(defrecord hydra_error_unexpected_term_variant_error [expected_variant actual_term])
(defn make-hydra_error_unexpected_term_variant_error [expected_variant actual_term] (->hydra_error_unexpected_term_variant_error expected_variant actual_term))

(defrecord hydra_error_unexpected_type_variant_error [expected_variant actual_type])
(defn make-hydra_error_unexpected_type_variant_error [expected_variant actual_type] (->hydra_error_unexpected_type_variant_error expected_variant actual_type))

(defrecord hydra_error_unification_error [left_type right_type message])
(defn make-hydra_error_unification_error [left_type right_type message] (->hydra_error_unification_error left_type right_type message))

(defrecord hydra_error_unsupported_term_variant_error [term_variant])
(defn make-hydra_error_unsupported_term_variant_error [term_variant] (->hydra_error_unsupported_term_variant_error term_variant))

(defrecord hydra_error_untyped_lambda_error [])
(defn make-hydra_error_untyped_lambda_error [] (->hydra_error_untyped_lambda_error))

(defrecord hydra_error_untyped_let_binding_error [binding])
(defn make-hydra_error_untyped_let_binding_error [binding] (->hydra_error_untyped_let_binding_error binding))

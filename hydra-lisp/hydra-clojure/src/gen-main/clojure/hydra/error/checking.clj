(ns hydra.error.checking
  (:require [hydra.core :refer :all] [hydra.typing :refer :all] [hydra.variants :refer :all]
))

(declare hydra_error_checking_checking_error-variants)

(def hydra_error_checking_checking_error-variants (list :incorrect_unification :not_a_forall_type :not_a_function_type :type_arity_mismatch :type_mismatch :unbound_type_variables :unequal_types :unsupported_term_variant :untyped_lambda :untyped_let_binding))

(defrecord hydra_error_checking_incorrect_unification_error [substitution])
(defn make-hydra_error_checking_incorrect_unification_error [substitution] (->hydra_error_checking_incorrect_unification_error substitution))

(defrecord hydra_error_checking_not_a_forall_type_error [type type_arguments])
(defn make-hydra_error_checking_not_a_forall_type_error [type type_arguments] (->hydra_error_checking_not_a_forall_type_error type type_arguments))

(defrecord hydra_error_checking_not_a_function_type_error [type])
(defn make-hydra_error_checking_not_a_function_type_error [type] (->hydra_error_checking_not_a_function_type_error type))

(defrecord hydra_error_checking_type_arity_mismatch_error [type expected_arity actual_arity type_arguments])
(defn make-hydra_error_checking_type_arity_mismatch_error [type expected_arity actual_arity type_arguments] (->hydra_error_checking_type_arity_mismatch_error type expected_arity actual_arity type_arguments))

(defrecord hydra_error_checking_type_mismatch_error [expected_type actual_type])
(defn make-hydra_error_checking_type_mismatch_error [expected_type actual_type] (->hydra_error_checking_type_mismatch_error expected_type actual_type))

(defrecord hydra_error_checking_unbound_type_variables_error [variables type])
(defn make-hydra_error_checking_unbound_type_variables_error [variables type] (->hydra_error_checking_unbound_type_variables_error variables type))

(defrecord hydra_error_checking_unequal_types_error [types description])
(defn make-hydra_error_checking_unequal_types_error [types description] (->hydra_error_checking_unequal_types_error types description))

(defrecord hydra_error_checking_unsupported_term_variant_error [term_variant])
(defn make-hydra_error_checking_unsupported_term_variant_error [term_variant] (->hydra_error_checking_unsupported_term_variant_error term_variant))

(defrecord hydra_error_checking_untyped_lambda_error [])
(defn make-hydra_error_checking_untyped_lambda_error [] (->hydra_error_checking_untyped_lambda_error))

(defrecord hydra_error_checking_untyped_let_binding_error [binding])
(defn make-hydra_error_checking_untyped_let_binding_error [binding] (->hydra_error_checking_untyped_let_binding_error binding))

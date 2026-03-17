(ns hydra.show.error
  (:require [hydra.core :refer :all] [hydra.error :refer :all] [hydra.formatting :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.sets :refer :all] [hydra.lib.strings :refer :all] [hydra.show.core :refer :all] [hydra.show.meta :refer :all] [hydra.show.typing :refer :all]
))

(declare hydra_show_error_incorrect_unification_error hydra_show_error_not_a_forall_type_error hydra_show_error_not_a_function_type_error hydra_show_error_type_arity_mismatch_error hydra_show_error_type_mismatch_error hydra_show_error_unbound_type_variables_error hydra_show_error_unequal_types_error hydra_show_error_unsupported_term_variant_error hydra_show_error_untyped_lambda_error hydra_show_error_untyped_let_binding_error hydra_show_error_checking_error hydra_show_error_decoding_error hydra_show_error_duplicate_binding_error hydra_show_error_duplicate_field_error hydra_show_error_other_error hydra_show_error_undefined_field_error hydra_show_error_undefined_term_error hydra_show_error_undefined_type_error hydra_show_error_unexpected_term_variant_error hydra_show_error_unexpected_type_variant_error hydra_show_error_unification_error hydra_show_error_error)

(def hydra_show_error_incorrect_unification_error (fn [e] (let [subst ((fn [v] (:substitution v)) e)] ((hydra_lib_strings_cat2 "incorrect unification: ") (hydra_show_typing_type_subst subst)))))

(def hydra_show_error_not_a_forall_type_error (fn [e] (let [typ ((fn [v] (:type v)) e)] (let [args ((fn [v] (:type_arguments v)) e)] (hydra_lib_strings_cat (list "not a forall type: " (hydra_show_core_type typ) ". Trying to apply " (hydra_lib_literals_show_int32 (hydra_lib_lists_length args)) " type argument(s): " ((hydra_formatting_show_list hydra_show_core_type) args)))))))

(def hydra_show_error_not_a_function_type_error (fn [e] (let [typ ((fn [v] (:type v)) e)] ((hydra_lib_strings_cat2 "not a function type: ") (hydra_show_core_type typ)))))

(def hydra_show_error_type_arity_mismatch_error (fn [e] (let [typ ((fn [v] (:type v)) e)] (let [expected ((fn [v] (:expected_arity v)) e)] (let [actual ((fn [v] (:actual_arity v)) e)] (let [args ((fn [v] (:type_arguments v)) e)] (hydra_lib_strings_cat (list "type " (hydra_show_core_type typ) " applied to the wrong number of type arguments (expected " (hydra_lib_literals_show_int32 expected) ", got " (hydra_lib_literals_show_int32 actual) "): " ((hydra_formatting_show_list hydra_show_core_type) args)))))))))

(def hydra_show_error_type_mismatch_error (fn [e] (let [expected ((fn [v] (:expected_type v)) e)] (let [actual ((fn [v] (:actual_type v)) e)] (hydra_lib_strings_cat (list "type mismatch: expected " (hydra_show_core_type expected) " but found " (hydra_show_core_type actual)))))))

(def hydra_show_error_unbound_type_variables_error (fn [e] (let [vars ((fn [v] (:variables v)) e)] (let [typ ((fn [v] (:type v)) e)] (hydra_lib_strings_cat (list "unbound type variables: {" ((hydra_lib_strings_intercalate ", ") ((hydra_lib_lists_map (fn [v] v)) (hydra_lib_sets_to_list vars))) "} in type " (hydra_show_core_type typ)))))))

(def hydra_show_error_unequal_types_error (fn [e] (let [types ((fn [v] (:types v)) e)] (let [desc ((fn [v] (:description v)) e)] (hydra_lib_strings_cat (list "unequal types " ((hydra_formatting_show_list hydra_show_core_type) types) " in " desc))))))

(def hydra_show_error_unsupported_term_variant_error (fn [e] ((hydra_lib_strings_cat2 "unsupported term variant: ") (hydra_show_meta_term_variant ((fn [v] (:term_variant v)) e)))))

(def hydra_show_error_untyped_lambda_error (fn [_] "untyped lambda"))

(def hydra_show_error_untyped_let_binding_error (fn [e] (let [b ((fn [v] (:binding v)) e)] ((hydra_lib_strings_cat2 "untyped let binding: ") (hydra_show_core_binding b)))))

(def hydra_show_error_checking_error (fn [ce] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :incorrect_unification) (hydra_show_error_incorrect_unification_error match_value) (= (first match_target) :not_a_forall_type) (hydra_show_error_not_a_forall_type_error match_value) (= (first match_target) :not_a_function_type) (hydra_show_error_not_a_function_type_error match_value) (= (first match_target) :type_arity_mismatch) (hydra_show_error_type_arity_mismatch_error match_value) (= (first match_target) :type_mismatch) (hydra_show_error_type_mismatch_error match_value) (= (first match_target) :unbound_type_variables) (hydra_show_error_unbound_type_variables_error match_value) (= (first match_target) :unequal_types) (hydra_show_error_unequal_types_error match_value) (= (first match_target) :unsupported_term_variant) (hydra_show_error_unsupported_term_variant_error match_value) (= (first match_target) :untyped_lambda) (hydra_show_error_untyped_lambda_error match_value) (= (first match_target) :untyped_let_binding) (hydra_show_error_untyped_let_binding_error match_value))) (second match_target))) ce)))

(def hydra_show_error_decoding_error (fn [de] ((hydra_lib_strings_cat2 "decoding error: ") ((fn [v] v) de))))

(def hydra_show_error_duplicate_binding_error (fn [e] ((hydra_lib_strings_cat2 "duplicate binding: ") ((fn [v] v) ((fn [v] (:name v)) e)))))

(def hydra_show_error_duplicate_field_error (fn [e] ((hydra_lib_strings_cat2 "duplicate field: ") ((fn [v] v) ((fn [v] (:name v)) e)))))

(def hydra_show_error_other_error (fn [oe] ((fn [v] v) oe)))

(def hydra_show_error_undefined_field_error (fn [e] (let [fname ((fn [v] (:field_name v)) e)] (let [tname ((fn [v] (:type_name v)) e)] (hydra_lib_strings_cat (list "no such field \"" ((fn [v] v) fname) "\" in type \"" ((fn [v] v) tname) "\""))))))

(def hydra_show_error_undefined_term_error (fn [e] ((hydra_lib_strings_cat2 "undefined term: ") ((fn [v] v) ((fn [v] (:name v)) e)))))

(def hydra_show_error_undefined_type_error (fn [e] ((hydra_lib_strings_cat2 "undefined type: ") ((fn [v] v) ((fn [v] (:name v)) e)))))

(def hydra_show_error_unexpected_term_variant_error (fn [e] (let [expected ((fn [v] (:expected_variant v)) e)] (let [actual ((fn [v] (:actual_term v)) e)] (hydra_lib_strings_cat (list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(def hydra_show_error_unexpected_type_variant_error (fn [e] (let [expected ((fn [v] (:expected_variant v)) e)] (let [actual ((fn [v] (:actual_type v)) e)] (hydra_lib_strings_cat (list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

(def hydra_show_error_unification_error (fn [e] (let [lt ((fn [v] (:left_type v)) e)] (let [rt ((fn [v] (:right_type v)) e)] (let [msg ((fn [v] (:message v)) e)] (hydra_lib_strings_cat (list "unification error: cannot unify " (hydra_show_core_type lt) " with " (hydra_show_core_type rt) ": " msg)))))))

(def hydra_show_error_error (fn [e] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :checking) (hydra_show_error_checking_error match_value) (= (first match_target) :decoding) (hydra_show_error_decoding_error match_value) (= (first match_target) :duplicate_binding) (hydra_show_error_duplicate_binding_error match_value) (= (first match_target) :duplicate_field) (hydra_show_error_duplicate_field_error match_value) (= (first match_target) :other) (hydra_show_error_other_error match_value) (= (first match_target) :undefined_field) (hydra_show_error_undefined_field_error match_value) (= (first match_target) :undefined_term) (hydra_show_error_undefined_term_error match_value) (= (first match_target) :undefined_type) (hydra_show_error_undefined_type_error match_value) (= (first match_target) :unexpected_term_variant) (hydra_show_error_unexpected_term_variant_error match_value) (= (first match_target) :unexpected_type_variant) (hydra_show_error_unexpected_type_variant_error match_value) (= (first match_target) :unification) (hydra_show_error_unification_error match_value))) (second match_target))) e)))

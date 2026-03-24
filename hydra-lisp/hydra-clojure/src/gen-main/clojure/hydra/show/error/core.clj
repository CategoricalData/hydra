(ns hydra.show.error.core
  (:require [hydra.core :refer :all] [hydra.error.core :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.strings :refer :all] [hydra.show.core :refer :all] [hydra.show.meta :refer :all]
))

(declare hydra_show_error_core_constant_condition_error hydra_show_error_core_duplicate_binding_error hydra_show_error_core_duplicate_field_error hydra_show_error_core_duplicate_record_type_field_names_error hydra_show_error_core_duplicate_union_type_field_names_error hydra_show_error_core_empty_case_statement_error hydra_show_error_core_empty_let_bindings_error hydra_show_error_core_empty_record_type_error hydra_show_error_core_empty_term_annotation_error hydra_show_error_core_empty_type_annotation_error hydra_show_error_core_empty_type_name_in_term_error hydra_show_error_core_empty_union_type_error hydra_show_error_core_invalid_forall_parameter_name_error hydra_show_error_core_invalid_lambda_parameter_name_error hydra_show_error_core_invalid_let_binding_name_error hydra_show_error_core_invalid_type_lambda_parameter_name_error hydra_show_error_core_nested_term_annotation_error hydra_show_error_core_redundant_wrap_unwrap_error hydra_show_error_core_self_application_error hydra_show_error_core_term_variable_shadowing_error hydra_show_error_core_type_variable_shadowing_in_type_lambda_error hydra_show_error_core_undefined_term_variable_error hydra_show_error_core_undefined_type_variable_in_binding_type_error hydra_show_error_core_undefined_type_variable_in_lambda_domain_error hydra_show_error_core_undefined_type_variable_in_type_application_error hydra_show_error_core_unknown_primitive_name_error hydra_show_error_core_unnecessary_identity_application_error hydra_show_error_core_untyped_term_variable_error hydra_show_error_core_invalid_term_error hydra_show_error_core_invalid_type_scheme_variable_name_error hydra_show_error_core_nested_type_annotation_error hydra_show_error_core_non_comparable_map_key_type_error hydra_show_error_core_non_comparable_set_element_type_error hydra_show_error_core_single_variant_union_error hydra_show_error_core_type_variable_shadowing_in_forall_error hydra_show_error_core_undefined_type_variable_error hydra_show_error_core_void_in_non_bottom_position_error hydra_show_error_core_invalid_type_error hydra_show_error_core_undefined_field_error hydra_show_error_core_unexpected_term_variant_error hydra_show_error_core_unexpected_type_variant_error)

(def hydra_show_error_core_constant_condition_error (fn [e] (hydra_lib_strings_cat (list "constant condition: ifElse with literal " (hydra_lib_literals_show_boolean ((fn [v] (:value v)) e))))))

(def hydra_show_error_core_duplicate_binding_error (fn [e] (hydra_lib_strings_cat (list "duplicate binding: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_duplicate_field_error (fn [e] (hydra_lib_strings_cat (list "duplicate field: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_duplicate_record_type_field_names_error (fn [e] (hydra_lib_strings_cat (list "duplicate field in record type: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_duplicate_union_type_field_names_error (fn [e] (hydra_lib_strings_cat (list "duplicate field in union type: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_empty_case_statement_error (fn [e] (hydra_lib_strings_cat (list "empty case statement for type: " ((fn [v] v) ((fn [v] (:type_name v)) e))))))

(def hydra_show_error_core_empty_let_bindings_error (fn [e] "let expression with no bindings"))

(def hydra_show_error_core_empty_record_type_error (fn [e] "record type with no fields (use TypeUnit instead)"))

(def hydra_show_error_core_empty_term_annotation_error (fn [e] "term annotation with empty annotation map"))

(def hydra_show_error_core_empty_type_annotation_error (fn [e] "type annotation with empty annotation map"))

(def hydra_show_error_core_empty_type_name_in_term_error (fn [e] "term with empty type name"))

(def hydra_show_error_core_empty_union_type_error (fn [e] "union type with no alternatives (use TypeVoid instead)"))

(def hydra_show_error_core_invalid_forall_parameter_name_error (fn [e] (hydra_lib_strings_cat (list "invalid forall parameter name: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_invalid_lambda_parameter_name_error (fn [e] (hydra_lib_strings_cat (list "invalid lambda parameter name: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_invalid_let_binding_name_error (fn [e] (hydra_lib_strings_cat (list "invalid let binding name: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_invalid_type_lambda_parameter_name_error (fn [e] (hydra_lib_strings_cat (list "invalid type lambda parameter name: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_nested_term_annotation_error (fn [e] "nested term annotations should be merged"))

(def hydra_show_error_core_redundant_wrap_unwrap_error (fn [e] (hydra_lib_strings_cat (list "redundant wrap/unwrap for type: " ((fn [v] v) ((fn [v] (:type_name v)) e))))))

(def hydra_show_error_core_self_application_error (fn [e] (hydra_lib_strings_cat (list "self-application of variable: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_term_variable_shadowing_error (fn [e] (hydra_lib_strings_cat (list "variable shadowing: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_type_variable_shadowing_in_type_lambda_error (fn [e] (hydra_lib_strings_cat (list "type variable shadowing in type lambda: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_undefined_term_variable_error (fn [e] (hydra_lib_strings_cat (list "undefined term variable: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_undefined_type_variable_in_binding_type_error (fn [e] (hydra_lib_strings_cat (list "undefined type variable in binding type: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_undefined_type_variable_in_lambda_domain_error (fn [e] (hydra_lib_strings_cat (list "undefined type variable in lambda domain: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_undefined_type_variable_in_type_application_error (fn [e] (hydra_lib_strings_cat (list "undefined type variable in type application: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_unknown_primitive_name_error (fn [e] (hydra_lib_strings_cat (list "unknown primitive: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_unnecessary_identity_application_error (fn [e] "unnecessary application of identity lambda"))

(def hydra_show_error_core_untyped_term_variable_error (fn [e] (hydra_lib_strings_cat (list "untyped term variable: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_invalid_term_error (fn [e] ((hydra_lib_strings_cat2 "invalid term: ") ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :constant_condition) (hydra_show_error_core_constant_condition_error match_value) (= (first match_target) :duplicate_binding) (hydra_show_error_core_duplicate_binding_error match_value) (= (first match_target) :duplicate_field) (hydra_show_error_core_duplicate_field_error match_value) (= (first match_target) :empty_case_statement) (hydra_show_error_core_empty_case_statement_error match_value) (= (first match_target) :empty_let_bindings) (hydra_show_error_core_empty_let_bindings_error match_value) (= (first match_target) :empty_term_annotation) (hydra_show_error_core_empty_term_annotation_error match_value) (= (first match_target) :empty_type_name_in_term) (hydra_show_error_core_empty_type_name_in_term_error match_value) (= (first match_target) :invalid_lambda_parameter_name) (hydra_show_error_core_invalid_lambda_parameter_name_error match_value) (= (first match_target) :invalid_let_binding_name) (hydra_show_error_core_invalid_let_binding_name_error match_value) (= (first match_target) :invalid_type_lambda_parameter_name) (hydra_show_error_core_invalid_type_lambda_parameter_name_error match_value) (= (first match_target) :nested_term_annotation) (hydra_show_error_core_nested_term_annotation_error match_value) (= (first match_target) :redundant_wrap_unwrap) (hydra_show_error_core_redundant_wrap_unwrap_error match_value) (= (first match_target) :self_application) (hydra_show_error_core_self_application_error match_value) (= (first match_target) :term_variable_shadowing) (hydra_show_error_core_term_variable_shadowing_error match_value) (= (first match_target) :type_variable_shadowing_in_type_lambda) (hydra_show_error_core_type_variable_shadowing_in_type_lambda_error match_value) (= (first match_target) :undefined_term_variable) (hydra_show_error_core_undefined_term_variable_error match_value) (= (first match_target) :undefined_type_variable_in_binding_type) (hydra_show_error_core_undefined_type_variable_in_binding_type_error match_value) (= (first match_target) :undefined_type_variable_in_lambda_domain) (hydra_show_error_core_undefined_type_variable_in_lambda_domain_error match_value) (= (first match_target) :undefined_type_variable_in_type_application) (hydra_show_error_core_undefined_type_variable_in_type_application_error match_value) (= (first match_target) :unknown_primitive_name) (hydra_show_error_core_unknown_primitive_name_error match_value) (= (first match_target) :unnecessary_identity_application) (hydra_show_error_core_unnecessary_identity_application_error match_value) (= (first match_target) :untyped_term_variable) (hydra_show_error_core_untyped_term_variable_error match_value))) (second match_target))) e))))

(def hydra_show_error_core_invalid_type_scheme_variable_name_error (fn [e] (hydra_lib_strings_cat (list "invalid type scheme variable name: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_nested_type_annotation_error (fn [e] "nested type annotations should be merged"))

(def hydra_show_error_core_non_comparable_map_key_type_error (fn [e] (hydra_lib_strings_cat (list "map key type contains a function type: " (hydra_show_core_type ((fn [v] (:key_type v)) e))))))

(def hydra_show_error_core_non_comparable_set_element_type_error (fn [e] (hydra_lib_strings_cat (list "set element type contains a function type: " (hydra_show_core_type ((fn [v] (:element_type v)) e))))))

(def hydra_show_error_core_single_variant_union_error (fn [e] (hydra_lib_strings_cat (list "union type with single variant: " ((fn [v] v) ((fn [v] (:field_name v)) e))))))

(def hydra_show_error_core_type_variable_shadowing_in_forall_error (fn [e] (hydra_lib_strings_cat (list "type variable shadowing in forall: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_undefined_type_variable_error (fn [e] (hydra_lib_strings_cat (list "undefined type variable: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_void_in_non_bottom_position_error (fn [e] "TypeVoid in a position where no value can be constructed"))

(def hydra_show_error_core_invalid_type_error (fn [e] ((hydra_lib_strings_cat2 "invalid type: ") ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :duplicate_record_type_field_names) (hydra_show_error_core_duplicate_record_type_field_names_error match_value) (= (first match_target) :duplicate_union_type_field_names) (hydra_show_error_core_duplicate_union_type_field_names_error match_value) (= (first match_target) :empty_record_type) (hydra_show_error_core_empty_record_type_error match_value) (= (first match_target) :empty_type_annotation) (hydra_show_error_core_empty_type_annotation_error match_value) (= (first match_target) :empty_union_type) (hydra_show_error_core_empty_union_type_error match_value) (= (first match_target) :invalid_forall_parameter_name) (hydra_show_error_core_invalid_forall_parameter_name_error match_value) (= (first match_target) :invalid_type_scheme_variable_name) (hydra_show_error_core_invalid_type_scheme_variable_name_error match_value) (= (first match_target) :nested_type_annotation) (hydra_show_error_core_nested_type_annotation_error match_value) (= (first match_target) :non_comparable_map_key_type) (hydra_show_error_core_non_comparable_map_key_type_error match_value) (= (first match_target) :non_comparable_set_element_type) (hydra_show_error_core_non_comparable_set_element_type_error match_value) (= (first match_target) :single_variant_union) (hydra_show_error_core_single_variant_union_error match_value) (= (first match_target) :type_variable_shadowing_in_forall) (hydra_show_error_core_type_variable_shadowing_in_forall_error match_value) (= (first match_target) :undefined_type_variable) (hydra_show_error_core_undefined_type_variable_error match_value) (= (first match_target) :void_in_non_bottom_position) (hydra_show_error_core_void_in_non_bottom_position_error match_value))) (second match_target))) e))))

(def hydra_show_error_core_undefined_field_error (fn [e] (let [fname ((fn [v] (:field_name v)) e)] (let [tname ((fn [v] (:type_name v)) e)] (hydra_lib_strings_cat (list "no such field \"" ((fn [v] v) fname) "\" in type \"" ((fn [v] v) tname) "\""))))))

(def hydra_show_error_core_unexpected_term_variant_error (fn [e] (let [expected ((fn [v] (:expected_variant v)) e)] (let [actual ((fn [v] (:actual_term v)) e)] (hydra_lib_strings_cat (list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(def hydra_show_error_core_unexpected_type_variant_error (fn [e] (let [expected ((fn [v] (:expected_variant v)) e)] (let [actual ((fn [v] (:actual_type v)) e)] (hydra_lib_strings_cat (list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

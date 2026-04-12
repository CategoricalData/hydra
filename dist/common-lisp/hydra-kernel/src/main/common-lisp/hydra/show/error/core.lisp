(defpackage :hydra.show.error.core
(:use :cl :hydra.core :hydra.error.core :hydra.lib.literals :hydra.lib.strings :hydra.show.core :hydra.show.variants)
(:export :hydra_show_error_core_constant_condition_error :hydra_show_error_core_duplicate_binding_error :hydra_show_error_core_duplicate_field_error :hydra_show_error_core_duplicate_record_type_field_names_error :hydra_show_error_core_duplicate_union_type_field_names_error :hydra_show_error_core_empty_case_statement_error :hydra_show_error_core_empty_let_bindings_error :hydra_show_error_core_empty_record_type_error :hydra_show_error_core_empty_term_annotation_error :hydra_show_error_core_empty_type_annotation_error :hydra_show_error_core_empty_type_name_in_term_error :hydra_show_error_core_empty_union_type_error :hydra_show_error_core_invalid_forall_parameter_name_error :hydra_show_error_core_invalid_lambda_parameter_name_error :hydra_show_error_core_invalid_let_binding_name_error :hydra_show_error_core_invalid_type_lambda_parameter_name_error :hydra_show_error_core_nested_term_annotation_error :hydra_show_error_core_redundant_wrap_unwrap_error :hydra_show_error_core_self_application_error :hydra_show_error_core_term_variable_shadowing_error :hydra_show_error_core_type_variable_shadowing_in_type_lambda_error :hydra_show_error_core_undefined_term_variable_error :hydra_show_error_core_undefined_type_variable_in_binding_type_error :hydra_show_error_core_undefined_type_variable_in_lambda_domain_error :hydra_show_error_core_undefined_type_variable_in_type_application_error :hydra_show_error_core_unknown_primitive_name_error :hydra_show_error_core_unnecessary_identity_application_error :hydra_show_error_core_untyped_term_variable_error :hydra_show_error_core_invalid_term_error :hydra_show_error_core_invalid_type_scheme_variable_name_error :hydra_show_error_core_nested_type_annotation_error :hydra_show_error_core_non_comparable_map_key_type_error :hydra_show_error_core_non_comparable_set_element_type_error :hydra_show_error_core_single_variant_union_error :hydra_show_error_core_type_variable_shadowing_in_forall_error :hydra_show_error_core_undefined_type_variable_error :hydra_show_error_core_void_in_non_bottom_position_error :hydra_show_error_core_invalid_type_error :hydra_show_error_core_undefined_field_error :hydra_show_error_core_unexpected_term_variant_error :hydra_show_error_core_unexpected_type_variant_error))

(in-package :hydra.show.error.core)

(cl:defvar hydra_show_error_core_constant_condition_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "constant condition: ifElse with literal " (hydra_lib_literals_show_boolean ((cl:lambda (v) (hydra_error_core_constant_condition_error-value v)) e))))))

(cl:defvar hydra_show_error_core_duplicate_binding_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "duplicate binding: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_duplicate_binding_error-name v)) e))))))

(cl:defvar hydra_show_error_core_duplicate_field_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "duplicate field: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_duplicate_field_error-name v)) e))))))

(cl:defvar hydra_show_error_core_duplicate_record_type_field_names_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "duplicate field in record type: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_duplicate_record_type_field_names_error-name v)) e))))))

(cl:defvar hydra_show_error_core_duplicate_union_type_field_names_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "duplicate field in union type: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_duplicate_union_type_field_names_error-name v)) e))))))

(cl:defvar hydra_show_error_core_empty_case_statement_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "empty case statement for type: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_empty_case_statement_error-type_name v)) e))))))

(cl:defvar hydra_show_error_core_empty_let_bindings_error (cl:lambda (e) "let expression with no bindings"))

(cl:defvar hydra_show_error_core_empty_record_type_error (cl:lambda (e) "record type with no fields (use TypeUnit instead)"))

(cl:defvar hydra_show_error_core_empty_term_annotation_error (cl:lambda (e) "term annotation with empty annotation map"))

(cl:defvar hydra_show_error_core_empty_type_annotation_error (cl:lambda (e) "type annotation with empty annotation map"))

(cl:defvar hydra_show_error_core_empty_type_name_in_term_error (cl:lambda (e) "term with empty type name"))

(cl:defvar hydra_show_error_core_empty_union_type_error (cl:lambda (e) "union type with no alternatives (use TypeVoid instead)"))

(cl:defvar hydra_show_error_core_invalid_forall_parameter_name_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "invalid forall parameter name: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_invalid_forall_parameter_name_error-name v)) e))))))

(cl:defvar hydra_show_error_core_invalid_lambda_parameter_name_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "invalid lambda parameter name: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_invalid_lambda_parameter_name_error-name v)) e))))))

(cl:defvar hydra_show_error_core_invalid_let_binding_name_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "invalid let binding name: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_invalid_let_binding_name_error-name v)) e))))))

(cl:defvar hydra_show_error_core_invalid_type_lambda_parameter_name_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "invalid type lambda parameter name: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_invalid_type_lambda_parameter_name_error-name v)) e))))))

(cl:defvar hydra_show_error_core_nested_term_annotation_error (cl:lambda (e) "nested term annotations should be merged"))

(cl:defvar hydra_show_error_core_redundant_wrap_unwrap_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "redundant wrap/unwrap for type: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_redundant_wrap_unwrap_error-type_name v)) e))))))

(cl:defvar hydra_show_error_core_self_application_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "self-application of variable: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_self_application_error-name v)) e))))))

(cl:defvar hydra_show_error_core_term_variable_shadowing_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "variable shadowing: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_term_variable_shadowing_error-name v)) e))))))

(cl:defvar hydra_show_error_core_type_variable_shadowing_in_type_lambda_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "type variable shadowing in type lambda: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_type_variable_shadowing_in_type_lambda_error-name v)) e))))))

(cl:defvar hydra_show_error_core_undefined_term_variable_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "undefined term variable: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_undefined_term_variable_error-name v)) e))))))

(cl:defvar hydra_show_error_core_undefined_type_variable_in_binding_type_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "undefined type variable in binding type: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_undefined_type_variable_in_binding_type_error-name v)) e))))))

(cl:defvar hydra_show_error_core_undefined_type_variable_in_lambda_domain_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "undefined type variable in lambda domain: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_undefined_type_variable_in_lambda_domain_error-name v)) e))))))

(cl:defvar hydra_show_error_core_undefined_type_variable_in_type_application_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "undefined type variable in type application: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_undefined_type_variable_in_type_application_error-name v)) e))))))

(cl:defvar hydra_show_error_core_unknown_primitive_name_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "unknown primitive: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_unknown_primitive_name_error-name v)) e))))))

(cl:defvar hydra_show_error_core_unnecessary_identity_application_error (cl:lambda (e) "unnecessary application of identity lambda"))

(cl:defvar hydra_show_error_core_untyped_term_variable_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "untyped term variable: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_untyped_term_variable_error-name v)) e))))))

(cl:defvar hydra_show_error_core_invalid_term_error (cl:lambda (e) ((hydra_lib_strings_cat2 "invalid term: ") ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :constant_condition) (hydra_show_error_core_constant_condition_error match_value)) ((equal (car match_target) :duplicate_binding) (hydra_show_error_core_duplicate_binding_error match_value)) ((equal (car match_target) :duplicate_field) (hydra_show_error_core_duplicate_field_error match_value)) ((equal (car match_target) :empty_case_statement) (hydra_show_error_core_empty_case_statement_error match_value)) ((equal (car match_target) :empty_let_bindings) (hydra_show_error_core_empty_let_bindings_error match_value)) ((equal (car match_target) :empty_term_annotation) (hydra_show_error_core_empty_term_annotation_error match_value)) ((equal (car match_target) :empty_type_name_in_term) (hydra_show_error_core_empty_type_name_in_term_error match_value)) ((equal (car match_target) :invalid_lambda_parameter_name) (hydra_show_error_core_invalid_lambda_parameter_name_error match_value)) ((equal (car match_target) :invalid_let_binding_name) (hydra_show_error_core_invalid_let_binding_name_error match_value)) ((equal (car match_target) :invalid_type_lambda_parameter_name) (hydra_show_error_core_invalid_type_lambda_parameter_name_error match_value)) ((equal (car match_target) :nested_term_annotation) (hydra_show_error_core_nested_term_annotation_error match_value)) ((equal (car match_target) :redundant_wrap_unwrap) (hydra_show_error_core_redundant_wrap_unwrap_error match_value)) ((equal (car match_target) :self_application) (hydra_show_error_core_self_application_error match_value)) ((equal (car match_target) :term_variable_shadowing) (hydra_show_error_core_term_variable_shadowing_error match_value)) ((equal (car match_target) :type_variable_shadowing_in_type_lambda) (hydra_show_error_core_type_variable_shadowing_in_type_lambda_error match_value)) ((equal (car match_target) :undefined_term_variable) (hydra_show_error_core_undefined_term_variable_error match_value)) ((equal (car match_target) :undefined_type_variable_in_binding_type) (hydra_show_error_core_undefined_type_variable_in_binding_type_error match_value)) ((equal (car match_target) :undefined_type_variable_in_lambda_domain) (hydra_show_error_core_undefined_type_variable_in_lambda_domain_error match_value)) ((equal (car match_target) :undefined_type_variable_in_type_application) (hydra_show_error_core_undefined_type_variable_in_type_application_error match_value)) ((equal (car match_target) :unknown_primitive_name) (hydra_show_error_core_unknown_primitive_name_error match_value)) ((equal (car match_target) :unnecessary_identity_application) (hydra_show_error_core_unnecessary_identity_application_error match_value)) ((equal (car match_target) :untyped_term_variable) (hydra_show_error_core_untyped_term_variable_error match_value)))) (cadr match_target))) e))))

(cl:defvar hydra_show_error_core_invalid_type_scheme_variable_name_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "invalid type scheme variable name: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_invalid_type_scheme_variable_name_error-name v)) e))))))

(cl:defvar hydra_show_error_core_nested_type_annotation_error (cl:lambda (e) "nested type annotations should be merged"))

(cl:defvar hydra_show_error_core_non_comparable_map_key_type_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "map key type contains a function type: " (hydra_show_core_type ((cl:lambda (v) (hydra_error_core_non_comparable_map_key_type_error-key_type v)) e))))))

(cl:defvar hydra_show_error_core_non_comparable_set_element_type_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "set element type contains a function type: " (hydra_show_core_type ((cl:lambda (v) (hydra_error_core_non_comparable_set_element_type_error-element_type v)) e))))))

(cl:defvar hydra_show_error_core_single_variant_union_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "union type with single variant: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_single_variant_union_error-field_name v)) e))))))

(cl:defvar hydra_show_error_core_type_variable_shadowing_in_forall_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "type variable shadowing in forall: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_type_variable_shadowing_in_forall_error-name v)) e))))))

(cl:defvar hydra_show_error_core_undefined_type_variable_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "undefined type variable: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_undefined_type_variable_error-name v)) e))))))

(cl:defvar hydra_show_error_core_void_in_non_bottom_position_error (cl:lambda (e) "TypeVoid in a position where no value can be constructed"))

(cl:defvar hydra_show_error_core_invalid_type_error (cl:lambda (e) ((hydra_lib_strings_cat2 "invalid type: ") ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :duplicate_record_type_field_names) (hydra_show_error_core_duplicate_record_type_field_names_error match_value)) ((equal (car match_target) :duplicate_union_type_field_names) (hydra_show_error_core_duplicate_union_type_field_names_error match_value)) ((equal (car match_target) :empty_record_type) (hydra_show_error_core_empty_record_type_error match_value)) ((equal (car match_target) :empty_type_annotation) (hydra_show_error_core_empty_type_annotation_error match_value)) ((equal (car match_target) :empty_union_type) (hydra_show_error_core_empty_union_type_error match_value)) ((equal (car match_target) :invalid_forall_parameter_name) (hydra_show_error_core_invalid_forall_parameter_name_error match_value)) ((equal (car match_target) :invalid_type_scheme_variable_name) (hydra_show_error_core_invalid_type_scheme_variable_name_error match_value)) ((equal (car match_target) :nested_type_annotation) (hydra_show_error_core_nested_type_annotation_error match_value)) ((equal (car match_target) :non_comparable_map_key_type) (hydra_show_error_core_non_comparable_map_key_type_error match_value)) ((equal (car match_target) :non_comparable_set_element_type) (hydra_show_error_core_non_comparable_set_element_type_error match_value)) ((equal (car match_target) :single_variant_union) (hydra_show_error_core_single_variant_union_error match_value)) ((equal (car match_target) :type_variable_shadowing_in_forall) (hydra_show_error_core_type_variable_shadowing_in_forall_error match_value)) ((equal (car match_target) :undefined_type_variable) (hydra_show_error_core_undefined_type_variable_error match_value)) ((equal (car match_target) :void_in_non_bottom_position) (hydra_show_error_core_void_in_non_bottom_position_error match_value)))) (cadr match_target))) e))))

(cl:defvar hydra_show_error_core_undefined_field_error (cl:lambda (e) (let ((fname ((cl:lambda (v) (hydra_error_core_undefined_field_error-field_name v)) e))) (let ((tname ((cl:lambda (v) (hydra_error_core_undefined_field_error-type_name v)) e))) (hydra_lib_strings_cat (cl:list "no such field \"" ((cl:lambda (v) v) fname) "\" in type \"" ((cl:lambda (v) v) tname) "\""))))))

(cl:defvar hydra_show_error_core_unexpected_term_variant_error (cl:lambda (e) (let ((expected ((cl:lambda (v) (hydra_error_core_unexpected_term_variant_error-expected_variant v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_core_unexpected_term_variant_error-actual_term v)) e))) (hydra_lib_strings_cat (cl:list "expected " (hydra_show_variants_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(cl:defvar hydra_show_error_core_unexpected_type_variant_error (cl:lambda (e) (let ((expected ((cl:lambda (v) (hydra_error_core_unexpected_type_variant_error-expected_variant v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_core_unexpected_type_variant_error-actual_type v)) e))) (hydra_lib_strings_cat (cl:list "expected " (hydra_show_variants_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

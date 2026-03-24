(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error.core)

(require 'hydra.lib.literals)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(require 'hydra.show.meta)

(defvar hydra_show_error_core_constant_condition_error (lambda (e) (hydra_lib_strings_cat (list "constant condition: ifElse with literal " (hydra_lib_literals_show_boolean (funcall (lambda (v) (hydra_error_core_constant_condition_error-value v)) e))))))

(defvar hydra_show_error_core_duplicate_binding_error (lambda (e) (hydra_lib_strings_cat (list "duplicate binding: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_duplicate_binding_error-name v)) e))))))

(defvar hydra_show_error_core_duplicate_field_error (lambda (e) (hydra_lib_strings_cat (list "duplicate field: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_duplicate_field_error-name v)) e))))))

(defvar hydra_show_error_core_duplicate_record_type_field_names_error (lambda (e) (hydra_lib_strings_cat (list "duplicate field in record type: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_duplicate_record_type_field_names_error-name v)) e))))))

(defvar hydra_show_error_core_duplicate_union_type_field_names_error (lambda (e) (hydra_lib_strings_cat (list "duplicate field in union type: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_duplicate_union_type_field_names_error-name v)) e))))))

(defvar hydra_show_error_core_empty_case_statement_error (lambda (e) (hydra_lib_strings_cat (list "empty case statement for type: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_empty_case_statement_error-type_name v)) e))))))

(defvar hydra_show_error_core_empty_let_bindings_error (lambda (e) "let expression with no bindings"))

(defvar hydra_show_error_core_empty_record_type_error (lambda (e) "record type with no fields (use TypeUnit instead)"))

(defvar hydra_show_error_core_empty_term_annotation_error (lambda (e) "term annotation with empty annotation map"))

(defvar hydra_show_error_core_empty_type_annotation_error (lambda (e) "type annotation with empty annotation map"))

(defvar hydra_show_error_core_empty_type_name_in_term_error (lambda (e) "term with empty type name"))

(defvar hydra_show_error_core_empty_union_type_error (lambda (e) "union type with no alternatives (use TypeVoid instead)"))

(defvar hydra_show_error_core_invalid_forall_parameter_name_error (lambda (e) (hydra_lib_strings_cat (list "invalid forall parameter name: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_invalid_forall_parameter_name_error-name v)) e))))))

(defvar hydra_show_error_core_invalid_lambda_parameter_name_error (lambda (e) (hydra_lib_strings_cat (list "invalid lambda parameter name: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_invalid_lambda_parameter_name_error-name v)) e))))))

(defvar hydra_show_error_core_invalid_let_binding_name_error (lambda (e) (hydra_lib_strings_cat (list "invalid let binding name: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_invalid_let_binding_name_error-name v)) e))))))

(defvar hydra_show_error_core_invalid_type_lambda_parameter_name_error (lambda (e) (hydra_lib_strings_cat (list "invalid type lambda parameter name: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_invalid_type_lambda_parameter_name_error-name v)) e))))))

(defvar hydra_show_error_core_nested_term_annotation_error (lambda (e) "nested term annotations should be merged"))

(defvar hydra_show_error_core_redundant_wrap_unwrap_error (lambda (e) (hydra_lib_strings_cat (list "redundant wrap/unwrap for type: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_redundant_wrap_unwrap_error-type_name v)) e))))))

(defvar hydra_show_error_core_self_application_error (lambda (e) (hydra_lib_strings_cat (list "self-application of variable: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_self_application_error-name v)) e))))))

(defvar hydra_show_error_core_term_variable_shadowing_error (lambda (e) (hydra_lib_strings_cat (list "variable shadowing: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_term_variable_shadowing_error-name v)) e))))))

(defvar hydra_show_error_core_type_variable_shadowing_in_type_lambda_error (lambda (e) (hydra_lib_strings_cat (list "type variable shadowing in type lambda: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_type_variable_shadowing_in_type_lambda_error-name v)) e))))))

(defvar hydra_show_error_core_undefined_term_variable_error (lambda (e) (hydra_lib_strings_cat (list "undefined term variable: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_undefined_term_variable_error-name v)) e))))))

(defvar hydra_show_error_core_undefined_type_variable_in_binding_type_error (lambda (e) (hydra_lib_strings_cat (list "undefined type variable in binding type: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_undefined_type_variable_in_binding_type_error-name v)) e))))))

(defvar hydra_show_error_core_undefined_type_variable_in_lambda_domain_error (lambda (e) (hydra_lib_strings_cat (list "undefined type variable in lambda domain: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_undefined_type_variable_in_lambda_domain_error-name v)) e))))))

(defvar hydra_show_error_core_undefined_type_variable_in_type_application_error (lambda (e) (hydra_lib_strings_cat (list "undefined type variable in type application: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_undefined_type_variable_in_type_application_error-name v)) e))))))

(defvar hydra_show_error_core_unknown_primitive_name_error (lambda (e) (hydra_lib_strings_cat (list "unknown primitive: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_unknown_primitive_name_error-name v)) e))))))

(defvar hydra_show_error_core_unnecessary_identity_application_error (lambda (e) "unnecessary application of identity lambda"))

(defvar hydra_show_error_core_untyped_term_variable_error (lambda (e) (hydra_lib_strings_cat (list "untyped term variable: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_untyped_term_variable_error-name v)) e))))))

(defvar hydra_show_error_core_invalid_term_error (lambda (e) (funcall (hydra_lib_strings_cat2 "invalid term: ") (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :constant_condition) (hydra_show_error_core_constant_condition_error match_value)) ((equal (car match_target) :duplicate_binding) (hydra_show_error_core_duplicate_binding_error match_value)) ((equal (car match_target) :duplicate_field) (hydra_show_error_core_duplicate_field_error match_value)) ((equal (car match_target) :empty_case_statement) (hydra_show_error_core_empty_case_statement_error match_value)) ((equal (car match_target) :empty_let_bindings) (hydra_show_error_core_empty_let_bindings_error match_value)) ((equal (car match_target) :empty_term_annotation) (hydra_show_error_core_empty_term_annotation_error match_value)) ((equal (car match_target) :empty_type_name_in_term) (hydra_show_error_core_empty_type_name_in_term_error match_value)) ((equal (car match_target) :invalid_lambda_parameter_name) (hydra_show_error_core_invalid_lambda_parameter_name_error match_value)) ((equal (car match_target) :invalid_let_binding_name) (hydra_show_error_core_invalid_let_binding_name_error match_value)) ((equal (car match_target) :invalid_type_lambda_parameter_name) (hydra_show_error_core_invalid_type_lambda_parameter_name_error match_value)) ((equal (car match_target) :nested_term_annotation) (hydra_show_error_core_nested_term_annotation_error match_value)) ((equal (car match_target) :redundant_wrap_unwrap) (hydra_show_error_core_redundant_wrap_unwrap_error match_value)) ((equal (car match_target) :self_application) (hydra_show_error_core_self_application_error match_value)) ((equal (car match_target) :term_variable_shadowing) (hydra_show_error_core_term_variable_shadowing_error match_value)) ((equal (car match_target) :type_variable_shadowing_in_type_lambda) (hydra_show_error_core_type_variable_shadowing_in_type_lambda_error match_value)) ((equal (car match_target) :undefined_term_variable) (hydra_show_error_core_undefined_term_variable_error match_value)) ((equal (car match_target) :undefined_type_variable_in_binding_type) (hydra_show_error_core_undefined_type_variable_in_binding_type_error match_value)) ((equal (car match_target) :undefined_type_variable_in_lambda_domain) (hydra_show_error_core_undefined_type_variable_in_lambda_domain_error match_value)) ((equal (car match_target) :undefined_type_variable_in_type_application) (hydra_show_error_core_undefined_type_variable_in_type_application_error match_value)) ((equal (car match_target) :unknown_primitive_name) (hydra_show_error_core_unknown_primitive_name_error match_value)) ((equal (car match_target) :unnecessary_identity_application) (hydra_show_error_core_unnecessary_identity_application_error match_value)) ((equal (car match_target) :untyped_term_variable) (hydra_show_error_core_untyped_term_variable_error match_value)))) (cadr match_target))) e))))

(defvar hydra_show_error_core_invalid_type_scheme_variable_name_error (lambda (e) (hydra_lib_strings_cat (list "invalid type scheme variable name: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_invalid_type_scheme_variable_name_error-name v)) e))))))

(defvar hydra_show_error_core_nested_type_annotation_error (lambda (e) "nested type annotations should be merged"))

(defvar hydra_show_error_core_non_comparable_map_key_type_error (lambda (e) (hydra_lib_strings_cat (list "map key type contains a function type: " (hydra_show_core_type (funcall (lambda (v) (hydra_error_core_non_comparable_map_key_type_error-key_type v)) e))))))

(defvar hydra_show_error_core_non_comparable_set_element_type_error (lambda (e) (hydra_lib_strings_cat (list "set element type contains a function type: " (hydra_show_core_type (funcall (lambda (v) (hydra_error_core_non_comparable_set_element_type_error-element_type v)) e))))))

(defvar hydra_show_error_core_single_variant_union_error (lambda (e) (hydra_lib_strings_cat (list "union type with single variant: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_single_variant_union_error-field_name v)) e))))))

(defvar hydra_show_error_core_type_variable_shadowing_in_forall_error (lambda (e) (hydra_lib_strings_cat (list "type variable shadowing in forall: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_type_variable_shadowing_in_forall_error-name v)) e))))))

(defvar hydra_show_error_core_undefined_type_variable_error (lambda (e) (hydra_lib_strings_cat (list "undefined type variable: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_undefined_type_variable_error-name v)) e))))))

(defvar hydra_show_error_core_void_in_non_bottom_position_error (lambda (e) "TypeVoid in a position where no value can be constructed"))

(defvar hydra_show_error_core_invalid_type_error (lambda (e) (funcall (hydra_lib_strings_cat2 "invalid type: ") (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :duplicate_record_type_field_names) (hydra_show_error_core_duplicate_record_type_field_names_error match_value)) ((equal (car match_target) :duplicate_union_type_field_names) (hydra_show_error_core_duplicate_union_type_field_names_error match_value)) ((equal (car match_target) :empty_record_type) (hydra_show_error_core_empty_record_type_error match_value)) ((equal (car match_target) :empty_type_annotation) (hydra_show_error_core_empty_type_annotation_error match_value)) ((equal (car match_target) :empty_union_type) (hydra_show_error_core_empty_union_type_error match_value)) ((equal (car match_target) :invalid_forall_parameter_name) (hydra_show_error_core_invalid_forall_parameter_name_error match_value)) ((equal (car match_target) :invalid_type_scheme_variable_name) (hydra_show_error_core_invalid_type_scheme_variable_name_error match_value)) ((equal (car match_target) :nested_type_annotation) (hydra_show_error_core_nested_type_annotation_error match_value)) ((equal (car match_target) :non_comparable_map_key_type) (hydra_show_error_core_non_comparable_map_key_type_error match_value)) ((equal (car match_target) :non_comparable_set_element_type) (hydra_show_error_core_non_comparable_set_element_type_error match_value)) ((equal (car match_target) :single_variant_union) (hydra_show_error_core_single_variant_union_error match_value)) ((equal (car match_target) :type_variable_shadowing_in_forall) (hydra_show_error_core_type_variable_shadowing_in_forall_error match_value)) ((equal (car match_target) :undefined_type_variable) (hydra_show_error_core_undefined_type_variable_error match_value)) ((equal (car match_target) :void_in_non_bottom_position) (hydra_show_error_core_void_in_non_bottom_position_error match_value)))) (cadr match_target))) e))))

(defvar hydra_show_error_core_undefined_field_error (lambda (e) (let ((fname (funcall (lambda (v) (hydra_error_core_undefined_field_error-field_name v)) e))) (let ((tname (funcall (lambda (v) (hydra_error_core_undefined_field_error-type_name v)) e))) (hydra_lib_strings_cat (list "no such field \"" (funcall (lambda (v) v) fname) "\" in type \"" (funcall (lambda (v) v) tname) "\""))))))

(defvar hydra_show_error_core_unexpected_term_variant_error (lambda (e) (let ((expected (funcall (lambda (v) (hydra_error_core_unexpected_term_variant_error-expected_variant v)) e))) (let ((actual (funcall (lambda (v) (hydra_error_core_unexpected_term_variant_error-actual_term v)) e))) (hydra_lib_strings_cat (list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(defvar hydra_show_error_core_unexpected_type_variant_error (lambda (e) (let ((expected (funcall (lambda (v) (hydra_error_core_unexpected_type_variant_error-expected_variant v)) e))) (let ((actual (funcall (lambda (v) (hydra_error_core_unexpected_type_variant_error-actual_type v)) e))) (hydra_lib_strings_cat (list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

(provide 'hydra.show.error.core)

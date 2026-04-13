(defpackage :hydra.show.error
(:use :cl :hydra.core :hydra.error :hydra.formatting :hydra.lib.lists :hydra.lib.literals :hydra.lib.sets :hydra.lib.strings :hydra.show.core :hydra.show.meta :hydra.show.typing)
(:export :hydra_show_error_incorrect_unification_error :hydra_show_error_not_a_forall_type_error :hydra_show_error_not_a_function_type_error :hydra_show_error_type_arity_mismatch_error :hydra_show_error_type_mismatch_error :hydra_show_error_unbound_type_variables_error :hydra_show_error_unequal_types_error :hydra_show_error_unsupported_term_variant_error :hydra_show_error_untyped_lambda_error :hydra_show_error_untyped_let_binding_error :hydra_show_error_checking_error :hydra_show_error_decoding_error :hydra_show_error_duplicate_binding_error :hydra_show_error_duplicate_field_error :hydra_show_error_other_error :hydra_show_error_undefined_field_error :hydra_show_error_undefined_term_error :hydra_show_error_undefined_type_error :hydra_show_error_unexpected_term_variant_error :hydra_show_error_unexpected_type_variant_error :hydra_show_error_unification_error :hydra_show_error_error))

(in-package :hydra.show.error)

(cl:defvar hydra_show_error_incorrect_unification_error (cl:lambda (e) (let ((subst ((cl:lambda (v) (hydra_error_incorrect_unification_error-substitution v)) e))) ((hydra_lib_strings_cat2 "incorrect unification: ") (hydra_show_typing_type_subst subst)))))

(cl:defvar hydra_show_error_not_a_forall_type_error (cl:lambda (e) (let ((typ ((cl:lambda (v) (hydra_error_not_a_forall_type_error-type v)) e))) (let ((args ((cl:lambda (v) (hydra_error_not_a_forall_type_error-type_arguments v)) e))) (hydra_lib_strings_cat (cl:list "not a forall type: " (hydra_show_core_type typ) ". Trying to apply " (hydra_lib_literals_show_int32 (hydra_lib_lists_length args)) " type argument(s): " ((hydra_formatting_show_list hydra_show_core_type) args)))))))

(cl:defvar hydra_show_error_not_a_function_type_error (cl:lambda (e) (let ((typ ((cl:lambda (v) (hydra_error_not_a_function_type_error-type v)) e))) ((hydra_lib_strings_cat2 "not a function type: ") (hydra_show_core_type typ)))))

(cl:defvar hydra_show_error_type_arity_mismatch_error (cl:lambda (e) (let ((typ ((cl:lambda (v) (hydra_error_type_arity_mismatch_error-type v)) e))) (let ((expected ((cl:lambda (v) (hydra_error_type_arity_mismatch_error-expected_arity v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_type_arity_mismatch_error-actual_arity v)) e))) (let ((args ((cl:lambda (v) (hydra_error_type_arity_mismatch_error-type_arguments v)) e))) (hydra_lib_strings_cat (cl:list "type " (hydra_show_core_type typ) " applied to the wrong number of type arguments (expected " (hydra_lib_literals_show_int32 expected) ", got " (hydra_lib_literals_show_int32 actual) "): " ((hydra_formatting_show_list hydra_show_core_type) args)))))))))

(cl:defvar hydra_show_error_type_mismatch_error (cl:lambda (e) (let ((expected ((cl:lambda (v) (hydra_error_type_mismatch_error-expected_type v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_type_mismatch_error-actual_type v)) e))) (hydra_lib_strings_cat (cl:list "type mismatch: expected " (hydra_show_core_type expected) " but found " (hydra_show_core_type actual)))))))

(cl:defvar hydra_show_error_unbound_type_variables_error (cl:lambda (e) (let ((vars ((cl:lambda (v) (hydra_error_unbound_type_variables_error-variables v)) e))) (let ((typ ((cl:lambda (v) (hydra_error_unbound_type_variables_error-type v)) e))) (hydra_lib_strings_cat (cl:list "unbound type variables: {" ((hydra_lib_strings_intercalate ", ") ((hydra_lib_lists_map (cl:lambda (v) v)) (hydra_lib_sets_to_list vars))) "} in type " (hydra_show_core_type typ)))))))

(cl:defvar hydra_show_error_unequal_types_error (cl:lambda (e) (let ((types ((cl:lambda (v) (hydra_error_unequal_types_error-types v)) e))) (let ((desc ((cl:lambda (v) (hydra_error_unequal_types_error-description v)) e))) (hydra_lib_strings_cat (cl:list "unequal types " ((hydra_formatting_show_list hydra_show_core_type) types) " in " desc))))))

(cl:defvar hydra_show_error_unsupported_term_variant_error (cl:lambda (e) ((hydra_lib_strings_cat2 "unsupported term variant: ") (hydra_show_meta_term_variant ((cl:lambda (v) (hydra_error_unsupported_term_variant_error-term_variant v)) e)))))

(cl:defvar hydra_show_error_untyped_lambda_error (cl:lambda (_) "untyped lambda"))

(cl:defvar hydra_show_error_untyped_let_binding_error (cl:lambda (e) (let ((b ((cl:lambda (v) (hydra_error_untyped_let_binding_error-binding v)) e))) ((hydra_lib_strings_cat2 "untyped let binding: ") (hydra_show_core_binding b)))))

(cl:defvar hydra_show_error_checking_error (cl:lambda (ce) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :incorrect_unification) (hydra_show_error_incorrect_unification_error match_value)) ((equal (car match_target) :not_a_forall_type) (hydra_show_error_not_a_forall_type_error match_value)) ((equal (car match_target) :not_a_function_type) (hydra_show_error_not_a_function_type_error match_value)) ((equal (car match_target) :type_arity_mismatch) (hydra_show_error_type_arity_mismatch_error match_value)) ((equal (car match_target) :type_mismatch) (hydra_show_error_type_mismatch_error match_value)) ((equal (car match_target) :unbound_type_variables) (hydra_show_error_unbound_type_variables_error match_value)) ((equal (car match_target) :unequal_types) (hydra_show_error_unequal_types_error match_value)) ((equal (car match_target) :unsupported_term_variant) (hydra_show_error_unsupported_term_variant_error match_value)) ((equal (car match_target) :untyped_lambda) (hydra_show_error_untyped_lambda_error match_value)) ((equal (car match_target) :untyped_let_binding) (hydra_show_error_untyped_let_binding_error match_value)))) (cadr match_target))) ce)))

(cl:defvar hydra_show_error_decoding_error (cl:lambda (de) ((hydra_lib_strings_cat2 "decoding error: ") ((cl:lambda (v) v) de))))

(cl:defvar hydra_show_error_duplicate_binding_error (cl:lambda (e) ((hydra_lib_strings_cat2 "duplicate binding: ") ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_duplicate_binding_error-name v)) e)))))

(cl:defvar hydra_show_error_duplicate_field_error (cl:lambda (e) ((hydra_lib_strings_cat2 "duplicate field: ") ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_duplicate_field_error-name v)) e)))))

(cl:defvar hydra_show_error_other_error (cl:lambda (oe) ((cl:lambda (v) v) oe)))

(cl:defvar hydra_show_error_undefined_field_error (cl:lambda (e) (let ((fname ((cl:lambda (v) (hydra_error_undefined_field_error-field_name v)) e))) (let ((tname ((cl:lambda (v) (hydra_error_undefined_field_error-type_name v)) e))) (hydra_lib_strings_cat (cl:list "no such field \"" ((cl:lambda (v) v) fname) "\" in type \"" ((cl:lambda (v) v) tname) "\""))))))

(cl:defvar hydra_show_error_undefined_term_error (cl:lambda (e) ((hydra_lib_strings_cat2 "undefined term: ") ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_undefined_term_error-name v)) e)))))

(cl:defvar hydra_show_error_undefined_type_error (cl:lambda (e) ((hydra_lib_strings_cat2 "undefined type: ") ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_undefined_type_error-name v)) e)))))

(cl:defvar hydra_show_error_unexpected_term_variant_error (cl:lambda (e) (let ((expected ((cl:lambda (v) (hydra_error_unexpected_term_variant_error-expected_variant v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_unexpected_term_variant_error-actual_term v)) e))) (hydra_lib_strings_cat (cl:list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(cl:defvar hydra_show_error_unexpected_type_variant_error (cl:lambda (e) (let ((expected ((cl:lambda (v) (hydra_error_unexpected_type_variant_error-expected_variant v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_unexpected_type_variant_error-actual_type v)) e))) (hydra_lib_strings_cat (cl:list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

(cl:defvar hydra_show_error_unification_error (cl:lambda (e) (let ((lt ((cl:lambda (v) (hydra_error_unification_error-left_type v)) e))) (let ((rt ((cl:lambda (v) (hydra_error_unification_error-right_type v)) e))) (let ((msg ((cl:lambda (v) (hydra_error_unification_error-message v)) e))) (hydra_lib_strings_cat (cl:list "unification error: cannot unify " (hydra_show_core_type lt) " with " (hydra_show_core_type rt) ": " msg)))))))

(cl:defvar hydra_show_error_error (cl:lambda (e) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :checking) (hydra_show_error_checking_error match_value)) ((equal (car match_target) :decoding) (hydra_show_error_decoding_error match_value)) ((equal (car match_target) :duplicate_binding) (hydra_show_error_duplicate_binding_error match_value)) ((equal (car match_target) :duplicate_field) (hydra_show_error_duplicate_field_error match_value)) ((equal (car match_target) :other) (hydra_show_error_other_error match_value)) ((equal (car match_target) :undefined_field) (hydra_show_error_undefined_field_error match_value)) ((equal (car match_target) :undefined_term) (hydra_show_error_undefined_term_error match_value)) ((equal (car match_target) :undefined_type) (hydra_show_error_undefined_type_error match_value)) ((equal (car match_target) :unexpected_term_variant) (hydra_show_error_unexpected_term_variant_error match_value)) ((equal (car match_target) :unexpected_type_variant) (hydra_show_error_unexpected_type_variant_error match_value)) ((equal (car match_target) :unification) (hydra_show_error_unification_error match_value)))) (cadr match_target))) e)))

(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error)

(require 'hydra.formatting)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(require 'hydra.show.meta)

(require 'hydra.show.typing)

(defvar hydra_show_error_incorrect_unification_error (lambda (e) (let ((subst ((lambda (v) (hydra_error_incorrect_unification_error-substitution v)) e))) ((hydra_lib_strings_cat2 "incorrect unification: ") (hydra_show_typing_type_subst subst)))))

(defvar hydra_show_error_not_a_forall_type_error (lambda (e) (let ((typ ((lambda (v) (hydra_error_not_a_forall_type_error-type v)) e))) (let ((args ((lambda (v) (hydra_error_not_a_forall_type_error-type_arguments v)) e))) (hydra_lib_strings_cat (list "not a forall type: " (hydra_show_core_type typ) ". Trying to apply " (hydra_lib_literals_show_int32 (hydra_lib_lists_length args)) " type argument(s): " ((hydra_formatting_show_list hydra_show_core_type) args)))))))

(defvar hydra_show_error_not_a_function_type_error (lambda (e) (let ((typ ((lambda (v) (hydra_error_not_a_function_type_error-type v)) e))) ((hydra_lib_strings_cat2 "not a function type: ") (hydra_show_core_type typ)))))

(defvar hydra_show_error_type_arity_mismatch_error (lambda (e) (let ((typ ((lambda (v) (hydra_error_type_arity_mismatch_error-type v)) e))) (let ((expected ((lambda (v) (hydra_error_type_arity_mismatch_error-expected_arity v)) e))) (let ((actual ((lambda (v) (hydra_error_type_arity_mismatch_error-actual_arity v)) e))) (let ((args ((lambda (v) (hydra_error_type_arity_mismatch_error-type_arguments v)) e))) (hydra_lib_strings_cat (list "type " (hydra_show_core_type typ) " applied to the wrong number of type arguments (expected " (hydra_lib_literals_show_int32 expected) ", got " (hydra_lib_literals_show_int32 actual) "): " ((hydra_formatting_show_list hydra_show_core_type) args)))))))))

(defvar hydra_show_error_type_mismatch_error (lambda (e) (let ((expected ((lambda (v) (hydra_error_type_mismatch_error-expected_type v)) e))) (let ((actual ((lambda (v) (hydra_error_type_mismatch_error-actual_type v)) e))) (hydra_lib_strings_cat (list "type mismatch: expected " (hydra_show_core_type expected) " but found " (hydra_show_core_type actual)))))))

(defvar hydra_show_error_unbound_type_variables_error (lambda (e) (let ((vars ((lambda (v) (hydra_error_unbound_type_variables_error-variables v)) e))) (let ((typ ((lambda (v) (hydra_error_unbound_type_variables_error-type v)) e))) (hydra_lib_strings_cat (list "unbound type variables: {" ((hydra_lib_strings_intercalate ", ") ((hydra_lib_lists_map (lambda (v) v)) (hydra_lib_sets_to_list vars))) "} in type " (hydra_show_core_type typ)))))))

(defvar hydra_show_error_unequal_types_error (lambda (e) (let ((types ((lambda (v) (hydra_error_unequal_types_error-types v)) e))) (let ((desc ((lambda (v) (hydra_error_unequal_types_error-description v)) e))) (hydra_lib_strings_cat (list "unequal types " ((hydra_formatting_show_list hydra_show_core_type) types) " in " desc))))))

(defvar hydra_show_error_unsupported_term_variant_error (lambda (e) ((hydra_lib_strings_cat2 "unsupported term variant: ") (hydra_show_meta_term_variant ((lambda (v) (hydra_error_unsupported_term_variant_error-term_variant v)) e)))))

(defvar hydra_show_error_untyped_lambda_error (lambda (_) "untyped lambda"))

(defvar hydra_show_error_untyped_let_binding_error (lambda (e) (let ((b ((lambda (v) (hydra_error_untyped_let_binding_error-binding v)) e))) ((hydra_lib_strings_cat2 "untyped let binding: ") (hydra_show_core_binding b)))))

(defvar hydra_show_error_checking_error (lambda (ce) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :incorrect_unification) (hydra_show_error_incorrect_unification_error match_value)) ((equal (car match_target) :not_a_forall_type) (hydra_show_error_not_a_forall_type_error match_value)) ((equal (car match_target) :not_a_function_type) (hydra_show_error_not_a_function_type_error match_value)) ((equal (car match_target) :type_arity_mismatch) (hydra_show_error_type_arity_mismatch_error match_value)) ((equal (car match_target) :type_mismatch) (hydra_show_error_type_mismatch_error match_value)) ((equal (car match_target) :unbound_type_variables) (hydra_show_error_unbound_type_variables_error match_value)) ((equal (car match_target) :unequal_types) (hydra_show_error_unequal_types_error match_value)) ((equal (car match_target) :unsupported_term_variant) (hydra_show_error_unsupported_term_variant_error match_value)) ((equal (car match_target) :untyped_lambda) (hydra_show_error_untyped_lambda_error match_value)) ((equal (car match_target) :untyped_let_binding) (hydra_show_error_untyped_let_binding_error match_value)))) (cadr match_target))) ce)))

(defvar hydra_show_error_decoding_error (lambda (de) ((hydra_lib_strings_cat2 "decoding error: ") ((lambda (v) v) de))))

(defvar hydra_show_error_duplicate_binding_error (lambda (e) ((hydra_lib_strings_cat2 "duplicate binding: ") ((lambda (v) v) ((lambda (v) (hydra_error_duplicate_binding_error-name v)) e)))))

(defvar hydra_show_error_duplicate_field_error (lambda (e) ((hydra_lib_strings_cat2 "duplicate field: ") ((lambda (v) v) ((lambda (v) (hydra_error_duplicate_field_error-name v)) e)))))

(defvar hydra_show_error_other_error (lambda (oe) ((lambda (v) v) oe)))

(defvar hydra_show_error_undefined_field_error (lambda (e) (let ((fname ((lambda (v) (hydra_error_undefined_field_error-field_name v)) e))) (let ((tname ((lambda (v) (hydra_error_undefined_field_error-type_name v)) e))) (hydra_lib_strings_cat (list "no such field \"" ((lambda (v) v) fname) "\" in type \"" ((lambda (v) v) tname) "\""))))))

(defvar hydra_show_error_undefined_term_error (lambda (e) ((hydra_lib_strings_cat2 "undefined term: ") ((lambda (v) v) ((lambda (v) (hydra_error_undefined_term_error-name v)) e)))))

(defvar hydra_show_error_undefined_type_error (lambda (e) ((hydra_lib_strings_cat2 "undefined type: ") ((lambda (v) v) ((lambda (v) (hydra_error_undefined_type_error-name v)) e)))))

(defvar hydra_show_error_unexpected_term_variant_error (lambda (e) (let ((expected ((lambda (v) (hydra_error_unexpected_term_variant_error-expected_variant v)) e))) (let ((actual ((lambda (v) (hydra_error_unexpected_term_variant_error-actual_term v)) e))) (hydra_lib_strings_cat (list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(defvar hydra_show_error_unexpected_type_variant_error (lambda (e) (let ((expected ((lambda (v) (hydra_error_unexpected_type_variant_error-expected_variant v)) e))) (let ((actual ((lambda (v) (hydra_error_unexpected_type_variant_error-actual_type v)) e))) (hydra_lib_strings_cat (list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

(defvar hydra_show_error_unification_error (lambda (e) (let ((lt ((lambda (v) (hydra_error_unification_error-left_type v)) e))) (let ((rt ((lambda (v) (hydra_error_unification_error-right_type v)) e))) (let ((msg ((lambda (v) (hydra_error_unification_error-message v)) e))) (hydra_lib_strings_cat (list "unification error: cannot unify " (hydra_show_core_type lt) " with " (hydra_show_core_type rt) ": " msg)))))))

(defvar hydra_show_error_error (lambda (e) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :checking) (hydra_show_error_checking_error match_value)) ((equal (car match_target) :decoding) (hydra_show_error_decoding_error match_value)) ((equal (car match_target) :duplicate_binding) (hydra_show_error_duplicate_binding_error match_value)) ((equal (car match_target) :duplicate_field) (hydra_show_error_duplicate_field_error match_value)) ((equal (car match_target) :other) (hydra_show_error_other_error match_value)) ((equal (car match_target) :undefined_field) (hydra_show_error_undefined_field_error match_value)) ((equal (car match_target) :undefined_term) (hydra_show_error_undefined_term_error match_value)) ((equal (car match_target) :undefined_type) (hydra_show_error_undefined_type_error match_value)) ((equal (car match_target) :unexpected_term_variant) (hydra_show_error_unexpected_term_variant_error match_value)) ((equal (car match_target) :unexpected_type_variant) (hydra_show_error_unexpected_type_variant_error match_value)) ((equal (car match_target) :unification) (hydra_show_error_unification_error match_value)))) (cadr match_target))) e)))

(provide 'hydra.show.error)

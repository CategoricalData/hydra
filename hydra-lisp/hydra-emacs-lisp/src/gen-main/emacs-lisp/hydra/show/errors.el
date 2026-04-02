(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error.checking)

(require 'hydra.errors)

(require 'hydra.formatting)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(require 'hydra.show.error.core)

(require 'hydra.show.typing)

(require 'hydra.show.variants)

(defvar hydra_show_errors_incorrect_unification_error (lambda (e) (let ((subst (funcall (lambda (v) (hydra_error_checking_incorrect_unification_error-substitution v)) e))) (funcall (hydra_lib_strings_cat2 "incorrect unification: ") (hydra_show_typing_type_subst subst)))))

(defvar hydra_show_errors_not_a_forall_type_error (lambda (e) (let ((typ (funcall (lambda (v) (hydra_error_checking_not_a_forall_type_error-type v)) e))) (let ((args (funcall (lambda (v) (hydra_error_checking_not_a_forall_type_error-type_arguments v)) e))) (hydra_lib_strings_cat (list "not a forall type: " (hydra_show_core_type typ) ". Trying to apply " (hydra_lib_literals_show_int32 (hydra_lib_lists_length args)) " type argument(s): " (funcall (hydra_formatting_show_list hydra_show_core_type) args)))))))

(defvar hydra_show_errors_not_a_function_type_error (lambda (e) (let ((typ (funcall (lambda (v) (hydra_error_checking_not_a_function_type_error-type v)) e))) (funcall (hydra_lib_strings_cat2 "not a function type: ") (hydra_show_core_type typ)))))

(defvar hydra_show_errors_type_arity_mismatch_error (lambda (e) (let ((typ (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-type v)) e))) (let ((expected (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-expected_arity v)) e))) (let ((actual (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-actual_arity v)) e))) (let ((args (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-type_arguments v)) e))) (hydra_lib_strings_cat (list "type " (hydra_show_core_type typ) " applied to the wrong number of type arguments (expected " (hydra_lib_literals_show_int32 expected) ", got " (hydra_lib_literals_show_int32 actual) "): " (funcall (hydra_formatting_show_list hydra_show_core_type) args)))))))))

(defvar hydra_show_errors_type_mismatch_error (lambda (e) (let ((expected (funcall (lambda (v) (hydra_error_checking_type_mismatch_error-expected_type v)) e))) (let ((actual (funcall (lambda (v) (hydra_error_checking_type_mismatch_error-actual_type v)) e))) (hydra_lib_strings_cat (list "type mismatch: expected " (hydra_show_core_type expected) " but found " (hydra_show_core_type actual)))))))

(defvar hydra_show_errors_unbound_type_variables_error (lambda (e) (let ((vars (funcall (lambda (v) (hydra_error_checking_unbound_type_variables_error-variables v)) e))) (let ((typ (funcall (lambda (v) (hydra_error_checking_unbound_type_variables_error-type v)) e))) (hydra_lib_strings_cat (list "unbound type variables: {" (funcall (hydra_lib_strings_intercalate ", ") (funcall (hydra_lib_lists_map (lambda (v) v)) (hydra_lib_sets_to_list vars))) "} in type " (hydra_show_core_type typ)))))))

(defvar hydra_show_errors_unequal_types_error (lambda (e) (let ((types (funcall (lambda (v) (hydra_error_checking_unequal_types_error-types v)) e))) (let ((desc (funcall (lambda (v) (hydra_error_checking_unequal_types_error-description v)) e))) (hydra_lib_strings_cat (list "unequal types " (funcall (hydra_formatting_show_list hydra_show_core_type) types) " in " desc))))))

(defvar hydra_show_errors_unsupported_term_variant_error (lambda (e) (funcall (hydra_lib_strings_cat2 "unsupported term variant: ") (hydra_show_variants_term_variant (funcall (lambda (v) (hydra_error_checking_unsupported_term_variant_error-term_variant v)) e)))))

(defvar hydra_show_errors_untyped_lambda_error (lambda (_) "untyped lambda"))

(defvar hydra_show_errors_untyped_let_binding_error (lambda (e) (let ((b (funcall (lambda (v) (hydra_error_checking_untyped_let_binding_error-binding v)) e))) (funcall (hydra_lib_strings_cat2 "untyped let binding: ") (hydra_show_core_binding b)))))

(defvar hydra_show_errors_checking_error (lambda (ce) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :incorrect_unification) (hydra_show_errors_incorrect_unification_error match_value)) ((equal (car match_target) :not_a_forall_type) (hydra_show_errors_not_a_forall_type_error match_value)) ((equal (car match_target) :not_a_function_type) (hydra_show_errors_not_a_function_type_error match_value)) ((equal (car match_target) :type_arity_mismatch) (hydra_show_errors_type_arity_mismatch_error match_value)) ((equal (car match_target) :type_mismatch) (hydra_show_errors_type_mismatch_error match_value)) ((equal (car match_target) :unbound_type_variables) (hydra_show_errors_unbound_type_variables_error match_value)) ((equal (car match_target) :unequal_types) (hydra_show_errors_unequal_types_error match_value)) ((equal (car match_target) :unsupported_term_variant) (hydra_show_errors_unsupported_term_variant_error match_value)) ((equal (car match_target) :untyped_lambda) (hydra_show_errors_untyped_lambda_error match_value)) ((equal (car match_target) :untyped_let_binding) (hydra_show_errors_untyped_let_binding_error match_value)))) (cadr match_target))) ce)))

(defvar hydra_show_errors_decoding_error (lambda (de) (funcall (hydra_lib_strings_cat2 "decoding error: ") (funcall (lambda (v) v) de))))

(defvar hydra_show_errors_other_error (lambda (oe) (funcall (lambda (v) v) oe)))

(defvar hydra_show_errors_unification_error (lambda (e) (let ((lt (funcall (lambda (v) (hydra_errors_unification_error-left_type v)) e))) (let ((rt (funcall (lambda (v) (hydra_errors_unification_error-right_type v)) e))) (let ((msg (funcall (lambda (v) (hydra_errors_unification_error-message v)) e))) (hydra_lib_strings_cat (list "unification error: cannot unify " (hydra_show_core_type lt) " with " (hydra_show_core_type rt) ": " msg)))))))

(defvar hydra_show_errors_error (lambda (e) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :checking) (hydra_show_errors_checking_error match_value)) ((equal (car match_target) :decoding) (hydra_show_errors_decoding_error match_value)) ((equal (car match_target) :duplicate_binding) (hydra_show_error_core_duplicate_binding_error match_value)) ((equal (car match_target) :duplicate_field) (hydra_show_error_core_duplicate_field_error match_value)) ((equal (car match_target) :other) (hydra_show_errors_other_error match_value)) ((equal (car match_target) :undefined_field) (hydra_show_error_core_undefined_field_error match_value)) ((equal (car match_target) :undefined_term_variable) (hydra_show_error_core_undefined_term_variable_error match_value)) ((equal (car match_target) :untyped_term_variable) (hydra_show_error_core_untyped_term_variable_error match_value)) ((equal (car match_target) :unexpected_term_variant) (hydra_show_error_core_unexpected_term_variant_error match_value)) ((equal (car match_target) :unexpected_type_variant) (hydra_show_error_core_unexpected_type_variant_error match_value)) ((equal (car match_target) :unification) (hydra_show_errors_unification_error match_value)))) (cadr match_target))) e)))

(provide 'hydra.show.errors)

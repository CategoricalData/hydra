(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.paths)

(require 'hydra.typing)

(require 'hydra.variants)

(defvar hydra_error_checking_checking_error-variants (list :incorrect_unification :not_a_forall_type :not_a_function_type :other :type_arity_mismatch :type_mismatch :unbound_type_variables :undefined_term_variable :unequal_types :unsupported_term_variant :untyped_lambda :untyped_let_binding :untyped_term_variable))

(cl-defstruct hydra_error_checking_incorrect_unification_error substitution)

(cl-defstruct hydra_error_checking_not_a_forall_type_error type type_arguments)

(cl-defstruct hydra_error_checking_not_a_function_type_error type)

(cl-defstruct hydra_error_checking_other_checking_error path message)

(cl-defstruct hydra_error_checking_type_arity_mismatch_error type expected_arity actual_arity type_arguments)

(cl-defstruct hydra_error_checking_type_mismatch_error expected_type actual_type)

(cl-defstruct hydra_error_checking_unbound_type_variables_error variables type)

(cl-defstruct hydra_error_checking_undefined_term_variable_checking_error path name)

(cl-defstruct hydra_error_checking_unequal_types_error types description)

(cl-defstruct hydra_error_checking_unsupported_term_variant_error term_variant)

(cl-defstruct hydra_error_checking_untyped_lambda_error)

(cl-defstruct hydra_error_checking_untyped_let_binding_error binding)

(cl-defstruct hydra_error_checking_untyped_term_variable_checking_error path name)

(provide 'hydra.error.checking)

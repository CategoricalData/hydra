(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.typing)

(require 'hydra.variants)

(defvar hydra_error_checking_error-variants (list :incorrect_unification :not_a_forall_type :not_a_function_type :type_arity_mismatch :type_mismatch :unbound_type_variables :unequal_types :unsupported_term_variant :untyped_lambda :untyped_let_binding))

(cl-defstruct hydra_error_decoding_error value)

(cl-defstruct hydra_error_duplicate_binding_error name)

(cl-defstruct hydra_error_duplicate_field_error name)

(defvar hydra_error_error-variants (list :checking :decoding :duplicate_binding :duplicate_field :other :undefined_field :undefined_term :undefined_type :unexpected_term_variant :unexpected_type_variant :unification))

(cl-defstruct hydra_error_incorrect_unification_error substitution)

(cl-defstruct hydra_error_not_a_forall_type_error type type_arguments)

(cl-defstruct hydra_error_not_a_function_type_error type)

(cl-defstruct hydra_error_other_error value)

(cl-defstruct hydra_error_type_arity_mismatch_error type expected_arity actual_arity type_arguments)

(cl-defstruct hydra_error_type_mismatch_error expected_type actual_type)

(cl-defstruct hydra_error_unbound_type_variables_error variables type)

(cl-defstruct hydra_error_undefined_field_error field_name type_name)

(cl-defstruct hydra_error_undefined_term_error name)

(cl-defstruct hydra_error_undefined_type_error name)

(cl-defstruct hydra_error_unequal_types_error types description)

(cl-defstruct hydra_error_unexpected_term_variant_error expected_variant actual_term)

(cl-defstruct hydra_error_unexpected_type_variant_error expected_variant actual_type)

(cl-defstruct hydra_error_unification_error left_type right_type message)

(cl-defstruct hydra_error_unsupported_term_variant_error term_variant)

(cl-defstruct hydra_error_untyped_lambda_error)

(cl-defstruct hydra_error_untyped_let_binding_error binding)

(provide 'hydra.error)

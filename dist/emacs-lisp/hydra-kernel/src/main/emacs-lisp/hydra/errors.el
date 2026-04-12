(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error.checking)

(require 'hydra.error.core)

(require 'hydra.paths)

(cl-defstruct hydra_errors_decoding_error value)

(defvar hydra_errors_error-variants (list :checking :decoding :duplicate_binding :duplicate_field :extraction :inference :other :resolution :undefined_field :undefined_term_variable :untyped_term_variable :unexpected_term_variant :unexpected_type_variant :unification))

(defvar hydra_errors_extraction_error-variants (list :empty_list :multiple_bindings :multiple_fields :no_matching_field :no_such_binding :not_enough_cases :unexpected_shape))

(defvar hydra_errors_inference_error-variants (list :checking :other :unification))

(cl-defstruct hydra_errors_multiple_bindings_error name)

(cl-defstruct hydra_errors_multiple_fields_error field_name)

(cl-defstruct hydra_errors_no_matching_field_error field_name)

(cl-defstruct hydra_errors_no_such_binding_error name)

(cl-defstruct hydra_errors_no_such_primitive_error name)

(cl-defstruct hydra_errors_other_error value)

(cl-defstruct hydra_errors_other_inference_error path message)

(cl-defstruct hydra_errors_other_resolution_error value)

(defvar hydra_errors_resolution_error-variants (list :no_such_binding :no_such_primitive :no_matching_field :other :unexpected_shape))

(cl-defstruct hydra_errors_unexpected_shape_error expected actual)

(cl-defstruct hydra_errors_unification_error left_type right_type message)

(cl-defstruct hydra_errors_unification_inference_error path cause)

(provide 'hydra.errors)

(require 'cl-lib)

(require 'hydra.accessors)

(require 'hydra.core)

(require 'hydra.variants)

(cl-defstruct hydra_error_core_duplicate_binding_error location name)

(cl-defstruct hydra_error_core_duplicate_field_error location name)

(defvar hydra_error_core_invalid_term_error-variants (list :duplicate_binding :duplicate_field))

(cl-defstruct hydra_error_core_undefined_field_error field_name type_name)

(cl-defstruct hydra_error_core_undefined_term_error name)

(cl-defstruct hydra_error_core_undefined_type_error name)

(cl-defstruct hydra_error_core_unexpected_term_variant_error expected_variant actual_term)

(cl-defstruct hydra_error_core_unexpected_type_variant_error expected_variant actual_type)

(provide 'hydra.error.core)

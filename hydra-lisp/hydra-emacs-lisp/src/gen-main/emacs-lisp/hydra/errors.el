(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error.checking)

(require 'hydra.error.core)

(cl-defstruct hydra_errors_decoding_error value)

(defvar hydra_errors_error-variants (list :checking :decoding :duplicate_binding :duplicate_field :other :undefined_field :undefined_term_variable :untyped_term_variable :unexpected_term_variant :unexpected_type_variant :unification))

(cl-defstruct hydra_errors_other_error value)

(cl-defstruct hydra_errors_unification_error left_type right_type message)

(provide 'hydra.errors)

(require 'cl-lib)

(require 'hydra.core)

(cl-defstruct hydra_error_decoding_error value)

(defvar hydra_error_error-variants (list :decoding :other :unification))

(cl-defstruct hydra_error_other_error value)

(cl-defstruct hydra_error_unification_error left_type right_type message)

(provide 'hydra.error)

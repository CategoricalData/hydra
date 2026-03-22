(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.errors)

(cl-defstruct hydra_util_adapter is_lossy source target coder)

(cl-defstruct hydra_util_bicoder encode decode)

(defvar hydra_util_case_convention-variants (list :camel :pascal :lower_snake :upper_snake))

(cl-defstruct hydra_util_coder encode decode)

(defvar hydra_util_comparison-variants (list :less_than :equal_to :greater_than))

(defvar hydra_util_precision-variants (list :arbitrary :bits))

(provide 'hydra.util)

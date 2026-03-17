(require 'cl-lib)

(require 'hydra.core)

(cl-defstruct hydra_context_context trace messages other)

(cl-defstruct hydra_context_in_context object context)

(provide 'hydra.context)

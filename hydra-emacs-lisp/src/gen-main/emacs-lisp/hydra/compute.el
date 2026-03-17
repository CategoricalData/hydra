(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.error)

(cl-defstruct hydra_compute_adapter is_lossy source target coder)

(cl-defstruct hydra_compute_bicoder encode decode)

(cl-defstruct hydra_compute_coder encode decode)

(provide 'hydra.compute)

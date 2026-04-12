(require 'cl-lib)

(require 'hydra.core)

(cl-defstruct hydra_phantoms_t_binding name term)

(cl-defstruct hydra_phantoms_t_term value)

(cl-defstruct hydra_phantoms_t_term_definition name term)

(provide 'hydra.phantoms)

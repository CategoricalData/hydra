(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.graph)

(require 'hydra.util)

(require 'hydra.variants)

(cl-defstruct hydra_coders_adapter_context graph language adapters)

(defvar hydra_coders_coder_direction-variants (list :encode :decode))

(cl-defstruct hydra_coders_language name constraints)

(cl-defstruct hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types)

(cl-defstruct hydra_coders_language_name value)

(defvar hydra_coders_traversal_order-variants (list :pre :post))

(provide 'hydra.coders)

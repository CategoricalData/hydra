(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.errors)

(cl-defstruct hydra_graph_graph bound_terms bound_types class_constraints lambda_variables metadata primitives schema_types type_variables)

(cl-defstruct hydra_graph_primitive name type implementation)

(cl-defstruct hydra_graph_term_coder type encode decode)

(provide 'hydra.graph)

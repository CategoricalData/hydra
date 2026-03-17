(defpackage :hydra.graph
(:use :cl :hydra.context :hydra.core :hydra.error)
(:export :make-hydra_graph_graph :hydra_graph_graph? :hydra_graph_graph-bound_terms :hydra_graph_graph-bound_types :hydra_graph_graph-class_constraints :hydra_graph_graph-lambda_variables :hydra_graph_graph-metadata :hydra_graph_graph-primitives :hydra_graph_graph-schema_types :hydra_graph_graph-type_variables :make-hydra_graph_primitive :hydra_graph_primitive? :hydra_graph_primitive-name :hydra_graph_primitive-type :hydra_graph_primitive-implementation :make-hydra_graph_term_coder :hydra_graph_term_coder? :hydra_graph_term_coder-type :hydra_graph_term_coder-encode :hydra_graph_term_coder-decode))

(in-package :hydra.graph)

(cl:defstruct hydra_graph_graph bound_terms bound_types class_constraints lambda_variables metadata primitives schema_types type_variables)

(cl:defstruct hydra_graph_primitive name type implementation)

(cl:defstruct hydra_graph_term_coder type encode decode)

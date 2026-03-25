(defpackage :hydra.paths
(:use :cl :hydra.core)
(:export :make-hydra_paths_subterm_edge :hydra_paths_subterm_edge? :hydra_paths_subterm_edge-source :hydra_paths_subterm_edge-path :hydra_paths_subterm_edge-target :make-hydra_paths_subterm_graph :hydra_paths_subterm_graph? :hydra_paths_subterm_graph-nodes :hydra_paths_subterm_graph-edges :make-hydra_paths_subterm_node :hydra_paths_subterm_node? :hydra_paths_subterm_node-name :hydra_paths_subterm_node-label :hydra_paths_subterm_node-id :make-hydra_paths_subterm_path :hydra_paths_subterm_path? :hydra_paths_subterm_path-value :hydra_paths_subterm_step-variants :make-hydra_paths_subtype_edge :hydra_paths_subtype_edge? :hydra_paths_subtype_edge-source :hydra_paths_subtype_edge-path :hydra_paths_subtype_edge-target :make-hydra_paths_subtype_graph :hydra_paths_subtype_graph? :hydra_paths_subtype_graph-nodes :hydra_paths_subtype_graph-edges :make-hydra_paths_subtype_node :hydra_paths_subtype_node? :hydra_paths_subtype_node-name :hydra_paths_subtype_node-label :hydra_paths_subtype_node-id :make-hydra_paths_subtype_path :hydra_paths_subtype_path? :hydra_paths_subtype_path-value :hydra_paths_subtype_step-variants))

(in-package :hydra.paths)

(cl:defstruct hydra_paths_subterm_edge source path target)

(cl:defstruct hydra_paths_subterm_graph nodes edges)

(cl:defstruct hydra_paths_subterm_node name label id)

(cl:defstruct hydra_paths_subterm_path value)

(cl:defvar hydra_paths_subterm_step-variants (cl:list :annotated_body :application_function :application_argument :lambda_body :union_cases_default :union_cases_branch :let_body :let_binding :list_element :map_key :map_value :maybe_term :product_term :record_field :set_element :sum_term :type_lambda_body :type_application_term :injection_term :wrapped_term))

(cl:defstruct hydra_paths_subtype_edge source path target)

(cl:defstruct hydra_paths_subtype_graph nodes edges)

(cl:defstruct hydra_paths_subtype_node name label id)

(cl:defstruct hydra_paths_subtype_path value)

(cl:defvar hydra_paths_subtype_step-variants (cl:list :annotated_body :application_function :application_argument :either_left :either_right :forall_body :function_domain :function_codomain :list_element :map_keys :map_values :maybe_element :pair_first :pair_second :record_field :set_element :union_field :wrapped_type))

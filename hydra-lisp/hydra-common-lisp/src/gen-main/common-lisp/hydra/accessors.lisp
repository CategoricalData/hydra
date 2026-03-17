(defpackage :hydra.accessors
(:use :cl :hydra.core)
(:export :make-hydra_accessors_accessor_edge :hydra_accessors_accessor_edge? :hydra_accessors_accessor_edge-source :hydra_accessors_accessor_edge-path :hydra_accessors_accessor_edge-target :make-hydra_accessors_accessor_graph :hydra_accessors_accessor_graph? :hydra_accessors_accessor_graph-nodes :hydra_accessors_accessor_graph-edges :make-hydra_accessors_accessor_node :hydra_accessors_accessor_node? :hydra_accessors_accessor_node-name :hydra_accessors_accessor_node-label :hydra_accessors_accessor_node-id :make-hydra_accessors_accessor_path :hydra_accessors_accessor_path? :hydra_accessors_accessor_path-value :hydra_accessors_term_accessor-variants))

(in-package :hydra.accessors)

(cl:defstruct hydra_accessors_accessor_edge source path target)

(cl:defstruct hydra_accessors_accessor_graph nodes edges)

(cl:defstruct hydra_accessors_accessor_node name label id)

(cl:defstruct hydra_accessors_accessor_path value)

(cl:defvar hydra_accessors_term_accessor-variants (cl:list :annotated_body :application_function :application_argument :lambda_body :union_cases_default :union_cases_branch :let_body :let_binding :list_element :map_key :map_value :maybe_term :product_term :record_field :set_element :sum_term :type_lambda_body :type_application_term :injection_term :wrapped_term))

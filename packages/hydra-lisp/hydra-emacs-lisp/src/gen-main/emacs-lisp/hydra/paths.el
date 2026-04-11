(require 'cl-lib)

(require 'hydra.core)

(cl-defstruct hydra_paths_subterm_edge source path target)

(cl-defstruct hydra_paths_subterm_graph nodes edges)

(cl-defstruct hydra_paths_subterm_node name label id)

(cl-defstruct hydra_paths_subterm_path value)

(defvar hydra_paths_subterm_step-variants (list :annotated_body :application_function :application_argument :lambda_body :union_cases_default :union_cases_branch :let_body :let_binding :list_element :map_key :map_value :maybe_term :product_term :record_field :set_element :sum_term :type_lambda_body :type_application_term :injection_term :wrapped_term))

(cl-defstruct hydra_paths_subtype_edge source path target)

(cl-defstruct hydra_paths_subtype_graph nodes edges)

(cl-defstruct hydra_paths_subtype_node name label id)

(cl-defstruct hydra_paths_subtype_path value)

(defvar hydra_paths_subtype_step-variants (list :annotated_body :application_function :application_argument :either_left :either_right :forall_body :function_domain :function_codomain :list_element :map_keys :map_values :maybe_element :pair_first :pair_second :record_field :set_element :union_field :wrapped_type))

(provide 'hydra.paths)

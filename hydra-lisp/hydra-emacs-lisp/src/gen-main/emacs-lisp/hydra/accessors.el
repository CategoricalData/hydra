(require 'cl-lib)

(require 'hydra.core)

(cl-defstruct hydra_accessors_accessor_edge source path target)

(cl-defstruct hydra_accessors_accessor_graph nodes edges)

(cl-defstruct hydra_accessors_accessor_node name label id)

(cl-defstruct hydra_accessors_accessor_path value)

(defvar hydra_accessors_term_accessor-variants (list :annotated_body :application_function :application_argument :lambda_body :union_cases_default :union_cases_branch :let_body :let_binding :list_element :map_key :map_value :maybe_term :product_term :record_field :set_element :sum_term :type_lambda_body :type_application_term :injection_term :wrapped_term))

(provide 'hydra.accessors)

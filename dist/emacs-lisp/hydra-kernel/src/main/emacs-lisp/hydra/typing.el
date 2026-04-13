(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(cl-defstruct hydra_typing_function_structure type_params params bindings body domains codomain environment)

(cl-defstruct hydra_typing_inference_result term type subst class_constraints context)

(cl-defstruct hydra_typing_term_subst value)

(cl-defstruct hydra_typing_type_constraint left right comment)

(cl-defstruct hydra_typing_type_subst value)

(provide 'hydra.typing)

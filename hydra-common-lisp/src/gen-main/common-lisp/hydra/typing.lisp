(defpackage :hydra.typing
(:use :cl :hydra.context :hydra.core)
(:export :make-hydra_typing_function_structure :hydra_typing_function_structure? :hydra_typing_function_structure-type_params :hydra_typing_function_structure-params :hydra_typing_function_structure-bindings :hydra_typing_function_structure-body :hydra_typing_function_structure-domains :hydra_typing_function_structure-codomain :hydra_typing_function_structure-environment :make-hydra_typing_inference_result :hydra_typing_inference_result? :hydra_typing_inference_result-term :hydra_typing_inference_result-type :hydra_typing_inference_result-subst :hydra_typing_inference_result-class_constraints :hydra_typing_inference_result-context :make-hydra_typing_term_subst :hydra_typing_term_subst? :hydra_typing_term_subst-value :make-hydra_typing_type_constraint :hydra_typing_type_constraint? :hydra_typing_type_constraint-left :hydra_typing_type_constraint-right :hydra_typing_type_constraint-comment :make-hydra_typing_type_subst :hydra_typing_type_subst? :hydra_typing_type_subst-value))

(in-package :hydra.typing)

(cl:defstruct hydra_typing_function_structure type_params params bindings body domains codomain environment)

(cl:defstruct hydra_typing_inference_result term type subst class_constraints context)

(cl:defstruct hydra_typing_term_subst value)

(cl:defstruct hydra_typing_type_constraint left right comment)

(cl:defstruct hydra_typing_type_subst value)

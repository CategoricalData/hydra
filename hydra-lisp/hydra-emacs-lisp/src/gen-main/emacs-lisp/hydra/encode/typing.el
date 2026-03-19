(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.context)

(require 'hydra.encode.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.typing)

(defvar hydra_encode_typing_function_structure (lambda (env) (lambda (x) (list :record (make-hydra_core_record "hydra.typing.FunctionStructure" (list (make-hydra_core_field "typeParams" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_core_name) xs))) (funcall (lambda (v) (hydra_typing_function_structure-type_params v)) x))) (make-hydra_core_field "params" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_core_name) xs))) (funcall (lambda (v) (hydra_typing_function_structure-params v)) x))) (make-hydra_core_field "bindings" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_core_binding) xs))) (funcall (lambda (v) (hydra_typing_function_structure-bindings v)) x))) (make-hydra_core_field "body" (hydra_encode_core_term (funcall (lambda (v) (hydra_typing_function_structure-body v)) x))) (make-hydra_core_field "domains" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_core_type) xs))) (funcall (lambda (v) (hydra_typing_function_structure-domains v)) x))) (make-hydra_core_field "codomain" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map hydra_encode_core_type) opt))) (funcall (lambda (v) (hydra_typing_function_structure-codomain v)) x))) (make-hydra_core_field "environment" (env (funcall (lambda (v) (hydra_typing_function_structure-environment v)) x)))))))))

(defvar hydra_encode_typing_type_subst (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.typing.TypeSubst" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_type) m))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_typing_inference_result (lambda (x) (list :record (make-hydra_core_record "hydra.typing.InferenceResult" (list (make-hydra_core_field "term" (hydra_encode_core_term (funcall (lambda (v) (hydra_typing_inference_result-term v)) x))) (make-hydra_core_field "type" (hydra_encode_core_type (funcall (lambda (v) (hydra_typing_inference_result-type v)) x))) (make-hydra_core_field "subst" (hydra_encode_typing_type_subst (funcall (lambda (v) (hydra_typing_inference_result-subst v)) x))) (make-hydra_core_field "classConstraints" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_type_variable_metadata) m))) (funcall (lambda (v) (hydra_typing_inference_result-class_constraints v)) x))) (make-hydra_core_field "context" (hydra_encode_context_context (funcall (lambda (v) (hydra_typing_inference_result-context v)) x))))))))

(defvar hydra_encode_typing_term_subst (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.typing.TermSubst" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_term) m))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_typing_type_constraint (lambda (x) (list :record (make-hydra_core_record "hydra.typing.TypeConstraint" (list (make-hydra_core_field "left" (hydra_encode_core_type (funcall (lambda (v) (hydra_typing_type_constraint-left v)) x))) (make-hydra_core_field "right" (hydra_encode_core_type (funcall (lambda (v) (hydra_typing_type_constraint-right v)) x))) (make-hydra_core_field "comment" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) (hydra_typing_type_constraint-comment v)) x))))))))

(provide 'hydra.encode.typing)

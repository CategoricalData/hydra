(ns hydra.encode.typing
  (:require [hydra.core :refer :all] [hydra.encode.context :refer :all] [hydra.encode.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.typing :refer :all]
))

(declare hydra_encode_typing_function_structure hydra_encode_typing_type_subst hydra_encode_typing_inference_result hydra_encode_typing_term_subst hydra_encode_typing_type_constraint)

(def hydra_encode_typing_function_structure (fn [env] (fn [x] (list :record (->hydra_core_record "hydra.typing.FunctionStructure" (list (->hydra_core_field "typeParams" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_core_name) xs))) ((fn [v] (:type_params v)) x))) (->hydra_core_field "params" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_core_name) xs))) ((fn [v] (:params v)) x))) (->hydra_core_field "bindings" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_core_binding) xs))) ((fn [v] (:bindings v)) x))) (->hydra_core_field "body" (hydra_encode_core_term ((fn [v] (:body v)) x))) (->hydra_core_field "domains" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_core_type) xs))) ((fn [v] (:domains v)) x))) (->hydra_core_field "codomain" ((fn [opt] (list :maybe ((hydra_lib_maybes_map hydra_encode_core_type) opt))) ((fn [v] (:codomain v)) x))) (->hydra_core_field "environment" (env ((fn [v] (:environment v)) x)))))))))

(def hydra_encode_typing_type_subst (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.typing.TypeSubst" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_type) m))) ((fn [v] v) x))))))

(def hydra_encode_typing_inference_result (fn [x] (list :record (->hydra_core_record "hydra.typing.InferenceResult" (list (->hydra_core_field "term" (hydra_encode_core_term ((fn [v] (:term v)) x))) (->hydra_core_field "type" (hydra_encode_core_type ((fn [v] (:type v)) x))) (->hydra_core_field "subst" (hydra_encode_typing_type_subst ((fn [v] (:subst v)) x))) (->hydra_core_field "classConstraints" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_type_variable_metadata) m))) ((fn [v] (:class_constraints v)) x))) (->hydra_core_field "context" (hydra_encode_context_context ((fn [v] (:context v)) x))))))))

(def hydra_encode_typing_term_subst (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.typing.TermSubst" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_term) m))) ((fn [v] v) x))))))

(def hydra_encode_typing_type_constraint (fn [x] (list :record (->hydra_core_record "hydra.typing.TypeConstraint" (list (->hydra_core_field "left" (hydra_encode_core_type ((fn [v] (:left v)) x))) (->hydra_core_field "right" (hydra_encode_core_type ((fn [v] (:right v)) x))) (->hydra_core_field "comment" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:comment v)) x))))))))

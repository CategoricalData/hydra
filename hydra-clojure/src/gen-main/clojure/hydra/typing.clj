(ns hydra.typing
  (:require [hydra.context :refer :all] [hydra.core :refer :all]
))

(defrecord hydra_typing_function_structure [type_params params bindings body domains codomain environment])
(defn make-hydra_typing_function_structure [type_params params bindings body domains codomain environment] (->hydra_typing_function_structure type_params params bindings body domains codomain environment))

(defrecord hydra_typing_inference_result [term type subst class_constraints context])
(defn make-hydra_typing_inference_result [term type subst class_constraints context] (->hydra_typing_inference_result term type subst class_constraints context))

(defrecord hydra_typing_term_subst [value])
(defn make-hydra_typing_term_subst [value] (->hydra_typing_term_subst value))

(defrecord hydra_typing_type_constraint [left right comment])
(defn make-hydra_typing_type_constraint [left right comment] (->hydra_typing_type_constraint left right comment))

(defrecord hydra_typing_type_subst [value])
(defn make-hydra_typing_type_subst [value] (->hydra_typing_type_subst value))

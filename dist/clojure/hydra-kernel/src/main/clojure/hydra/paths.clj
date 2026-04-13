(ns hydra.paths
  (:require [hydra.core :refer :all]
))

(declare hydra_paths_subterm_step-variants hydra_paths_subtype_step-variants)

(defrecord hydra_paths_subterm_edge [source path target])
(defn make-hydra_paths_subterm_edge [source path target] (->hydra_paths_subterm_edge source path target))

(defrecord hydra_paths_subterm_graph [nodes edges])
(defn make-hydra_paths_subterm_graph [nodes edges] (->hydra_paths_subterm_graph nodes edges))

(defrecord hydra_paths_subterm_node [name label id])
(defn make-hydra_paths_subterm_node [name label id] (->hydra_paths_subterm_node name label id))

(defrecord hydra_paths_subterm_path [value])
(defn make-hydra_paths_subterm_path [value] (->hydra_paths_subterm_path value))

(def hydra_paths_subterm_step-variants (list :annotated_body :application_function :application_argument :lambda_body :union_cases_default :union_cases_branch :let_body :let_binding :list_element :map_key :map_value :maybe_term :product_term :record_field :set_element :sum_term :type_lambda_body :type_application_term :injection_term :wrapped_term))

(defrecord hydra_paths_subtype_edge [source path target])
(defn make-hydra_paths_subtype_edge [source path target] (->hydra_paths_subtype_edge source path target))

(defrecord hydra_paths_subtype_graph [nodes edges])
(defn make-hydra_paths_subtype_graph [nodes edges] (->hydra_paths_subtype_graph nodes edges))

(defrecord hydra_paths_subtype_node [name label id])
(defn make-hydra_paths_subtype_node [name label id] (->hydra_paths_subtype_node name label id))

(defrecord hydra_paths_subtype_path [value])
(defn make-hydra_paths_subtype_path [value] (->hydra_paths_subtype_path value))

(def hydra_paths_subtype_step-variants (list :annotated_body :application_function :application_argument :either_left :either_right :forall_body :function_domain :function_codomain :list_element :map_keys :map_values :maybe_element :pair_first :pair_second :record_field :set_element :union_field :wrapped_type))

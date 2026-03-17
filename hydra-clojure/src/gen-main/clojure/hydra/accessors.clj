(ns hydra.accessors
  (:require [hydra.core :refer :all]
))

(declare hydra_accessors_term_accessor-variants)

(defrecord hydra_accessors_accessor_edge [source path target])
(defn make-hydra_accessors_accessor_edge [source path target] (->hydra_accessors_accessor_edge source path target))

(defrecord hydra_accessors_accessor_graph [nodes edges])
(defn make-hydra_accessors_accessor_graph [nodes edges] (->hydra_accessors_accessor_graph nodes edges))

(defrecord hydra_accessors_accessor_node [name label id])
(defn make-hydra_accessors_accessor_node [name label id] (->hydra_accessors_accessor_node name label id))

(defrecord hydra_accessors_accessor_path [value])
(defn make-hydra_accessors_accessor_path [value] (->hydra_accessors_accessor_path value))

(def hydra_accessors_term_accessor-variants (list :annotated_body :application_function :application_argument :lambda_body :union_cases_default :union_cases_branch :let_body :let_binding :list_element :map_key :map_value :maybe_term :product_term :record_field :set_element :sum_term :type_lambda_body :type_application_term :injection_term :wrapped_term))

(ns hydra.graph
  (:require [hydra.context :refer :all] [hydra.core :refer :all] [hydra.errors :refer :all]
))

(defrecord hydra_graph_graph [bound_terms bound_types class_constraints lambda_variables metadata primitives schema_types type_variables])
(defn make-hydra_graph_graph [bound_terms bound_types class_constraints lambda_variables metadata primitives schema_types type_variables] (->hydra_graph_graph bound_terms bound_types class_constraints lambda_variables metadata primitives schema_types type_variables))

(defrecord hydra_graph_primitive [name type implementation])
(defn make-hydra_graph_primitive [name type implementation] (->hydra_graph_primitive name type implementation))

(defrecord hydra_graph_term_coder [type encode decode])
(defn make-hydra_graph_term_coder [type encode decode] (->hydra_graph_term_coder type encode decode))

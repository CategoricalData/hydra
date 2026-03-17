(ns hydra.query
  (:require [hydra.core :refer :all]
))

(declare hydra_query_comparison_constraint-variants hydra_query_node-variants hydra_query_path-variants hydra_query_pattern-variants hydra_query_regex_quantifier-variants hydra_query_step-variants)

(def hydra_query_comparison_constraint-variants (list :equal :not_equal :less_than :greater_than :less_than_or_equal :greater_than_or_equal))

(defrecord hydra_query_edge [type out in])
(defn make-hydra_query_edge [type out in] (->hydra_query_edge type out in))

(defrecord hydra_query_graph_pattern [graph patterns])
(defn make-hydra_query_graph_pattern [graph patterns] (->hydra_query_graph_pattern graph patterns))

(def hydra_query_node-variants (list :term :variable :wildcard))

(def hydra_query_path-variants (list :step :regex :inverse))

(defrecord hydra_query_path_equation [left right])
(defn make-hydra_query_path_equation [left right] (->hydra_query_path_equation left right))

(def hydra_query_pattern-variants (list :triple :negation :conjunction :disjunction :graph))

(defrecord hydra_query_pattern_implication [antecedent consequent])
(defn make-hydra_query_pattern_implication [antecedent consequent] (->hydra_query_pattern_implication antecedent consequent))

(defrecord hydra_query_query [variables patterns])
(defn make-hydra_query_query [variables patterns] (->hydra_query_query variables patterns))

(defrecord hydra_query_range [min max])
(defn make-hydra_query_range [min max] (->hydra_query_range min max))

(def hydra_query_regex_quantifier-variants (list :one :zero_or_one :zero_or_more :one_or_more :exactly :at_least :range))

(defrecord hydra_query_regex_sequence [path quantifier])
(defn make-hydra_query_regex_sequence [path quantifier] (->hydra_query_regex_sequence path quantifier))

(def hydra_query_step-variants (list :edge :project :compare))

(defrecord hydra_query_triple_pattern [subject predicate object])
(defn make-hydra_query_triple_pattern [subject predicate object] (->hydra_query_triple_pattern subject predicate object))

(defrecord hydra_query_variable [value])
(defn make-hydra_query_variable [value] (->hydra_query_variable value))

(defpackage :hydra.query
(:use :cl :hydra.core)
(:export :hydra_query_comparison_constraint-variants :make-hydra_query_edge :hydra_query_edge? :hydra_query_edge-type :hydra_query_edge-out :hydra_query_edge-in :make-hydra_query_graph_pattern :hydra_query_graph_pattern? :hydra_query_graph_pattern-graph :hydra_query_graph_pattern-patterns :hydra_query_node-variants :hydra_query_path-variants :make-hydra_query_path_equation :hydra_query_path_equation? :hydra_query_path_equation-left :hydra_query_path_equation-right :hydra_query_pattern-variants :make-hydra_query_pattern_implication :hydra_query_pattern_implication? :hydra_query_pattern_implication-antecedent :hydra_query_pattern_implication-consequent :make-hydra_query_query :hydra_query_query? :hydra_query_query-variables :hydra_query_query-patterns :make-hydra_query_range :hydra_query_range? :hydra_query_range-min :hydra_query_range-max :hydra_query_regex_quantifier-variants :make-hydra_query_regex_sequence :hydra_query_regex_sequence? :hydra_query_regex_sequence-path :hydra_query_regex_sequence-quantifier :hydra_query_step-variants :make-hydra_query_triple_pattern :hydra_query_triple_pattern? :hydra_query_triple_pattern-subject :hydra_query_triple_pattern-predicate :hydra_query_triple_pattern-object :make-hydra_query_variable :hydra_query_variable? :hydra_query_variable-value))

(in-package :hydra.query)

(cl:defvar hydra_query_comparison_constraint-variants (cl:list :equal :not_equal :less_than :greater_than :less_than_or_equal :greater_than_or_equal))

(cl:defstruct hydra_query_edge type out in)

(cl:defstruct hydra_query_graph_pattern graph patterns)

(cl:defvar hydra_query_node-variants (cl:list :term :variable :wildcard))

(cl:defvar hydra_query_path-variants (cl:list :step :regex :inverse))

(cl:defstruct hydra_query_path_equation left right)

(cl:defvar hydra_query_pattern-variants (cl:list :triple :negation :conjunction :disjunction :graph))

(cl:defstruct hydra_query_pattern_implication antecedent consequent)

(cl:defstruct hydra_query_query variables patterns)

(cl:defstruct hydra_query_range min max)

(cl:defvar hydra_query_regex_quantifier-variants (cl:list :one :zero_or_one :zero_or_more :one_or_more :exactly :at_least :range))

(cl:defstruct hydra_query_regex_sequence path quantifier)

(cl:defvar hydra_query_step-variants (cl:list :edge :project :compare))

(cl:defstruct hydra_query_triple_pattern subject predicate object)

(cl:defstruct hydra_query_variable value)

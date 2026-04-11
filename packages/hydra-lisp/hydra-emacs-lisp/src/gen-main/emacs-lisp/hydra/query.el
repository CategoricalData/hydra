(require 'cl-lib)

(require 'hydra.core)

(defvar hydra_query_comparison_constraint-variants (list :equal :not_equal :less_than :greater_than :less_than_or_equal :greater_than_or_equal))

(cl-defstruct hydra_query_edge type out in)

(cl-defstruct hydra_query_graph_pattern graph patterns)

(defvar hydra_query_node-variants (list :term :variable :wildcard))

(defvar hydra_query_path-variants (list :step :regex :inverse))

(cl-defstruct hydra_query_path_equation left right)

(defvar hydra_query_pattern-variants (list :triple :negation :conjunction :disjunction :graph))

(cl-defstruct hydra_query_pattern_implication antecedent consequent)

(cl-defstruct hydra_query_query variables patterns)

(cl-defstruct hydra_query_range min max)

(defvar hydra_query_regex_quantifier-variants (list :one :zero_or_one :zero_or_more :one_or_more :exactly :at_least :range))

(cl-defstruct hydra_query_regex_sequence path quantifier)

(defvar hydra_query_step-variants (list :edge :project :compare))

(cl-defstruct hydra_query_triple_pattern subject predicate object)

(cl-defstruct hydra_query_variable value)

(provide 'hydra.query)

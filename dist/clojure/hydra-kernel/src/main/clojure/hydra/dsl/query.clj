(ns hydra.dsl.query
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_query_comparison_constraint_equal hydra_dsl_query_comparison_constraint_greater_than hydra_dsl_query_comparison_constraint_greater_than_or_equal hydra_dsl_query_comparison_constraint_less_than hydra_dsl_query_comparison_constraint_less_than_or_equal hydra_dsl_query_comparison_constraint_not_equal hydra_dsl_query_edge hydra_dsl_query_edge_in hydra_dsl_query_edge_out hydra_dsl_query_edge_type hydra_dsl_query_edge_with_in hydra_dsl_query_edge_with_out hydra_dsl_query_edge_with_type hydra_dsl_query_graph_pattern hydra_dsl_query_graph_pattern_graph hydra_dsl_query_graph_pattern_patterns hydra_dsl_query_graph_pattern_with_graph hydra_dsl_query_graph_pattern_with_patterns hydra_dsl_query_node_term hydra_dsl_query_node_variable hydra_dsl_query_node_wildcard hydra_dsl_query_path_equation hydra_dsl_query_path_equation_left hydra_dsl_query_path_equation_right hydra_dsl_query_path_equation_with_left hydra_dsl_query_path_equation_with_right hydra_dsl_query_path_inverse hydra_dsl_query_path_regex hydra_dsl_query_path_step hydra_dsl_query_pattern_conjunction hydra_dsl_query_pattern_disjunction hydra_dsl_query_pattern_graph hydra_dsl_query_pattern_implication hydra_dsl_query_pattern_implication_antecedent hydra_dsl_query_pattern_implication_consequent hydra_dsl_query_pattern_implication_with_antecedent hydra_dsl_query_pattern_implication_with_consequent hydra_dsl_query_pattern_negation hydra_dsl_query_pattern_triple hydra_dsl_query_query hydra_dsl_query_query_patterns hydra_dsl_query_query_variables hydra_dsl_query_query_with_patterns hydra_dsl_query_query_with_variables hydra_dsl_query_range hydra_dsl_query_range_max hydra_dsl_query_range_min hydra_dsl_query_range_with_max hydra_dsl_query_range_with_min hydra_dsl_query_regex_quantifier_at_least hydra_dsl_query_regex_quantifier_exactly hydra_dsl_query_regex_quantifier_one hydra_dsl_query_regex_quantifier_one_or_more hydra_dsl_query_regex_quantifier_range hydra_dsl_query_regex_quantifier_zero_or_more hydra_dsl_query_regex_quantifier_zero_or_one hydra_dsl_query_regex_sequence hydra_dsl_query_regex_sequence_path hydra_dsl_query_regex_sequence_quantifier hydra_dsl_query_regex_sequence_with_path hydra_dsl_query_regex_sequence_with_quantifier hydra_dsl_query_step_compare hydra_dsl_query_step_edge hydra_dsl_query_step_project hydra_dsl_query_triple_pattern hydra_dsl_query_triple_pattern_object hydra_dsl_query_triple_pattern_predicate hydra_dsl_query_triple_pattern_subject hydra_dsl_query_triple_pattern_with_object hydra_dsl_query_triple_pattern_with_predicate hydra_dsl_query_triple_pattern_with_subject hydra_dsl_query_un_variable hydra_dsl_query_variable)

(def hydra_dsl_query_comparison_constraint_equal (list :inject (->hydra_core_injection "hydra.query.ComparisonConstraint" (->hydra_core_field "equal" (list :unit nil)))))

(def hydra_dsl_query_comparison_constraint_greater_than (list :inject (->hydra_core_injection "hydra.query.ComparisonConstraint" (->hydra_core_field "greaterThan" (list :unit nil)))))

(def hydra_dsl_query_comparison_constraint_greater_than_or_equal (list :inject (->hydra_core_injection "hydra.query.ComparisonConstraint" (->hydra_core_field "greaterThanOrEqual" (list :unit nil)))))

(def hydra_dsl_query_comparison_constraint_less_than (list :inject (->hydra_core_injection "hydra.query.ComparisonConstraint" (->hydra_core_field "lessThan" (list :unit nil)))))

(def hydra_dsl_query_comparison_constraint_less_than_or_equal (list :inject (->hydra_core_injection "hydra.query.ComparisonConstraint" (->hydra_core_field "lessThanOrEqual" (list :unit nil)))))

(def hydra_dsl_query_comparison_constraint_not_equal (list :inject (->hydra_core_injection "hydra.query.ComparisonConstraint" (->hydra_core_field "notEqual" (list :unit nil)))))

(def hydra_dsl_query_edge (fn [type] (fn [out] (fn [in] (list :record (->hydra_core_record "hydra.query.Edge" (list (->hydra_core_field "type" ((fn [v] v) type)) (->hydra_core_field "out" ((fn [v] v) out)) (->hydra_core_field "in" ((fn [v] v) in)))))))))

(def hydra_dsl_query_edge_in (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "in")) ((fn [v] v) x)))))

(def hydra_dsl_query_edge_out (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "out")) ((fn [v] v) x)))))

(def hydra_dsl_query_edge_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "type")) ((fn [v] v) x)))))

(def hydra_dsl_query_edge_with_in (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.Edge" (list (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "type")) ((fn [v] v) original)))) (->hydra_core_field "out" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "out")) ((fn [v] v) original)))) (->hydra_core_field "in" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_edge_with_out (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.Edge" (list (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "type")) ((fn [v] v) original)))) (->hydra_core_field "out" ((fn [v] v) new_val)) (->hydra_core_field "in" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "in")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_edge_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.Edge" (list (->hydra_core_field "type" ((fn [v] v) new_val)) (->hydra_core_field "out" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "out")) ((fn [v] v) original)))) (->hydra_core_field "in" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Edge" "in")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_graph_pattern (fn [graph] (fn [patterns] (list :record (->hydra_core_record "hydra.query.GraphPattern" (list (->hydra_core_field "graph" ((fn [v] v) graph)) (->hydra_core_field "patterns" ((fn [v] v) patterns))))))))

(def hydra_dsl_query_graph_pattern_graph (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.GraphPattern" "graph")) ((fn [v] v) x)))))

(def hydra_dsl_query_graph_pattern_patterns (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.GraphPattern" "patterns")) ((fn [v] v) x)))))

(def hydra_dsl_query_graph_pattern_with_graph (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.GraphPattern" (list (->hydra_core_field "graph" ((fn [v] v) new_val)) (->hydra_core_field "patterns" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.GraphPattern" "patterns")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_graph_pattern_with_patterns (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.GraphPattern" (list (->hydra_core_field "graph" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.GraphPattern" "graph")) ((fn [v] v) original)))) (->hydra_core_field "patterns" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_node_term (fn [x] (list :inject (->hydra_core_injection "hydra.query.Node" (->hydra_core_field "term" ((fn [v] v) x))))))

(def hydra_dsl_query_node_variable (fn [x] (list :inject (->hydra_core_injection "hydra.query.Node" (->hydra_core_field "variable" ((fn [v] v) x))))))

(def hydra_dsl_query_node_wildcard (list :inject (->hydra_core_injection "hydra.query.Node" (->hydra_core_field "wildcard" (list :unit nil)))))

(def hydra_dsl_query_path_equation (fn [left] (fn [right] (list :record (->hydra_core_record "hydra.query.PathEquation" (list (->hydra_core_field "left" ((fn [v] v) left)) (->hydra_core_field "right" ((fn [v] v) right))))))))

(def hydra_dsl_query_path_equation_left (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PathEquation" "left")) ((fn [v] v) x)))))

(def hydra_dsl_query_path_equation_right (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PathEquation" "right")) ((fn [v] v) x)))))

(def hydra_dsl_query_path_equation_with_left (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.PathEquation" (list (->hydra_core_field "left" ((fn [v] v) new_val)) (->hydra_core_field "right" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PathEquation" "right")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_path_equation_with_right (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.PathEquation" (list (->hydra_core_field "left" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PathEquation" "left")) ((fn [v] v) original)))) (->hydra_core_field "right" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_path_inverse (fn [x] (list :inject (->hydra_core_injection "hydra.query.Path" (->hydra_core_field "inverse" ((fn [v] v) x))))))

(def hydra_dsl_query_path_regex (fn [x] (list :inject (->hydra_core_injection "hydra.query.Path" (->hydra_core_field "regex" ((fn [v] v) x))))))

(def hydra_dsl_query_path_step (fn [x] (list :inject (->hydra_core_injection "hydra.query.Path" (->hydra_core_field "step" ((fn [v] v) x))))))

(def hydra_dsl_query_pattern_conjunction (fn [x] (list :inject (->hydra_core_injection "hydra.query.Pattern" (->hydra_core_field "conjunction" ((fn [v] v) x))))))

(def hydra_dsl_query_pattern_disjunction (fn [x] (list :inject (->hydra_core_injection "hydra.query.Pattern" (->hydra_core_field "disjunction" ((fn [v] v) x))))))

(def hydra_dsl_query_pattern_graph (fn [x] (list :inject (->hydra_core_injection "hydra.query.Pattern" (->hydra_core_field "graph" ((fn [v] v) x))))))

(def hydra_dsl_query_pattern_implication (fn [antecedent] (fn [consequent] (list :record (->hydra_core_record "hydra.query.PatternImplication" (list (->hydra_core_field "antecedent" ((fn [v] v) antecedent)) (->hydra_core_field "consequent" ((fn [v] v) consequent))))))))

(def hydra_dsl_query_pattern_implication_antecedent (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PatternImplication" "antecedent")) ((fn [v] v) x)))))

(def hydra_dsl_query_pattern_implication_consequent (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PatternImplication" "consequent")) ((fn [v] v) x)))))

(def hydra_dsl_query_pattern_implication_with_antecedent (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.PatternImplication" (list (->hydra_core_field "antecedent" ((fn [v] v) new_val)) (->hydra_core_field "consequent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PatternImplication" "consequent")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_pattern_implication_with_consequent (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.PatternImplication" (list (->hydra_core_field "antecedent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.PatternImplication" "antecedent")) ((fn [v] v) original)))) (->hydra_core_field "consequent" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_pattern_negation (fn [x] (list :inject (->hydra_core_injection "hydra.query.Pattern" (->hydra_core_field "negation" ((fn [v] v) x))))))

(def hydra_dsl_query_pattern_triple (fn [x] (list :inject (->hydra_core_injection "hydra.query.Pattern" (->hydra_core_field "triple" ((fn [v] v) x))))))

(def hydra_dsl_query_query (fn [variables] (fn [patterns] (list :record (->hydra_core_record "hydra.query.Query" (list (->hydra_core_field "variables" ((fn [v] v) variables)) (->hydra_core_field "patterns" ((fn [v] v) patterns))))))))

(def hydra_dsl_query_query_patterns (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Query" "patterns")) ((fn [v] v) x)))))

(def hydra_dsl_query_query_variables (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Query" "variables")) ((fn [v] v) x)))))

(def hydra_dsl_query_query_with_patterns (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.Query" (list (->hydra_core_field "variables" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Query" "variables")) ((fn [v] v) original)))) (->hydra_core_field "patterns" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_query_with_variables (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.Query" (list (->hydra_core_field "variables" ((fn [v] v) new_val)) (->hydra_core_field "patterns" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Query" "patterns")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_range (fn [min] (fn [max] (list :record (->hydra_core_record "hydra.query.Range" (list (->hydra_core_field "min" ((fn [v] v) min)) (->hydra_core_field "max" ((fn [v] v) max))))))))

(def hydra_dsl_query_range_max (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Range" "max")) ((fn [v] v) x)))))

(def hydra_dsl_query_range_min (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Range" "min")) ((fn [v] v) x)))))

(def hydra_dsl_query_range_with_max (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.Range" (list (->hydra_core_field "min" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Range" "min")) ((fn [v] v) original)))) (->hydra_core_field "max" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_range_with_min (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.Range" (list (->hydra_core_field "min" ((fn [v] v) new_val)) (->hydra_core_field "max" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.Range" "max")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_regex_quantifier_at_least (fn [x] (list :inject (->hydra_core_injection "hydra.query.RegexQuantifier" (->hydra_core_field "atLeast" ((fn [v] v) x))))))

(def hydra_dsl_query_regex_quantifier_exactly (fn [x] (list :inject (->hydra_core_injection "hydra.query.RegexQuantifier" (->hydra_core_field "exactly" ((fn [v] v) x))))))

(def hydra_dsl_query_regex_quantifier_one (list :inject (->hydra_core_injection "hydra.query.RegexQuantifier" (->hydra_core_field "one" (list :unit nil)))))

(def hydra_dsl_query_regex_quantifier_one_or_more (list :inject (->hydra_core_injection "hydra.query.RegexQuantifier" (->hydra_core_field "oneOrMore" (list :unit nil)))))

(def hydra_dsl_query_regex_quantifier_range (fn [x] (list :inject (->hydra_core_injection "hydra.query.RegexQuantifier" (->hydra_core_field "range" ((fn [v] v) x))))))

(def hydra_dsl_query_regex_quantifier_zero_or_more (list :inject (->hydra_core_injection "hydra.query.RegexQuantifier" (->hydra_core_field "zeroOrMore" (list :unit nil)))))

(def hydra_dsl_query_regex_quantifier_zero_or_one (list :inject (->hydra_core_injection "hydra.query.RegexQuantifier" (->hydra_core_field "zeroOrOne" (list :unit nil)))))

(def hydra_dsl_query_regex_sequence (fn [path] (fn [quantifier] (list :record (->hydra_core_record "hydra.query.RegexSequence" (list (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "quantifier" ((fn [v] v) quantifier))))))))

(def hydra_dsl_query_regex_sequence_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.RegexSequence" "path")) ((fn [v] v) x)))))

(def hydra_dsl_query_regex_sequence_quantifier (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.RegexSequence" "quantifier")) ((fn [v] v) x)))))

(def hydra_dsl_query_regex_sequence_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.RegexSequence" (list (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "quantifier" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.RegexSequence" "quantifier")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_regex_sequence_with_quantifier (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.RegexSequence" (list (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.RegexSequence" "path")) ((fn [v] v) original)))) (->hydra_core_field "quantifier" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_step_compare (fn [x] (list :inject (->hydra_core_injection "hydra.query.Step" (->hydra_core_field "compare" ((fn [v] v) x))))))

(def hydra_dsl_query_step_edge (fn [x] (list :inject (->hydra_core_injection "hydra.query.Step" (->hydra_core_field "edge" ((fn [v] v) x))))))

(def hydra_dsl_query_step_project (fn [x] (list :inject (->hydra_core_injection "hydra.query.Step" (->hydra_core_field "project" ((fn [v] v) x))))))

(def hydra_dsl_query_triple_pattern (fn [subject] (fn [predicate] (fn [object] (list :record (->hydra_core_record "hydra.query.TriplePattern" (list (->hydra_core_field "subject" ((fn [v] v) subject)) (->hydra_core_field "predicate" ((fn [v] v) predicate)) (->hydra_core_field "object" ((fn [v] v) object)))))))))

(def hydra_dsl_query_triple_pattern_object (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "object")) ((fn [v] v) x)))))

(def hydra_dsl_query_triple_pattern_predicate (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "predicate")) ((fn [v] v) x)))))

(def hydra_dsl_query_triple_pattern_subject (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "subject")) ((fn [v] v) x)))))

(def hydra_dsl_query_triple_pattern_with_object (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.TriplePattern" (list (->hydra_core_field "subject" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "subject")) ((fn [v] v) original)))) (->hydra_core_field "predicate" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "predicate")) ((fn [v] v) original)))) (->hydra_core_field "object" ((fn [v] v) new_val))))))))

(def hydra_dsl_query_triple_pattern_with_predicate (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.TriplePattern" (list (->hydra_core_field "subject" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "subject")) ((fn [v] v) original)))) (->hydra_core_field "predicate" ((fn [v] v) new_val)) (->hydra_core_field "object" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "object")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_triple_pattern_with_subject (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.query.TriplePattern" (list (->hydra_core_field "subject" ((fn [v] v) new_val)) (->hydra_core_field "predicate" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "predicate")) ((fn [v] v) original)))) (->hydra_core_field "object" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.query.TriplePattern" "object")) ((fn [v] v) original))))))))))

(def hydra_dsl_query_un_variable (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.query.Variable") ((fn [v] v) x)))))

(def hydra_dsl_query_variable (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.query.Variable" ((fn [v] v) x)))))

(ns hydra.encode.grammar
  (:require [hydra.core :refer :all] [hydra.grammar :refer :all] [hydra.lib.lists :refer :all]
))

(declare hydra_encode_grammar_constant hydra_encode_grammar_label hydra_encode_grammar_regex hydra_encode_grammar_symbol hydra_encode_grammar_labeled_pattern hydra_encode_grammar_pattern hydra_encode_grammar_production hydra_encode_grammar_grammar)

(def hydra_encode_grammar_constant (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.grammar.Constant" ((fn [x] (list :literal (list :string x))) ((fn [v] v) x))))))

(def hydra_encode_grammar_label (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.grammar.Label" ((fn [x] (list :literal (list :string x))) ((fn [v] v) x))))))

(def hydra_encode_grammar_regex (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.grammar.Regex" ((fn [x] (list :literal (list :string x))) ((fn [v] v) x))))))

(def hydra_encode_grammar_symbol (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.grammar.Symbol" ((fn [x] (list :literal (list :string x))) ((fn [v] v) x))))))

(def hydra_encode_grammar_labeled_pattern (fn [x] (list :record (->hydra_core_record "hydra.grammar.LabeledPattern" (list (->hydra_core_field "label" (hydra_encode_grammar_label ((fn [v] (:label v)) x))) (->hydra_core_field "pattern" (hydra_encode_grammar_pattern ((fn [v] (:pattern v)) x))))))))

(def hydra_encode_grammar_pattern (fn [match_target] ((fn [match_value] (cond (= (first match_target) :alternatives) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "alternatives" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_grammar_pattern) xs))) y))))) match_value) (= (first match_target) :constant) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "constant" (hydra_encode_grammar_constant y))))) match_value) (= (first match_target) :ignored) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "ignored" (hydra_encode_grammar_pattern y))))) match_value) (= (first match_target) :labeled) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "labeled" (hydra_encode_grammar_labeled_pattern y))))) match_value) (= (first match_target) :nil) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "nil" ((fn [_] (list :unit nil)) y))))) match_value) (= (first match_target) :nonterminal) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "nonterminal" (hydra_encode_grammar_symbol y))))) match_value) (= (first match_target) :option) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "option" (hydra_encode_grammar_pattern y))))) match_value) (= (first match_target) :plus) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "plus" (hydra_encode_grammar_pattern y))))) match_value) (= (first match_target) :regex) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "regex" (hydra_encode_grammar_regex y))))) match_value) (= (first match_target) :sequence) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "sequence" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_grammar_pattern) xs))) y))))) match_value) (= (first match_target) :star) ((fn [y] (list :union (->hydra_core_injection "hydra.grammar.Pattern" (->hydra_core_field "star" (hydra_encode_grammar_pattern y))))) match_value))) (second match_target))))

(def hydra_encode_grammar_production (fn [x] (list :record (->hydra_core_record "hydra.grammar.Production" (list (->hydra_core_field "symbol" (hydra_encode_grammar_symbol ((fn [v] (:symbol v)) x))) (->hydra_core_field "pattern" (hydra_encode_grammar_pattern ((fn [v] (:pattern v)) x))))))))

(def hydra_encode_grammar_grammar (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.grammar.Grammar" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_grammar_production) xs))) ((fn [v] v) x))))))

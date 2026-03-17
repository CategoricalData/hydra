(ns hydra.grammar)

(declare hydra_grammar_pattern-variants)

(defrecord hydra_grammar_constant [value])
(defn make-hydra_grammar_constant [value] (->hydra_grammar_constant value))

(defrecord hydra_grammar_grammar [value])
(defn make-hydra_grammar_grammar [value] (->hydra_grammar_grammar value))

(defrecord hydra_grammar_label [value])
(defn make-hydra_grammar_label [value] (->hydra_grammar_label value))

(defrecord hydra_grammar_labeled_pattern [label pattern])
(defn make-hydra_grammar_labeled_pattern [label pattern] (->hydra_grammar_labeled_pattern label pattern))

(def hydra_grammar_pattern-variants (list :alternatives :constant :ignored :labeled :nil :nonterminal :option :plus :regex :sequence :star))

(defrecord hydra_grammar_production [symbol pattern])
(defn make-hydra_grammar_production [symbol pattern] (->hydra_grammar_production symbol pattern))

(defrecord hydra_grammar_regex [value])
(defn make-hydra_grammar_regex [value] (->hydra_grammar_regex value))

(defrecord hydra_grammar_symbol [value])
(defn make-hydra_grammar_symbol [value] (->hydra_grammar_symbol value))

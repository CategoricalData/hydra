(defpackage :hydra.grammar
(:use :cl)
(:export :make-hydra_grammar_constant :hydra_grammar_constant? :hydra_grammar_constant-value :make-hydra_grammar_grammar :hydra_grammar_grammar? :hydra_grammar_grammar-value :make-hydra_grammar_label :hydra_grammar_label? :hydra_grammar_label-value :make-hydra_grammar_labeled_pattern :hydra_grammar_labeled_pattern? :hydra_grammar_labeled_pattern-label :hydra_grammar_labeled_pattern-pattern :hydra_grammar_pattern-variants :make-hydra_grammar_production :hydra_grammar_production? :hydra_grammar_production-symbol :hydra_grammar_production-pattern :make-hydra_grammar_regex :hydra_grammar_regex? :hydra_grammar_regex-value :make-hydra_grammar_symbol :hydra_grammar_symbol? :hydra_grammar_symbol-value))

(in-package :hydra.grammar)

(cl:defstruct hydra_grammar_constant value)

(cl:defstruct hydra_grammar_grammar value)

(cl:defstruct hydra_grammar_label value)

(cl:defstruct hydra_grammar_labeled_pattern label pattern)

(cl:defvar hydra_grammar_pattern-variants (cl:list :alternatives :constant :ignored :labeled :nil :nonterminal :option :plus :regex :sequence :star))

(cl:defstruct hydra_grammar_production symbol pattern)

(cl:defstruct hydra_grammar_regex value)

(cl:defstruct hydra_grammar_symbol value)

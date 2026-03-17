(require 'cl-lib)

(cl-defstruct hydra_grammar_constant value)

(cl-defstruct hydra_grammar_grammar value)

(cl-defstruct hydra_grammar_label value)

(cl-defstruct hydra_grammar_labeled_pattern label pattern)

(defvar hydra_grammar_pattern-variants (list :alternatives :constant :ignored :labeled :nil :nonterminal :option :plus :regex :sequence :star))

(cl-defstruct hydra_grammar_production symbol pattern)

(cl-defstruct hydra_grammar_regex value)

(cl-defstruct hydra_grammar_symbol value)

(provide 'hydra.grammar)

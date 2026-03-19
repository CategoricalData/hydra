(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.grammar)

(require 'hydra.lib.lists)

(defvar hydra_encode_grammar_constant (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.grammar.Constant" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_grammar_label (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.grammar.Label" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_grammar_regex (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.grammar.Regex" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_grammar_symbol (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.grammar.Symbol" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_grammar_labeled_pattern (lambda (x) (list :record (make-hydra_core_record "hydra.grammar.LabeledPattern" (list (make-hydra_core_field "label" (hydra_encode_grammar_label (funcall (lambda (v) (hydra_grammar_labeled_pattern-label v)) x))) (make-hydra_core_field "pattern" (hydra_encode_grammar_pattern (funcall (lambda (v) (hydra_grammar_labeled_pattern-pattern v)) x))))))))

(defvar hydra_encode_grammar_pattern (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :alternatives) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "alternatives" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_grammar_pattern) xs))) y))))) match_value)) ((equal (car match_target) :constant) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "constant" (hydra_encode_grammar_constant y))))) match_value)) ((equal (car match_target) :ignored) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "ignored" (hydra_encode_grammar_pattern y))))) match_value)) ((equal (car match_target) :labeled) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "labeled" (hydra_encode_grammar_labeled_pattern y))))) match_value)) ((equal (car match_target) :nil) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "nil" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :nonterminal) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "nonterminal" (hydra_encode_grammar_symbol y))))) match_value)) ((equal (car match_target) :option) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "option" (hydra_encode_grammar_pattern y))))) match_value)) ((equal (car match_target) :plus) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "plus" (hydra_encode_grammar_pattern y))))) match_value)) ((equal (car match_target) :regex) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "regex" (hydra_encode_grammar_regex y))))) match_value)) ((equal (car match_target) :sequence) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "sequence" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_grammar_pattern) xs))) y))))) match_value)) ((equal (car match_target) :star) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.grammar.Pattern" (make-hydra_core_field "star" (hydra_encode_grammar_pattern y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_grammar_production (lambda (x) (list :record (make-hydra_core_record "hydra.grammar.Production" (list (make-hydra_core_field "symbol" (hydra_encode_grammar_symbol (funcall (lambda (v) (hydra_grammar_production-symbol v)) x))) (make-hydra_core_field "pattern" (hydra_encode_grammar_pattern (funcall (lambda (v) (hydra_grammar_production-pattern v)) x))))))))

(defvar hydra_encode_grammar_grammar (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.grammar.Grammar" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_grammar_production) xs))) (funcall (lambda (v) v) x))))))

(provide 'hydra.encode.grammar)

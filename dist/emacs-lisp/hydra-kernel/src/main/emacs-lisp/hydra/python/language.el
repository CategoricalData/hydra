(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.sets)

(require 'hydra.variants)

(defvar hydra_python_language_python_language (let* ((elimination_variants (hydra_lib_sets_from_list (list (list :record nil) (list :union nil) (list :wrap nil)))) (float_types (hydra_lib_sets_from_list (list (list :bigfloat nil) (list :float64 nil)))) (function_variants (hydra_lib_sets_from_list (list (list :elimination nil) (list :lambda nil)))) (integer_types (hydra_lib_sets_from_list (list (list :bigint nil)))) (literal_variants (hydra_lib_sets_from_list (list (list :binary nil) (list :boolean nil) (list :float nil) (list :integer nil) (list :string nil)))) (term_variants (hydra_lib_sets_from_list (list (list :annotated nil) (list :application nil) (list :either nil) (list :cases nil) (list :lambda nil) (list :project nil) (list :unwrap nil) (list :let nil) (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :pair nil) (list :record nil) (list :set nil) (list :type_application nil) (list :type_lambda nil) (list :union nil) (list :unit nil) (list :variable nil) (list :wrap nil)))) (type_predicate (lambda (_) t)) (type_variants (hydra_lib_sets_from_list (list (list :annotated nil) (list :application nil) (list :either nil) (list :function nil) (list :forall nil) (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :pair nil) (list :record nil) (list :set nil) (list :union nil) (list :unit nil) (list :variable nil) (list :void nil) (list :wrap nil))))) (make-hydra_coders_language "hydra.python" (make-hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants type_predicate))))

(defvar hydra_python_language_python_reserved_words (let* ((hydra_python_keywords (list "Node" "FrozenDict")) (python_built_in_functions (list "range")) (python_keywords (list "False" "None" "True" "and" "as" "assert" "async" "await" "break" "class" "continue" "def" "del" "elif" "else" "except" "finally" "for" "from" "global" "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass" "raise" "return" "try" "while" "with" "yield"))) (hydra_lib_sets_from_list (hydra_lib_lists_concat (list python_keywords python_built_in_functions hydra_python_keywords)))))

(provide 'hydra.python.language)

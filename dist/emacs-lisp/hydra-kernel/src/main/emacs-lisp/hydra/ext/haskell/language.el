(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.sets)

(require 'hydra.variants)

(defvar hydra_ext_haskell_language_haskell_language (let* ((elimination_variants (hydra_lib_sets_from_list (list (list :record nil) (list :union nil) (list :wrap nil)))) (float_types (hydra_lib_sets_from_list (list (list :float32 nil) (list :float64 nil)))) (function_variants (hydra_lib_sets_from_list (list (list :elimination nil) (list :lambda nil)))) (integer_types (hydra_lib_sets_from_list (list (list :bigint nil) (list :int8 nil) (list :int16 nil) (list :int32 nil) (list :int64 nil)))) (literal_variants (hydra_lib_sets_from_list (list (list :binary nil) (list :boolean nil) (list :float nil) (list :integer nil) (list :string nil)))) (term_variants (hydra_lib_sets_from_list (list (list :annotated nil) (list :application nil) (list :either nil) (list :function nil) (list :let nil) (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :pair nil) (list :record nil) (list :set nil) (list :union nil) (list :unit nil) (list :variable nil) (list :wrap nil)))) (type_predicate (lambda (_) t)) (type_variants (hydra_lib_sets_from_list (list (list :annotated nil) (list :application nil) (list :either nil) (list :function nil) (list :forall nil) (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :pair nil) (list :record nil) (list :set nil) (list :union nil) (list :unit nil) (list :variable nil) (list :void nil) (list :wrap nil))))) (make-hydra_coders_language "hydra.ext.haskell" (make-hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants type_predicate))))

(defvar hydra_ext_haskell_language_reserved_words (let* ((keyword_symbols (list "case" "class" "data" "default" "deriving" "do" "else" "forall" "foreign" "if" "import" "in" "infix" "infixl" "infixr" "instance" "let" "module" "newtype" "of" "then" "type" "where")) (reserved_symbols (list "Bool" "Double" "False" "Float" "Int" "Integer" "Just" "Maybe" "Nothing" "Ord" "Show" "String" "True"))) (hydra_lib_sets_from_list (funcall (hydra_lib_lists_concat2 keyword_symbols) reserved_symbols))))

(provide 'hydra.ext.haskell.language)

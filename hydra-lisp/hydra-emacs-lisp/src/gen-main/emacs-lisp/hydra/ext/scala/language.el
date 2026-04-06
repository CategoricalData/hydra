(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.sets)

(require 'hydra.variants)

(defvar hydra_ext_scala_language_scala_language (let* ((elimination_variants (hydra_lib_sets_from_list (list (list :record nil) (list :union nil) (list :wrap nil)))) (float_types (hydra_lib_sets_from_list (list (list :bigfloat nil) (list :float32 nil) (list :float64 nil)))) (function_variants (hydra_lib_sets_from_list (list (list :elimination nil) (list :lambda nil)))) (integer_types (hydra_lib_sets_from_list (list (list :bigint nil) (list :int8 nil) (list :int16 nil) (list :int32 nil) (list :int64 nil) (list :uint8 nil) (list :uint16 nil) (list :uint32 nil) (list :uint64 nil)))) (literal_variants (hydra_lib_sets_from_list (list (list :boolean nil) (list :float nil) (list :integer nil) (list :string nil)))) (term_variants (hydra_lib_sets_from_list (list (list :application nil) (list :either nil) (list :function nil) (list :let nil) (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :pair nil) (list :record nil) (list :set nil) (list :union nil) (list :unit nil) (list :variable nil) (list :wrap nil)))) (type_predicate (lambda (_) t)) (type_variants (hydra_lib_sets_from_list (list (list :annotated nil) (list :application nil) (list :either nil) (list :function nil) (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :pair nil) (list :record nil) (list :set nil) (list :union nil) (list :unit nil) (list :forall nil) (list :variable nil) (list :void nil) (list :wrap nil))))) (make-hydra_coders_language "hydra.ext.scala" (make-hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants type_predicate))))

(defvar hydra_ext_scala_language_scala_reserved_words (let* ((class_names (list "Any" "AnyVal" "App" "Array" "Boolean" "Byte" "Char" "Console" "DelayedInit" "Double" "DummyExplicit" "Dynamic" "Enumeration" "Equals" "Float" "Function" "Int" "Long" "MatchError" "None" "Nothing" "Null" "Option" "PartialFunction" "Predef" "Product" "Proxy" "SerialVersionUID" "Short" "Singleton" "Some" "Specializable" "StringContext" "Symbol" "Unit" "ValueOf")) (hydra_scala_keywords (list)) (keywords (list "abstract" "case" "catch" "class" "def" "do" "else" "end" "enum" "export" "extends" "false" "final" "finally" "for" "forSome" "given" "if" "implicit" "import" "lazy" "macro" "match" "new" "null" "object" "override" "package" "private" "protected" "return" "sealed" "super" "then" "this" "throw" "trait" "true" "try" "type" "val" "var" "while" "with" "yield"))) (hydra_lib_sets_from_list (hydra_lib_lists_concat (list keywords class_names hydra_scala_keywords)))))

(provide 'hydra.ext.scala.language)

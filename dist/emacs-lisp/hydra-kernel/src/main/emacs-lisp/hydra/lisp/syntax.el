(require 'cl-lib)

(cl-defstruct hydra_lisp_syntax_program dialect module imports exports forms)

(defvar hydra_lisp_syntax_top_level_form-variants (list :function :variable :constant :record_type :macro :expression))

(cl-defstruct hydra_lisp_syntax_top_level_form_with_comments doc comment form)

(cl-defstruct hydra_lisp_syntax_function_definition name params rest_param doc type_hints body)

(cl-defstruct hydra_lisp_syntax_variable_definition name value doc)

(cl-defstruct hydra_lisp_syntax_constant_definition name value doc)

(cl-defstruct hydra_lisp_syntax_record_type_definition name fields doc)

(cl-defstruct hydra_lisp_syntax_field_definition name default_value)

(cl-defstruct hydra_lisp_syntax_macro_definition name params rest_param body)

(defvar hydra_lisp_syntax_expression-variants (list :application :lambda :let :if :cond :case :and :or :not :do :begin :variable :literal :list :vector :map :set :cons :dotted_pair :field_access :type_annotation :quote :quasiquote :unquote :splicing_unquote :s_expression))

(cl-defstruct hydra_lisp_syntax_application function arguments)

(cl-defstruct hydra_lisp_syntax_lambda name params rest_param body)

(cl-defstruct hydra_lisp_syntax_variable_reference name function_namespace)

(cl-defstruct hydra_lisp_syntax_field_access record_type field target)

(cl-defstruct hydra_lisp_syntax_type_annotation expression type)

(cl-defstruct hydra_lisp_syntax_if_expression condition then else)

(cl-defstruct hydra_lisp_syntax_cond_expression clauses default)

(cl-defstruct hydra_lisp_syntax_cond_clause condition body)

(cl-defstruct hydra_lisp_syntax_case_expression scrutinee clauses default)

(cl-defstruct hydra_lisp_syntax_case_clause keys body)

(cl-defstruct hydra_lisp_syntax_and_expression expressions)

(cl-defstruct hydra_lisp_syntax_or_expression expressions)

(cl-defstruct hydra_lisp_syntax_not_expression expression)

(cl-defstruct hydra_lisp_syntax_do_expression expressions)

(cl-defstruct hydra_lisp_syntax_begin_expression expressions)

(cl-defstruct hydra_lisp_syntax_quote_expression body)

(cl-defstruct hydra_lisp_syntax_quasiquote_expression body)

(cl-defstruct hydra_lisp_syntax_unquote_expression body)

(cl-defstruct hydra_lisp_syntax_splicing_unquote_expression body)

(cl-defstruct hydra_lisp_syntax_let_expression kind bindings body)

(defvar hydra_lisp_syntax_let_kind-variants (list :parallel :sequential :recursive))

(defvar hydra_lisp_syntax_let_binding-variants (list :simple :destructuring))

(cl-defstruct hydra_lisp_syntax_simple_binding name value)

(cl-defstruct hydra_lisp_syntax_destructuring_binding pattern value)

(defvar hydra_lisp_syntax_destructuring_pattern-variants (list :sequential :associative :rest))

(defvar hydra_lisp_syntax_pattern-variants (list :constructor :literal :wildcard :variable))

(cl-defstruct hydra_lisp_syntax_constructor_pattern constructor arguments)

(cl-defstruct hydra_lisp_syntax_literal_pattern value)

(cl-defstruct hydra_lisp_syntax_wildcard_pattern)

(defvar hydra_lisp_syntax_literal-variants (list :integer :float :string :character :boolean :nil :keyword :symbol))

(cl-defstruct hydra_lisp_syntax_integer_literal value bigint)

(cl-defstruct hydra_lisp_syntax_float_literal value precision)

(cl-defstruct hydra_lisp_syntax_character_literal value)

(defvar hydra_lisp_syntax_boolean_style-variants (list :true_false :t_nil :hash_t_f))

(defvar hydra_lisp_syntax_nil_style-variants (list :nil :empty_list))

(cl-defstruct hydra_lisp_syntax_symbol value)

(cl-defstruct hydra_lisp_syntax_keyword name namespace)

(cl-defstruct hydra_lisp_syntax_qualified_symbol namespace name)

(cl-defstruct hydra_lisp_syntax_namespace_name value)

(cl-defstruct hydra_lisp_syntax_list_literal elements quoted)

(cl-defstruct hydra_lisp_syntax_vector_literal elements)

(cl-defstruct hydra_lisp_syntax_map_literal entries)

(cl-defstruct hydra_lisp_syntax_map_entry key value)

(cl-defstruct hydra_lisp_syntax_set_literal elements)

(cl-defstruct hydra_lisp_syntax_cons_expression head tail)

(cl-defstruct hydra_lisp_syntax_dotted_pair car cdr)

(cl-defstruct hydra_lisp_syntax_type_hint name type)

(defvar hydra_lisp_syntax_type_specifier-variants (list :named :list :function :maybe :map :set :pair :either :unit))

(cl-defstruct hydra_lisp_syntax_module_declaration name doc)

(cl-defstruct hydra_lisp_syntax_import_declaration module spec)

(defvar hydra_lisp_syntax_import_spec-variants (list :all :alias :only :rename))

(cl-defstruct hydra_lisp_syntax_export_declaration symbols)

(cl-defstruct hydra_lisp_syntax_comment style text)

(defvar hydra_lisp_syntax_comment_style-variants (list :line :block :datum))

(cl-defstruct hydra_lisp_syntax_docstring value)

(defvar hydra_lisp_syntax_dialect-variants (list :clojure :emacs_lisp :common_lisp :scheme))

(defvar hydra_lisp_syntax_s_expression-variants (list :atom :list))

(provide 'hydra.lisp.syntax)

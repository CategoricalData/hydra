(ns hydra.ext.lisp.syntax)

(declare hydra_ext_lisp_syntax_top_level_form-variants hydra_ext_lisp_syntax_expression-variants hydra_ext_lisp_syntax_let_kind-variants hydra_ext_lisp_syntax_let_binding-variants hydra_ext_lisp_syntax_destructuring_pattern-variants hydra_ext_lisp_syntax_pattern-variants hydra_ext_lisp_syntax_literal-variants hydra_ext_lisp_syntax_boolean_style-variants hydra_ext_lisp_syntax_nil_style-variants hydra_ext_lisp_syntax_type_specifier-variants hydra_ext_lisp_syntax_import_spec-variants hydra_ext_lisp_syntax_comment_style-variants hydra_ext_lisp_syntax_dialect-variants hydra_ext_lisp_syntax_s_expression-variants)

(defrecord hydra_ext_lisp_syntax_program [dialect module imports exports forms])
(defn make-hydra_ext_lisp_syntax_program [dialect module imports exports forms] (->hydra_ext_lisp_syntax_program dialect module imports exports forms))

(def hydra_ext_lisp_syntax_top_level_form-variants (list :function :variable :constant :record_type :macro :expression))

(defrecord hydra_ext_lisp_syntax_top_level_form_with_comments [doc comment form])
(defn make-hydra_ext_lisp_syntax_top_level_form_with_comments [doc comment form] (->hydra_ext_lisp_syntax_top_level_form_with_comments doc comment form))

(defrecord hydra_ext_lisp_syntax_function_definition [name params rest_param doc type_hints body])
(defn make-hydra_ext_lisp_syntax_function_definition [name params rest_param doc type_hints body] (->hydra_ext_lisp_syntax_function_definition name params rest_param doc type_hints body))

(defrecord hydra_ext_lisp_syntax_variable_definition [name value doc])
(defn make-hydra_ext_lisp_syntax_variable_definition [name value doc] (->hydra_ext_lisp_syntax_variable_definition name value doc))

(defrecord hydra_ext_lisp_syntax_constant_definition [name value doc])
(defn make-hydra_ext_lisp_syntax_constant_definition [name value doc] (->hydra_ext_lisp_syntax_constant_definition name value doc))

(defrecord hydra_ext_lisp_syntax_record_type_definition [name fields doc])
(defn make-hydra_ext_lisp_syntax_record_type_definition [name fields doc] (->hydra_ext_lisp_syntax_record_type_definition name fields doc))

(defrecord hydra_ext_lisp_syntax_field_definition [name default_value])
(defn make-hydra_ext_lisp_syntax_field_definition [name default_value] (->hydra_ext_lisp_syntax_field_definition name default_value))

(defrecord hydra_ext_lisp_syntax_macro_definition [name params rest_param body])
(defn make-hydra_ext_lisp_syntax_macro_definition [name params rest_param body] (->hydra_ext_lisp_syntax_macro_definition name params rest_param body))

(def hydra_ext_lisp_syntax_expression-variants (list :application :lambda :let :if :cond :case :and :or :not :do :begin :variable :literal :list :vector :map :set :cons :dotted_pair :field_access :type_annotation :quote :quasiquote :unquote :splicing_unquote :s_expression))

(defrecord hydra_ext_lisp_syntax_application [function arguments])
(defn make-hydra_ext_lisp_syntax_application [function arguments] (->hydra_ext_lisp_syntax_application function arguments))

(defrecord hydra_ext_lisp_syntax_lambda [name params rest_param body])
(defn make-hydra_ext_lisp_syntax_lambda [name params rest_param body] (->hydra_ext_lisp_syntax_lambda name params rest_param body))

(defrecord hydra_ext_lisp_syntax_variable_reference [name function_namespace])
(defn make-hydra_ext_lisp_syntax_variable_reference [name function_namespace] (->hydra_ext_lisp_syntax_variable_reference name function_namespace))

(defrecord hydra_ext_lisp_syntax_field_access [record_type field target])
(defn make-hydra_ext_lisp_syntax_field_access [record_type field target] (->hydra_ext_lisp_syntax_field_access record_type field target))

(defrecord hydra_ext_lisp_syntax_type_annotation [expression type])
(defn make-hydra_ext_lisp_syntax_type_annotation [expression type] (->hydra_ext_lisp_syntax_type_annotation expression type))

(defrecord hydra_ext_lisp_syntax_if_expression [condition then else])
(defn make-hydra_ext_lisp_syntax_if_expression [condition then else] (->hydra_ext_lisp_syntax_if_expression condition then else))

(defrecord hydra_ext_lisp_syntax_cond_expression [clauses default])
(defn make-hydra_ext_lisp_syntax_cond_expression [clauses default] (->hydra_ext_lisp_syntax_cond_expression clauses default))

(defrecord hydra_ext_lisp_syntax_cond_clause [condition body])
(defn make-hydra_ext_lisp_syntax_cond_clause [condition body] (->hydra_ext_lisp_syntax_cond_clause condition body))

(defrecord hydra_ext_lisp_syntax_case_expression [scrutinee clauses default])
(defn make-hydra_ext_lisp_syntax_case_expression [scrutinee clauses default] (->hydra_ext_lisp_syntax_case_expression scrutinee clauses default))

(defrecord hydra_ext_lisp_syntax_case_clause [keys body])
(defn make-hydra_ext_lisp_syntax_case_clause [keys body] (->hydra_ext_lisp_syntax_case_clause keys body))

(defrecord hydra_ext_lisp_syntax_and_expression [expressions])
(defn make-hydra_ext_lisp_syntax_and_expression [expressions] (->hydra_ext_lisp_syntax_and_expression expressions))

(defrecord hydra_ext_lisp_syntax_or_expression [expressions])
(defn make-hydra_ext_lisp_syntax_or_expression [expressions] (->hydra_ext_lisp_syntax_or_expression expressions))

(defrecord hydra_ext_lisp_syntax_not_expression [expression])
(defn make-hydra_ext_lisp_syntax_not_expression [expression] (->hydra_ext_lisp_syntax_not_expression expression))

(defrecord hydra_ext_lisp_syntax_do_expression [expressions])
(defn make-hydra_ext_lisp_syntax_do_expression [expressions] (->hydra_ext_lisp_syntax_do_expression expressions))

(defrecord hydra_ext_lisp_syntax_begin_expression [expressions])
(defn make-hydra_ext_lisp_syntax_begin_expression [expressions] (->hydra_ext_lisp_syntax_begin_expression expressions))

(defrecord hydra_ext_lisp_syntax_quote_expression [body])
(defn make-hydra_ext_lisp_syntax_quote_expression [body] (->hydra_ext_lisp_syntax_quote_expression body))

(defrecord hydra_ext_lisp_syntax_quasiquote_expression [body])
(defn make-hydra_ext_lisp_syntax_quasiquote_expression [body] (->hydra_ext_lisp_syntax_quasiquote_expression body))

(defrecord hydra_ext_lisp_syntax_unquote_expression [body])
(defn make-hydra_ext_lisp_syntax_unquote_expression [body] (->hydra_ext_lisp_syntax_unquote_expression body))

(defrecord hydra_ext_lisp_syntax_splicing_unquote_expression [body])
(defn make-hydra_ext_lisp_syntax_splicing_unquote_expression [body] (->hydra_ext_lisp_syntax_splicing_unquote_expression body))

(defrecord hydra_ext_lisp_syntax_let_expression [kind bindings body])
(defn make-hydra_ext_lisp_syntax_let_expression [kind bindings body] (->hydra_ext_lisp_syntax_let_expression kind bindings body))

(def hydra_ext_lisp_syntax_let_kind-variants (list :parallel :sequential :recursive))

(def hydra_ext_lisp_syntax_let_binding-variants (list :simple :destructuring))

(defrecord hydra_ext_lisp_syntax_simple_binding [name value])
(defn make-hydra_ext_lisp_syntax_simple_binding [name value] (->hydra_ext_lisp_syntax_simple_binding name value))

(defrecord hydra_ext_lisp_syntax_destructuring_binding [pattern value])
(defn make-hydra_ext_lisp_syntax_destructuring_binding [pattern value] (->hydra_ext_lisp_syntax_destructuring_binding pattern value))

(def hydra_ext_lisp_syntax_destructuring_pattern-variants (list :sequential :associative :rest))

(def hydra_ext_lisp_syntax_pattern-variants (list :constructor :literal :wildcard :variable))

(defrecord hydra_ext_lisp_syntax_constructor_pattern [constructor arguments])
(defn make-hydra_ext_lisp_syntax_constructor_pattern [constructor arguments] (->hydra_ext_lisp_syntax_constructor_pattern constructor arguments))

(defrecord hydra_ext_lisp_syntax_literal_pattern [value])
(defn make-hydra_ext_lisp_syntax_literal_pattern [value] (->hydra_ext_lisp_syntax_literal_pattern value))

(defrecord hydra_ext_lisp_syntax_wildcard_pattern [])
(defn make-hydra_ext_lisp_syntax_wildcard_pattern [] (->hydra_ext_lisp_syntax_wildcard_pattern))

(def hydra_ext_lisp_syntax_literal-variants (list :integer :float :string :character :boolean :nil :keyword :symbol))

(defrecord hydra_ext_lisp_syntax_integer_literal [value bigint])
(defn make-hydra_ext_lisp_syntax_integer_literal [value bigint] (->hydra_ext_lisp_syntax_integer_literal value bigint))

(defrecord hydra_ext_lisp_syntax_float_literal [value precision])
(defn make-hydra_ext_lisp_syntax_float_literal [value precision] (->hydra_ext_lisp_syntax_float_literal value precision))

(defrecord hydra_ext_lisp_syntax_character_literal [value])
(defn make-hydra_ext_lisp_syntax_character_literal [value] (->hydra_ext_lisp_syntax_character_literal value))

(def hydra_ext_lisp_syntax_boolean_style-variants (list :true_false :t_nil :hash_t_f))

(def hydra_ext_lisp_syntax_nil_style-variants (list :nil :empty_list))

(defrecord hydra_ext_lisp_syntax_symbol [value])
(defn make-hydra_ext_lisp_syntax_symbol [value] (->hydra_ext_lisp_syntax_symbol value))

(defrecord hydra_ext_lisp_syntax_keyword [name namespace])
(defn make-hydra_ext_lisp_syntax_keyword [name namespace] (->hydra_ext_lisp_syntax_keyword name namespace))

(defrecord hydra_ext_lisp_syntax_qualified_symbol [namespace name])
(defn make-hydra_ext_lisp_syntax_qualified_symbol [namespace name] (->hydra_ext_lisp_syntax_qualified_symbol namespace name))

(defrecord hydra_ext_lisp_syntax_namespace_name [value])
(defn make-hydra_ext_lisp_syntax_namespace_name [value] (->hydra_ext_lisp_syntax_namespace_name value))

(defrecord hydra_ext_lisp_syntax_list_literal [elements quoted])
(defn make-hydra_ext_lisp_syntax_list_literal [elements quoted] (->hydra_ext_lisp_syntax_list_literal elements quoted))

(defrecord hydra_ext_lisp_syntax_vector_literal [elements])
(defn make-hydra_ext_lisp_syntax_vector_literal [elements] (->hydra_ext_lisp_syntax_vector_literal elements))

(defrecord hydra_ext_lisp_syntax_map_literal [entries])
(defn make-hydra_ext_lisp_syntax_map_literal [entries] (->hydra_ext_lisp_syntax_map_literal entries))

(defrecord hydra_ext_lisp_syntax_map_entry [key value])
(defn make-hydra_ext_lisp_syntax_map_entry [key value] (->hydra_ext_lisp_syntax_map_entry key value))

(defrecord hydra_ext_lisp_syntax_set_literal [elements])
(defn make-hydra_ext_lisp_syntax_set_literal [elements] (->hydra_ext_lisp_syntax_set_literal elements))

(defrecord hydra_ext_lisp_syntax_cons_expression [head tail])
(defn make-hydra_ext_lisp_syntax_cons_expression [head tail] (->hydra_ext_lisp_syntax_cons_expression head tail))

(defrecord hydra_ext_lisp_syntax_dotted_pair [car cdr])
(defn make-hydra_ext_lisp_syntax_dotted_pair [car cdr] (->hydra_ext_lisp_syntax_dotted_pair car cdr))

(defrecord hydra_ext_lisp_syntax_type_hint [name type])
(defn make-hydra_ext_lisp_syntax_type_hint [name type] (->hydra_ext_lisp_syntax_type_hint name type))

(def hydra_ext_lisp_syntax_type_specifier-variants (list :named :list :function :maybe :map :set :pair :either :unit))

(defrecord hydra_ext_lisp_syntax_module_declaration [name doc])
(defn make-hydra_ext_lisp_syntax_module_declaration [name doc] (->hydra_ext_lisp_syntax_module_declaration name doc))

(defrecord hydra_ext_lisp_syntax_import_declaration [module spec])
(defn make-hydra_ext_lisp_syntax_import_declaration [module spec] (->hydra_ext_lisp_syntax_import_declaration module spec))

(def hydra_ext_lisp_syntax_import_spec-variants (list :all :alias :only :rename))

(defrecord hydra_ext_lisp_syntax_export_declaration [symbols])
(defn make-hydra_ext_lisp_syntax_export_declaration [symbols] (->hydra_ext_lisp_syntax_export_declaration symbols))

(defrecord hydra_ext_lisp_syntax_comment [style text])
(defn make-hydra_ext_lisp_syntax_comment [style text] (->hydra_ext_lisp_syntax_comment style text))

(def hydra_ext_lisp_syntax_comment_style-variants (list :line :block :datum))

(defrecord hydra_ext_lisp_syntax_docstring [value])
(defn make-hydra_ext_lisp_syntax_docstring [value] (->hydra_ext_lisp_syntax_docstring value))

(def hydra_ext_lisp_syntax_dialect-variants (list :clojure :emacs_lisp :common_lisp :scheme))

(def hydra_ext_lisp_syntax_s_expression-variants (list :atom :list))

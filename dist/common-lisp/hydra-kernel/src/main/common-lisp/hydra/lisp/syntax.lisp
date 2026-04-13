(defpackage :hydra.lisp.syntax
(:use :cl)
(:export :make-hydra_lisp_syntax_program :hydra_lisp_syntax_program? :hydra_lisp_syntax_program-dialect :hydra_lisp_syntax_program-module :hydra_lisp_syntax_program-imports :hydra_lisp_syntax_program-exports :hydra_lisp_syntax_program-forms :hydra_lisp_syntax_top_level_form-variants :make-hydra_lisp_syntax_top_level_form_with_comments :hydra_lisp_syntax_top_level_form_with_comments? :hydra_lisp_syntax_top_level_form_with_comments-doc :hydra_lisp_syntax_top_level_form_with_comments-comment :hydra_lisp_syntax_top_level_form_with_comments-form :make-hydra_lisp_syntax_function_definition :hydra_lisp_syntax_function_definition? :hydra_lisp_syntax_function_definition-name :hydra_lisp_syntax_function_definition-params :hydra_lisp_syntax_function_definition-rest_param :hydra_lisp_syntax_function_definition-doc :hydra_lisp_syntax_function_definition-type_hints :hydra_lisp_syntax_function_definition-body :make-hydra_lisp_syntax_variable_definition :hydra_lisp_syntax_variable_definition? :hydra_lisp_syntax_variable_definition-name :hydra_lisp_syntax_variable_definition-value :hydra_lisp_syntax_variable_definition-doc :make-hydra_lisp_syntax_constant_definition :hydra_lisp_syntax_constant_definition? :hydra_lisp_syntax_constant_definition-name :hydra_lisp_syntax_constant_definition-value :hydra_lisp_syntax_constant_definition-doc :make-hydra_lisp_syntax_record_type_definition :hydra_lisp_syntax_record_type_definition? :hydra_lisp_syntax_record_type_definition-name :hydra_lisp_syntax_record_type_definition-fields :hydra_lisp_syntax_record_type_definition-doc :make-hydra_lisp_syntax_field_definition :hydra_lisp_syntax_field_definition? :hydra_lisp_syntax_field_definition-name :hydra_lisp_syntax_field_definition-default_value :make-hydra_lisp_syntax_macro_definition :hydra_lisp_syntax_macro_definition? :hydra_lisp_syntax_macro_definition-name :hydra_lisp_syntax_macro_definition-params :hydra_lisp_syntax_macro_definition-rest_param :hydra_lisp_syntax_macro_definition-body :hydra_lisp_syntax_expression-variants :make-hydra_lisp_syntax_application :hydra_lisp_syntax_application? :hydra_lisp_syntax_application-function :hydra_lisp_syntax_application-arguments :make-hydra_lisp_syntax_lambda :hydra_lisp_syntax_lambda? :hydra_lisp_syntax_lambda-name :hydra_lisp_syntax_lambda-params :hydra_lisp_syntax_lambda-rest_param :hydra_lisp_syntax_lambda-body :make-hydra_lisp_syntax_variable_reference :hydra_lisp_syntax_variable_reference? :hydra_lisp_syntax_variable_reference-name :hydra_lisp_syntax_variable_reference-function_namespace :make-hydra_lisp_syntax_field_access :hydra_lisp_syntax_field_access? :hydra_lisp_syntax_field_access-record_type :hydra_lisp_syntax_field_access-field :hydra_lisp_syntax_field_access-target :make-hydra_lisp_syntax_type_annotation :hydra_lisp_syntax_type_annotation? :hydra_lisp_syntax_type_annotation-expression :hydra_lisp_syntax_type_annotation-type :make-hydra_lisp_syntax_if_expression :hydra_lisp_syntax_if_expression? :hydra_lisp_syntax_if_expression-condition :hydra_lisp_syntax_if_expression-then :hydra_lisp_syntax_if_expression-else :make-hydra_lisp_syntax_cond_expression :hydra_lisp_syntax_cond_expression? :hydra_lisp_syntax_cond_expression-clauses :hydra_lisp_syntax_cond_expression-default :make-hydra_lisp_syntax_cond_clause :hydra_lisp_syntax_cond_clause? :hydra_lisp_syntax_cond_clause-condition :hydra_lisp_syntax_cond_clause-body :make-hydra_lisp_syntax_case_expression :hydra_lisp_syntax_case_expression? :hydra_lisp_syntax_case_expression-scrutinee :hydra_lisp_syntax_case_expression-clauses :hydra_lisp_syntax_case_expression-default :make-hydra_lisp_syntax_case_clause :hydra_lisp_syntax_case_clause? :hydra_lisp_syntax_case_clause-keys :hydra_lisp_syntax_case_clause-body :make-hydra_lisp_syntax_and_expression :hydra_lisp_syntax_and_expression? :hydra_lisp_syntax_and_expression-expressions :make-hydra_lisp_syntax_or_expression :hydra_lisp_syntax_or_expression? :hydra_lisp_syntax_or_expression-expressions :make-hydra_lisp_syntax_not_expression :hydra_lisp_syntax_not_expression? :hydra_lisp_syntax_not_expression-expression :make-hydra_lisp_syntax_do_expression :hydra_lisp_syntax_do_expression? :hydra_lisp_syntax_do_expression-expressions :make-hydra_lisp_syntax_begin_expression :hydra_lisp_syntax_begin_expression? :hydra_lisp_syntax_begin_expression-expressions :make-hydra_lisp_syntax_quote_expression :hydra_lisp_syntax_quote_expression? :hydra_lisp_syntax_quote_expression-body :make-hydra_lisp_syntax_quasiquote_expression :hydra_lisp_syntax_quasiquote_expression? :hydra_lisp_syntax_quasiquote_expression-body :make-hydra_lisp_syntax_unquote_expression :hydra_lisp_syntax_unquote_expression? :hydra_lisp_syntax_unquote_expression-body :make-hydra_lisp_syntax_splicing_unquote_expression :hydra_lisp_syntax_splicing_unquote_expression? :hydra_lisp_syntax_splicing_unquote_expression-body :make-hydra_lisp_syntax_let_expression :hydra_lisp_syntax_let_expression? :hydra_lisp_syntax_let_expression-kind :hydra_lisp_syntax_let_expression-bindings :hydra_lisp_syntax_let_expression-body :hydra_lisp_syntax_let_kind-variants :hydra_lisp_syntax_let_binding-variants :make-hydra_lisp_syntax_simple_binding :hydra_lisp_syntax_simple_binding? :hydra_lisp_syntax_simple_binding-name :hydra_lisp_syntax_simple_binding-value :make-hydra_lisp_syntax_destructuring_binding :hydra_lisp_syntax_destructuring_binding? :hydra_lisp_syntax_destructuring_binding-pattern :hydra_lisp_syntax_destructuring_binding-value :hydra_lisp_syntax_destructuring_pattern-variants :hydra_lisp_syntax_pattern-variants :make-hydra_lisp_syntax_constructor_pattern :hydra_lisp_syntax_constructor_pattern? :hydra_lisp_syntax_constructor_pattern-constructor :hydra_lisp_syntax_constructor_pattern-arguments :make-hydra_lisp_syntax_literal_pattern :hydra_lisp_syntax_literal_pattern? :hydra_lisp_syntax_literal_pattern-value :make-hydra_lisp_syntax_wildcard_pattern :hydra_lisp_syntax_wildcard_pattern? :hydra_lisp_syntax_literal-variants :make-hydra_lisp_syntax_integer_literal :hydra_lisp_syntax_integer_literal? :hydra_lisp_syntax_integer_literal-value :hydra_lisp_syntax_integer_literal-bigint :make-hydra_lisp_syntax_float_literal :hydra_lisp_syntax_float_literal? :hydra_lisp_syntax_float_literal-value :hydra_lisp_syntax_float_literal-precision :make-hydra_lisp_syntax_character_literal :hydra_lisp_syntax_character_literal? :hydra_lisp_syntax_character_literal-value :hydra_lisp_syntax_boolean_style-variants :hydra_lisp_syntax_nil_style-variants :make-hydra_lisp_syntax_symbol :hydra_lisp_syntax_symbol? :hydra_lisp_syntax_symbol-value :make-hydra_lisp_syntax_keyword :hydra_lisp_syntax_keyword? :hydra_lisp_syntax_keyword-name :hydra_lisp_syntax_keyword-namespace :make-hydra_lisp_syntax_qualified_symbol :hydra_lisp_syntax_qualified_symbol? :hydra_lisp_syntax_qualified_symbol-namespace :hydra_lisp_syntax_qualified_symbol-name :make-hydra_lisp_syntax_namespace_name :hydra_lisp_syntax_namespace_name? :hydra_lisp_syntax_namespace_name-value :make-hydra_lisp_syntax_list_literal :hydra_lisp_syntax_list_literal? :hydra_lisp_syntax_list_literal-elements :hydra_lisp_syntax_list_literal-quoted :make-hydra_lisp_syntax_vector_literal :hydra_lisp_syntax_vector_literal? :hydra_lisp_syntax_vector_literal-elements :make-hydra_lisp_syntax_map_literal :hydra_lisp_syntax_map_literal? :hydra_lisp_syntax_map_literal-entries :make-hydra_lisp_syntax_map_entry :hydra_lisp_syntax_map_entry? :hydra_lisp_syntax_map_entry-key :hydra_lisp_syntax_map_entry-value :make-hydra_lisp_syntax_set_literal :hydra_lisp_syntax_set_literal? :hydra_lisp_syntax_set_literal-elements :make-hydra_lisp_syntax_cons_expression :hydra_lisp_syntax_cons_expression? :hydra_lisp_syntax_cons_expression-head :hydra_lisp_syntax_cons_expression-tail :make-hydra_lisp_syntax_dotted_pair :hydra_lisp_syntax_dotted_pair? :hydra_lisp_syntax_dotted_pair-car :hydra_lisp_syntax_dotted_pair-cdr :make-hydra_lisp_syntax_type_hint :hydra_lisp_syntax_type_hint? :hydra_lisp_syntax_type_hint-name :hydra_lisp_syntax_type_hint-type :hydra_lisp_syntax_type_specifier-variants :make-hydra_lisp_syntax_module_declaration :hydra_lisp_syntax_module_declaration? :hydra_lisp_syntax_module_declaration-name :hydra_lisp_syntax_module_declaration-doc :make-hydra_lisp_syntax_import_declaration :hydra_lisp_syntax_import_declaration? :hydra_lisp_syntax_import_declaration-module :hydra_lisp_syntax_import_declaration-spec :hydra_lisp_syntax_import_spec-variants :make-hydra_lisp_syntax_export_declaration :hydra_lisp_syntax_export_declaration? :hydra_lisp_syntax_export_declaration-symbols :make-hydra_lisp_syntax_comment :hydra_lisp_syntax_comment? :hydra_lisp_syntax_comment-style :hydra_lisp_syntax_comment-text :hydra_lisp_syntax_comment_style-variants :make-hydra_lisp_syntax_docstring :hydra_lisp_syntax_docstring? :hydra_lisp_syntax_docstring-value :hydra_lisp_syntax_dialect-variants :hydra_lisp_syntax_s_expression-variants))

(in-package :hydra.lisp.syntax)

(cl:defstruct hydra_lisp_syntax_program dialect module imports exports forms)

(cl:defvar hydra_lisp_syntax_top_level_form-variants (cl:list :function :variable :constant :record_type :macro :expression))

(cl:defstruct hydra_lisp_syntax_top_level_form_with_comments doc comment form)

(cl:defstruct hydra_lisp_syntax_function_definition name params rest_param doc type_hints body)

(cl:defstruct hydra_lisp_syntax_variable_definition name value doc)

(cl:defstruct hydra_lisp_syntax_constant_definition name value doc)

(cl:defstruct hydra_lisp_syntax_record_type_definition name fields doc)

(cl:defstruct hydra_lisp_syntax_field_definition name default_value)

(cl:defstruct hydra_lisp_syntax_macro_definition name params rest_param body)

(cl:defvar hydra_lisp_syntax_expression-variants (cl:list :application :lambda :let :if :cond :case :and :or :not :do :begin :variable :literal :list :vector :map :set :cons :dotted_pair :field_access :type_annotation :quote :quasiquote :unquote :splicing_unquote :s_expression))

(cl:defstruct hydra_lisp_syntax_application function arguments)

(cl:defstruct hydra_lisp_syntax_lambda name params rest_param body)

(cl:defstruct hydra_lisp_syntax_variable_reference name function_namespace)

(cl:defstruct hydra_lisp_syntax_field_access record_type field target)

(cl:defstruct hydra_lisp_syntax_type_annotation expression type)

(cl:defstruct hydra_lisp_syntax_if_expression condition then else)

(cl:defstruct hydra_lisp_syntax_cond_expression clauses default)

(cl:defstruct hydra_lisp_syntax_cond_clause condition body)

(cl:defstruct hydra_lisp_syntax_case_expression scrutinee clauses default)

(cl:defstruct hydra_lisp_syntax_case_clause keys body)

(cl:defstruct hydra_lisp_syntax_and_expression expressions)

(cl:defstruct hydra_lisp_syntax_or_expression expressions)

(cl:defstruct hydra_lisp_syntax_not_expression expression)

(cl:defstruct hydra_lisp_syntax_do_expression expressions)

(cl:defstruct hydra_lisp_syntax_begin_expression expressions)

(cl:defstruct hydra_lisp_syntax_quote_expression body)

(cl:defstruct hydra_lisp_syntax_quasiquote_expression body)

(cl:defstruct hydra_lisp_syntax_unquote_expression body)

(cl:defstruct hydra_lisp_syntax_splicing_unquote_expression body)

(cl:defstruct hydra_lisp_syntax_let_expression kind bindings body)

(cl:defvar hydra_lisp_syntax_let_kind-variants (cl:list :parallel :sequential :recursive))

(cl:defvar hydra_lisp_syntax_let_binding-variants (cl:list :simple :destructuring))

(cl:defstruct hydra_lisp_syntax_simple_binding name value)

(cl:defstruct hydra_lisp_syntax_destructuring_binding pattern value)

(cl:defvar hydra_lisp_syntax_destructuring_pattern-variants (cl:list :sequential :associative :rest))

(cl:defvar hydra_lisp_syntax_pattern-variants (cl:list :constructor :literal :wildcard :variable))

(cl:defstruct hydra_lisp_syntax_constructor_pattern constructor arguments)

(cl:defstruct hydra_lisp_syntax_literal_pattern value)

(cl:defstruct hydra_lisp_syntax_wildcard_pattern)

(cl:defvar hydra_lisp_syntax_literal-variants (cl:list :integer :float :string :character :boolean :nil :keyword :symbol))

(cl:defstruct hydra_lisp_syntax_integer_literal value bigint)

(cl:defstruct hydra_lisp_syntax_float_literal value precision)

(cl:defstruct hydra_lisp_syntax_character_literal value)

(cl:defvar hydra_lisp_syntax_boolean_style-variants (cl:list :true_false :t_nil :hash_t_f))

(cl:defvar hydra_lisp_syntax_nil_style-variants (cl:list :nil :empty_list))

(cl:defstruct hydra_lisp_syntax_symbol value)

(cl:defstruct hydra_lisp_syntax_keyword name namespace)

(cl:defstruct hydra_lisp_syntax_qualified_symbol namespace name)

(cl:defstruct hydra_lisp_syntax_namespace_name value)

(cl:defstruct hydra_lisp_syntax_list_literal elements quoted)

(cl:defstruct hydra_lisp_syntax_vector_literal elements)

(cl:defstruct hydra_lisp_syntax_map_literal entries)

(cl:defstruct hydra_lisp_syntax_map_entry key value)

(cl:defstruct hydra_lisp_syntax_set_literal elements)

(cl:defstruct hydra_lisp_syntax_cons_expression head tail)

(cl:defstruct hydra_lisp_syntax_dotted_pair car cdr)

(cl:defstruct hydra_lisp_syntax_type_hint name type)

(cl:defvar hydra_lisp_syntax_type_specifier-variants (cl:list :named :list :function :maybe :map :set :pair :either :unit))

(cl:defstruct hydra_lisp_syntax_module_declaration name doc)

(cl:defstruct hydra_lisp_syntax_import_declaration module spec)

(cl:defvar hydra_lisp_syntax_import_spec-variants (cl:list :all :alias :only :rename))

(cl:defstruct hydra_lisp_syntax_export_declaration symbols)

(cl:defstruct hydra_lisp_syntax_comment style text)

(cl:defvar hydra_lisp_syntax_comment_style-variants (cl:list :line :block :datum))

(cl:defstruct hydra_lisp_syntax_docstring value)

(cl:defvar hydra_lisp_syntax_dialect-variants (cl:list :clojure :emacs_lisp :common_lisp :scheme))

(cl:defvar hydra_lisp_syntax_s_expression-variants (cl:list :atom :list))

(ns hydra.ext.haskell.syntax)

(declare hydra_ext_haskell_syntax_assertion-variants hydra_ext_haskell_syntax_constructor-variants hydra_ext_haskell_syntax_data_or_newtype-variants hydra_ext_haskell_syntax_declaration-variants hydra_ext_haskell_syntax_declaration_head-variants hydra_ext_haskell_syntax_export-variants hydra_ext_haskell_syntax_expression-variants hydra_ext_haskell_syntax_spec_import-variants hydra_ext_haskell_syntax_import_modifier-variants hydra_ext_haskell_syntax_subspec_import_export_spec-variants hydra_ext_haskell_syntax_literal-variants hydra_ext_haskell_syntax_local_binding-variants hydra_ext_haskell_syntax_name-variants hydra_ext_haskell_syntax_operator-variants hydra_ext_haskell_syntax_pattern-variants hydra_ext_haskell_syntax_type-variants hydra_ext_haskell_syntax_value_binding-variants)

(defrecord hydra_ext_haskell_syntax_alternative [pattern rhs binds])
(defn make-hydra_ext_haskell_syntax_alternative [pattern rhs binds] (->hydra_ext_haskell_syntax_alternative pattern rhs binds))

(def hydra_ext_haskell_syntax_assertion-variants (list :class :tuple))

(defrecord hydra_ext_haskell_syntax_class_assertion [name types])
(defn make-hydra_ext_haskell_syntax_class_assertion [name types] (->hydra_ext_haskell_syntax_class_assertion name types))

(defrecord hydra_ext_haskell_syntax_case_rhs [value])
(defn make-hydra_ext_haskell_syntax_case_rhs [value] (->hydra_ext_haskell_syntax_case_rhs value))

(def hydra_ext_haskell_syntax_constructor-variants (list :ordinary :record))

(defrecord hydra_ext_haskell_syntax_ordinary_constructor [name fields])
(defn make-hydra_ext_haskell_syntax_ordinary_constructor [name fields] (->hydra_ext_haskell_syntax_ordinary_constructor name fields))

(defrecord hydra_ext_haskell_syntax_record_constructor [name fields])
(defn make-hydra_ext_haskell_syntax_record_constructor [name fields] (->hydra_ext_haskell_syntax_record_constructor name fields))

(defrecord hydra_ext_haskell_syntax_constructor_with_comments [body comments])
(defn make-hydra_ext_haskell_syntax_constructor_with_comments [body comments] (->hydra_ext_haskell_syntax_constructor_with_comments body comments))

(defrecord hydra_ext_haskell_syntax_data_declaration [keyword context head constructors deriving])
(defn make-hydra_ext_haskell_syntax_data_declaration [keyword context head constructors deriving] (->hydra_ext_haskell_syntax_data_declaration keyword context head constructors deriving))

(def hydra_ext_haskell_syntax_data_or_newtype-variants (list :data :newtype))

(defrecord hydra_ext_haskell_syntax_declaration_with_comments [body comments])
(defn make-hydra_ext_haskell_syntax_declaration_with_comments [body comments] (->hydra_ext_haskell_syntax_declaration_with_comments body comments))

(def hydra_ext_haskell_syntax_declaration-variants (list :data :type :value_binding :typed_binding))

(def hydra_ext_haskell_syntax_declaration_head-variants (list :application :parens :simple))

(defrecord hydra_ext_haskell_syntax_application_declaration_head [function operand])
(defn make-hydra_ext_haskell_syntax_application_declaration_head [function operand] (->hydra_ext_haskell_syntax_application_declaration_head function operand))

(defrecord hydra_ext_haskell_syntax_deriving [value])
(defn make-hydra_ext_haskell_syntax_deriving [value] (->hydra_ext_haskell_syntax_deriving value))

(def hydra_ext_haskell_syntax_export-variants (list :declaration :module))

(def hydra_ext_haskell_syntax_expression-variants (list :application :case :construct_record :do :if :infix_application :literal :lambda :left_section :let :list :parens :prefix_application :right_section :tuple :type_signature :update_record :variable))

(defrecord hydra_ext_haskell_syntax_application_expression [function argument])
(defn make-hydra_ext_haskell_syntax_application_expression [function argument] (->hydra_ext_haskell_syntax_application_expression function argument))

(defrecord hydra_ext_haskell_syntax_case_expression [case alternatives])
(defn make-hydra_ext_haskell_syntax_case_expression [case alternatives] (->hydra_ext_haskell_syntax_case_expression case alternatives))

(defrecord hydra_ext_haskell_syntax_construct_record_expression [name fields])
(defn make-hydra_ext_haskell_syntax_construct_record_expression [name fields] (->hydra_ext_haskell_syntax_construct_record_expression name fields))

(defrecord hydra_ext_haskell_syntax_if_expression [condition then else])
(defn make-hydra_ext_haskell_syntax_if_expression [condition then else] (->hydra_ext_haskell_syntax_if_expression condition then else))

(defrecord hydra_ext_haskell_syntax_infix_application_expression [lhs operator rhs])
(defn make-hydra_ext_haskell_syntax_infix_application_expression [lhs operator rhs] (->hydra_ext_haskell_syntax_infix_application_expression lhs operator rhs))

(defrecord hydra_ext_haskell_syntax_lambda_expression [bindings inner])
(defn make-hydra_ext_haskell_syntax_lambda_expression [bindings inner] (->hydra_ext_haskell_syntax_lambda_expression bindings inner))

(defrecord hydra_ext_haskell_syntax_let_expression [bindings inner])
(defn make-hydra_ext_haskell_syntax_let_expression [bindings inner] (->hydra_ext_haskell_syntax_let_expression bindings inner))

(defrecord hydra_ext_haskell_syntax_prefix_application_expression [operator rhs])
(defn make-hydra_ext_haskell_syntax_prefix_application_expression [operator rhs] (->hydra_ext_haskell_syntax_prefix_application_expression operator rhs))

(defrecord hydra_ext_haskell_syntax_section_expression [operator expression])
(defn make-hydra_ext_haskell_syntax_section_expression [operator expression] (->hydra_ext_haskell_syntax_section_expression operator expression))

(defrecord hydra_ext_haskell_syntax_type_signature_expression [inner type])
(defn make-hydra_ext_haskell_syntax_type_signature_expression [inner type] (->hydra_ext_haskell_syntax_type_signature_expression inner type))

(defrecord hydra_ext_haskell_syntax_update_record_expression [inner fields])
(defn make-hydra_ext_haskell_syntax_update_record_expression [inner fields] (->hydra_ext_haskell_syntax_update_record_expression inner fields))

(defrecord hydra_ext_haskell_syntax_field [name type])
(defn make-hydra_ext_haskell_syntax_field [name type] (->hydra_ext_haskell_syntax_field name type))

(defrecord hydra_ext_haskell_syntax_field_with_comments [field comments])
(defn make-hydra_ext_haskell_syntax_field_with_comments [field comments] (->hydra_ext_haskell_syntax_field_with_comments field comments))

(defrecord hydra_ext_haskell_syntax_field_update [name value])
(defn make-hydra_ext_haskell_syntax_field_update [name value] (->hydra_ext_haskell_syntax_field_update name value))

(defrecord hydra_ext_haskell_syntax_import [qualified module as spec])
(defn make-hydra_ext_haskell_syntax_import [qualified module as spec] (->hydra_ext_haskell_syntax_import qualified module as spec))

(def hydra_ext_haskell_syntax_spec_import-variants (list :list :hiding))

(def hydra_ext_haskell_syntax_import_modifier-variants (list :pattern :type))

(defrecord hydra_ext_haskell_syntax_import_export_spec [modifier name subspec])
(defn make-hydra_ext_haskell_syntax_import_export_spec [modifier name subspec] (->hydra_ext_haskell_syntax_import_export_spec modifier name subspec))

(def hydra_ext_haskell_syntax_subspec_import_export_spec-variants (list :all :list))

(def hydra_ext_haskell_syntax_literal-variants (list :char :double :float :int :integer :string))

(def hydra_ext_haskell_syntax_local_binding-variants (list :signature :value))

(defrecord hydra_ext_haskell_syntax_local_bindings [value])
(defn make-hydra_ext_haskell_syntax_local_bindings [value] (->hydra_ext_haskell_syntax_local_bindings value))

(defrecord hydra_ext_haskell_syntax_module [head imports declarations])
(defn make-hydra_ext_haskell_syntax_module [head imports declarations] (->hydra_ext_haskell_syntax_module head imports declarations))

(defrecord hydra_ext_haskell_syntax_module_head [comments name exports])
(defn make-hydra_ext_haskell_syntax_module_head [comments name exports] (->hydra_ext_haskell_syntax_module_head comments name exports))

(defrecord hydra_ext_haskell_syntax_module_name [value])
(defn make-hydra_ext_haskell_syntax_module_name [value] (->hydra_ext_haskell_syntax_module_name value))

(def hydra_ext_haskell_syntax_name-variants (list :implicit :normal :parens))

(defrecord hydra_ext_haskell_syntax_name_part [value])
(defn make-hydra_ext_haskell_syntax_name_part [value] (->hydra_ext_haskell_syntax_name_part value))

(def hydra_ext_haskell_syntax_operator-variants (list :backtick :normal))

(def hydra_ext_haskell_syntax_pattern-variants (list :application :as :list :literal :name :parens :record :tuple :typed :wildcard))

(defrecord hydra_ext_haskell_syntax_application_pattern [name args])
(defn make-hydra_ext_haskell_syntax_application_pattern [name args] (->hydra_ext_haskell_syntax_application_pattern name args))

(defrecord hydra_ext_haskell_syntax_as_pattern [name inner])
(defn make-hydra_ext_haskell_syntax_as_pattern [name inner] (->hydra_ext_haskell_syntax_as_pattern name inner))

(defrecord hydra_ext_haskell_syntax_record_pattern [name fields])
(defn make-hydra_ext_haskell_syntax_record_pattern [name fields] (->hydra_ext_haskell_syntax_record_pattern name fields))

(defrecord hydra_ext_haskell_syntax_typed_pattern [inner type])
(defn make-hydra_ext_haskell_syntax_typed_pattern [inner type] (->hydra_ext_haskell_syntax_typed_pattern inner type))

(defrecord hydra_ext_haskell_syntax_pattern_field [name pattern])
(defn make-hydra_ext_haskell_syntax_pattern_field [name pattern] (->hydra_ext_haskell_syntax_pattern_field name pattern))

(defrecord hydra_ext_haskell_syntax_qualified_name [qualifiers unqualified])
(defn make-hydra_ext_haskell_syntax_qualified_name [qualifiers unqualified] (->hydra_ext_haskell_syntax_qualified_name qualifiers unqualified))

(defrecord hydra_ext_haskell_syntax_right_hand_side [value])
(defn make-hydra_ext_haskell_syntax_right_hand_side [value] (->hydra_ext_haskell_syntax_right_hand_side value))

(defrecord hydra_ext_haskell_syntax_statement [value])
(defn make-hydra_ext_haskell_syntax_statement [value] (->hydra_ext_haskell_syntax_statement value))

(def hydra_ext_haskell_syntax_type-variants (list :application :ctx :function :infix :list :parens :tuple :variable))

(defrecord hydra_ext_haskell_syntax_application_type [context argument])
(defn make-hydra_ext_haskell_syntax_application_type [context argument] (->hydra_ext_haskell_syntax_application_type context argument))

(defrecord hydra_ext_haskell_syntax_context_type [ctx type])
(defn make-hydra_ext_haskell_syntax_context_type [ctx type] (->hydra_ext_haskell_syntax_context_type ctx type))

(defrecord hydra_ext_haskell_syntax_function_type [domain codomain])
(defn make-hydra_ext_haskell_syntax_function_type [domain codomain] (->hydra_ext_haskell_syntax_function_type domain codomain))

(defrecord hydra_ext_haskell_syntax_infix_type [lhs operator rhs])
(defn make-hydra_ext_haskell_syntax_infix_type [lhs operator rhs] (->hydra_ext_haskell_syntax_infix_type lhs operator rhs))

(defrecord hydra_ext_haskell_syntax_type_declaration [name type])
(defn make-hydra_ext_haskell_syntax_type_declaration [name type] (->hydra_ext_haskell_syntax_type_declaration name type))

(defrecord hydra_ext_haskell_syntax_type_signature [name type])
(defn make-hydra_ext_haskell_syntax_type_signature [name type] (->hydra_ext_haskell_syntax_type_signature name type))

(defrecord hydra_ext_haskell_syntax_typed_binding [type_signature value_binding])
(defn make-hydra_ext_haskell_syntax_typed_binding [type_signature value_binding] (->hydra_ext_haskell_syntax_typed_binding type_signature value_binding))

(def hydra_ext_haskell_syntax_value_binding-variants (list :simple))

(defrecord hydra_ext_haskell_syntax_simple_value_binding [pattern rhs local_bindings])
(defn make-hydra_ext_haskell_syntax_simple_value_binding [pattern rhs local_bindings] (->hydra_ext_haskell_syntax_simple_value_binding pattern rhs local_bindings))

(defrecord hydra_ext_haskell_syntax_variable [value])
(defn make-hydra_ext_haskell_syntax_variable [value] (->hydra_ext_haskell_syntax_variable value))

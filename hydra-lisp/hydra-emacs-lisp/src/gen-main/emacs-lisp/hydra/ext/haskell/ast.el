(require 'cl-lib)

(cl-defstruct hydra_ext_haskell_ast_alternative pattern rhs binds)

(defvar hydra_ext_haskell_ast_assertion-variants (list :class :tuple))

(cl-defstruct hydra_ext_haskell_ast_class_assertion name types)

(cl-defstruct hydra_ext_haskell_ast_case_rhs value)

(defvar hydra_ext_haskell_ast_constructor-variants (list :ordinary :record))

(cl-defstruct hydra_ext_haskell_ast_ordinary_constructor name fields)

(cl-defstruct hydra_ext_haskell_ast_record_constructor name fields)

(cl-defstruct hydra_ext_haskell_ast_constructor_with_comments body comments)

(cl-defstruct hydra_ext_haskell_ast_data_declaration keyword context head constructors deriving)

(defvar hydra_ext_haskell_ast_data_or_newtype-variants (list :data :newtype))

(cl-defstruct hydra_ext_haskell_ast_declaration_with_comments body comments)

(defvar hydra_ext_haskell_ast_declaration-variants (list :data :type :value_binding :typed_binding))

(defvar hydra_ext_haskell_ast_declaration_head-variants (list :application :parens :simple))

(cl-defstruct hydra_ext_haskell_ast_application_declaration_head function operand)

(cl-defstruct hydra_ext_haskell_ast_deriving value)

(defvar hydra_ext_haskell_ast_export-variants (list :declaration :module))

(defvar hydra_ext_haskell_ast_expression-variants (list :application :case :construct_record :do :if :infix_application :literal :lambda :left_section :let :list :parens :prefix_application :right_section :tuple :type_signature :update_record :variable))

(cl-defstruct hydra_ext_haskell_ast_application_expression function argument)

(cl-defstruct hydra_ext_haskell_ast_case_expression case alternatives)

(cl-defstruct hydra_ext_haskell_ast_construct_record_expression name fields)

(cl-defstruct hydra_ext_haskell_ast_if_expression condition then else)

(cl-defstruct hydra_ext_haskell_ast_infix_application_expression lhs operator rhs)

(cl-defstruct hydra_ext_haskell_ast_lambda_expression bindings inner)

(cl-defstruct hydra_ext_haskell_ast_let_expression bindings inner)

(cl-defstruct hydra_ext_haskell_ast_prefix_application_expression operator rhs)

(cl-defstruct hydra_ext_haskell_ast_section_expression operator expression)

(cl-defstruct hydra_ext_haskell_ast_type_signature_expression inner type)

(cl-defstruct hydra_ext_haskell_ast_update_record_expression inner fields)

(cl-defstruct hydra_ext_haskell_ast_field name type)

(cl-defstruct hydra_ext_haskell_ast_field_with_comments field comments)

(cl-defstruct hydra_ext_haskell_ast_field_update name value)

(cl-defstruct hydra_ext_haskell_ast_import qualified module as spec)

(defvar hydra_ext_haskell_ast_spec_import-variants (list :list :hiding))

(defvar hydra_ext_haskell_ast_import_modifier-variants (list :pattern :type))

(cl-defstruct hydra_ext_haskell_ast_import_export_spec modifier name subspec)

(defvar hydra_ext_haskell_ast_subspec_import_export_spec-variants (list :all :list))

(defvar hydra_ext_haskell_ast_literal-variants (list :char :double :float :int :integer :string))

(defvar hydra_ext_haskell_ast_local_binding-variants (list :signature :value))

(cl-defstruct hydra_ext_haskell_ast_local_bindings value)

(cl-defstruct hydra_ext_haskell_ast_module head imports declarations)

(cl-defstruct hydra_ext_haskell_ast_module_head comments name exports)

(cl-defstruct hydra_ext_haskell_ast_module_name value)

(defvar hydra_ext_haskell_ast_name-variants (list :implicit :normal :parens))

(cl-defstruct hydra_ext_haskell_ast_name_part value)

(defvar hydra_ext_haskell_ast_operator-variants (list :backtick :normal))

(defvar hydra_ext_haskell_ast_pattern-variants (list :application :as :list :literal :name :parens :record :tuple :typed :wildcard))

(cl-defstruct hydra_ext_haskell_ast_application_pattern name args)

(cl-defstruct hydra_ext_haskell_ast_as_pattern name inner)

(cl-defstruct hydra_ext_haskell_ast_record_pattern name fields)

(cl-defstruct hydra_ext_haskell_ast_typed_pattern inner type)

(cl-defstruct hydra_ext_haskell_ast_pattern_field name pattern)

(cl-defstruct hydra_ext_haskell_ast_qualified_name qualifiers unqualified)

(cl-defstruct hydra_ext_haskell_ast_right_hand_side value)

(cl-defstruct hydra_ext_haskell_ast_statement value)

(defvar hydra_ext_haskell_ast_type-variants (list :application :ctx :function :infix :list :parens :tuple :variable))

(cl-defstruct hydra_ext_haskell_ast_application_type context argument)

(cl-defstruct hydra_ext_haskell_ast_context_type ctx type)

(cl-defstruct hydra_ext_haskell_ast_function_type domain codomain)

(cl-defstruct hydra_ext_haskell_ast_infix_type lhs operator rhs)

(cl-defstruct hydra_ext_haskell_ast_type_declaration name type)

(cl-defstruct hydra_ext_haskell_ast_type_signature name type)

(cl-defstruct hydra_ext_haskell_ast_typed_binding type_signature value_binding)

(defvar hydra_ext_haskell_ast_value_binding-variants (list :simple))

(cl-defstruct hydra_ext_haskell_ast_simple_value_binding pattern rhs local_bindings)

(cl-defstruct hydra_ext_haskell_ast_variable value)

(provide 'hydra.ext.haskell.ast)

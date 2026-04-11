(require 'cl-lib)

(cl-defstruct hydra_ext_haskell_syntax_alternative pattern rhs binds)

(defvar hydra_ext_haskell_syntax_assertion-variants (list :class :tuple))

(cl-defstruct hydra_ext_haskell_syntax_class_assertion name types)

(cl-defstruct hydra_ext_haskell_syntax_case_rhs value)

(defvar hydra_ext_haskell_syntax_constructor-variants (list :ordinary :record))

(cl-defstruct hydra_ext_haskell_syntax_ordinary_constructor name fields)

(cl-defstruct hydra_ext_haskell_syntax_record_constructor name fields)

(cl-defstruct hydra_ext_haskell_syntax_constructor_with_comments body comments)

(cl-defstruct hydra_ext_haskell_syntax_data_declaration keyword context head constructors deriving)

(defvar hydra_ext_haskell_syntax_data_or_newtype-variants (list :data :newtype))

(cl-defstruct hydra_ext_haskell_syntax_declaration_with_comments body comments)

(defvar hydra_ext_haskell_syntax_declaration-variants (list :data :type :value_binding :typed_binding))

(defvar hydra_ext_haskell_syntax_declaration_head-variants (list :application :parens :simple))

(cl-defstruct hydra_ext_haskell_syntax_application_declaration_head function operand)

(cl-defstruct hydra_ext_haskell_syntax_deriving value)

(defvar hydra_ext_haskell_syntax_export-variants (list :declaration :module))

(defvar hydra_ext_haskell_syntax_expression-variants (list :application :case :construct_record :do :if :infix_application :literal :lambda :left_section :let :list :parens :prefix_application :right_section :tuple :type_signature :update_record :variable))

(cl-defstruct hydra_ext_haskell_syntax_application_expression function argument)

(cl-defstruct hydra_ext_haskell_syntax_case_expression case alternatives)

(cl-defstruct hydra_ext_haskell_syntax_construct_record_expression name fields)

(cl-defstruct hydra_ext_haskell_syntax_if_expression condition then else)

(cl-defstruct hydra_ext_haskell_syntax_infix_application_expression lhs operator rhs)

(cl-defstruct hydra_ext_haskell_syntax_lambda_expression bindings inner)

(cl-defstruct hydra_ext_haskell_syntax_let_expression bindings inner)

(cl-defstruct hydra_ext_haskell_syntax_prefix_application_expression operator rhs)

(cl-defstruct hydra_ext_haskell_syntax_section_expression operator expression)

(cl-defstruct hydra_ext_haskell_syntax_type_signature_expression inner type)

(cl-defstruct hydra_ext_haskell_syntax_update_record_expression inner fields)

(cl-defstruct hydra_ext_haskell_syntax_field name type)

(cl-defstruct hydra_ext_haskell_syntax_field_with_comments field comments)

(cl-defstruct hydra_ext_haskell_syntax_field_update name value)

(cl-defstruct hydra_ext_haskell_syntax_import qualified module as spec)

(defvar hydra_ext_haskell_syntax_spec_import-variants (list :list :hiding))

(defvar hydra_ext_haskell_syntax_import_modifier-variants (list :pattern :type))

(cl-defstruct hydra_ext_haskell_syntax_import_export_spec modifier name subspec)

(defvar hydra_ext_haskell_syntax_subspec_import_export_spec-variants (list :all :list))

(defvar hydra_ext_haskell_syntax_literal-variants (list :char :double :float :int :integer :string))

(defvar hydra_ext_haskell_syntax_local_binding-variants (list :signature :value))

(cl-defstruct hydra_ext_haskell_syntax_local_bindings value)

(cl-defstruct hydra_ext_haskell_syntax_module head imports declarations)

(cl-defstruct hydra_ext_haskell_syntax_module_head comments name exports)

(cl-defstruct hydra_ext_haskell_syntax_module_name value)

(defvar hydra_ext_haskell_syntax_name-variants (list :implicit :normal :parens))

(cl-defstruct hydra_ext_haskell_syntax_name_part value)

(defvar hydra_ext_haskell_syntax_operator-variants (list :backtick :normal))

(defvar hydra_ext_haskell_syntax_pattern-variants (list :application :as :list :literal :name :parens :record :tuple :typed :wildcard))

(cl-defstruct hydra_ext_haskell_syntax_application_pattern name args)

(cl-defstruct hydra_ext_haskell_syntax_as_pattern name inner)

(cl-defstruct hydra_ext_haskell_syntax_record_pattern name fields)

(cl-defstruct hydra_ext_haskell_syntax_typed_pattern inner type)

(cl-defstruct hydra_ext_haskell_syntax_pattern_field name pattern)

(cl-defstruct hydra_ext_haskell_syntax_qualified_name qualifiers unqualified)

(cl-defstruct hydra_ext_haskell_syntax_right_hand_side value)

(cl-defstruct hydra_ext_haskell_syntax_statement value)

(defvar hydra_ext_haskell_syntax_type-variants (list :application :ctx :function :infix :list :parens :tuple :variable))

(cl-defstruct hydra_ext_haskell_syntax_application_type context argument)

(cl-defstruct hydra_ext_haskell_syntax_context_type ctx type)

(cl-defstruct hydra_ext_haskell_syntax_function_type domain codomain)

(cl-defstruct hydra_ext_haskell_syntax_infix_type lhs operator rhs)

(cl-defstruct hydra_ext_haskell_syntax_type_declaration name type)

(cl-defstruct hydra_ext_haskell_syntax_type_signature name type)

(cl-defstruct hydra_ext_haskell_syntax_typed_binding type_signature value_binding)

(defvar hydra_ext_haskell_syntax_value_binding-variants (list :simple))

(cl-defstruct hydra_ext_haskell_syntax_simple_value_binding pattern rhs local_bindings)

(cl-defstruct hydra_ext_haskell_syntax_variable value)

(provide 'hydra.ext.haskell.syntax)

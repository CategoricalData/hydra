(require 'cl-lib)

(cl-defstruct hydra_ext_java_syntax_identifier value)

(cl-defstruct hydra_ext_java_syntax_type_identifier value)

(defvar hydra_ext_java_syntax_literal-variants (list :null :integer :floating_point :boolean :character :string))

(cl-defstruct hydra_ext_java_syntax_integer_literal value)

(cl-defstruct hydra_ext_java_syntax_floating_point_literal value)

(cl-defstruct hydra_ext_java_syntax_string_literal value)

(defvar hydra_ext_java_syntax_type-variants (list :primitive :reference))

(cl-defstruct hydra_ext_java_syntax_primitive_type_with_annotations type annotations)

(defvar hydra_ext_java_syntax_primitive_type-variants (list :numeric :boolean))

(defvar hydra_ext_java_syntax_numeric_type-variants (list :integral :floating_point))

(defvar hydra_ext_java_syntax_integral_type-variants (list :byte :short :int :long :char))

(defvar hydra_ext_java_syntax_floating_point_type-variants (list :float :double))

(defvar hydra_ext_java_syntax_reference_type-variants (list :class_or_interface :variable :array))

(defvar hydra_ext_java_syntax_class_or_interface_type-variants (list :class :interface))

(cl-defstruct hydra_ext_java_syntax_class_type annotations qualifier identifier arguments)

(defvar hydra_ext_java_syntax_class_type_qualifier-variants (list :none :package :parent))

(cl-defstruct hydra_ext_java_syntax_interface_type value)

(cl-defstruct hydra_ext_java_syntax_type_variable annotations identifier)

(cl-defstruct hydra_ext_java_syntax_array_type dims variant)

(defvar hydra_ext_java_syntax_array_type_variant-variants (list :primitive :class_or_interface :variable))

(cl-defstruct hydra_ext_java_syntax_dims value)

(cl-defstruct hydra_ext_java_syntax_type_parameter modifiers identifier bound)

(cl-defstruct hydra_ext_java_syntax_type_parameter_modifier value)

(defvar hydra_ext_java_syntax_type_bound-variants (list :variable :class_or_interface))

(cl-defstruct hydra_ext_java_syntax_type_bound_class_or_interface type additional)

(cl-defstruct hydra_ext_java_syntax_additional_bound value)

(defvar hydra_ext_java_syntax_type_argument-variants (list :reference :wildcard))

(cl-defstruct hydra_ext_java_syntax_wildcard annotations wildcard)

(defvar hydra_ext_java_syntax_wildcard_bounds-variants (list :extends :super))

(cl-defstruct hydra_ext_java_syntax_module_name identifier name)

(cl-defstruct hydra_ext_java_syntax_package_name value)

(cl-defstruct hydra_ext_java_syntax_type_name identifier qualifier)

(cl-defstruct hydra_ext_java_syntax_expression_name qualifier identifier)

(cl-defstruct hydra_ext_java_syntax_method_name value)

(cl-defstruct hydra_ext_java_syntax_package_or_type_name value)

(cl-defstruct hydra_ext_java_syntax_ambiguous_name value)

(defvar hydra_ext_java_syntax_compilation_unit-variants (list :ordinary :modular))

(cl-defstruct hydra_ext_java_syntax_ordinary_compilation_unit package imports types)

(cl-defstruct hydra_ext_java_syntax_modular_compilation_unit imports module)

(cl-defstruct hydra_ext_java_syntax_package_declaration modifiers identifiers)

(cl-defstruct hydra_ext_java_syntax_package_modifier value)

(defvar hydra_ext_java_syntax_import_declaration-variants (list :single_type :type_import_on_demand :single_static_import :static_import_on_demand))

(cl-defstruct hydra_ext_java_syntax_single_type_import_declaration value)

(cl-defstruct hydra_ext_java_syntax_type_import_on_demand_declaration value)

(cl-defstruct hydra_ext_java_syntax_single_static_import_declaration type_name identifier)

(cl-defstruct hydra_ext_java_syntax_static_import_on_demand_declaration value)

(defvar hydra_ext_java_syntax_type_declaration-variants (list :class :interface :none))

(cl-defstruct hydra_ext_java_syntax_type_declaration_with_comments value comments)

(cl-defstruct hydra_ext_java_syntax_module_declaration annotations open identifiers directives)

(defvar hydra_ext_java_syntax_module_directive-variants (list :requires :exports :opens :uses :provides))

(cl-defstruct hydra_ext_java_syntax_module_directive_requires modifiers module)

(cl-defstruct hydra_ext_java_syntax_module_directive_exports_or_opens package modules)

(cl-defstruct hydra_ext_java_syntax_module_directive_provides to with)

(defvar hydra_ext_java_syntax_requires_modifier-variants (list :transitive :static))

(defvar hydra_ext_java_syntax_class_declaration-variants (list :normal :enum))

(cl-defstruct hydra_ext_java_syntax_normal_class_declaration modifiers identifier parameters extends implements body)

(defvar hydra_ext_java_syntax_class_modifier-variants (list :annotation :public :protected :private :abstract :static :final :strictfp))

(cl-defstruct hydra_ext_java_syntax_class_body value)

(defvar hydra_ext_java_syntax_class_body_declaration-variants (list :class_member :instance_initializer :static_initializer :constructor_declaration))

(cl-defstruct hydra_ext_java_syntax_class_body_declaration_with_comments value comments)

(defvar hydra_ext_java_syntax_class_member_declaration-variants (list :field :method :class :interface :none))

(cl-defstruct hydra_ext_java_syntax_field_declaration modifiers unann_type variable_declarators)

(defvar hydra_ext_java_syntax_field_modifier-variants (list :annotation :public :protected :private :static :final :transient :volatile))

(cl-defstruct hydra_ext_java_syntax_variable_declarator id initializer)

(cl-defstruct hydra_ext_java_syntax_variable_declarator_id identifier dims)

(defvar hydra_ext_java_syntax_variable_initializer-variants (list :expression :array_initializer))

(cl-defstruct hydra_ext_java_syntax_unann_type value)

(cl-defstruct hydra_ext_java_syntax_unann_class_type value)

(cl-defstruct hydra_ext_java_syntax_method_declaration annotations modifiers header body)

(defvar hydra_ext_java_syntax_method_modifier-variants (list :annotation :public :protected :private :abstract :static :final :synchronized :native :strictfb))

(cl-defstruct hydra_ext_java_syntax_method_header parameters result declarator throws)

(defvar hydra_ext_java_syntax_result-variants (list :type :void))

(cl-defstruct hydra_ext_java_syntax_method_declarator identifier receiver_parameter formal_parameters)

(cl-defstruct hydra_ext_java_syntax_receiver_parameter annotations unann_type identifier)

(defvar hydra_ext_java_syntax_formal_parameter-variants (list :simple :variable_arity))

(cl-defstruct hydra_ext_java_syntax_formal_parameter_simple modifiers type id)

(cl-defstruct hydra_ext_java_syntax_variable_arity_parameter modifiers type annotations identifier)

(defvar hydra_ext_java_syntax_variable_modifier-variants (list :annotation :final))

(cl-defstruct hydra_ext_java_syntax_throws value)

(defvar hydra_ext_java_syntax_exception_type-variants (list :class :variable))

(defvar hydra_ext_java_syntax_method_body-variants (list :block :none))

(cl-defstruct hydra_ext_java_syntax_instance_initializer value)

(cl-defstruct hydra_ext_java_syntax_static_initializer value)

(cl-defstruct hydra_ext_java_syntax_constructor_declaration modifiers constructor throws body)

(defvar hydra_ext_java_syntax_constructor_modifier-variants (list :annotation :public :protected :private))

(cl-defstruct hydra_ext_java_syntax_constructor_declarator parameters name receiver_parameter formal_parameters)

(cl-defstruct hydra_ext_java_syntax_simple_type_name value)

(cl-defstruct hydra_ext_java_syntax_constructor_body invocation statements)

(cl-defstruct hydra_ext_java_syntax_explicit_constructor_invocation type_arguments arguments variant)

(defvar hydra_ext_java_syntax_explicit_constructor_invocation_variant-variants (list :this :super :primary))

(cl-defstruct hydra_ext_java_syntax_enum_declaration modifiers identifier implements body)

(cl-defstruct hydra_ext_java_syntax_enum_body value)

(cl-defstruct hydra_ext_java_syntax_enum_body_element constants body_declarations)

(cl-defstruct hydra_ext_java_syntax_enum_constant modifiers identifier arguments body)

(cl-defstruct hydra_ext_java_syntax_enum_constant_modifier value)

(defvar hydra_ext_java_syntax_interface_declaration-variants (list :normal_interface :annotation_type))

(cl-defstruct hydra_ext_java_syntax_normal_interface_declaration modifiers identifier parameters extends body)

(defvar hydra_ext_java_syntax_interface_modifier-variants (list :annotation :public :protected :private :abstract :static :strictfb))

(cl-defstruct hydra_ext_java_syntax_interface_body value)

(defvar hydra_ext_java_syntax_interface_member_declaration-variants (list :constant :interface_method :class :interface))

(cl-defstruct hydra_ext_java_syntax_constant_declaration modifiers type variables)

(defvar hydra_ext_java_syntax_constant_modifier-variants (list :annotation :public :static :final))

(cl-defstruct hydra_ext_java_syntax_interface_method_declaration modifiers header body)

(defvar hydra_ext_java_syntax_interface_method_modifier-variants (list :annotation :public :private :abstract :default :static :strictfp))

(cl-defstruct hydra_ext_java_syntax_annotation_type_declaration modifiers identifier body)

(cl-defstruct hydra_ext_java_syntax_annotation_type_body value)

(defvar hydra_ext_java_syntax_annotation_type_member_declaration-variants (list :annotation_type :constant :class :interface))

(cl-defstruct hydra_ext_java_syntax_annotation_type_element_declaration modifiers type identifier dims default)

(defvar hydra_ext_java_syntax_annotation_type_element_modifier-variants (list :public :abstract))

(cl-defstruct hydra_ext_java_syntax_default_value value)

(defvar hydra_ext_java_syntax_annotation-variants (list :normal :marker :single_element))

(cl-defstruct hydra_ext_java_syntax_normal_annotation type_name pairs)

(cl-defstruct hydra_ext_java_syntax_element_value_pair key value)

(defvar hydra_ext_java_syntax_element_value-variants (list :conditional_expression :element_value_array_initializer :annotation))

(cl-defstruct hydra_ext_java_syntax_element_value_array_initializer value)

(cl-defstruct hydra_ext_java_syntax_marker_annotation value)

(cl-defstruct hydra_ext_java_syntax_single_element_annotation name value)

(cl-defstruct hydra_ext_java_syntax_array_initializer value)

(cl-defstruct hydra_ext_java_syntax_block value)

(defvar hydra_ext_java_syntax_block_statement-variants (list :local_variable_declaration :class :statement))

(cl-defstruct hydra_ext_java_syntax_local_variable_declaration_statement value)

(cl-defstruct hydra_ext_java_syntax_local_variable_declaration modifiers type declarators)

(defvar hydra_ext_java_syntax_local_variable_type-variants (list :type :var))

(defvar hydra_ext_java_syntax_statement-variants (list :without_trailing :labeled :if_then :if_then_else :while :for))

(defvar hydra_ext_java_syntax_statement_no_short_if-variants (list :without_trailing :labeled :if_then_else :while :for))

(defvar hydra_ext_java_syntax_statement_without_trailing_substatement-variants (list :block :empty :expression :assert :switch :do :break :continue :return :synchronized :throw :try))

(cl-defstruct hydra_ext_java_syntax_labeled_statement identifier statement)

(cl-defstruct hydra_ext_java_syntax_labeled_statement_no_short_if identifier statement)

(cl-defstruct hydra_ext_java_syntax_expression_statement value)

(defvar hydra_ext_java_syntax_statement_expression-variants (list :assignment :pre_increment :pre_decrement :post_increment :post_decrement :method_invocation :class_instance_creation))

(cl-defstruct hydra_ext_java_syntax_if_then_statement expression statement)

(cl-defstruct hydra_ext_java_syntax_if_then_else_statement cond then else)

(cl-defstruct hydra_ext_java_syntax_if_then_else_statement_no_short_if cond then else)

(defvar hydra_ext_java_syntax_assert_statement-variants (list :single :pair))

(cl-defstruct hydra_ext_java_syntax_assert_statement_pair first second)

(cl-defstruct hydra_ext_java_syntax_switch_statement cond block)

(cl-defstruct hydra_ext_java_syntax_switch_block value)

(cl-defstruct hydra_ext_java_syntax_switch_block_pair statements labels)

(cl-defstruct hydra_ext_java_syntax_switch_block_statement_group labels statements)

(defvar hydra_ext_java_syntax_switch_label-variants (list :constant :enum_constant :default))

(cl-defstruct hydra_ext_java_syntax_enum_constant_name value)

(cl-defstruct hydra_ext_java_syntax_while_statement cond body)

(cl-defstruct hydra_ext_java_syntax_while_statement_no_short_if cond body)

(cl-defstruct hydra_ext_java_syntax_do_statement body conde)

(defvar hydra_ext_java_syntax_for_statement-variants (list :basic :enhanced))

(defvar hydra_ext_java_syntax_for_statement_no_short_if-variants (list :basic :enhanced))

(cl-defstruct hydra_ext_java_syntax_basic_for_statement cond body)

(cl-defstruct hydra_ext_java_syntax_for_cond init cond update)

(cl-defstruct hydra_ext_java_syntax_basic_for_statement_no_short_if cond body)

(defvar hydra_ext_java_syntax_for_init-variants (list :statements :local_variable))

(cl-defstruct hydra_ext_java_syntax_for_update value)

(cl-defstruct hydra_ext_java_syntax_enhanced_for_statement cond body)

(cl-defstruct hydra_ext_java_syntax_enhanced_for_cond modifiers type id expression)

(cl-defstruct hydra_ext_java_syntax_enhanced_for_statement_no_short_if cond body)

(cl-defstruct hydra_ext_java_syntax_break_statement value)

(cl-defstruct hydra_ext_java_syntax_continue_statement value)

(cl-defstruct hydra_ext_java_syntax_return_statement value)

(cl-defstruct hydra_ext_java_syntax_throw_statement value)

(cl-defstruct hydra_ext_java_syntax_synchronized_statement expression block)

(defvar hydra_ext_java_syntax_try_statement-variants (list :simple :with_finally :with_resources))

(cl-defstruct hydra_ext_java_syntax_try_statement_simple block catches)

(cl-defstruct hydra_ext_java_syntax_try_statement_with_finally block catches finally)

(cl-defstruct hydra_ext_java_syntax_catches value)

(cl-defstruct hydra_ext_java_syntax_catch_clause parameter block)

(cl-defstruct hydra_ext_java_syntax_catch_formal_parameter modifiers type id)

(cl-defstruct hydra_ext_java_syntax_catch_type type types)

(cl-defstruct hydra_ext_java_syntax_finally value)

(cl-defstruct hydra_ext_java_syntax_try_with_resources_statement resource_specification block catches finally)

(cl-defstruct hydra_ext_java_syntax_resource_specification value)

(defvar hydra_ext_java_syntax_resource-variants (list :local :variable))

(cl-defstruct hydra_ext_java_syntax_resource_local modifiers type identifier expression)

(defvar hydra_ext_java_syntax_variable_access-variants (list :expression_name :field_access))

(defvar hydra_ext_java_syntax_primary-variants (list :no_new_array :array_creation))

(defvar hydra_ext_java_syntax_primary_no_new_array_expression-variants (list :literal :class_literal :this :dot_this :parens :class_instance :field_access :array_access :method_invocation :method_reference))

(defvar hydra_ext_java_syntax_class_literal-variants (list :type :numeric_type :boolean :void))

(defvar hydra_ext_java_syntax_type_name_array-variants (list :simple :array))

(defvar hydra_ext_java_syntax_numeric_type_array-variants (list :simple :array))

(defvar hydra_ext_java_syntax_boolean_array-variants (list :simple :array))

(cl-defstruct hydra_ext_java_syntax_class_instance_creation_expression qualifier expression)

(defvar hydra_ext_java_syntax_class_instance_creation_expression_qualifier-variants (list :expression :primary))

(cl-defstruct hydra_ext_java_syntax_unqualified_class_instance_creation_expression type_arguments class_or_interface arguments body)

(cl-defstruct hydra_ext_java_syntax_class_or_interface_type_to_instantiate identifiers type_arguments)

(cl-defstruct hydra_ext_java_syntax_annotated_identifier annotations identifier)

(defvar hydra_ext_java_syntax_type_arguments_or_diamond-variants (list :arguments :diamond))

(cl-defstruct hydra_ext_java_syntax_field_access qualifier identifier)

(defvar hydra_ext_java_syntax_field_access_qualifier-variants (list :primary :super :typed))

(cl-defstruct hydra_ext_java_syntax_array_access expression variant)

(defvar hydra_ext_java_syntax_array_access_variant-variants (list :name :primary))

(cl-defstruct hydra_ext_java_syntax_method_invocation header arguments)

(defvar hydra_ext_java_syntax_method_invocation_header-variants (list :simple :complex))

(cl-defstruct hydra_ext_java_syntax_method_invocation_complex variant type_arguments identifier)

(defvar hydra_ext_java_syntax_method_invocation_variant-variants (list :type :expression :primary :super :type_super))

(defvar hydra_ext_java_syntax_method_reference-variants (list :expression :primary :reference_type :super :new :array))

(cl-defstruct hydra_ext_java_syntax_method_reference_expression name type_arguments identifier)

(cl-defstruct hydra_ext_java_syntax_method_reference_primary primary type_arguments identifier)

(cl-defstruct hydra_ext_java_syntax_method_reference_reference_type reference_type type_arguments identifier)

(cl-defstruct hydra_ext_java_syntax_method_reference_super type_arguments identifier super)

(cl-defstruct hydra_ext_java_syntax_method_reference_new class_type type_arguments)

(cl-defstruct hydra_ext_java_syntax_method_reference_array value)

(defvar hydra_ext_java_syntax_array_creation_expression-variants (list :primitive :class_or_interface :primitive_array :class_or_interface_array))

(cl-defstruct hydra_ext_java_syntax_array_creation_expression_primitive type dim_exprs dims)

(cl-defstruct hydra_ext_java_syntax_array_creation_expression_class_or_interface type dim_exprs dims)

(cl-defstruct hydra_ext_java_syntax_array_creation_expression_primitive_array type dims array)

(cl-defstruct hydra_ext_java_syntax_array_creation_expression_class_or_interface_array type dims array)

(cl-defstruct hydra_ext_java_syntax_dim_expr annotations expression)

(defvar hydra_ext_java_syntax_expression-variants (list :lambda :assignment))

(cl-defstruct hydra_ext_java_syntax_lambda_expression parameters body)

(defvar hydra_ext_java_syntax_lambda_parameters-variants (list :tuple :single))

(defvar hydra_ext_java_syntax_lambda_parameter-variants (list :normal :variable_arity))

(cl-defstruct hydra_ext_java_syntax_lambda_parameter_normal modifiers type id)

(defvar hydra_ext_java_syntax_lambda_parameter_type-variants (list :type :var))

(defvar hydra_ext_java_syntax_lambda_body-variants (list :expression :block))

(defvar hydra_ext_java_syntax_assignment_expression-variants (list :conditional :assignment))

(cl-defstruct hydra_ext_java_syntax_assignment lhs op expression)

(defvar hydra_ext_java_syntax_left_hand_side-variants (list :expression_name :field_access :array_access))

(defvar hydra_ext_java_syntax_assignment_operator-variants (list :simple :times :div :mod :plus :minus :shift_left :shift_right :shift_right_zero_fill :and :xor :or))

(defvar hydra_ext_java_syntax_conditional_expression-variants (list :simple :ternary_cond :ternary_lambda))

(cl-defstruct hydra_ext_java_syntax_conditional_expression_ternary_cond cond if_true if_false)

(cl-defstruct hydra_ext_java_syntax_conditional_expression_ternary_lambda cond if_true if_false)

(cl-defstruct hydra_ext_java_syntax_conditional_or_expression value)

(cl-defstruct hydra_ext_java_syntax_conditional_and_expression value)

(cl-defstruct hydra_ext_java_syntax_inclusive_or_expression value)

(cl-defstruct hydra_ext_java_syntax_exclusive_or_expression value)

(cl-defstruct hydra_ext_java_syntax_and_expression value)

(defvar hydra_ext_java_syntax_equality_expression-variants (list :unary :equal :not_equal))

(cl-defstruct hydra_ext_java_syntax_equality_expression_binary lhs rhs)

(defvar hydra_ext_java_syntax_relational_expression-variants (list :simple :less_than :greater_than :less_than_equal :greater_than_equal :instanceof))

(cl-defstruct hydra_ext_java_syntax_relational_expression_less_than lhs rhs)

(cl-defstruct hydra_ext_java_syntax_relational_expression_greater_than lhs rhs)

(cl-defstruct hydra_ext_java_syntax_relational_expression_less_than_equal lhs rhs)

(cl-defstruct hydra_ext_java_syntax_relational_expression_greater_than_equal lhs rhs)

(cl-defstruct hydra_ext_java_syntax_relational_expression_instance_of lhs rhs)

(defvar hydra_ext_java_syntax_shift_expression-variants (list :unary :shift_left :shift_right :shift_right_zero_fill))

(cl-defstruct hydra_ext_java_syntax_shift_expression_binary lhs rhs)

(defvar hydra_ext_java_syntax_additive_expression-variants (list :unary :plus :minus))

(cl-defstruct hydra_ext_java_syntax_additive_expression_binary lhs rhs)

(defvar hydra_ext_java_syntax_multiplicative_expression-variants (list :unary :times :divide :mod))

(cl-defstruct hydra_ext_java_syntax_multiplicative_expression_binary lhs rhs)

(defvar hydra_ext_java_syntax_unary_expression-variants (list :pre_increment :pre_decrement :plus :minus :other))

(cl-defstruct hydra_ext_java_syntax_pre_increment_expression value)

(cl-defstruct hydra_ext_java_syntax_pre_decrement_expression value)

(defvar hydra_ext_java_syntax_unary_expression_not_plus_minus-variants (list :postfix :tilde :not :cast))

(defvar hydra_ext_java_syntax_postfix_expression-variants (list :primary :name :post_increment :post_decrement))

(cl-defstruct hydra_ext_java_syntax_post_increment_expression value)

(cl-defstruct hydra_ext_java_syntax_post_decrement_expression value)

(defvar hydra_ext_java_syntax_cast_expression-variants (list :primitive :not_plus_minus :lambda))

(cl-defstruct hydra_ext_java_syntax_cast_expression_primitive type expression)

(cl-defstruct hydra_ext_java_syntax_cast_expression_not_plus_minus ref_and_bounds expression)

(cl-defstruct hydra_ext_java_syntax_cast_expression_lambda ref_and_bounds expression)

(cl-defstruct hydra_ext_java_syntax_cast_expression_ref_and_bounds type bounds)

(cl-defstruct hydra_ext_java_syntax_constant_expression value)

(provide 'hydra.ext.java.syntax)

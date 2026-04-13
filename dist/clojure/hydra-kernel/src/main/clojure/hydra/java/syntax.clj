(ns hydra.java.syntax)

(declare hydra_java_syntax_literal-variants hydra_java_syntax_type-variants hydra_java_syntax_primitive_type-variants hydra_java_syntax_numeric_type-variants hydra_java_syntax_integral_type-variants hydra_java_syntax_floating_point_type-variants hydra_java_syntax_reference_type-variants hydra_java_syntax_class_or_interface_type-variants hydra_java_syntax_class_type_qualifier-variants hydra_java_syntax_array_type_variant-variants hydra_java_syntax_type_bound-variants hydra_java_syntax_type_argument-variants hydra_java_syntax_wildcard_bounds-variants hydra_java_syntax_compilation_unit-variants hydra_java_syntax_import_declaration-variants hydra_java_syntax_type_declaration-variants hydra_java_syntax_module_directive-variants hydra_java_syntax_requires_modifier-variants hydra_java_syntax_class_declaration-variants hydra_java_syntax_class_modifier-variants hydra_java_syntax_class_body_declaration-variants hydra_java_syntax_class_member_declaration-variants hydra_java_syntax_field_modifier-variants hydra_java_syntax_variable_initializer-variants hydra_java_syntax_method_modifier-variants hydra_java_syntax_result-variants hydra_java_syntax_formal_parameter-variants hydra_java_syntax_variable_modifier-variants hydra_java_syntax_exception_type-variants hydra_java_syntax_method_body-variants hydra_java_syntax_constructor_modifier-variants hydra_java_syntax_explicit_constructor_invocation_variant-variants hydra_java_syntax_interface_declaration-variants hydra_java_syntax_interface_modifier-variants hydra_java_syntax_interface_member_declaration-variants hydra_java_syntax_constant_modifier-variants hydra_java_syntax_interface_method_modifier-variants hydra_java_syntax_annotation_type_member_declaration-variants hydra_java_syntax_annotation_type_element_modifier-variants hydra_java_syntax_annotation-variants hydra_java_syntax_element_value-variants hydra_java_syntax_block_statement-variants hydra_java_syntax_local_variable_type-variants hydra_java_syntax_statement-variants hydra_java_syntax_statement_no_short_if-variants hydra_java_syntax_statement_without_trailing_substatement-variants hydra_java_syntax_statement_expression-variants hydra_java_syntax_assert_statement-variants hydra_java_syntax_switch_label-variants hydra_java_syntax_for_statement-variants hydra_java_syntax_for_statement_no_short_if-variants hydra_java_syntax_for_init-variants hydra_java_syntax_try_statement-variants hydra_java_syntax_resource-variants hydra_java_syntax_variable_access-variants hydra_java_syntax_primary-variants hydra_java_syntax_primary_no_new_array_expression-variants hydra_java_syntax_class_literal-variants hydra_java_syntax_type_name_array-variants hydra_java_syntax_numeric_type_array-variants hydra_java_syntax_boolean_array-variants hydra_java_syntax_class_instance_creation_expression_qualifier-variants hydra_java_syntax_type_arguments_or_diamond-variants hydra_java_syntax_field_access_qualifier-variants hydra_java_syntax_array_access_variant-variants hydra_java_syntax_method_invocation_header-variants hydra_java_syntax_method_invocation_variant-variants hydra_java_syntax_method_reference-variants hydra_java_syntax_array_creation_expression-variants hydra_java_syntax_expression-variants hydra_java_syntax_lambda_parameters-variants hydra_java_syntax_lambda_parameter-variants hydra_java_syntax_lambda_parameter_type-variants hydra_java_syntax_lambda_body-variants hydra_java_syntax_assignment_expression-variants hydra_java_syntax_left_hand_side-variants hydra_java_syntax_assignment_operator-variants hydra_java_syntax_conditional_expression-variants hydra_java_syntax_equality_expression-variants hydra_java_syntax_relational_expression-variants hydra_java_syntax_shift_expression-variants hydra_java_syntax_additive_expression-variants hydra_java_syntax_multiplicative_expression-variants hydra_java_syntax_unary_expression-variants hydra_java_syntax_unary_expression_not_plus_minus-variants hydra_java_syntax_postfix_expression-variants hydra_java_syntax_cast_expression-variants)

(defrecord hydra_java_syntax_identifier [value])
(defn make-hydra_java_syntax_identifier [value] (->hydra_java_syntax_identifier value))

(defrecord hydra_java_syntax_type_identifier [value])
(defn make-hydra_java_syntax_type_identifier [value] (->hydra_java_syntax_type_identifier value))

(def hydra_java_syntax_literal-variants (list :null :integer :floating_point :boolean :character :string))

(defrecord hydra_java_syntax_integer_literal [value])
(defn make-hydra_java_syntax_integer_literal [value] (->hydra_java_syntax_integer_literal value))

(defrecord hydra_java_syntax_floating_point_literal [value])
(defn make-hydra_java_syntax_floating_point_literal [value] (->hydra_java_syntax_floating_point_literal value))

(defrecord hydra_java_syntax_string_literal [value])
(defn make-hydra_java_syntax_string_literal [value] (->hydra_java_syntax_string_literal value))

(def hydra_java_syntax_type-variants (list :primitive :reference))

(defrecord hydra_java_syntax_primitive_type_with_annotations [type annotations])
(defn make-hydra_java_syntax_primitive_type_with_annotations [type annotations] (->hydra_java_syntax_primitive_type_with_annotations type annotations))

(def hydra_java_syntax_primitive_type-variants (list :numeric :boolean))

(def hydra_java_syntax_numeric_type-variants (list :integral :floating_point))

(def hydra_java_syntax_integral_type-variants (list :byte :short :int :long :char))

(def hydra_java_syntax_floating_point_type-variants (list :float :double))

(def hydra_java_syntax_reference_type-variants (list :class_or_interface :variable :array))

(def hydra_java_syntax_class_or_interface_type-variants (list :class :interface))

(defrecord hydra_java_syntax_class_type [annotations qualifier identifier arguments])
(defn make-hydra_java_syntax_class_type [annotations qualifier identifier arguments] (->hydra_java_syntax_class_type annotations qualifier identifier arguments))

(def hydra_java_syntax_class_type_qualifier-variants (list :none :package :parent))

(defrecord hydra_java_syntax_interface_type [value])
(defn make-hydra_java_syntax_interface_type [value] (->hydra_java_syntax_interface_type value))

(defrecord hydra_java_syntax_type_variable [annotations identifier])
(defn make-hydra_java_syntax_type_variable [annotations identifier] (->hydra_java_syntax_type_variable annotations identifier))

(defrecord hydra_java_syntax_array_type [dims variant])
(defn make-hydra_java_syntax_array_type [dims variant] (->hydra_java_syntax_array_type dims variant))

(def hydra_java_syntax_array_type_variant-variants (list :primitive :class_or_interface :variable))

(defrecord hydra_java_syntax_dims [value])
(defn make-hydra_java_syntax_dims [value] (->hydra_java_syntax_dims value))

(defrecord hydra_java_syntax_type_parameter [modifiers identifier bound])
(defn make-hydra_java_syntax_type_parameter [modifiers identifier bound] (->hydra_java_syntax_type_parameter modifiers identifier bound))

(defrecord hydra_java_syntax_type_parameter_modifier [value])
(defn make-hydra_java_syntax_type_parameter_modifier [value] (->hydra_java_syntax_type_parameter_modifier value))

(def hydra_java_syntax_type_bound-variants (list :variable :class_or_interface))

(defrecord hydra_java_syntax_type_bound_class_or_interface [type additional])
(defn make-hydra_java_syntax_type_bound_class_or_interface [type additional] (->hydra_java_syntax_type_bound_class_or_interface type additional))

(defrecord hydra_java_syntax_additional_bound [value])
(defn make-hydra_java_syntax_additional_bound [value] (->hydra_java_syntax_additional_bound value))

(def hydra_java_syntax_type_argument-variants (list :reference :wildcard))

(defrecord hydra_java_syntax_wildcard [annotations wildcard])
(defn make-hydra_java_syntax_wildcard [annotations wildcard] (->hydra_java_syntax_wildcard annotations wildcard))

(def hydra_java_syntax_wildcard_bounds-variants (list :extends :super))

(defrecord hydra_java_syntax_module_name [identifier name])
(defn make-hydra_java_syntax_module_name [identifier name] (->hydra_java_syntax_module_name identifier name))

(defrecord hydra_java_syntax_package_name [value])
(defn make-hydra_java_syntax_package_name [value] (->hydra_java_syntax_package_name value))

(defrecord hydra_java_syntax_type_name [identifier qualifier])
(defn make-hydra_java_syntax_type_name [identifier qualifier] (->hydra_java_syntax_type_name identifier qualifier))

(defrecord hydra_java_syntax_expression_name [qualifier identifier])
(defn make-hydra_java_syntax_expression_name [qualifier identifier] (->hydra_java_syntax_expression_name qualifier identifier))

(defrecord hydra_java_syntax_method_name [value])
(defn make-hydra_java_syntax_method_name [value] (->hydra_java_syntax_method_name value))

(defrecord hydra_java_syntax_package_or_type_name [value])
(defn make-hydra_java_syntax_package_or_type_name [value] (->hydra_java_syntax_package_or_type_name value))

(defrecord hydra_java_syntax_ambiguous_name [value])
(defn make-hydra_java_syntax_ambiguous_name [value] (->hydra_java_syntax_ambiguous_name value))

(def hydra_java_syntax_compilation_unit-variants (list :ordinary :modular))

(defrecord hydra_java_syntax_ordinary_compilation_unit [package imports types])
(defn make-hydra_java_syntax_ordinary_compilation_unit [package imports types] (->hydra_java_syntax_ordinary_compilation_unit package imports types))

(defrecord hydra_java_syntax_modular_compilation_unit [imports module])
(defn make-hydra_java_syntax_modular_compilation_unit [imports module] (->hydra_java_syntax_modular_compilation_unit imports module))

(defrecord hydra_java_syntax_package_declaration [modifiers identifiers])
(defn make-hydra_java_syntax_package_declaration [modifiers identifiers] (->hydra_java_syntax_package_declaration modifiers identifiers))

(defrecord hydra_java_syntax_package_modifier [value])
(defn make-hydra_java_syntax_package_modifier [value] (->hydra_java_syntax_package_modifier value))

(def hydra_java_syntax_import_declaration-variants (list :single_type :type_import_on_demand :single_static_import :static_import_on_demand))

(defrecord hydra_java_syntax_single_type_import_declaration [value])
(defn make-hydra_java_syntax_single_type_import_declaration [value] (->hydra_java_syntax_single_type_import_declaration value))

(defrecord hydra_java_syntax_type_import_on_demand_declaration [value])
(defn make-hydra_java_syntax_type_import_on_demand_declaration [value] (->hydra_java_syntax_type_import_on_demand_declaration value))

(defrecord hydra_java_syntax_single_static_import_declaration [type_name identifier])
(defn make-hydra_java_syntax_single_static_import_declaration [type_name identifier] (->hydra_java_syntax_single_static_import_declaration type_name identifier))

(defrecord hydra_java_syntax_static_import_on_demand_declaration [value])
(defn make-hydra_java_syntax_static_import_on_demand_declaration [value] (->hydra_java_syntax_static_import_on_demand_declaration value))

(def hydra_java_syntax_type_declaration-variants (list :class :interface :none))

(defrecord hydra_java_syntax_type_declaration_with_comments [value comments])
(defn make-hydra_java_syntax_type_declaration_with_comments [value comments] (->hydra_java_syntax_type_declaration_with_comments value comments))

(defrecord hydra_java_syntax_module_declaration [annotations open identifiers directives])
(defn make-hydra_java_syntax_module_declaration [annotations open identifiers directives] (->hydra_java_syntax_module_declaration annotations open identifiers directives))

(def hydra_java_syntax_module_directive-variants (list :requires :exports :opens :uses :provides))

(defrecord hydra_java_syntax_module_directive_requires [modifiers module])
(defn make-hydra_java_syntax_module_directive_requires [modifiers module] (->hydra_java_syntax_module_directive_requires modifiers module))

(defrecord hydra_java_syntax_module_directive_exports_or_opens [package modules])
(defn make-hydra_java_syntax_module_directive_exports_or_opens [package modules] (->hydra_java_syntax_module_directive_exports_or_opens package modules))

(defrecord hydra_java_syntax_module_directive_provides [to with])
(defn make-hydra_java_syntax_module_directive_provides [to with] (->hydra_java_syntax_module_directive_provides to with))

(def hydra_java_syntax_requires_modifier-variants (list :transitive :static))

(def hydra_java_syntax_class_declaration-variants (list :normal :enum))

(defrecord hydra_java_syntax_normal_class_declaration [modifiers identifier parameters extends implements body])
(defn make-hydra_java_syntax_normal_class_declaration [modifiers identifier parameters extends implements body] (->hydra_java_syntax_normal_class_declaration modifiers identifier parameters extends implements body))

(def hydra_java_syntax_class_modifier-variants (list :annotation :public :protected :private :abstract :static :final :strictfp))

(defrecord hydra_java_syntax_class_body [value])
(defn make-hydra_java_syntax_class_body [value] (->hydra_java_syntax_class_body value))

(def hydra_java_syntax_class_body_declaration-variants (list :class_member :instance_initializer :static_initializer :constructor_declaration))

(defrecord hydra_java_syntax_class_body_declaration_with_comments [value comments])
(defn make-hydra_java_syntax_class_body_declaration_with_comments [value comments] (->hydra_java_syntax_class_body_declaration_with_comments value comments))

(def hydra_java_syntax_class_member_declaration-variants (list :field :method :class :interface :none))

(defrecord hydra_java_syntax_field_declaration [modifiers unann_type variable_declarators])
(defn make-hydra_java_syntax_field_declaration [modifiers unann_type variable_declarators] (->hydra_java_syntax_field_declaration modifiers unann_type variable_declarators))

(def hydra_java_syntax_field_modifier-variants (list :annotation :public :protected :private :static :final :transient :volatile))

(defrecord hydra_java_syntax_variable_declarator [id initializer])
(defn make-hydra_java_syntax_variable_declarator [id initializer] (->hydra_java_syntax_variable_declarator id initializer))

(defrecord hydra_java_syntax_variable_declarator_id [identifier dims])
(defn make-hydra_java_syntax_variable_declarator_id [identifier dims] (->hydra_java_syntax_variable_declarator_id identifier dims))

(def hydra_java_syntax_variable_initializer-variants (list :expression :array_initializer))

(defrecord hydra_java_syntax_unann_type [value])
(defn make-hydra_java_syntax_unann_type [value] (->hydra_java_syntax_unann_type value))

(defrecord hydra_java_syntax_unann_class_type [value])
(defn make-hydra_java_syntax_unann_class_type [value] (->hydra_java_syntax_unann_class_type value))

(defrecord hydra_java_syntax_method_declaration [annotations modifiers header body])
(defn make-hydra_java_syntax_method_declaration [annotations modifiers header body] (->hydra_java_syntax_method_declaration annotations modifiers header body))

(def hydra_java_syntax_method_modifier-variants (list :annotation :public :protected :private :abstract :static :final :synchronized :native :strictfb))

(defrecord hydra_java_syntax_method_header [parameters result declarator throws])
(defn make-hydra_java_syntax_method_header [parameters result declarator throws] (->hydra_java_syntax_method_header parameters result declarator throws))

(def hydra_java_syntax_result-variants (list :type :void))

(defrecord hydra_java_syntax_method_declarator [identifier receiver_parameter formal_parameters])
(defn make-hydra_java_syntax_method_declarator [identifier receiver_parameter formal_parameters] (->hydra_java_syntax_method_declarator identifier receiver_parameter formal_parameters))

(defrecord hydra_java_syntax_receiver_parameter [annotations unann_type identifier])
(defn make-hydra_java_syntax_receiver_parameter [annotations unann_type identifier] (->hydra_java_syntax_receiver_parameter annotations unann_type identifier))

(def hydra_java_syntax_formal_parameter-variants (list :simple :variable_arity))

(defrecord hydra_java_syntax_formal_parameter_simple [modifiers type id])
(defn make-hydra_java_syntax_formal_parameter_simple [modifiers type id] (->hydra_java_syntax_formal_parameter_simple modifiers type id))

(defrecord hydra_java_syntax_variable_arity_parameter [modifiers type annotations identifier])
(defn make-hydra_java_syntax_variable_arity_parameter [modifiers type annotations identifier] (->hydra_java_syntax_variable_arity_parameter modifiers type annotations identifier))

(def hydra_java_syntax_variable_modifier-variants (list :annotation :final))

(defrecord hydra_java_syntax_throws [value])
(defn make-hydra_java_syntax_throws [value] (->hydra_java_syntax_throws value))

(def hydra_java_syntax_exception_type-variants (list :class :variable))

(def hydra_java_syntax_method_body-variants (list :block :none))

(defrecord hydra_java_syntax_instance_initializer [value])
(defn make-hydra_java_syntax_instance_initializer [value] (->hydra_java_syntax_instance_initializer value))

(defrecord hydra_java_syntax_static_initializer [value])
(defn make-hydra_java_syntax_static_initializer [value] (->hydra_java_syntax_static_initializer value))

(defrecord hydra_java_syntax_constructor_declaration [modifiers constructor throws body])
(defn make-hydra_java_syntax_constructor_declaration [modifiers constructor throws body] (->hydra_java_syntax_constructor_declaration modifiers constructor throws body))

(def hydra_java_syntax_constructor_modifier-variants (list :annotation :public :protected :private))

(defrecord hydra_java_syntax_constructor_declarator [parameters name receiver_parameter formal_parameters])
(defn make-hydra_java_syntax_constructor_declarator [parameters name receiver_parameter formal_parameters] (->hydra_java_syntax_constructor_declarator parameters name receiver_parameter formal_parameters))

(defrecord hydra_java_syntax_simple_type_name [value])
(defn make-hydra_java_syntax_simple_type_name [value] (->hydra_java_syntax_simple_type_name value))

(defrecord hydra_java_syntax_constructor_body [invocation statements])
(defn make-hydra_java_syntax_constructor_body [invocation statements] (->hydra_java_syntax_constructor_body invocation statements))

(defrecord hydra_java_syntax_explicit_constructor_invocation [type_arguments arguments variant])
(defn make-hydra_java_syntax_explicit_constructor_invocation [type_arguments arguments variant] (->hydra_java_syntax_explicit_constructor_invocation type_arguments arguments variant))

(def hydra_java_syntax_explicit_constructor_invocation_variant-variants (list :this :super :primary))

(defrecord hydra_java_syntax_enum_declaration [modifiers identifier implements body])
(defn make-hydra_java_syntax_enum_declaration [modifiers identifier implements body] (->hydra_java_syntax_enum_declaration modifiers identifier implements body))

(defrecord hydra_java_syntax_enum_body [value])
(defn make-hydra_java_syntax_enum_body [value] (->hydra_java_syntax_enum_body value))

(defrecord hydra_java_syntax_enum_body_element [constants body_declarations])
(defn make-hydra_java_syntax_enum_body_element [constants body_declarations] (->hydra_java_syntax_enum_body_element constants body_declarations))

(defrecord hydra_java_syntax_enum_constant [modifiers identifier arguments body])
(defn make-hydra_java_syntax_enum_constant [modifiers identifier arguments body] (->hydra_java_syntax_enum_constant modifiers identifier arguments body))

(defrecord hydra_java_syntax_enum_constant_modifier [value])
(defn make-hydra_java_syntax_enum_constant_modifier [value] (->hydra_java_syntax_enum_constant_modifier value))

(def hydra_java_syntax_interface_declaration-variants (list :normal_interface :annotation_type))

(defrecord hydra_java_syntax_normal_interface_declaration [modifiers identifier parameters extends body])
(defn make-hydra_java_syntax_normal_interface_declaration [modifiers identifier parameters extends body] (->hydra_java_syntax_normal_interface_declaration modifiers identifier parameters extends body))

(def hydra_java_syntax_interface_modifier-variants (list :annotation :public :protected :private :abstract :static :strictfb))

(defrecord hydra_java_syntax_interface_body [value])
(defn make-hydra_java_syntax_interface_body [value] (->hydra_java_syntax_interface_body value))

(def hydra_java_syntax_interface_member_declaration-variants (list :constant :interface_method :class :interface))

(defrecord hydra_java_syntax_constant_declaration [modifiers type variables])
(defn make-hydra_java_syntax_constant_declaration [modifiers type variables] (->hydra_java_syntax_constant_declaration modifiers type variables))

(def hydra_java_syntax_constant_modifier-variants (list :annotation :public :static :final))

(defrecord hydra_java_syntax_interface_method_declaration [modifiers header body])
(defn make-hydra_java_syntax_interface_method_declaration [modifiers header body] (->hydra_java_syntax_interface_method_declaration modifiers header body))

(def hydra_java_syntax_interface_method_modifier-variants (list :annotation :public :private :abstract :default :static :strictfp))

(defrecord hydra_java_syntax_annotation_type_declaration [modifiers identifier body])
(defn make-hydra_java_syntax_annotation_type_declaration [modifiers identifier body] (->hydra_java_syntax_annotation_type_declaration modifiers identifier body))

(defrecord hydra_java_syntax_annotation_type_body [value])
(defn make-hydra_java_syntax_annotation_type_body [value] (->hydra_java_syntax_annotation_type_body value))

(def hydra_java_syntax_annotation_type_member_declaration-variants (list :annotation_type :constant :class :interface))

(defrecord hydra_java_syntax_annotation_type_element_declaration [modifiers type identifier dims default])
(defn make-hydra_java_syntax_annotation_type_element_declaration [modifiers type identifier dims default] (->hydra_java_syntax_annotation_type_element_declaration modifiers type identifier dims default))

(def hydra_java_syntax_annotation_type_element_modifier-variants (list :public :abstract))

(defrecord hydra_java_syntax_default_value [value])
(defn make-hydra_java_syntax_default_value [value] (->hydra_java_syntax_default_value value))

(def hydra_java_syntax_annotation-variants (list :normal :marker :single_element))

(defrecord hydra_java_syntax_normal_annotation [type_name pairs])
(defn make-hydra_java_syntax_normal_annotation [type_name pairs] (->hydra_java_syntax_normal_annotation type_name pairs))

(defrecord hydra_java_syntax_element_value_pair [key value])
(defn make-hydra_java_syntax_element_value_pair [key value] (->hydra_java_syntax_element_value_pair key value))

(def hydra_java_syntax_element_value-variants (list :conditional_expression :element_value_array_initializer :annotation))

(defrecord hydra_java_syntax_element_value_array_initializer [value])
(defn make-hydra_java_syntax_element_value_array_initializer [value] (->hydra_java_syntax_element_value_array_initializer value))

(defrecord hydra_java_syntax_marker_annotation [value])
(defn make-hydra_java_syntax_marker_annotation [value] (->hydra_java_syntax_marker_annotation value))

(defrecord hydra_java_syntax_single_element_annotation [name value])
(defn make-hydra_java_syntax_single_element_annotation [name value] (->hydra_java_syntax_single_element_annotation name value))

(defrecord hydra_java_syntax_array_initializer [value])
(defn make-hydra_java_syntax_array_initializer [value] (->hydra_java_syntax_array_initializer value))

(defrecord hydra_java_syntax_block [value])
(defn make-hydra_java_syntax_block [value] (->hydra_java_syntax_block value))

(def hydra_java_syntax_block_statement-variants (list :local_variable_declaration :class :statement))

(defrecord hydra_java_syntax_local_variable_declaration_statement [value])
(defn make-hydra_java_syntax_local_variable_declaration_statement [value] (->hydra_java_syntax_local_variable_declaration_statement value))

(defrecord hydra_java_syntax_local_variable_declaration [modifiers type declarators])
(defn make-hydra_java_syntax_local_variable_declaration [modifiers type declarators] (->hydra_java_syntax_local_variable_declaration modifiers type declarators))

(def hydra_java_syntax_local_variable_type-variants (list :type :var))

(def hydra_java_syntax_statement-variants (list :without_trailing :labeled :if_then :if_then_else :while :for))

(def hydra_java_syntax_statement_no_short_if-variants (list :without_trailing :labeled :if_then_else :while :for))

(def hydra_java_syntax_statement_without_trailing_substatement-variants (list :block :empty :expression :assert :switch :do :break :continue :return :synchronized :throw :try))

(defrecord hydra_java_syntax_labeled_statement [identifier statement])
(defn make-hydra_java_syntax_labeled_statement [identifier statement] (->hydra_java_syntax_labeled_statement identifier statement))

(defrecord hydra_java_syntax_labeled_statement_no_short_if [identifier statement])
(defn make-hydra_java_syntax_labeled_statement_no_short_if [identifier statement] (->hydra_java_syntax_labeled_statement_no_short_if identifier statement))

(defrecord hydra_java_syntax_expression_statement [value])
(defn make-hydra_java_syntax_expression_statement [value] (->hydra_java_syntax_expression_statement value))

(def hydra_java_syntax_statement_expression-variants (list :assignment :pre_increment :pre_decrement :post_increment :post_decrement :method_invocation :class_instance_creation))

(defrecord hydra_java_syntax_if_then_statement [expression statement])
(defn make-hydra_java_syntax_if_then_statement [expression statement] (->hydra_java_syntax_if_then_statement expression statement))

(defrecord hydra_java_syntax_if_then_else_statement [cond then else])
(defn make-hydra_java_syntax_if_then_else_statement [cond then else] (->hydra_java_syntax_if_then_else_statement cond then else))

(defrecord hydra_java_syntax_if_then_else_statement_no_short_if [cond then else])
(defn make-hydra_java_syntax_if_then_else_statement_no_short_if [cond then else] (->hydra_java_syntax_if_then_else_statement_no_short_if cond then else))

(def hydra_java_syntax_assert_statement-variants (list :single :pair))

(defrecord hydra_java_syntax_assert_statement_pair [first second])
(defn make-hydra_java_syntax_assert_statement_pair [first second] (->hydra_java_syntax_assert_statement_pair first second))

(defrecord hydra_java_syntax_switch_statement [cond block])
(defn make-hydra_java_syntax_switch_statement [cond block] (->hydra_java_syntax_switch_statement cond block))

(defrecord hydra_java_syntax_switch_block [value])
(defn make-hydra_java_syntax_switch_block [value] (->hydra_java_syntax_switch_block value))

(defrecord hydra_java_syntax_switch_block_pair [statements labels])
(defn make-hydra_java_syntax_switch_block_pair [statements labels] (->hydra_java_syntax_switch_block_pair statements labels))

(defrecord hydra_java_syntax_switch_block_statement_group [labels statements])
(defn make-hydra_java_syntax_switch_block_statement_group [labels statements] (->hydra_java_syntax_switch_block_statement_group labels statements))

(def hydra_java_syntax_switch_label-variants (list :constant :enum_constant :default))

(defrecord hydra_java_syntax_enum_constant_name [value])
(defn make-hydra_java_syntax_enum_constant_name [value] (->hydra_java_syntax_enum_constant_name value))

(defrecord hydra_java_syntax_while_statement [cond body])
(defn make-hydra_java_syntax_while_statement [cond body] (->hydra_java_syntax_while_statement cond body))

(defrecord hydra_java_syntax_while_statement_no_short_if [cond body])
(defn make-hydra_java_syntax_while_statement_no_short_if [cond body] (->hydra_java_syntax_while_statement_no_short_if cond body))

(defrecord hydra_java_syntax_do_statement [body conde])
(defn make-hydra_java_syntax_do_statement [body conde] (->hydra_java_syntax_do_statement body conde))

(def hydra_java_syntax_for_statement-variants (list :basic :enhanced))

(def hydra_java_syntax_for_statement_no_short_if-variants (list :basic :enhanced))

(defrecord hydra_java_syntax_basic_for_statement [cond body])
(defn make-hydra_java_syntax_basic_for_statement [cond body] (->hydra_java_syntax_basic_for_statement cond body))

(defrecord hydra_java_syntax_for_cond [init cond update])
(defn make-hydra_java_syntax_for_cond [init cond update] (->hydra_java_syntax_for_cond init cond update))

(defrecord hydra_java_syntax_basic_for_statement_no_short_if [cond body])
(defn make-hydra_java_syntax_basic_for_statement_no_short_if [cond body] (->hydra_java_syntax_basic_for_statement_no_short_if cond body))

(def hydra_java_syntax_for_init-variants (list :statements :local_variable))

(defrecord hydra_java_syntax_for_update [value])
(defn make-hydra_java_syntax_for_update [value] (->hydra_java_syntax_for_update value))

(defrecord hydra_java_syntax_enhanced_for_statement [cond body])
(defn make-hydra_java_syntax_enhanced_for_statement [cond body] (->hydra_java_syntax_enhanced_for_statement cond body))

(defrecord hydra_java_syntax_enhanced_for_cond [modifiers type id expression])
(defn make-hydra_java_syntax_enhanced_for_cond [modifiers type id expression] (->hydra_java_syntax_enhanced_for_cond modifiers type id expression))

(defrecord hydra_java_syntax_enhanced_for_statement_no_short_if [cond body])
(defn make-hydra_java_syntax_enhanced_for_statement_no_short_if [cond body] (->hydra_java_syntax_enhanced_for_statement_no_short_if cond body))

(defrecord hydra_java_syntax_break_statement [value])
(defn make-hydra_java_syntax_break_statement [value] (->hydra_java_syntax_break_statement value))

(defrecord hydra_java_syntax_continue_statement [value])
(defn make-hydra_java_syntax_continue_statement [value] (->hydra_java_syntax_continue_statement value))

(defrecord hydra_java_syntax_return_statement [value])
(defn make-hydra_java_syntax_return_statement [value] (->hydra_java_syntax_return_statement value))

(defrecord hydra_java_syntax_throw_statement [value])
(defn make-hydra_java_syntax_throw_statement [value] (->hydra_java_syntax_throw_statement value))

(defrecord hydra_java_syntax_synchronized_statement [expression block])
(defn make-hydra_java_syntax_synchronized_statement [expression block] (->hydra_java_syntax_synchronized_statement expression block))

(def hydra_java_syntax_try_statement-variants (list :simple :with_finally :with_resources))

(defrecord hydra_java_syntax_try_statement_simple [block catches])
(defn make-hydra_java_syntax_try_statement_simple [block catches] (->hydra_java_syntax_try_statement_simple block catches))

(defrecord hydra_java_syntax_try_statement_with_finally [block catches finally])
(defn make-hydra_java_syntax_try_statement_with_finally [block catches finally] (->hydra_java_syntax_try_statement_with_finally block catches finally))

(defrecord hydra_java_syntax_catches [value])
(defn make-hydra_java_syntax_catches [value] (->hydra_java_syntax_catches value))

(defrecord hydra_java_syntax_catch_clause [parameter block])
(defn make-hydra_java_syntax_catch_clause [parameter block] (->hydra_java_syntax_catch_clause parameter block))

(defrecord hydra_java_syntax_catch_formal_parameter [modifiers type id])
(defn make-hydra_java_syntax_catch_formal_parameter [modifiers type id] (->hydra_java_syntax_catch_formal_parameter modifiers type id))

(defrecord hydra_java_syntax_catch_type [type types])
(defn make-hydra_java_syntax_catch_type [type types] (->hydra_java_syntax_catch_type type types))

(defrecord hydra_java_syntax_finally [value])
(defn make-hydra_java_syntax_finally [value] (->hydra_java_syntax_finally value))

(defrecord hydra_java_syntax_try_with_resources_statement [resource_specification block catches finally])
(defn make-hydra_java_syntax_try_with_resources_statement [resource_specification block catches finally] (->hydra_java_syntax_try_with_resources_statement resource_specification block catches finally))

(defrecord hydra_java_syntax_resource_specification [value])
(defn make-hydra_java_syntax_resource_specification [value] (->hydra_java_syntax_resource_specification value))

(def hydra_java_syntax_resource-variants (list :local :variable))

(defrecord hydra_java_syntax_resource_local [modifiers type identifier expression])
(defn make-hydra_java_syntax_resource_local [modifiers type identifier expression] (->hydra_java_syntax_resource_local modifiers type identifier expression))

(def hydra_java_syntax_variable_access-variants (list :expression_name :field_access))

(def hydra_java_syntax_primary-variants (list :no_new_array :array_creation))

(def hydra_java_syntax_primary_no_new_array_expression-variants (list :literal :class_literal :this :dot_this :parens :class_instance :field_access :array_access :method_invocation :method_reference))

(def hydra_java_syntax_class_literal-variants (list :type :numeric_type :boolean :void))

(def hydra_java_syntax_type_name_array-variants (list :simple :array))

(def hydra_java_syntax_numeric_type_array-variants (list :simple :array))

(def hydra_java_syntax_boolean_array-variants (list :simple :array))

(defrecord hydra_java_syntax_class_instance_creation_expression [qualifier expression])
(defn make-hydra_java_syntax_class_instance_creation_expression [qualifier expression] (->hydra_java_syntax_class_instance_creation_expression qualifier expression))

(def hydra_java_syntax_class_instance_creation_expression_qualifier-variants (list :expression :primary))

(defrecord hydra_java_syntax_unqualified_class_instance_creation_expression [type_arguments class_or_interface arguments body])
(defn make-hydra_java_syntax_unqualified_class_instance_creation_expression [type_arguments class_or_interface arguments body] (->hydra_java_syntax_unqualified_class_instance_creation_expression type_arguments class_or_interface arguments body))

(defrecord hydra_java_syntax_class_or_interface_type_to_instantiate [identifiers type_arguments])
(defn make-hydra_java_syntax_class_or_interface_type_to_instantiate [identifiers type_arguments] (->hydra_java_syntax_class_or_interface_type_to_instantiate identifiers type_arguments))

(defrecord hydra_java_syntax_annotated_identifier [annotations identifier])
(defn make-hydra_java_syntax_annotated_identifier [annotations identifier] (->hydra_java_syntax_annotated_identifier annotations identifier))

(def hydra_java_syntax_type_arguments_or_diamond-variants (list :arguments :diamond))

(defrecord hydra_java_syntax_field_access [qualifier identifier])
(defn make-hydra_java_syntax_field_access [qualifier identifier] (->hydra_java_syntax_field_access qualifier identifier))

(def hydra_java_syntax_field_access_qualifier-variants (list :primary :super :typed))

(defrecord hydra_java_syntax_array_access [expression variant])
(defn make-hydra_java_syntax_array_access [expression variant] (->hydra_java_syntax_array_access expression variant))

(def hydra_java_syntax_array_access_variant-variants (list :name :primary))

(defrecord hydra_java_syntax_method_invocation [header arguments])
(defn make-hydra_java_syntax_method_invocation [header arguments] (->hydra_java_syntax_method_invocation header arguments))

(def hydra_java_syntax_method_invocation_header-variants (list :simple :complex))

(defrecord hydra_java_syntax_method_invocation_complex [variant type_arguments identifier])
(defn make-hydra_java_syntax_method_invocation_complex [variant type_arguments identifier] (->hydra_java_syntax_method_invocation_complex variant type_arguments identifier))

(def hydra_java_syntax_method_invocation_variant-variants (list :type :expression :primary :super :type_super))

(def hydra_java_syntax_method_reference-variants (list :expression :primary :reference_type :super :new :array))

(defrecord hydra_java_syntax_method_reference_expression [name type_arguments identifier])
(defn make-hydra_java_syntax_method_reference_expression [name type_arguments identifier] (->hydra_java_syntax_method_reference_expression name type_arguments identifier))

(defrecord hydra_java_syntax_method_reference_primary [primary type_arguments identifier])
(defn make-hydra_java_syntax_method_reference_primary [primary type_arguments identifier] (->hydra_java_syntax_method_reference_primary primary type_arguments identifier))

(defrecord hydra_java_syntax_method_reference_reference_type [reference_type type_arguments identifier])
(defn make-hydra_java_syntax_method_reference_reference_type [reference_type type_arguments identifier] (->hydra_java_syntax_method_reference_reference_type reference_type type_arguments identifier))

(defrecord hydra_java_syntax_method_reference_super [type_arguments identifier super])
(defn make-hydra_java_syntax_method_reference_super [type_arguments identifier super] (->hydra_java_syntax_method_reference_super type_arguments identifier super))

(defrecord hydra_java_syntax_method_reference_new [class_type type_arguments])
(defn make-hydra_java_syntax_method_reference_new [class_type type_arguments] (->hydra_java_syntax_method_reference_new class_type type_arguments))

(defrecord hydra_java_syntax_method_reference_array [value])
(defn make-hydra_java_syntax_method_reference_array [value] (->hydra_java_syntax_method_reference_array value))

(def hydra_java_syntax_array_creation_expression-variants (list :primitive :class_or_interface :primitive_array :class_or_interface_array))

(defrecord hydra_java_syntax_array_creation_expression_primitive [type dim_exprs dims])
(defn make-hydra_java_syntax_array_creation_expression_primitive [type dim_exprs dims] (->hydra_java_syntax_array_creation_expression_primitive type dim_exprs dims))

(defrecord hydra_java_syntax_array_creation_expression_class_or_interface [type dim_exprs dims])
(defn make-hydra_java_syntax_array_creation_expression_class_or_interface [type dim_exprs dims] (->hydra_java_syntax_array_creation_expression_class_or_interface type dim_exprs dims))

(defrecord hydra_java_syntax_array_creation_expression_primitive_array [type dims array])
(defn make-hydra_java_syntax_array_creation_expression_primitive_array [type dims array] (->hydra_java_syntax_array_creation_expression_primitive_array type dims array))

(defrecord hydra_java_syntax_array_creation_expression_class_or_interface_array [type dims array])
(defn make-hydra_java_syntax_array_creation_expression_class_or_interface_array [type dims array] (->hydra_java_syntax_array_creation_expression_class_or_interface_array type dims array))

(defrecord hydra_java_syntax_dim_expr [annotations expression])
(defn make-hydra_java_syntax_dim_expr [annotations expression] (->hydra_java_syntax_dim_expr annotations expression))

(def hydra_java_syntax_expression-variants (list :lambda :assignment))

(defrecord hydra_java_syntax_lambda_expression [parameters body])
(defn make-hydra_java_syntax_lambda_expression [parameters body] (->hydra_java_syntax_lambda_expression parameters body))

(def hydra_java_syntax_lambda_parameters-variants (list :tuple :single))

(def hydra_java_syntax_lambda_parameter-variants (list :normal :variable_arity))

(defrecord hydra_java_syntax_lambda_parameter_normal [modifiers type id])
(defn make-hydra_java_syntax_lambda_parameter_normal [modifiers type id] (->hydra_java_syntax_lambda_parameter_normal modifiers type id))

(def hydra_java_syntax_lambda_parameter_type-variants (list :type :var))

(def hydra_java_syntax_lambda_body-variants (list :expression :block))

(def hydra_java_syntax_assignment_expression-variants (list :conditional :assignment))

(defrecord hydra_java_syntax_assignment [lhs op expression])
(defn make-hydra_java_syntax_assignment [lhs op expression] (->hydra_java_syntax_assignment lhs op expression))

(def hydra_java_syntax_left_hand_side-variants (list :expression_name :field_access :array_access))

(def hydra_java_syntax_assignment_operator-variants (list :simple :times :div :mod :plus :minus :shift_left :shift_right :shift_right_zero_fill :and :xor :or))

(def hydra_java_syntax_conditional_expression-variants (list :simple :ternary_cond :ternary_lambda))

(defrecord hydra_java_syntax_conditional_expression_ternary_cond [cond if_true if_false])
(defn make-hydra_java_syntax_conditional_expression_ternary_cond [cond if_true if_false] (->hydra_java_syntax_conditional_expression_ternary_cond cond if_true if_false))

(defrecord hydra_java_syntax_conditional_expression_ternary_lambda [cond if_true if_false])
(defn make-hydra_java_syntax_conditional_expression_ternary_lambda [cond if_true if_false] (->hydra_java_syntax_conditional_expression_ternary_lambda cond if_true if_false))

(defrecord hydra_java_syntax_conditional_or_expression [value])
(defn make-hydra_java_syntax_conditional_or_expression [value] (->hydra_java_syntax_conditional_or_expression value))

(defrecord hydra_java_syntax_conditional_and_expression [value])
(defn make-hydra_java_syntax_conditional_and_expression [value] (->hydra_java_syntax_conditional_and_expression value))

(defrecord hydra_java_syntax_inclusive_or_expression [value])
(defn make-hydra_java_syntax_inclusive_or_expression [value] (->hydra_java_syntax_inclusive_or_expression value))

(defrecord hydra_java_syntax_exclusive_or_expression [value])
(defn make-hydra_java_syntax_exclusive_or_expression [value] (->hydra_java_syntax_exclusive_or_expression value))

(defrecord hydra_java_syntax_and_expression [value])
(defn make-hydra_java_syntax_and_expression [value] (->hydra_java_syntax_and_expression value))

(def hydra_java_syntax_equality_expression-variants (list :unary :equal :not_equal))

(defrecord hydra_java_syntax_equality_expression_binary [lhs rhs])
(defn make-hydra_java_syntax_equality_expression_binary [lhs rhs] (->hydra_java_syntax_equality_expression_binary lhs rhs))

(def hydra_java_syntax_relational_expression-variants (list :simple :less_than :greater_than :less_than_equal :greater_than_equal :instanceof))

(defrecord hydra_java_syntax_relational_expression_less_than [lhs rhs])
(defn make-hydra_java_syntax_relational_expression_less_than [lhs rhs] (->hydra_java_syntax_relational_expression_less_than lhs rhs))

(defrecord hydra_java_syntax_relational_expression_greater_than [lhs rhs])
(defn make-hydra_java_syntax_relational_expression_greater_than [lhs rhs] (->hydra_java_syntax_relational_expression_greater_than lhs rhs))

(defrecord hydra_java_syntax_relational_expression_less_than_equal [lhs rhs])
(defn make-hydra_java_syntax_relational_expression_less_than_equal [lhs rhs] (->hydra_java_syntax_relational_expression_less_than_equal lhs rhs))

(defrecord hydra_java_syntax_relational_expression_greater_than_equal [lhs rhs])
(defn make-hydra_java_syntax_relational_expression_greater_than_equal [lhs rhs] (->hydra_java_syntax_relational_expression_greater_than_equal lhs rhs))

(defrecord hydra_java_syntax_relational_expression_instance_of [lhs rhs])
(defn make-hydra_java_syntax_relational_expression_instance_of [lhs rhs] (->hydra_java_syntax_relational_expression_instance_of lhs rhs))

(def hydra_java_syntax_shift_expression-variants (list :unary :shift_left :shift_right :shift_right_zero_fill))

(defrecord hydra_java_syntax_shift_expression_binary [lhs rhs])
(defn make-hydra_java_syntax_shift_expression_binary [lhs rhs] (->hydra_java_syntax_shift_expression_binary lhs rhs))

(def hydra_java_syntax_additive_expression-variants (list :unary :plus :minus))

(defrecord hydra_java_syntax_additive_expression_binary [lhs rhs])
(defn make-hydra_java_syntax_additive_expression_binary [lhs rhs] (->hydra_java_syntax_additive_expression_binary lhs rhs))

(def hydra_java_syntax_multiplicative_expression-variants (list :unary :times :divide :mod))

(defrecord hydra_java_syntax_multiplicative_expression_binary [lhs rhs])
(defn make-hydra_java_syntax_multiplicative_expression_binary [lhs rhs] (->hydra_java_syntax_multiplicative_expression_binary lhs rhs))

(def hydra_java_syntax_unary_expression-variants (list :pre_increment :pre_decrement :plus :minus :other))

(defrecord hydra_java_syntax_pre_increment_expression [value])
(defn make-hydra_java_syntax_pre_increment_expression [value] (->hydra_java_syntax_pre_increment_expression value))

(defrecord hydra_java_syntax_pre_decrement_expression [value])
(defn make-hydra_java_syntax_pre_decrement_expression [value] (->hydra_java_syntax_pre_decrement_expression value))

(def hydra_java_syntax_unary_expression_not_plus_minus-variants (list :postfix :tilde :not :cast))

(def hydra_java_syntax_postfix_expression-variants (list :primary :name :post_increment :post_decrement))

(defrecord hydra_java_syntax_post_increment_expression [value])
(defn make-hydra_java_syntax_post_increment_expression [value] (->hydra_java_syntax_post_increment_expression value))

(defrecord hydra_java_syntax_post_decrement_expression [value])
(defn make-hydra_java_syntax_post_decrement_expression [value] (->hydra_java_syntax_post_decrement_expression value))

(def hydra_java_syntax_cast_expression-variants (list :primitive :not_plus_minus :lambda))

(defrecord hydra_java_syntax_cast_expression_primitive [type expression])
(defn make-hydra_java_syntax_cast_expression_primitive [type expression] (->hydra_java_syntax_cast_expression_primitive type expression))

(defrecord hydra_java_syntax_cast_expression_not_plus_minus [ref_and_bounds expression])
(defn make-hydra_java_syntax_cast_expression_not_plus_minus [ref_and_bounds expression] (->hydra_java_syntax_cast_expression_not_plus_minus ref_and_bounds expression))

(defrecord hydra_java_syntax_cast_expression_lambda [ref_and_bounds expression])
(defn make-hydra_java_syntax_cast_expression_lambda [ref_and_bounds expression] (->hydra_java_syntax_cast_expression_lambda ref_and_bounds expression))

(defrecord hydra_java_syntax_cast_expression_ref_and_bounds [type bounds])
(defn make-hydra_java_syntax_cast_expression_ref_and_bounds [type bounds] (->hydra_java_syntax_cast_expression_ref_and_bounds type bounds))

(defrecord hydra_java_syntax_constant_expression [value])
(defn make-hydra_java_syntax_constant_expression [value] (->hydra_java_syntax_constant_expression value))

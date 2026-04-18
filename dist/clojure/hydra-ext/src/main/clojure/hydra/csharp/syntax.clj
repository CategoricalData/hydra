(ns hydra.csharp.syntax)

(declare hydra_csharp_syntax_literal-variants hydra_csharp_syntax_integer_literal-variants hydra_csharp_syntax_namespace_or_type_name-variants hydra_csharp_syntax_type-variants hydra_csharp_syntax_reference_type-variants hydra_csharp_syntax_class_type-variants hydra_csharp_syntax_non_array_type-variants hydra_csharp_syntax_value_type-variants hydra_csharp_syntax_struct_or_enum_type-variants hydra_csharp_syntax_struct_type-variants hydra_csharp_syntax_simple_type-variants hydra_csharp_syntax_numeric_type-variants hydra_csharp_syntax_integral_type-variants hydra_csharp_syntax_floating_point_type-variants hydra_csharp_syntax_unmanaged_type-variants hydra_csharp_syntax_pattern-variants hydra_csharp_syntax_argument_value-variants hydra_csharp_syntax_primary_expression-variants hydra_csharp_syntax_primary_no_array_creation_expression-variants hydra_csharp_syntax_interpolated_string_expression-variants hydra_csharp_syntax_tuple_expression-variants hydra_csharp_syntax_deconstruction_element-variants hydra_csharp_syntax_member_access_head-variants hydra_csharp_syntax_predefined_type-variants hydra_csharp_syntax_dependent_access-variants hydra_csharp_syntax_null_conditional_invocation_expression_head-variants hydra_csharp_syntax_base_access-variants hydra_csharp_syntax_object_or_collection_initializer-variants hydra_csharp_syntax_initializer_target-variants hydra_csharp_syntax_initializer_value-variants hydra_csharp_syntax_element_initializer-variants hydra_csharp_syntax_array_creation_expression-variants hydra_csharp_syntax_member_declarator-variants hydra_csharp_syntax_typeof_expression-variants hydra_csharp_syntax_default_value_expression-variants hydra_csharp_syntax_named_entity_target-variants hydra_csharp_syntax_unary_expression-variants hydra_csharp_syntax_multiplicative_expression-variants hydra_csharp_syntax_multiplicative_operator-variants hydra_csharp_syntax_additive_expression-variants hydra_csharp_syntax_additive_operator-variants hydra_csharp_syntax_shift_expression-variants hydra_csharp_syntax_shift_operator-variants hydra_csharp_syntax_relational_expression-variants hydra_csharp_syntax_relational_operator-variants hydra_csharp_syntax_equality_expression-variants hydra_csharp_syntax_equality_operator-variants hydra_csharp_syntax_and_expression-variants hydra_csharp_syntax_exclusive_or_expression-variants hydra_csharp_syntax_inclusive_or_expression-variants hydra_csharp_syntax_conditional_and_expression-variants hydra_csharp_syntax_conditional_or_expression-variants hydra_csharp_syntax_null_coalescing_expression-variants hydra_csharp_syntax_local_variable_type-variants hydra_csharp_syntax_conditional_expression-variants hydra_csharp_syntax_anonymous_function_signature-variants hydra_csharp_syntax_anonymous_function_parameter_modifier-variants hydra_csharp_syntax_anonymous_function_body-variants hydra_csharp_syntax_query_body_clause-variants hydra_csharp_syntax_ordering_direction-variants hydra_csharp_syntax_select_or_group_clause-variants hydra_csharp_syntax_assignment_operator-variants hydra_csharp_syntax_expression-variants hydra_csharp_syntax_non_assignment_expression-variants hydra_csharp_syntax_statement-variants hydra_csharp_syntax_embedded_statement-variants hydra_csharp_syntax_declaration_statement-variants hydra_csharp_syntax_local_variable_declaration-variants hydra_csharp_syntax_implicitly_typed_local_variable_declaration-variants hydra_csharp_syntax_local_variable_initializer-variants hydra_csharp_syntax_local_function_declaration-variants hydra_csharp_syntax_local_function_modifier-variants hydra_csharp_syntax_ref_local_function_modifier-variants hydra_csharp_syntax_local_function_body-variants hydra_csharp_syntax_ref_local_function_body-variants hydra_csharp_syntax_statement_expression-variants hydra_csharp_syntax_selection_statement-variants hydra_csharp_syntax_switch_label-variants hydra_csharp_syntax_iteration_statement-variants hydra_csharp_syntax_for_initializer-variants hydra_csharp_syntax_jump_statement-variants hydra_csharp_syntax_goto_statement-variants hydra_csharp_syntax_return_statement-variants hydra_csharp_syntax_catch_clauses-variants hydra_csharp_syntax_resource_acquisition-variants hydra_csharp_syntax_yield_statement-variants hydra_csharp_syntax_using_directive-variants hydra_csharp_syntax_namespace_member_declaration-variants hydra_csharp_syntax_type_declaration-variants hydra_csharp_syntax_class_modifier-variants hydra_csharp_syntax_class_base-variants hydra_csharp_syntax_primary_constraint-variants hydra_csharp_syntax_secondary_constraint-variants hydra_csharp_syntax_class_member_declaration-variants hydra_csharp_syntax_constant_modifier-variants hydra_csharp_syntax_field_modifier-variants hydra_csharp_syntax_method_declaration-variants hydra_csharp_syntax_ref_kind-variants hydra_csharp_syntax_method_modifier-variants hydra_csharp_syntax_ref_method_modifier-variants hydra_csharp_syntax_return_type-variants hydra_csharp_syntax_method_body-variants hydra_csharp_syntax_ref_method_body-variants hydra_csharp_syntax_parameter_modifier-variants hydra_csharp_syntax_parameter_mode_modifier-variants hydra_csharp_syntax_property_declaration-variants hydra_csharp_syntax_property_modifier-variants hydra_csharp_syntax_property_body-variants hydra_csharp_syntax_ref_property_body-variants hydra_csharp_syntax_accessor_declarations-variants hydra_csharp_syntax_accessor_modifier-variants hydra_csharp_syntax_accessor_body-variants hydra_csharp_syntax_ref_accessor_body-variants hydra_csharp_syntax_event_declaration-variants hydra_csharp_syntax_event_modifier-variants hydra_csharp_syntax_event_accessor_declarations-variants hydra_csharp_syntax_indexer_declaration-variants hydra_csharp_syntax_indexer_modifier-variants hydra_csharp_syntax_indexer_body-variants hydra_csharp_syntax_ref_indexer_body-variants hydra_csharp_syntax_operator_modifier-variants hydra_csharp_syntax_operator_declarator-variants hydra_csharp_syntax_overloadable_unary_operator-variants hydra_csharp_syntax_overloadable_binary_operator-variants hydra_csharp_syntax_conversion_kind-variants hydra_csharp_syntax_operator_body-variants hydra_csharp_syntax_constructor_modifier-variants hydra_csharp_syntax_constructor_initializer-variants hydra_csharp_syntax_constructor_body-variants hydra_csharp_syntax_static_constructor_body-variants hydra_csharp_syntax_finalizer_body-variants hydra_csharp_syntax_struct_modifier-variants hydra_csharp_syntax_struct_member_declaration-variants hydra_csharp_syntax_variable_initializer-variants hydra_csharp_syntax_interface_modifier-variants hydra_csharp_syntax_variance_annotation-variants hydra_csharp_syntax_interface_member_declaration-variants hydra_csharp_syntax_enum_base-variants hydra_csharp_syntax_enum_modifier-variants hydra_csharp_syntax_delegate_modifier-variants hydra_csharp_syntax_attribute_target-variants hydra_csharp_syntax_pointer_type-variants hydra_csharp_syntax_fixed_pointer_declarator-variants hydra_csharp_syntax_fixed_size_buffer_modifier-variants)

(defrecord hydra_csharp_syntax_identifier [value])
(defn make-hydra_csharp_syntax_identifier [value] (->hydra_csharp_syntax_identifier value))

(defrecord hydra_csharp_syntax_keyword [value])
(defn make-hydra_csharp_syntax_keyword [value] (->hydra_csharp_syntax_keyword value))

(def hydra_csharp_syntax_literal-variants (list :boolean :integer :real :character :string :null))

(def hydra_csharp_syntax_integer_literal-variants (list :decimal :hexadecimal :binary))

(defrecord hydra_csharp_syntax_namespace_name [value])
(defn make-hydra_csharp_syntax_namespace_name [value] (->hydra_csharp_syntax_namespace_name value))

(defrecord hydra_csharp_syntax_type_name [value])
(defn make-hydra_csharp_syntax_type_name [value] (->hydra_csharp_syntax_type_name value))

(def hydra_csharp_syntax_namespace_or_type_name-variants (list :identifier :qualified :alias))

(defrecord hydra_csharp_syntax_identifier_namespace_or_type_name [identifier arguments])
(defn make-hydra_csharp_syntax_identifier_namespace_or_type_name [identifier arguments] (->hydra_csharp_syntax_identifier_namespace_or_type_name identifier arguments))

(defrecord hydra_csharp_syntax_qualified_namespace_or_type_name [namespace_or_type identifier arguments])
(defn make-hydra_csharp_syntax_qualified_namespace_or_type_name [namespace_or_type identifier arguments] (->hydra_csharp_syntax_qualified_namespace_or_type_name namespace_or_type identifier arguments))

(def hydra_csharp_syntax_type-variants (list :reference :value :param :pointer))

(def hydra_csharp_syntax_reference_type-variants (list :class :interface :array :delegate :dynamic))

(def hydra_csharp_syntax_class_type-variants (list :type_name :object :string))

(defrecord hydra_csharp_syntax_interface_type [value])
(defn make-hydra_csharp_syntax_interface_type [value] (->hydra_csharp_syntax_interface_type value))

(defrecord hydra_csharp_syntax_array_type [type rank])
(defn make-hydra_csharp_syntax_array_type [type rank] (->hydra_csharp_syntax_array_type type rank))

(def hydra_csharp_syntax_non_array_type-variants (list :value :class :interface :delegate :dynamic :parameter :pointer))

(defrecord hydra_csharp_syntax_rank_specifier [value])
(defn make-hydra_csharp_syntax_rank_specifier [value] (->hydra_csharp_syntax_rank_specifier value))

(defrecord hydra_csharp_syntax_delegate_type [value])
(defn make-hydra_csharp_syntax_delegate_type [value] (->hydra_csharp_syntax_delegate_type value))

(def hydra_csharp_syntax_value_type-variants (list :non_nullable :nullable))

(def hydra_csharp_syntax_struct_or_enum_type-variants (list :struct :enum))

(def hydra_csharp_syntax_struct_type-variants (list :type_name :simple :tuple))

(def hydra_csharp_syntax_simple_type-variants (list :numeric :bool))

(def hydra_csharp_syntax_numeric_type-variants (list :integral :floating_point :decimal))

(def hydra_csharp_syntax_integral_type-variants (list :sbyte :byte :short :ushort :int :uint :long :ulong :char))

(def hydra_csharp_syntax_floating_point_type-variants (list :float :double))

(defrecord hydra_csharp_syntax_tuple_type [value])
(defn make-hydra_csharp_syntax_tuple_type [value] (->hydra_csharp_syntax_tuple_type value))

(defrecord hydra_csharp_syntax_tuple_type_element [type identifier])
(defn make-hydra_csharp_syntax_tuple_type_element [type identifier] (->hydra_csharp_syntax_tuple_type_element type identifier))

(defrecord hydra_csharp_syntax_enum_type [value])
(defn make-hydra_csharp_syntax_enum_type [value] (->hydra_csharp_syntax_enum_type value))

(defrecord hydra_csharp_syntax_type_argument_list [value])
(defn make-hydra_csharp_syntax_type_argument_list [value] (->hydra_csharp_syntax_type_argument_list value))

(defrecord hydra_csharp_syntax_type_parameter [value])
(defn make-hydra_csharp_syntax_type_parameter [value] (->hydra_csharp_syntax_type_parameter value))

(def hydra_csharp_syntax_unmanaged_type-variants (list :value :pointer))

(defrecord hydra_csharp_syntax_variable_reference [value])
(defn make-hydra_csharp_syntax_variable_reference [value] (->hydra_csharp_syntax_variable_reference value))

(def hydra_csharp_syntax_pattern-variants (list :declaration :constant :var))

(defrecord hydra_csharp_syntax_declaration_pattern [type designation])
(defn make-hydra_csharp_syntax_declaration_pattern [type designation] (->hydra_csharp_syntax_declaration_pattern type designation))

(defrecord hydra_csharp_syntax_designation [value])
(defn make-hydra_csharp_syntax_designation [value] (->hydra_csharp_syntax_designation value))

(defrecord hydra_csharp_syntax_argument_list [value])
(defn make-hydra_csharp_syntax_argument_list [value] (->hydra_csharp_syntax_argument_list value))

(defrecord hydra_csharp_syntax_argument [name value])
(defn make-hydra_csharp_syntax_argument [name value] (->hydra_csharp_syntax_argument name value))

(def hydra_csharp_syntax_argument_value-variants (list :expression :in :ref :out))

(def hydra_csharp_syntax_primary_expression-variants (list :no_array :array))

(def hydra_csharp_syntax_primary_no_array_creation_expression-variants (list :literal :interpolated_string :simple_name :parenthesized :tuple :member_access :null_conditional_member_access :invocation :element_access :null_conditional_element_access :this_access :base_access :post_increment :post_decrement :object_creation :delegate_creation :anonymous_object_creation :typeof :sizeof :checked :unchecked :default_value :nameof :anonymous_method :pointer_member_access :pointer_element_access :stackalloc))

(def hydra_csharp_syntax_interpolated_string_expression-variants (list :regular :verbatim))

(defrecord hydra_csharp_syntax_interpolated_regular_string_expression [value])
(defn make-hydra_csharp_syntax_interpolated_regular_string_expression [value] (->hydra_csharp_syntax_interpolated_regular_string_expression value))

(defrecord hydra_csharp_syntax_regular_interpolation [expression width format])
(defn make-hydra_csharp_syntax_regular_interpolation [expression width format] (->hydra_csharp_syntax_regular_interpolation expression width format))

(defrecord hydra_csharp_syntax_interpolated_verbatim_string_expression [value])
(defn make-hydra_csharp_syntax_interpolated_verbatim_string_expression [value] (->hydra_csharp_syntax_interpolated_verbatim_string_expression value))

(defrecord hydra_csharp_syntax_verbatim_interpolation [expression width format])
(defn make-hydra_csharp_syntax_verbatim_interpolation [expression width format] (->hydra_csharp_syntax_verbatim_interpolation expression width format))

(defrecord hydra_csharp_syntax_simple_name [identifier type_arguments])
(defn make-hydra_csharp_syntax_simple_name [identifier type_arguments] (->hydra_csharp_syntax_simple_name identifier type_arguments))

(def hydra_csharp_syntax_tuple_expression-variants (list :elements :deconstruction))

(defrecord hydra_csharp_syntax_tuple_element [name expression])
(defn make-hydra_csharp_syntax_tuple_element [name expression] (->hydra_csharp_syntax_tuple_element name expression))

(defrecord hydra_csharp_syntax_deconstruction_tuple [value])
(defn make-hydra_csharp_syntax_deconstruction_tuple [value] (->hydra_csharp_syntax_deconstruction_tuple value))

(def hydra_csharp_syntax_deconstruction_element-variants (list :tuple :identifier))

(defrecord hydra_csharp_syntax_member_access [head identifier type_arguments])
(defn make-hydra_csharp_syntax_member_access [head identifier type_arguments] (->hydra_csharp_syntax_member_access head identifier type_arguments))

(def hydra_csharp_syntax_member_access_head-variants (list :primary :predefined :qualified_alias))

(def hydra_csharp_syntax_predefined_type-variants (list :bool :byte :char :decimal :double :float :int :long :object :sbyte :short :string :uint :ulong :ushort))

(defrecord hydra_csharp_syntax_null_conditional_member_access [expression identifier type_arguments dependent_access])
(defn make-hydra_csharp_syntax_null_conditional_member_access [expression identifier type_arguments dependent_access] (->hydra_csharp_syntax_null_conditional_member_access expression identifier type_arguments dependent_access))

(def hydra_csharp_syntax_dependent_access-variants (list :member_access :element_access :invocation))

(defrecord hydra_csharp_syntax_dependent_access_for_member [identifier type_arguments])
(defn make-hydra_csharp_syntax_dependent_access_for_member [identifier type_arguments] (->hydra_csharp_syntax_dependent_access_for_member identifier type_arguments))

(defrecord hydra_csharp_syntax_null_conditional_projection_initializer [expression identifier type_arguments])
(defn make-hydra_csharp_syntax_null_conditional_projection_initializer [expression identifier type_arguments] (->hydra_csharp_syntax_null_conditional_projection_initializer expression identifier type_arguments))

(defrecord hydra_csharp_syntax_invocation_expression [expression arguments])
(defn make-hydra_csharp_syntax_invocation_expression [expression arguments] (->hydra_csharp_syntax_invocation_expression expression arguments))

(defrecord hydra_csharp_syntax_null_conditional_invocation_expression [head arguments])
(defn make-hydra_csharp_syntax_null_conditional_invocation_expression [head arguments] (->hydra_csharp_syntax_null_conditional_invocation_expression head arguments))

(def hydra_csharp_syntax_null_conditional_invocation_expression_head-variants (list :member :element))

(defrecord hydra_csharp_syntax_element_access [expression arguments])
(defn make-hydra_csharp_syntax_element_access [expression arguments] (->hydra_csharp_syntax_element_access expression arguments))

(defrecord hydra_csharp_syntax_null_conditional_element_access [expression arguments dependent_access])
(defn make-hydra_csharp_syntax_null_conditional_element_access [expression arguments dependent_access] (->hydra_csharp_syntax_null_conditional_element_access expression arguments dependent_access))

(def hydra_csharp_syntax_base_access-variants (list :identifier :arguments))

(defrecord hydra_csharp_syntax_base_access_with_identifier [identifier type_arguments])
(defn make-hydra_csharp_syntax_base_access_with_identifier [identifier type_arguments] (->hydra_csharp_syntax_base_access_with_identifier identifier type_arguments))

(defrecord hydra_csharp_syntax_object_creation_expression [type arguments initializer])
(defn make-hydra_csharp_syntax_object_creation_expression [type arguments initializer] (->hydra_csharp_syntax_object_creation_expression type arguments initializer))

(def hydra_csharp_syntax_object_or_collection_initializer-variants (list :object :collection))

(defrecord hydra_csharp_syntax_member_initializer [target value])
(defn make-hydra_csharp_syntax_member_initializer [target value] (->hydra_csharp_syntax_member_initializer target value))

(def hydra_csharp_syntax_initializer_target-variants (list :identifier :arguments))

(def hydra_csharp_syntax_initializer_value-variants (list :expression :object_or_collection))

(def hydra_csharp_syntax_element_initializer-variants (list :single :list))

(defrecord hydra_csharp_syntax_expression_list [value])
(defn make-hydra_csharp_syntax_expression_list [value] (->hydra_csharp_syntax_expression_list value))

(def hydra_csharp_syntax_array_creation_expression-variants (list :non_array_type :array_type :rank_specifier))

(defrecord hydra_csharp_syntax_non_array_type_array_creation_expression [type expressions rank_specifiers initializer])
(defn make-hydra_csharp_syntax_non_array_type_array_creation_expression [type expressions rank_specifiers initializer] (->hydra_csharp_syntax_non_array_type_array_creation_expression type expressions rank_specifiers initializer))

(defrecord hydra_csharp_syntax_array_type_array_creation_expression [type initializer])
(defn make-hydra_csharp_syntax_array_type_array_creation_expression [type initializer] (->hydra_csharp_syntax_array_type_array_creation_expression type initializer))

(defrecord hydra_csharp_syntax_rank_specifier_array_creation_expression [rank_specifier initializer])
(defn make-hydra_csharp_syntax_rank_specifier_array_creation_expression [rank_specifier initializer] (->hydra_csharp_syntax_rank_specifier_array_creation_expression rank_specifier initializer))

(defrecord hydra_csharp_syntax_delegate_creation_expression [type expression])
(defn make-hydra_csharp_syntax_delegate_creation_expression [type expression] (->hydra_csharp_syntax_delegate_creation_expression type expression))

(defrecord hydra_csharp_syntax_member_declarator_list [value])
(defn make-hydra_csharp_syntax_member_declarator_list [value] (->hydra_csharp_syntax_member_declarator_list value))

(def hydra_csharp_syntax_member_declarator-variants (list :name :member_access :null_conditional_projection_initializer :base_access :assignment))

(defrecord hydra_csharp_syntax_assignment_member_declarator [identifier expression])
(defn make-hydra_csharp_syntax_assignment_member_declarator [identifier expression] (->hydra_csharp_syntax_assignment_member_declarator identifier expression))

(def hydra_csharp_syntax_typeof_expression-variants (list :type :unbound_type_name :void))

(defrecord hydra_csharp_syntax_unbound_type_name [value])
(defn make-hydra_csharp_syntax_unbound_type_name [value] (->hydra_csharp_syntax_unbound_type_name value))

(defrecord hydra_csharp_syntax_unbound_type_name_part [identifier aliased dimension])
(defn make-hydra_csharp_syntax_unbound_type_name_part [identifier aliased dimension] (->hydra_csharp_syntax_unbound_type_name_part identifier aliased dimension))

(def hydra_csharp_syntax_default_value_expression-variants (list :explicitly_typed :default_literal))

(defrecord hydra_csharp_syntax_stackalloc_expression [type expression initializer])
(defn make-hydra_csharp_syntax_stackalloc_expression [type expression initializer] (->hydra_csharp_syntax_stackalloc_expression type expression initializer))

(defrecord hydra_csharp_syntax_named_entity [target parts])
(defn make-hydra_csharp_syntax_named_entity [target parts] (->hydra_csharp_syntax_named_entity target parts))

(defrecord hydra_csharp_syntax_named_entity_part [identifier type_arguments])
(defn make-hydra_csharp_syntax_named_entity_part [identifier type_arguments] (->hydra_csharp_syntax_named_entity_part identifier type_arguments))

(def hydra_csharp_syntax_named_entity_target-variants (list :name :this :base :predefined_type :qualified_alias_member))

(def hydra_csharp_syntax_unary_expression-variants (list :primary :plus :minus :not :bitwise_complement :pre_increment :pre_decrement :cast :await :pointer_indirection :address_of))

(defrecord hydra_csharp_syntax_cast_expression [type expression])
(defn make-hydra_csharp_syntax_cast_expression [type expression] (->hydra_csharp_syntax_cast_expression type expression))

(def hydra_csharp_syntax_multiplicative_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_multiplicative_expression [left operator right])
(defn make-hydra_csharp_syntax_binary_multiplicative_expression [left operator right] (->hydra_csharp_syntax_binary_multiplicative_expression left operator right))

(def hydra_csharp_syntax_multiplicative_operator-variants (list :times :divide :modulo))

(def hydra_csharp_syntax_additive_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_additive_expression [left operator right])
(defn make-hydra_csharp_syntax_binary_additive_expression [left operator right] (->hydra_csharp_syntax_binary_additive_expression left operator right))

(def hydra_csharp_syntax_additive_operator-variants (list :plus :minus))

(def hydra_csharp_syntax_shift_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_shift_expression [left operator right])
(defn make-hydra_csharp_syntax_binary_shift_expression [left operator right] (->hydra_csharp_syntax_binary_shift_expression left operator right))

(def hydra_csharp_syntax_shift_operator-variants (list :left :right))

(def hydra_csharp_syntax_relational_expression-variants (list :simple :binary :is_type :is_pattern :as_type))

(defrecord hydra_csharp_syntax_binary_relational_expression [left operator right])
(defn make-hydra_csharp_syntax_binary_relational_expression [left operator right] (->hydra_csharp_syntax_binary_relational_expression left operator right))

(def hydra_csharp_syntax_relational_operator-variants (list :less_than :greater_than :less_than_or_equal :greater_than_or_equal))

(defrecord hydra_csharp_syntax_is_type_expression [expression type])
(defn make-hydra_csharp_syntax_is_type_expression [expression type] (->hydra_csharp_syntax_is_type_expression expression type))

(defrecord hydra_csharp_syntax_is_pattern_expression [expression pattern])
(defn make-hydra_csharp_syntax_is_pattern_expression [expression pattern] (->hydra_csharp_syntax_is_pattern_expression expression pattern))

(defrecord hydra_csharp_syntax_as_type_expression [expression type])
(defn make-hydra_csharp_syntax_as_type_expression [expression type] (->hydra_csharp_syntax_as_type_expression expression type))

(def hydra_csharp_syntax_equality_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_equality_expression [left operator right])
(defn make-hydra_csharp_syntax_binary_equality_expression [left operator right] (->hydra_csharp_syntax_binary_equality_expression left operator right))

(def hydra_csharp_syntax_equality_operator-variants (list :equal :not_equal))

(def hydra_csharp_syntax_and_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_and_expression [left right])
(defn make-hydra_csharp_syntax_binary_and_expression [left right] (->hydra_csharp_syntax_binary_and_expression left right))

(def hydra_csharp_syntax_exclusive_or_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_exclusive_or_expression [left right])
(defn make-hydra_csharp_syntax_binary_exclusive_or_expression [left right] (->hydra_csharp_syntax_binary_exclusive_or_expression left right))

(def hydra_csharp_syntax_inclusive_or_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_inclusive_or_expression [left right])
(defn make-hydra_csharp_syntax_binary_inclusive_or_expression [left right] (->hydra_csharp_syntax_binary_inclusive_or_expression left right))

(def hydra_csharp_syntax_conditional_and_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_conditional_and_expression [left right])
(defn make-hydra_csharp_syntax_binary_conditional_and_expression [left right] (->hydra_csharp_syntax_binary_conditional_and_expression left right))

(def hydra_csharp_syntax_conditional_or_expression-variants (list :simple :binary))

(defrecord hydra_csharp_syntax_binary_conditional_or_expression [left right])
(defn make-hydra_csharp_syntax_binary_conditional_or_expression [left right] (->hydra_csharp_syntax_binary_conditional_or_expression left right))

(def hydra_csharp_syntax_null_coalescing_expression-variants (list :simple :binary :throw))

(defrecord hydra_csharp_syntax_binary_null_coalescing_expression [left right])
(defn make-hydra_csharp_syntax_binary_null_coalescing_expression [left right] (->hydra_csharp_syntax_binary_null_coalescing_expression left right))

(defrecord hydra_csharp_syntax_declaration_expression [type identifier])
(defn make-hydra_csharp_syntax_declaration_expression [type identifier] (->hydra_csharp_syntax_declaration_expression type identifier))

(def hydra_csharp_syntax_local_variable_type-variants (list :type :var))

(def hydra_csharp_syntax_conditional_expression-variants (list :simple :simple_conditional :ref_conditional))

(defrecord hydra_csharp_syntax_simple_conditional_expression [condition true false])
(defn make-hydra_csharp_syntax_simple_conditional_expression [condition true false] (->hydra_csharp_syntax_simple_conditional_expression condition true false))

(defrecord hydra_csharp_syntax_ref_conditional_expression [condition true false])
(defn make-hydra_csharp_syntax_ref_conditional_expression [condition true false] (->hydra_csharp_syntax_ref_conditional_expression condition true false))

(defrecord hydra_csharp_syntax_lambda_expression [async signature body])
(defn make-hydra_csharp_syntax_lambda_expression [async signature body] (->hydra_csharp_syntax_lambda_expression async signature body))

(defrecord hydra_csharp_syntax_anonymous_method_expression [async signature body])
(defn make-hydra_csharp_syntax_anonymous_method_expression [async signature body] (->hydra_csharp_syntax_anonymous_method_expression async signature body))

(def hydra_csharp_syntax_anonymous_function_signature-variants (list :explicit :implicit))

(defrecord hydra_csharp_syntax_explicit_anonymous_function_parameter [modifier type identifier])
(defn make-hydra_csharp_syntax_explicit_anonymous_function_parameter [modifier type identifier] (->hydra_csharp_syntax_explicit_anonymous_function_parameter modifier type identifier))

(def hydra_csharp_syntax_anonymous_function_parameter_modifier-variants (list :ref :out :in))

(def hydra_csharp_syntax_anonymous_function_body-variants (list :null_conditional_invocation :expression :ref :block))

(defrecord hydra_csharp_syntax_query_expression [from body])
(defn make-hydra_csharp_syntax_query_expression [from body] (->hydra_csharp_syntax_query_expression from body))

(defrecord hydra_csharp_syntax_from_clause [type identifier in])
(defn make-hydra_csharp_syntax_from_clause [type identifier in] (->hydra_csharp_syntax_from_clause type identifier in))

(defrecord hydra_csharp_syntax_query_body [clauses select_or_group continuation])
(defn make-hydra_csharp_syntax_query_body [clauses select_or_group continuation] (->hydra_csharp_syntax_query_body clauses select_or_group continuation))

(def hydra_csharp_syntax_query_body_clause-variants (list :from :let :where :join :orderby))

(defrecord hydra_csharp_syntax_let_clause [left right])
(defn make-hydra_csharp_syntax_let_clause [left right] (->hydra_csharp_syntax_let_clause left right))

(defrecord hydra_csharp_syntax_join_clause [type identifier in on equals into])
(defn make-hydra_csharp_syntax_join_clause [type identifier in on equals into] (->hydra_csharp_syntax_join_clause type identifier in on equals into))

(defrecord hydra_csharp_syntax_ordering [expression direction])
(defn make-hydra_csharp_syntax_ordering [expression direction] (->hydra_csharp_syntax_ordering expression direction))

(def hydra_csharp_syntax_ordering_direction-variants (list :ascending :descending))

(def hydra_csharp_syntax_select_or_group_clause-variants (list :select :group))

(defrecord hydra_csharp_syntax_group_clause [grouped by])
(defn make-hydra_csharp_syntax_group_clause [grouped by] (->hydra_csharp_syntax_group_clause grouped by))

(defrecord hydra_csharp_syntax_query_continuation [into body])
(defn make-hydra_csharp_syntax_query_continuation [into body] (->hydra_csharp_syntax_query_continuation into body))

(defrecord hydra_csharp_syntax_assignment [left operator right])
(defn make-hydra_csharp_syntax_assignment [left operator right] (->hydra_csharp_syntax_assignment left operator right))

(def hydra_csharp_syntax_assignment_operator-variants (list :simple :plus_equals :minus_equals :times_equals :divide_equals :mod_equals :and_equals :or_equals :xor_equals :left_shift_equals :right_shift_equals))

(def hydra_csharp_syntax_expression-variants (list :non_assignment :assignment))

(def hydra_csharp_syntax_non_assignment_expression-variants (list :declaration :conditional :lambda :query))

(defrecord hydra_csharp_syntax_constant_expression [value])
(defn make-hydra_csharp_syntax_constant_expression [value] (->hydra_csharp_syntax_constant_expression value))

(defrecord hydra_csharp_syntax_boolean_expression [value])
(defn make-hydra_csharp_syntax_boolean_expression [value] (->hydra_csharp_syntax_boolean_expression value))

(def hydra_csharp_syntax_statement-variants (list :labeled :declaration :embedded))

(def hydra_csharp_syntax_embedded_statement-variants (list :block :empty :expression :selection :iteration :jump :try :checked :unchecked :lock :using :yield :unsafe :fixed))

(defrecord hydra_csharp_syntax_block [value])
(defn make-hydra_csharp_syntax_block [value] (->hydra_csharp_syntax_block value))

(defrecord hydra_csharp_syntax_labeled_statement [label statement])
(defn make-hydra_csharp_syntax_labeled_statement [label statement] (->hydra_csharp_syntax_labeled_statement label statement))

(def hydra_csharp_syntax_declaration_statement-variants (list :variable :constant :function))

(def hydra_csharp_syntax_local_variable_declaration-variants (list :implicitly_typed :explicitly_typed :ref))

(def hydra_csharp_syntax_implicitly_typed_local_variable_declaration-variants (list :var :ref_var))

(defrecord hydra_csharp_syntax_ref_var_implicitly_typed_local_variable_declaration [ref_kind declarator])
(defn make-hydra_csharp_syntax_ref_var_implicitly_typed_local_variable_declaration [ref_kind declarator] (->hydra_csharp_syntax_ref_var_implicitly_typed_local_variable_declaration ref_kind declarator))

(defrecord hydra_csharp_syntax_implicitly_typed_local_variable_declarator [identifier expression])
(defn make-hydra_csharp_syntax_implicitly_typed_local_variable_declarator [identifier expression] (->hydra_csharp_syntax_implicitly_typed_local_variable_declarator identifier expression))

(defrecord hydra_csharp_syntax_explicitly_typed_local_variable_declaration [type declarators])
(defn make-hydra_csharp_syntax_explicitly_typed_local_variable_declaration [type declarators] (->hydra_csharp_syntax_explicitly_typed_local_variable_declaration type declarators))

(defrecord hydra_csharp_syntax_explicitly_typed_local_variable_declarator [identifier initializer])
(defn make-hydra_csharp_syntax_explicitly_typed_local_variable_declarator [identifier initializer] (->hydra_csharp_syntax_explicitly_typed_local_variable_declarator identifier initializer))

(def hydra_csharp_syntax_local_variable_initializer-variants (list :expression :initializer))

(defrecord hydra_csharp_syntax_ref_local_variable_declaration [ref_kind type declarators])
(defn make-hydra_csharp_syntax_ref_local_variable_declaration [ref_kind type declarators] (->hydra_csharp_syntax_ref_local_variable_declaration ref_kind type declarators))

(defrecord hydra_csharp_syntax_ref_local_variable_declarator [left right])
(defn make-hydra_csharp_syntax_ref_local_variable_declarator [left right] (->hydra_csharp_syntax_ref_local_variable_declarator left right))

(defrecord hydra_csharp_syntax_local_constant_declaration [type declarators])
(defn make-hydra_csharp_syntax_local_constant_declaration [type declarators] (->hydra_csharp_syntax_local_constant_declaration type declarators))

(defrecord hydra_csharp_syntax_constant_declarator [identifier expression])
(defn make-hydra_csharp_syntax_constant_declarator [identifier expression] (->hydra_csharp_syntax_constant_declarator identifier expression))

(def hydra_csharp_syntax_local_function_declaration-variants (list :standard :ref))

(defrecord hydra_csharp_syntax_standard_local_function_declaration [modifiers return_type header body])
(defn make-hydra_csharp_syntax_standard_local_function_declaration [modifiers return_type header body] (->hydra_csharp_syntax_standard_local_function_declaration modifiers return_type header body))

(defrecord hydra_csharp_syntax_ref_local_function_declaration [modifiers ref_kind return_type header body])
(defn make-hydra_csharp_syntax_ref_local_function_declaration [modifiers ref_kind return_type header body] (->hydra_csharp_syntax_ref_local_function_declaration modifiers ref_kind return_type header body))

(defrecord hydra_csharp_syntax_local_function_header [identifier type_parameters parameters constraints])
(defn make-hydra_csharp_syntax_local_function_header [identifier type_parameters parameters constraints] (->hydra_csharp_syntax_local_function_header identifier type_parameters parameters constraints))

(def hydra_csharp_syntax_local_function_modifier-variants (list :ref :async))

(def hydra_csharp_syntax_ref_local_function_modifier-variants (list :static :unsafe))

(def hydra_csharp_syntax_local_function_body-variants (list :block :null_conditional_invocation :expression))

(def hydra_csharp_syntax_ref_local_function_body-variants (list :block :ref))

(def hydra_csharp_syntax_statement_expression-variants (list :null_conditional_invocation :invocation :object_creation :assignment :post_increment :post_decrement :pre_increment :pre_decrement :await))

(def hydra_csharp_syntax_selection_statement-variants (list :if :switch))

(defrecord hydra_csharp_syntax_if_statement [condition if_branch else_branch])
(defn make-hydra_csharp_syntax_if_statement [condition if_branch else_branch] (->hydra_csharp_syntax_if_statement condition if_branch else_branch))

(defrecord hydra_csharp_syntax_switch_statement [expression branches])
(defn make-hydra_csharp_syntax_switch_statement [expression branches] (->hydra_csharp_syntax_switch_statement expression branches))

(defrecord hydra_csharp_syntax_switch_section [labels statements])
(defn make-hydra_csharp_syntax_switch_section [labels statements] (->hydra_csharp_syntax_switch_section labels statements))

(def hydra_csharp_syntax_switch_label-variants (list :branch :default))

(defrecord hydra_csharp_syntax_switch_branch [pattern guard])
(defn make-hydra_csharp_syntax_switch_branch [pattern guard] (->hydra_csharp_syntax_switch_branch pattern guard))

(def hydra_csharp_syntax_iteration_statement-variants (list :while :do :for :foreach))

(defrecord hydra_csharp_syntax_while_statement [condition body])
(defn make-hydra_csharp_syntax_while_statement [condition body] (->hydra_csharp_syntax_while_statement condition body))

(defrecord hydra_csharp_syntax_do_statement [body while])
(defn make-hydra_csharp_syntax_do_statement [body while] (->hydra_csharp_syntax_do_statement body while))

(defrecord hydra_csharp_syntax_for_statement [initializer condition iterator body])
(defn make-hydra_csharp_syntax_for_statement [initializer condition iterator body] (->hydra_csharp_syntax_for_statement initializer condition iterator body))

(def hydra_csharp_syntax_for_initializer-variants (list :variable :statements))

(defrecord hydra_csharp_syntax_statement_expression_list [value])
(defn make-hydra_csharp_syntax_statement_expression_list [value] (->hydra_csharp_syntax_statement_expression_list value))

(defrecord hydra_csharp_syntax_foreach_statement [kind type identifier expression body])
(defn make-hydra_csharp_syntax_foreach_statement [kind type identifier expression body] (->hydra_csharp_syntax_foreach_statement kind type identifier expression body))

(def hydra_csharp_syntax_jump_statement-variants (list :break :continue :goto :return :throw))

(def hydra_csharp_syntax_goto_statement-variants (list :identifier :case :default))

(def hydra_csharp_syntax_return_statement-variants (list :simple :value :ref))

(defrecord hydra_csharp_syntax_try_statement [body catches finally])
(defn make-hydra_csharp_syntax_try_statement [body catches finally] (->hydra_csharp_syntax_try_statement body catches finally))

(def hydra_csharp_syntax_catch_clauses-variants (list :specific :general))

(defrecord hydra_csharp_syntax_specific_catch_clause [specifier filter body])
(defn make-hydra_csharp_syntax_specific_catch_clause [specifier filter body] (->hydra_csharp_syntax_specific_catch_clause specifier filter body))

(defrecord hydra_csharp_syntax_exception_specifier [type identifier])
(defn make-hydra_csharp_syntax_exception_specifier [type identifier] (->hydra_csharp_syntax_exception_specifier type identifier))

(defrecord hydra_csharp_syntax_lock_statement [expression body])
(defn make-hydra_csharp_syntax_lock_statement [expression body] (->hydra_csharp_syntax_lock_statement expression body))

(defrecord hydra_csharp_syntax_using_statement [acquisition body])
(defn make-hydra_csharp_syntax_using_statement [acquisition body] (->hydra_csharp_syntax_using_statement acquisition body))

(def hydra_csharp_syntax_resource_acquisition-variants (list :local :expression))

(def hydra_csharp_syntax_yield_statement-variants (list :return :break))

(defrecord hydra_csharp_syntax_compilation_unit [externs usings attributes members])
(defn make-hydra_csharp_syntax_compilation_unit [externs usings attributes members] (->hydra_csharp_syntax_compilation_unit externs usings attributes members))

(defrecord hydra_csharp_syntax_namespace_declaration [name body])
(defn make-hydra_csharp_syntax_namespace_declaration [name body] (->hydra_csharp_syntax_namespace_declaration name body))

(defrecord hydra_csharp_syntax_qualified_identifier [value])
(defn make-hydra_csharp_syntax_qualified_identifier [value] (->hydra_csharp_syntax_qualified_identifier value))

(defrecord hydra_csharp_syntax_namespace_body [externs usings members])
(defn make-hydra_csharp_syntax_namespace_body [externs usings members] (->hydra_csharp_syntax_namespace_body externs usings members))

(defrecord hydra_csharp_syntax_extern_alias_directive [value])
(defn make-hydra_csharp_syntax_extern_alias_directive [value] (->hydra_csharp_syntax_extern_alias_directive value))

(def hydra_csharp_syntax_using_directive-variants (list :alias :namespace :static))

(defrecord hydra_csharp_syntax_using_alias_directive [alias name])
(defn make-hydra_csharp_syntax_using_alias_directive [alias name] (->hydra_csharp_syntax_using_alias_directive alias name))

(def hydra_csharp_syntax_namespace_member_declaration-variants (list :namespace :type))

(def hydra_csharp_syntax_type_declaration-variants (list :class :struct :interface :enum :delegate))

(defrecord hydra_csharp_syntax_qualified_alias_member [alias member arguments])
(defn make-hydra_csharp_syntax_qualified_alias_member [alias member arguments] (->hydra_csharp_syntax_qualified_alias_member alias member arguments))

(defrecord hydra_csharp_syntax_class_declaration [attributes modifiers partial name parameters base constraints body])
(defn make-hydra_csharp_syntax_class_declaration [attributes modifiers partial name parameters base constraints body] (->hydra_csharp_syntax_class_declaration attributes modifiers partial name parameters base constraints body))

(def hydra_csharp_syntax_class_modifier-variants (list :new :public :protected :internal :private :abstract :sealed :static :unsafe))

(defrecord hydra_csharp_syntax_type_parameter_list [value])
(defn make-hydra_csharp_syntax_type_parameter_list [value] (->hydra_csharp_syntax_type_parameter_list value))

(defrecord hydra_csharp_syntax_type_parameter_part [attributes name])
(defn make-hydra_csharp_syntax_type_parameter_part [attributes name] (->hydra_csharp_syntax_type_parameter_part attributes name))

(def hydra_csharp_syntax_class_base-variants (list :class :interfaces))

(defrecord hydra_csharp_syntax_type_parameter_constraints_clause [parameter constraints])
(defn make-hydra_csharp_syntax_type_parameter_constraints_clause [parameter constraints] (->hydra_csharp_syntax_type_parameter_constraints_clause parameter constraints))

(defrecord hydra_csharp_syntax_type_parameter_constraints [primary secondary constructor])
(defn make-hydra_csharp_syntax_type_parameter_constraints [primary secondary constructor] (->hydra_csharp_syntax_type_parameter_constraints primary secondary constructor))

(def hydra_csharp_syntax_primary_constraint-variants (list :class_type :class :struct :unmanaged))

(defrecord hydra_csharp_syntax_secondary_constraints [value])
(defn make-hydra_csharp_syntax_secondary_constraints [value] (->hydra_csharp_syntax_secondary_constraints value))

(def hydra_csharp_syntax_secondary_constraint-variants (list :interface :parameter))

(defrecord hydra_csharp_syntax_class_body [value])
(defn make-hydra_csharp_syntax_class_body [value] (->hydra_csharp_syntax_class_body value))

(def hydra_csharp_syntax_class_member_declaration-variants (list :constant :field :method :property :event :indexer :operator :constructor :finalizer :static_constructor :type))

(defrecord hydra_csharp_syntax_constant_declaration [attributes modifiers type declarators])
(defn make-hydra_csharp_syntax_constant_declaration [attributes modifiers type declarators] (->hydra_csharp_syntax_constant_declaration attributes modifiers type declarators))

(def hydra_csharp_syntax_constant_modifier-variants (list :new :public :protected :internal :private))

(defrecord hydra_csharp_syntax_field_declaration [attributes modifiers type declarators])
(defn make-hydra_csharp_syntax_field_declaration [attributes modifiers type declarators] (->hydra_csharp_syntax_field_declaration attributes modifiers type declarators))

(def hydra_csharp_syntax_field_modifier-variants (list :new :public :protected :internal :private :static :readonly :volatile :unsafe))

(defrecord hydra_csharp_syntax_variable_declarators [value])
(defn make-hydra_csharp_syntax_variable_declarators [value] (->hydra_csharp_syntax_variable_declarators value))

(defrecord hydra_csharp_syntax_variable_declarator [identifier initializer])
(defn make-hydra_csharp_syntax_variable_declarator [identifier initializer] (->hydra_csharp_syntax_variable_declarator identifier initializer))

(def hydra_csharp_syntax_method_declaration-variants (list :standard :ref_return))

(defrecord hydra_csharp_syntax_standard_method_declaration [attributes modifiers return_type header body])
(defn make-hydra_csharp_syntax_standard_method_declaration [attributes modifiers return_type header body] (->hydra_csharp_syntax_standard_method_declaration attributes modifiers return_type header body))

(defrecord hydra_csharp_syntax_ref_return_method_declaration [attributes modifiers kind return_type header body])
(defn make-hydra_csharp_syntax_ref_return_method_declaration [attributes modifiers kind return_type header body] (->hydra_csharp_syntax_ref_return_method_declaration attributes modifiers kind return_type header body))

(defrecord hydra_csharp_syntax_method_modifiers [modifiers partial])
(defn make-hydra_csharp_syntax_method_modifiers [modifiers partial] (->hydra_csharp_syntax_method_modifiers modifiers partial))

(def hydra_csharp_syntax_ref_kind-variants (list :ref :ref_readonly))

(defrecord hydra_csharp_syntax_method_header [name type_parameters parameters constraints])
(defn make-hydra_csharp_syntax_method_header [name type_parameters parameters constraints] (->hydra_csharp_syntax_method_header name type_parameters parameters constraints))

(def hydra_csharp_syntax_method_modifier-variants (list :ref :async))

(def hydra_csharp_syntax_ref_method_modifier-variants (list :new :public :protected :internal :private :static :virtual :sealed :override :abstract :extern :unsafe))

(def hydra_csharp_syntax_return_type-variants (list :ref :void))

(defrecord hydra_csharp_syntax_member_name [interface_type identifier])
(defn make-hydra_csharp_syntax_member_name [interface_type identifier] (->hydra_csharp_syntax_member_name interface_type identifier))

(def hydra_csharp_syntax_method_body-variants (list :block :null_conditional_invocation :expression :empty))

(def hydra_csharp_syntax_ref_method_body-variants (list :block :ref :empty))

(defrecord hydra_csharp_syntax_formal_parameter_list [fixed array])
(defn make-hydra_csharp_syntax_formal_parameter_list [fixed array] (->hydra_csharp_syntax_formal_parameter_list fixed array))

(defrecord hydra_csharp_syntax_fixed_parameter [attributes modifier type identifier default_argument])
(defn make-hydra_csharp_syntax_fixed_parameter [attributes modifier type identifier default_argument] (->hydra_csharp_syntax_fixed_parameter attributes modifier type identifier default_argument))

(def hydra_csharp_syntax_parameter_modifier-variants (list :mode :this))

(def hydra_csharp_syntax_parameter_mode_modifier-variants (list :ref :out :in))

(defrecord hydra_csharp_syntax_parameter_array [attributes type identifier])
(defn make-hydra_csharp_syntax_parameter_array [attributes type identifier] (->hydra_csharp_syntax_parameter_array attributes type identifier))

(def hydra_csharp_syntax_property_declaration-variants (list :standard :ref_return))

(defrecord hydra_csharp_syntax_standard_property_declaration [attributes modifiers type name body])
(defn make-hydra_csharp_syntax_standard_property_declaration [attributes modifiers type name body] (->hydra_csharp_syntax_standard_property_declaration attributes modifiers type name body))

(defrecord hydra_csharp_syntax_ref_return_property_declaration [attributes modifiers ref_kind type name body])
(defn make-hydra_csharp_syntax_ref_return_property_declaration [attributes modifiers ref_kind type name body] (->hydra_csharp_syntax_ref_return_property_declaration attributes modifiers ref_kind type name body))

(def hydra_csharp_syntax_property_modifier-variants (list :new :public :protected :internal :private :static :virtual :sealed :override :abstract :extern :unsafe))

(def hydra_csharp_syntax_property_body-variants (list :block :expression))

(defrecord hydra_csharp_syntax_block_property_body [accessors initializer])
(defn make-hydra_csharp_syntax_block_property_body [accessors initializer] (->hydra_csharp_syntax_block_property_body accessors initializer))

(def hydra_csharp_syntax_ref_property_body-variants (list :block :ref))

(def hydra_csharp_syntax_accessor_declarations-variants (list :get :set))

(defrecord hydra_csharp_syntax_accessor_declaration [attributes modifier body])
(defn make-hydra_csharp_syntax_accessor_declaration [attributes modifier body] (->hydra_csharp_syntax_accessor_declaration attributes modifier body))

(def hydra_csharp_syntax_accessor_modifier-variants (list :protected :internal :private :protected_internal :internal_protected :protected_private :private_protected))

(def hydra_csharp_syntax_accessor_body-variants (list :block :expression :empty))

(defrecord hydra_csharp_syntax_ref_get_accessor_declaration [attributes modifier body])
(defn make-hydra_csharp_syntax_ref_get_accessor_declaration [attributes modifier body] (->hydra_csharp_syntax_ref_get_accessor_declaration attributes modifier body))

(def hydra_csharp_syntax_ref_accessor_body-variants (list :block :ref :empty))

(def hydra_csharp_syntax_event_declaration-variants (list :standard :accessors))

(defrecord hydra_csharp_syntax_standard_event_declaration [attributes modifiers type declarators])
(defn make-hydra_csharp_syntax_standard_event_declaration [attributes modifiers type declarators] (->hydra_csharp_syntax_standard_event_declaration attributes modifiers type declarators))

(defrecord hydra_csharp_syntax_accessors_event_declaration [attributes modifiers type name accessors])
(defn make-hydra_csharp_syntax_accessors_event_declaration [attributes modifiers type name accessors] (->hydra_csharp_syntax_accessors_event_declaration attributes modifiers type name accessors))

(def hydra_csharp_syntax_event_modifier-variants (list :new :public :protected :internal :private :static :virtual :sealed :override :abstract :extern :unsafe))

(def hydra_csharp_syntax_event_accessor_declarations-variants (list :add :remove))

(defrecord hydra_csharp_syntax_add_remove_accessor_declaration [attributes body])
(defn make-hydra_csharp_syntax_add_remove_accessor_declaration [attributes body] (->hydra_csharp_syntax_add_remove_accessor_declaration attributes body))

(def hydra_csharp_syntax_indexer_declaration-variants (list :standard :ref))

(defrecord hydra_csharp_syntax_standard_indexer_declaration [attributes modifiers declarator body])
(defn make-hydra_csharp_syntax_standard_indexer_declaration [attributes modifiers declarator body] (->hydra_csharp_syntax_standard_indexer_declaration attributes modifiers declarator body))

(defrecord hydra_csharp_syntax_ref_indexer_declaration [attributes modifiers ref_kind declarator body])
(defn make-hydra_csharp_syntax_ref_indexer_declaration [attributes modifiers ref_kind declarator body] (->hydra_csharp_syntax_ref_indexer_declaration attributes modifiers ref_kind declarator body))

(def hydra_csharp_syntax_indexer_modifier-variants (list :new :public :protected :internal :private :virtual :sealed :override :abstract :extern :unsafe))

(defrecord hydra_csharp_syntax_indexer_declarator [type interface parameters])
(defn make-hydra_csharp_syntax_indexer_declarator [type interface parameters] (->hydra_csharp_syntax_indexer_declarator type interface parameters))

(def hydra_csharp_syntax_indexer_body-variants (list :block :expression))

(def hydra_csharp_syntax_ref_indexer_body-variants (list :block :ref))

(defrecord hydra_csharp_syntax_operator_declaration [attributes modifiers declarator body])
(defn make-hydra_csharp_syntax_operator_declaration [attributes modifiers declarator body] (->hydra_csharp_syntax_operator_declaration attributes modifiers declarator body))

(def hydra_csharp_syntax_operator_modifier-variants (list :public :static :extern :unsafe))

(def hydra_csharp_syntax_operator_declarator-variants (list :unary :binary :conversion))

(defrecord hydra_csharp_syntax_unary_operator_declarator [type operator parameter])
(defn make-hydra_csharp_syntax_unary_operator_declarator [type operator parameter] (->hydra_csharp_syntax_unary_operator_declarator type operator parameter))

(def hydra_csharp_syntax_overloadable_unary_operator-variants (list :plus :minus :not :complement :increment :decrement :true :false))

(defrecord hydra_csharp_syntax_binary_operator_declarator [type operator left right])
(defn make-hydra_csharp_syntax_binary_operator_declarator [type operator left right] (->hydra_csharp_syntax_binary_operator_declarator type operator left right))

(def hydra_csharp_syntax_overloadable_binary_operator-variants (list :add :subtract :multiply :divide :modulus :and :or :xor :left_shift :right_shift :equal :not_equal :greater_than :less_than :greater_than_or_equal :less_than_or_equal))

(defrecord hydra_csharp_syntax_conversion_operator_declarator [kind type parameter])
(defn make-hydra_csharp_syntax_conversion_operator_declarator [kind type parameter] (->hydra_csharp_syntax_conversion_operator_declarator kind type parameter))

(def hydra_csharp_syntax_conversion_kind-variants (list :implicit :explicit))

(def hydra_csharp_syntax_operator_body-variants (list :block :expression :empty))

(defrecord hydra_csharp_syntax_constructor_declaration [attributes modifiers declarator body])
(defn make-hydra_csharp_syntax_constructor_declaration [attributes modifiers declarator body] (->hydra_csharp_syntax_constructor_declaration attributes modifiers declarator body))

(def hydra_csharp_syntax_constructor_modifier-variants (list :public :protected :internal :private :extern :unsafe))

(defrecord hydra_csharp_syntax_constructor_declarator [name parameters initializer])
(defn make-hydra_csharp_syntax_constructor_declarator [name parameters initializer] (->hydra_csharp_syntax_constructor_declarator name parameters initializer))

(def hydra_csharp_syntax_constructor_initializer-variants (list :base :this))

(def hydra_csharp_syntax_constructor_body-variants (list :block :expression :empty))

(defrecord hydra_csharp_syntax_static_constructor_declaration [attributes modifiers name body])
(defn make-hydra_csharp_syntax_static_constructor_declaration [attributes modifiers name body] (->hydra_csharp_syntax_static_constructor_declaration attributes modifiers name body))

(defrecord hydra_csharp_syntax_static_constructor_modifiers [extern unsafe])
(defn make-hydra_csharp_syntax_static_constructor_modifiers [extern unsafe] (->hydra_csharp_syntax_static_constructor_modifiers extern unsafe))

(def hydra_csharp_syntax_static_constructor_body-variants (list :block :expression :empty))

(defrecord hydra_csharp_syntax_finalizer_declaration [attributes extern unsafe name body])
(defn make-hydra_csharp_syntax_finalizer_declaration [attributes extern unsafe name body] (->hydra_csharp_syntax_finalizer_declaration attributes extern unsafe name body))

(def hydra_csharp_syntax_finalizer_body-variants (list :block :expression :empty))

(defrecord hydra_csharp_syntax_struct_declaration [attributes modifiers ref partial name parameters interfaces constraints body])
(defn make-hydra_csharp_syntax_struct_declaration [attributes modifiers ref partial name parameters interfaces constraints body] (->hydra_csharp_syntax_struct_declaration attributes modifiers ref partial name parameters interfaces constraints body))

(def hydra_csharp_syntax_struct_modifier-variants (list :new :public :protected :internal :private :readonly :unsafe))

(def hydra_csharp_syntax_struct_member_declaration-variants (list :constant :field :method :property :event :indexer :operator :constructor :static_constructor :type :fixed_size_buffer))

(defrecord hydra_csharp_syntax_array_initializer [value])
(defn make-hydra_csharp_syntax_array_initializer [value] (->hydra_csharp_syntax_array_initializer value))

(def hydra_csharp_syntax_variable_initializer-variants (list :expression :array))

(defrecord hydra_csharp_syntax_interface_declaration [attributes modifiers partial name parameters base constraints body])
(defn make-hydra_csharp_syntax_interface_declaration [attributes modifiers partial name parameters base constraints body] (->hydra_csharp_syntax_interface_declaration attributes modifiers partial name parameters base constraints body))

(def hydra_csharp_syntax_interface_modifier-variants (list :new :public :protected :internal :private :unsafe))

(defrecord hydra_csharp_syntax_variant_type_parameters [value])
(defn make-hydra_csharp_syntax_variant_type_parameters [value] (->hydra_csharp_syntax_variant_type_parameters value))

(defrecord hydra_csharp_syntax_variant_type_parameter [attributes variance parameter])
(defn make-hydra_csharp_syntax_variant_type_parameter [attributes variance parameter] (->hydra_csharp_syntax_variant_type_parameter attributes variance parameter))

(def hydra_csharp_syntax_variance_annotation-variants (list :in :out))

(def hydra_csharp_syntax_interface_member_declaration-variants (list :method :property :event :indexer))

(defrecord hydra_csharp_syntax_interface_method_declaration [attributes new return_type ref_kind header])
(defn make-hydra_csharp_syntax_interface_method_declaration [attributes new return_type ref_kind header] (->hydra_csharp_syntax_interface_method_declaration attributes new return_type ref_kind header))

(defrecord hydra_csharp_syntax_interface_method_header [name parameters type_parameters constraints])
(defn make-hydra_csharp_syntax_interface_method_header [name parameters type_parameters constraints] (->hydra_csharp_syntax_interface_method_header name parameters type_parameters constraints))

(defrecord hydra_csharp_syntax_interface_property_declaration [attributes new ref_kind type name accessors])
(defn make-hydra_csharp_syntax_interface_property_declaration [attributes new ref_kind type name accessors] (->hydra_csharp_syntax_interface_property_declaration attributes new ref_kind type name accessors))

(defrecord hydra_csharp_syntax_interface_accessors [attributes get set])
(defn make-hydra_csharp_syntax_interface_accessors [attributes get set] (->hydra_csharp_syntax_interface_accessors attributes get set))

(defrecord hydra_csharp_syntax_interface_event_declaration [attributes new type name])
(defn make-hydra_csharp_syntax_interface_event_declaration [attributes new type name] (->hydra_csharp_syntax_interface_event_declaration attributes new type name))

(defrecord hydra_csharp_syntax_interface_indexer_declaration [attributes new ref_kind type parameters accessors])
(defn make-hydra_csharp_syntax_interface_indexer_declaration [attributes new ref_kind type parameters accessors] (->hydra_csharp_syntax_interface_indexer_declaration attributes new ref_kind type parameters accessors))

(defrecord hydra_csharp_syntax_enum_declaration [attributes modifiers name base body])
(defn make-hydra_csharp_syntax_enum_declaration [attributes modifiers name base body] (->hydra_csharp_syntax_enum_declaration attributes modifiers name base body))

(def hydra_csharp_syntax_enum_base-variants (list :type :name))

(defrecord hydra_csharp_syntax_enum_body [value])
(defn make-hydra_csharp_syntax_enum_body [value] (->hydra_csharp_syntax_enum_body value))

(def hydra_csharp_syntax_enum_modifier-variants (list :new :public :protected :internal :private))

(defrecord hydra_csharp_syntax_enum_member_declaration [attributes name value])
(defn make-hydra_csharp_syntax_enum_member_declaration [attributes name value] (->hydra_csharp_syntax_enum_member_declaration attributes name value))

(defrecord hydra_csharp_syntax_delegate_declaration [attributes modifiers return_type ref_kind ref_return_type header])
(defn make-hydra_csharp_syntax_delegate_declaration [attributes modifiers return_type ref_kind ref_return_type header] (->hydra_csharp_syntax_delegate_declaration attributes modifiers return_type ref_kind ref_return_type header))

(defrecord hydra_csharp_syntax_delegate_header [name type_parameters parameters constraints])
(defn make-hydra_csharp_syntax_delegate_header [name type_parameters parameters constraints] (->hydra_csharp_syntax_delegate_header name type_parameters parameters constraints))

(def hydra_csharp_syntax_delegate_modifier-variants (list :new :public :protected :internal :private :unsafe))

(defrecord hydra_csharp_syntax_global_attribute_section [target attributes])
(defn make-hydra_csharp_syntax_global_attribute_section [target attributes] (->hydra_csharp_syntax_global_attribute_section target attributes))

(defrecord hydra_csharp_syntax_attributes [value])
(defn make-hydra_csharp_syntax_attributes [value] (->hydra_csharp_syntax_attributes value))

(defrecord hydra_csharp_syntax_attribute_section [target attributes])
(defn make-hydra_csharp_syntax_attribute_section [target attributes] (->hydra_csharp_syntax_attribute_section target attributes))

(def hydra_csharp_syntax_attribute_target-variants (list :identifier :keyword))

(defrecord hydra_csharp_syntax_attribute_list [value])
(defn make-hydra_csharp_syntax_attribute_list [value] (->hydra_csharp_syntax_attribute_list value))

(defrecord hydra_csharp_syntax_attribute [name arguments])
(defn make-hydra_csharp_syntax_attribute [name arguments] (->hydra_csharp_syntax_attribute name arguments))

(defrecord hydra_csharp_syntax_attribute_name [value])
(defn make-hydra_csharp_syntax_attribute_name [value] (->hydra_csharp_syntax_attribute_name value))

(defrecord hydra_csharp_syntax_attribute_arguments [positonal named])
(defn make-hydra_csharp_syntax_attribute_arguments [positonal named] (->hydra_csharp_syntax_attribute_arguments positonal named))

(defrecord hydra_csharp_syntax_positional_argument_list [value])
(defn make-hydra_csharp_syntax_positional_argument_list [value] (->hydra_csharp_syntax_positional_argument_list value))

(defrecord hydra_csharp_syntax_positional_argument [name value])
(defn make-hydra_csharp_syntax_positional_argument [name value] (->hydra_csharp_syntax_positional_argument name value))

(defrecord hydra_csharp_syntax_named_argument_list [value])
(defn make-hydra_csharp_syntax_named_argument_list [value] (->hydra_csharp_syntax_named_argument_list value))

(defrecord hydra_csharp_syntax_named_argument [name value])
(defn make-hydra_csharp_syntax_named_argument [name value] (->hydra_csharp_syntax_named_argument name value))

(defrecord hydra_csharp_syntax_attribute_argument_expression [value])
(defn make-hydra_csharp_syntax_attribute_argument_expression [value] (->hydra_csharp_syntax_attribute_argument_expression value))

(def hydra_csharp_syntax_pointer_type-variants (list :value_type :pointer_depth))

(defrecord hydra_csharp_syntax_pointer_member_access [pointer member type_arguments])
(defn make-hydra_csharp_syntax_pointer_member_access [pointer member type_arguments] (->hydra_csharp_syntax_pointer_member_access pointer member type_arguments))

(defrecord hydra_csharp_syntax_pointer_element_access [pointer index])
(defn make-hydra_csharp_syntax_pointer_element_access [pointer index] (->hydra_csharp_syntax_pointer_element_access pointer index))

(defrecord hydra_csharp_syntax_fixed_statement [pointer_type declarators statement])
(defn make-hydra_csharp_syntax_fixed_statement [pointer_type declarators statement] (->hydra_csharp_syntax_fixed_statement pointer_type declarators statement))

(def hydra_csharp_syntax_fixed_pointer_declarator-variants (list :reference :expression))

(defrecord hydra_csharp_syntax_fixed_size_buffer_declaration [attributes modifiers element_type declarators])
(defn make-hydra_csharp_syntax_fixed_size_buffer_declaration [attributes modifiers element_type declarators] (->hydra_csharp_syntax_fixed_size_buffer_declaration attributes modifiers element_type declarators))

(def hydra_csharp_syntax_fixed_size_buffer_modifier-variants (list :new :public :internal :private :unsafe))

(defrecord hydra_csharp_syntax_fixed_size_buffer_declarator [name size])
(defn make-hydra_csharp_syntax_fixed_size_buffer_declarator [name size] (->hydra_csharp_syntax_fixed_size_buffer_declarator name size))

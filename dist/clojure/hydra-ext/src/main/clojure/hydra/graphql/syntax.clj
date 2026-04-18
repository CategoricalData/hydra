(ns hydra.graphql.syntax)

(declare hydra_graphql_syntax_definition-variants hydra_graphql_syntax_executable_definition-variants hydra_graphql_syntax_operation_definition-variants hydra_graphql_syntax_operation_type-variants hydra_graphql_syntax_selection-variants hydra_graphql_syntax_alias-variants hydra_graphql_syntax_type_condition-variants hydra_graphql_syntax_value-variants hydra_graphql_syntax_boolean_value-variants hydra_graphql_syntax_list_value-variants hydra_graphql_syntax_object_value-variants hydra_graphql_syntax_type-variants hydra_graphql_syntax_non_null_type-variants hydra_graphql_syntax_type_system_definition-variants hydra_graphql_syntax_type_system_definition_or_extension-variants hydra_graphql_syntax_type_system_extension-variants hydra_graphql_syntax_schema_extension-variants hydra_graphql_syntax_type_definition-variants hydra_graphql_syntax_type_extension-variants hydra_graphql_syntax_object_type_extension-variants hydra_graphql_syntax_implements_interfaces-variants hydra_graphql_syntax_interface_type_definition-variants hydra_graphql_syntax_interface_type_extension-variants hydra_graphql_syntax_union_member_types-variants hydra_graphql_syntax_union_type_extension-variants hydra_graphql_syntax_enum_type_extension-variants hydra_graphql_syntax_input_object_type_definition-variants hydra_graphql_syntax_input_object_type_extension-variants hydra_graphql_syntax_directive_locations-variants hydra_graphql_syntax_directive_location-variants hydra_graphql_syntax_executable_directive_location-variants hydra_graphql_syntax_type_system_directive_location-variants)

(defrecord hydra_graphql_syntax_name [value])
(defn make-hydra_graphql_syntax_name [value] (->hydra_graphql_syntax_name value))

(defrecord hydra_graphql_syntax_int_value [value])
(defn make-hydra_graphql_syntax_int_value [value] (->hydra_graphql_syntax_int_value value))

(defrecord hydra_graphql_syntax_float_value [value])
(defn make-hydra_graphql_syntax_float_value [value] (->hydra_graphql_syntax_float_value value))

(defrecord hydra_graphql_syntax_string_value [value])
(defn make-hydra_graphql_syntax_string_value [value] (->hydra_graphql_syntax_string_value value))

(defrecord hydra_graphql_syntax_document [value])
(defn make-hydra_graphql_syntax_document [value] (->hydra_graphql_syntax_document value))

(def hydra_graphql_syntax_definition-variants (list :executable :type_system))

(defrecord hydra_graphql_syntax_executable_document [value])
(defn make-hydra_graphql_syntax_executable_document [value] (->hydra_graphql_syntax_executable_document value))

(def hydra_graphql_syntax_executable_definition-variants (list :operation :fragment))

(def hydra_graphql_syntax_operation_definition-variants (list :sequence :selection_set))

(defrecord hydra_graphql_syntax_operation_definition_sequence [operation_type name variables_definition directives selection_set])
(defn make-hydra_graphql_syntax_operation_definition_sequence [operation_type name variables_definition directives selection_set] (->hydra_graphql_syntax_operation_definition_sequence operation_type name variables_definition directives selection_set))

(def hydra_graphql_syntax_operation_type-variants (list :query :mutation :subscription))

(defrecord hydra_graphql_syntax_selection_set [value])
(defn make-hydra_graphql_syntax_selection_set [value] (->hydra_graphql_syntax_selection_set value))

(def hydra_graphql_syntax_selection-variants (list :field :fragment_spread :inline_fragment))

(defrecord hydra_graphql_syntax_field [alias name arguments directives selection_set])
(defn make-hydra_graphql_syntax_field [alias name arguments directives selection_set] (->hydra_graphql_syntax_field alias name arguments directives selection_set))

(def hydra_graphql_syntax_alias-variants (list :name :colon))

(defrecord hydra_graphql_syntax_arguments [value])
(defn make-hydra_graphql_syntax_arguments [value] (->hydra_graphql_syntax_arguments value))

(defrecord hydra_graphql_syntax_argument [name value])
(defn make-hydra_graphql_syntax_argument [name value] (->hydra_graphql_syntax_argument name value))

(defrecord hydra_graphql_syntax_fragment_spread [fragment_name directives])
(defn make-hydra_graphql_syntax_fragment_spread [fragment_name directives] (->hydra_graphql_syntax_fragment_spread fragment_name directives))

(defrecord hydra_graphql_syntax_inline_fragment [type_condition directives selection_set])
(defn make-hydra_graphql_syntax_inline_fragment [type_condition directives selection_set] (->hydra_graphql_syntax_inline_fragment type_condition directives selection_set))

(defrecord hydra_graphql_syntax_fragment_definition [fragment_name type_condition directives selection_set])
(defn make-hydra_graphql_syntax_fragment_definition [fragment_name type_condition directives selection_set] (->hydra_graphql_syntax_fragment_definition fragment_name type_condition directives selection_set))

(defrecord hydra_graphql_syntax_fragment_name [value])
(defn make-hydra_graphql_syntax_fragment_name [value] (->hydra_graphql_syntax_fragment_name value))

(def hydra_graphql_syntax_type_condition-variants (list :on :named_type))

(def hydra_graphql_syntax_value-variants (list :variable :int :float :string :boolean :null :enum :list :object))

(def hydra_graphql_syntax_boolean_value-variants (list :true :false))

(defrecord hydra_graphql_syntax_null_value [value])
(defn make-hydra_graphql_syntax_null_value [value] (->hydra_graphql_syntax_null_value value))

(defrecord hydra_graphql_syntax_enum_value [value])
(defn make-hydra_graphql_syntax_enum_value [value] (->hydra_graphql_syntax_enum_value value))

(def hydra_graphql_syntax_list_value-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_list_value_sequence [])
(defn make-hydra_graphql_syntax_list_value_sequence [] (->hydra_graphql_syntax_list_value_sequence))

(def hydra_graphql_syntax_object_value-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_object_value_sequence [])
(defn make-hydra_graphql_syntax_object_value_sequence [] (->hydra_graphql_syntax_object_value_sequence))

(defrecord hydra_graphql_syntax_object_field [name value])
(defn make-hydra_graphql_syntax_object_field [name value] (->hydra_graphql_syntax_object_field name value))

(defrecord hydra_graphql_syntax_variables_definition [variable type default_value directives])
(defn make-hydra_graphql_syntax_variables_definition [variable type default_value directives] (->hydra_graphql_syntax_variables_definition variable type default_value directives))

(defrecord hydra_graphql_syntax_variable [value])
(defn make-hydra_graphql_syntax_variable [value] (->hydra_graphql_syntax_variable value))

(defrecord hydra_graphql_syntax_default_value [value])
(defn make-hydra_graphql_syntax_default_value [value] (->hydra_graphql_syntax_default_value value))

(def hydra_graphql_syntax_type-variants (list :named :list :non_null))

(defrecord hydra_graphql_syntax_named_type [value])
(defn make-hydra_graphql_syntax_named_type [value] (->hydra_graphql_syntax_named_type value))

(defrecord hydra_graphql_syntax_list_type [value])
(defn make-hydra_graphql_syntax_list_type [value] (->hydra_graphql_syntax_list_type value))

(def hydra_graphql_syntax_non_null_type-variants (list :named :list))

(defrecord hydra_graphql_syntax_directives [value])
(defn make-hydra_graphql_syntax_directives [value] (->hydra_graphql_syntax_directives value))

(defrecord hydra_graphql_syntax_directive [name arguments])
(defn make-hydra_graphql_syntax_directive [name arguments] (->hydra_graphql_syntax_directive name arguments))

(defrecord hydra_graphql_syntax_type_system_docment [value])
(defn make-hydra_graphql_syntax_type_system_docment [value] (->hydra_graphql_syntax_type_system_docment value))

(def hydra_graphql_syntax_type_system_definition-variants (list :schema :type :directive))

(defrecord hydra_graphql_syntax_type_system_extension_document [value])
(defn make-hydra_graphql_syntax_type_system_extension_document [value] (->hydra_graphql_syntax_type_system_extension_document value))

(def hydra_graphql_syntax_type_system_definition_or_extension-variants (list :definition :extension))

(def hydra_graphql_syntax_type_system_extension-variants (list :schema :type))

(defrecord hydra_graphql_syntax_schema_definition [description directives root_operation_type_definition])
(defn make-hydra_graphql_syntax_schema_definition [description directives root_operation_type_definition] (->hydra_graphql_syntax_schema_definition description directives root_operation_type_definition))

(def hydra_graphql_syntax_schema_extension-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_schema_extension_sequence [directives root_operation_type_definition])
(defn make-hydra_graphql_syntax_schema_extension_sequence [directives root_operation_type_definition] (->hydra_graphql_syntax_schema_extension_sequence directives root_operation_type_definition))

(defrecord hydra_graphql_syntax_root_operation_type_definition [operation_type named_type])
(defn make-hydra_graphql_syntax_root_operation_type_definition [operation_type named_type] (->hydra_graphql_syntax_root_operation_type_definition operation_type named_type))

(defrecord hydra_graphql_syntax_description [value])
(defn make-hydra_graphql_syntax_description [value] (->hydra_graphql_syntax_description value))

(def hydra_graphql_syntax_type_definition-variants (list :scalar :object :interface :union :enum :input_object))

(def hydra_graphql_syntax_type_extension-variants (list :scalar :object :interface :union :enum :input_object))

(defrecord hydra_graphql_syntax_scalar_type_definition [description name directives])
(defn make-hydra_graphql_syntax_scalar_type_definition [description name directives] (->hydra_graphql_syntax_scalar_type_definition description name directives))

(defrecord hydra_graphql_syntax_scalar_type_extension [name directives])
(defn make-hydra_graphql_syntax_scalar_type_extension [name directives] (->hydra_graphql_syntax_scalar_type_extension name directives))

(defrecord hydra_graphql_syntax_object_type_definition [description name implements_interfaces directives fields_definition])
(defn make-hydra_graphql_syntax_object_type_definition [description name implements_interfaces directives fields_definition] (->hydra_graphql_syntax_object_type_definition description name implements_interfaces directives fields_definition))

(def hydra_graphql_syntax_object_type_extension-variants (list :sequence :sequence2 :sequence3))

(defrecord hydra_graphql_syntax_object_type_extension_sequence [name implements_interfaces directives fields_definition])
(defn make-hydra_graphql_syntax_object_type_extension_sequence [name implements_interfaces directives fields_definition] (->hydra_graphql_syntax_object_type_extension_sequence name implements_interfaces directives fields_definition))

(defrecord hydra_graphql_syntax_object_type_extension_sequence2 [name implements_interfaces directives])
(defn make-hydra_graphql_syntax_object_type_extension_sequence2 [name implements_interfaces directives] (->hydra_graphql_syntax_object_type_extension_sequence2 name implements_interfaces directives))

(defrecord hydra_graphql_syntax_object_type_extension_sequence3 [name implements_interfaces])
(defn make-hydra_graphql_syntax_object_type_extension_sequence3 [name implements_interfaces] (->hydra_graphql_syntax_object_type_extension_sequence3 name implements_interfaces))

(def hydra_graphql_syntax_implements_interfaces-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_implements_interfaces_sequence [implements_interfaces named_type])
(defn make-hydra_graphql_syntax_implements_interfaces_sequence [implements_interfaces named_type] (->hydra_graphql_syntax_implements_interfaces_sequence implements_interfaces named_type))

(defrecord hydra_graphql_syntax_implements_interfaces_sequence2 [amp named_type])
(defn make-hydra_graphql_syntax_implements_interfaces_sequence2 [amp named_type] (->hydra_graphql_syntax_implements_interfaces_sequence2 amp named_type))

(defrecord hydra_graphql_syntax_fields_definition [value])
(defn make-hydra_graphql_syntax_fields_definition [value] (->hydra_graphql_syntax_fields_definition value))

(defrecord hydra_graphql_syntax_field_definition [description name arguments_definition type directives])
(defn make-hydra_graphql_syntax_field_definition [description name arguments_definition type directives] (->hydra_graphql_syntax_field_definition description name arguments_definition type directives))

(defrecord hydra_graphql_syntax_arguments_definition [value])
(defn make-hydra_graphql_syntax_arguments_definition [value] (->hydra_graphql_syntax_arguments_definition value))

(defrecord hydra_graphql_syntax_input_value_definition [description name type default_value directives])
(defn make-hydra_graphql_syntax_input_value_definition [description name type default_value directives] (->hydra_graphql_syntax_input_value_definition description name type default_value directives))

(def hydra_graphql_syntax_interface_type_definition-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_interface_type_definition_sequence [description name implements_interfaces directives fields_definition])
(defn make-hydra_graphql_syntax_interface_type_definition_sequence [description name implements_interfaces directives fields_definition] (->hydra_graphql_syntax_interface_type_definition_sequence description name implements_interfaces directives fields_definition))

(defrecord hydra_graphql_syntax_interface_type_definition_sequence2 [description name implements_interfaces directives])
(defn make-hydra_graphql_syntax_interface_type_definition_sequence2 [description name implements_interfaces directives] (->hydra_graphql_syntax_interface_type_definition_sequence2 description name implements_interfaces directives))

(def hydra_graphql_syntax_interface_type_extension-variants (list :sequence :sequence2 :sequence3))

(defrecord hydra_graphql_syntax_interface_type_extension_sequence [name implements_interfaces directives fields_definition])
(defn make-hydra_graphql_syntax_interface_type_extension_sequence [name implements_interfaces directives fields_definition] (->hydra_graphql_syntax_interface_type_extension_sequence name implements_interfaces directives fields_definition))

(defrecord hydra_graphql_syntax_interface_type_extension_sequence2 [name implements_interfaces directives])
(defn make-hydra_graphql_syntax_interface_type_extension_sequence2 [name implements_interfaces directives] (->hydra_graphql_syntax_interface_type_extension_sequence2 name implements_interfaces directives))

(defrecord hydra_graphql_syntax_interface_type_extension_sequence3 [name implements_interfaces])
(defn make-hydra_graphql_syntax_interface_type_extension_sequence3 [name implements_interfaces] (->hydra_graphql_syntax_interface_type_extension_sequence3 name implements_interfaces))

(defrecord hydra_graphql_syntax_union_type_definition [description name directives union_member_types])
(defn make-hydra_graphql_syntax_union_type_definition [description name directives union_member_types] (->hydra_graphql_syntax_union_type_definition description name directives union_member_types))

(def hydra_graphql_syntax_union_member_types-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_union_member_types_sequence [union_member_types named_type])
(defn make-hydra_graphql_syntax_union_member_types_sequence [union_member_types named_type] (->hydra_graphql_syntax_union_member_types_sequence union_member_types named_type))

(defrecord hydra_graphql_syntax_union_member_types_sequence2 [or named_type])
(defn make-hydra_graphql_syntax_union_member_types_sequence2 [or named_type] (->hydra_graphql_syntax_union_member_types_sequence2 or named_type))

(def hydra_graphql_syntax_union_type_extension-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_union_type_extension_sequence [name directives union_member_types])
(defn make-hydra_graphql_syntax_union_type_extension_sequence [name directives union_member_types] (->hydra_graphql_syntax_union_type_extension_sequence name directives union_member_types))

(defrecord hydra_graphql_syntax_union_type_extension_sequence2 [name directives])
(defn make-hydra_graphql_syntax_union_type_extension_sequence2 [name directives] (->hydra_graphql_syntax_union_type_extension_sequence2 name directives))

(defrecord hydra_graphql_syntax_enum_type_definition [description name directives enum_values_definition])
(defn make-hydra_graphql_syntax_enum_type_definition [description name directives enum_values_definition] (->hydra_graphql_syntax_enum_type_definition description name directives enum_values_definition))

(defrecord hydra_graphql_syntax_enum_values_definition [value])
(defn make-hydra_graphql_syntax_enum_values_definition [value] (->hydra_graphql_syntax_enum_values_definition value))

(defrecord hydra_graphql_syntax_enum_value_definition [description enum_value directives])
(defn make-hydra_graphql_syntax_enum_value_definition [description enum_value directives] (->hydra_graphql_syntax_enum_value_definition description enum_value directives))

(def hydra_graphql_syntax_enum_type_extension-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_enum_type_extension_sequence [name directives enum_values_definition])
(defn make-hydra_graphql_syntax_enum_type_extension_sequence [name directives enum_values_definition] (->hydra_graphql_syntax_enum_type_extension_sequence name directives enum_values_definition))

(defrecord hydra_graphql_syntax_enum_type_extension_sequence2 [name directives])
(defn make-hydra_graphql_syntax_enum_type_extension_sequence2 [name directives] (->hydra_graphql_syntax_enum_type_extension_sequence2 name directives))

(def hydra_graphql_syntax_input_object_type_definition-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_input_object_type_definition_sequence [description name directives input_fields_definition])
(defn make-hydra_graphql_syntax_input_object_type_definition_sequence [description name directives input_fields_definition] (->hydra_graphql_syntax_input_object_type_definition_sequence description name directives input_fields_definition))

(defrecord hydra_graphql_syntax_input_object_type_definition_sequence2 [description name directives])
(defn make-hydra_graphql_syntax_input_object_type_definition_sequence2 [description name directives] (->hydra_graphql_syntax_input_object_type_definition_sequence2 description name directives))

(defrecord hydra_graphql_syntax_input_fields_definition [value])
(defn make-hydra_graphql_syntax_input_fields_definition [value] (->hydra_graphql_syntax_input_fields_definition value))

(def hydra_graphql_syntax_input_object_type_extension-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_input_object_type_extension_sequence [name directives input_fields_definition])
(defn make-hydra_graphql_syntax_input_object_type_extension_sequence [name directives input_fields_definition] (->hydra_graphql_syntax_input_object_type_extension_sequence name directives input_fields_definition))

(defrecord hydra_graphql_syntax_input_object_type_extension_sequence2 [name directives])
(defn make-hydra_graphql_syntax_input_object_type_extension_sequence2 [name directives] (->hydra_graphql_syntax_input_object_type_extension_sequence2 name directives))

(defrecord hydra_graphql_syntax_directive_definition [description name arguments_definition repeatable directive_locations])
(defn make-hydra_graphql_syntax_directive_definition [description name arguments_definition repeatable directive_locations] (->hydra_graphql_syntax_directive_definition description name arguments_definition repeatable directive_locations))

(def hydra_graphql_syntax_directive_locations-variants (list :sequence :sequence2))

(defrecord hydra_graphql_syntax_directive_locations_sequence [directive_locations directive_location])
(defn make-hydra_graphql_syntax_directive_locations_sequence [directive_locations directive_location] (->hydra_graphql_syntax_directive_locations_sequence directive_locations directive_location))

(defrecord hydra_graphql_syntax_directive_locations_sequence2 [or directive_location])
(defn make-hydra_graphql_syntax_directive_locations_sequence2 [or directive_location] (->hydra_graphql_syntax_directive_locations_sequence2 or directive_location))

(def hydra_graphql_syntax_directive_location-variants (list :executable :type_system))

(def hydra_graphql_syntax_executable_directive_location-variants (list :q_u_e_r_y :m_u_t_a_t_i_o_n :s_u_b_s_c_r_i_p_t_i_o_n :f_i_e_l_d :f_r_a_g_m_e_n_t__d_e_f_i_n_i_t_i_o_n :f_r_a_g_m_e_n_t__s_p_r_e_a_d :i_n_l_i_n_e__f_r_a_g_m_e_n_t :v_a_r_i_a_b_l_e__d_e_f_i_n_i_t_i_o_n))

(def hydra_graphql_syntax_type_system_directive_location-variants (list :s_c_h_e_m_a :s_c_a_l_a_r :o_b_j_e_c_t :f_i_e_l_d__d_e_f_i_n_i_t_i_o_n :a_r_g_u_m_e_n_t__d_e_f_i_n_i_t_i_o_n :i_n_t_e_r_f_a_c_e :u_n_i_o_n :e_n_u_m :e_n_u_m__v_a_l_u_e :i_n_p_u_t__o_b_j_e_c_t :i_n_p_u_t__f_i_e_l_d__d_e_f_i_n_i_t_i_o_n))

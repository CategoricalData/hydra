(ns hydra.sql.syntax)

(declare hydra_sql_syntax_approximate_numeric_type-variants hydra_sql_syntax_array_value_constructor-variants hydra_sql_syntax_binary_large_object_string_type-variants hydra_sql_syntax_boolean_literal-variants hydra_sql_syntax_boolean_primary-variants hydra_sql_syntax_boolean_term-variants hydra_sql_syntax_boolean_value_expression-variants hydra_sql_syntax_character_string_type-variants hydra_sql_syntax_collection_type-variants hydra_sql_syntax_collection_value_constructor-variants hydra_sql_syntax_collection_value_expression-variants hydra_sql_syntax_column_definition_type_or_domain_option-variants hydra_sql_syntax_column_definition_default_or_identity_or_generation_option-variants hydra_sql_syntax_common_value_expression-variants hydra_sql_syntax_contextually_typed_row_value_expression-variants hydra_sql_syntax_data_type-variants hydra_sql_syntax_datetime_literal-variants hydra_sql_syntax_exact_numeric_type-variants hydra_sql_syntax_general_literal-variants hydra_sql_syntax_global_or_local-variants hydra_sql_syntax_insert_columns_and_source-variants hydra_sql_syntax_numeric_type-variants hydra_sql_syntax_override_clause-variants hydra_sql_syntax_predefined_type-variants hydra_sql_syntax_nonparenthesized_value_expression_primary-variants hydra_sql_syntax_table_commit_action-variants hydra_sql_syntax_table_contents_source-variants hydra_sql_syntax_table_element-variants hydra_sql_syntax_truth_value-variants hydra_sql_syntax_unsigned_literal-variants hydra_sql_syntax_unsigned_numeric_literal-variants hydra_sql_syntax_unsigned_value_specification-variants hydra_sql_syntax_value_expression-variants hydra_sql_syntax_value_expression_primary-variants)

(defrecord hydra_sql_syntax_approximate_numeric_literal [value])
(defn make-hydra_sql_syntax_approximate_numeric_literal [value] (->hydra_sql_syntax_approximate_numeric_literal value))

(defrecord hydra_sql_syntax_binary_string_literal [value])
(defn make-hydra_sql_syntax_binary_string_literal [value] (->hydra_sql_syntax_binary_string_literal value))

(defrecord hydra_sql_syntax_character_string_literal [value])
(defn make-hydra_sql_syntax_character_string_literal [value] (->hydra_sql_syntax_character_string_literal value))

(defrecord hydra_sql_syntax_column_name [value])
(defn make-hydra_sql_syntax_column_name [value] (->hydra_sql_syntax_column_name value))

(defrecord hydra_sql_syntax_date_string [value])
(defn make-hydra_sql_syntax_date_string [value] (->hydra_sql_syntax_date_string value))

(defrecord hydra_sql_syntax_domain_name [value])
(defn make-hydra_sql_syntax_domain_name [value] (->hydra_sql_syntax_domain_name value))

(defrecord hydra_sql_syntax_exact_numeric_literal [value])
(defn make-hydra_sql_syntax_exact_numeric_literal [value] (->hydra_sql_syntax_exact_numeric_literal value))

(defrecord hydra_sql_syntax_left_bracket_or_trigraph [value])
(defn make-hydra_sql_syntax_left_bracket_or_trigraph [value] (->hydra_sql_syntax_left_bracket_or_trigraph value))

(defrecord hydra_sql_syntax_right_bracket_or_trigraph [value])
(defn make-hydra_sql_syntax_right_bracket_or_trigraph [value] (->hydra_sql_syntax_right_bracket_or_trigraph value))

(defrecord hydra_sql_syntax_national_character_string_literal [value])
(defn make-hydra_sql_syntax_national_character_string_literal [value] (->hydra_sql_syntax_national_character_string_literal value))

(defrecord hydra_sql_syntax_path_resolved_user_defined_type_name [value])
(defn make-hydra_sql_syntax_path_resolved_user_defined_type_name [value] (->hydra_sql_syntax_path_resolved_user_defined_type_name value))

(defrecord hydra_sql_syntax_table_name [value])
(defn make-hydra_sql_syntax_table_name [value] (->hydra_sql_syntax_table_name value))

(defrecord hydra_sql_syntax_time_string [value])
(defn make-hydra_sql_syntax_time_string [value] (->hydra_sql_syntax_time_string value))

(defrecord hydra_sql_syntax_timestamp_literal [value])
(defn make-hydra_sql_syntax_timestamp_literal [value] (->hydra_sql_syntax_timestamp_literal value))

(defrecord hydra_sql_syntax_unicode_character_string_literal [value])
(defn make-hydra_sql_syntax_unicode_character_string_literal [value] (->hydra_sql_syntax_unicode_character_string_literal value))

(defrecord hydra_sql_syntax_unsigned_integer [value])
(defn make-hydra_sql_syntax_unsigned_integer [value] (->hydra_sql_syntax_unsigned_integer value))

(def hydra_sql_syntax_approximate_numeric_type-variants (list :float :real :double))

(defrecord hydra_sql_syntax_array_element [value])
(defn make-hydra_sql_syntax_array_element [value] (->hydra_sql_syntax_array_element value))

(defrecord hydra_sql_syntax_array_element_list [first rest])
(defn make-hydra_sql_syntax_array_element_list [first rest] (->hydra_sql_syntax_array_element_list first rest))

(defrecord hydra_sql_syntax_array_element_reference [value])
(defn make-hydra_sql_syntax_array_element_reference [value] (->hydra_sql_syntax_array_element_reference value))

(defrecord hydra_sql_syntax_array_type [value])
(defn make-hydra_sql_syntax_array_type [value] (->hydra_sql_syntax_array_type value))

(def hydra_sql_syntax_array_value_constructor-variants (list :enumeration :query))

(defrecord hydra_sql_syntax_array_value_constructor_by_query [value])
(defn make-hydra_sql_syntax_array_value_constructor_by_query [value] (->hydra_sql_syntax_array_value_constructor_by_query value))

(defrecord hydra_sql_syntax_array_value_constructor_by_enumeration [left_bracket_or_trigraph array_element_list right_bracket_or_trigraph])
(defn make-hydra_sql_syntax_array_value_constructor_by_enumeration [left_bracket_or_trigraph array_element_list right_bracket_or_trigraph] (->hydra_sql_syntax_array_value_constructor_by_enumeration left_bracket_or_trigraph array_element_list right_bracket_or_trigraph))

(defrecord hydra_sql_syntax_array_value_expression [value])
(defn make-hydra_sql_syntax_array_value_expression [value] (->hydra_sql_syntax_array_value_expression value))

(defrecord hydra_sql_syntax_as_subquery_clause [value])
(defn make-hydra_sql_syntax_as_subquery_clause [value] (->hydra_sql_syntax_as_subquery_clause value))

(defrecord hydra_sql_syntax_attribute_or_method_reference [value])
(defn make-hydra_sql_syntax_attribute_or_method_reference [value] (->hydra_sql_syntax_attribute_or_method_reference value))

(def hydra_sql_syntax_binary_large_object_string_type-variants (list :binary :blob))

(defrecord hydra_sql_syntax_boolean_factor [n_o_t boolean_test])
(defn make-hydra_sql_syntax_boolean_factor [n_o_t boolean_test] (->hydra_sql_syntax_boolean_factor n_o_t boolean_test))

(def hydra_sql_syntax_boolean_literal-variants (list :t_r_u_e :f_a_l_s_e :u_n_k_n_o_w_n))

(defrecord hydra_sql_syntax_boolean_predicand [value])
(defn make-hydra_sql_syntax_boolean_predicand [value] (->hydra_sql_syntax_boolean_predicand value))

(def hydra_sql_syntax_boolean_primary-variants (list :predicate :predicand))

(def hydra_sql_syntax_boolean_term-variants (list :factor :and))

(defrecord hydra_sql_syntax_boolean_term_and [lhs rhs])
(defn make-hydra_sql_syntax_boolean_term_and [lhs rhs] (->hydra_sql_syntax_boolean_term_and lhs rhs))

(defrecord hydra_sql_syntax_boolean_test [boolean_primary sequence])
(defn make-hydra_sql_syntax_boolean_test [boolean_primary sequence] (->hydra_sql_syntax_boolean_test boolean_primary sequence))

(defrecord hydra_sql_syntax_boolean_test_sequence_option [n_o_t truth_value])
(defn make-hydra_sql_syntax_boolean_test_sequence_option [n_o_t truth_value] (->hydra_sql_syntax_boolean_test_sequence_option n_o_t truth_value))

(defrecord hydra_sql_syntax_boolean_type [value])
(defn make-hydra_sql_syntax_boolean_type [value] (->hydra_sql_syntax_boolean_type value))

(def hydra_sql_syntax_boolean_value_expression-variants (list :term :or))

(defrecord hydra_sql_syntax_boolean_value_expression_or [lhs rhs])
(defn make-hydra_sql_syntax_boolean_value_expression_or [lhs rhs] (->hydra_sql_syntax_boolean_value_expression_or lhs rhs))

(defrecord hydra_sql_syntax_case_expression [value])
(defn make-hydra_sql_syntax_case_expression [value] (->hydra_sql_syntax_case_expression value))

(defrecord hydra_sql_syntax_cast_specification [value])
(defn make-hydra_sql_syntax_cast_specification [value] (->hydra_sql_syntax_cast_specification value))

(defrecord hydra_sql_syntax_character_set_specification [value])
(defn make-hydra_sql_syntax_character_set_specification [value] (->hydra_sql_syntax_character_set_specification value))

(def hydra_sql_syntax_character_string_type-variants (list :character :char :character_varying :char_varying :varchar :character_large_object :char_large_object :clob))

(defrecord hydra_sql_syntax_collate_clause [value])
(defn make-hydra_sql_syntax_collate_clause [value] (->hydra_sql_syntax_collate_clause value))

(def hydra_sql_syntax_collection_type-variants (list :array :multiset))

(def hydra_sql_syntax_collection_value_constructor-variants (list :array :multiset))

(def hydra_sql_syntax_collection_value_expression-variants (list :array :multiset))

(defrecord hydra_sql_syntax_column_constraint_definition [value])
(defn make-hydra_sql_syntax_column_constraint_definition [value] (->hydra_sql_syntax_column_constraint_definition value))

(defrecord hydra_sql_syntax_column_definition [name type_or_domain ref_scope default_or_identity_or_generation constraints collate])
(defn make-hydra_sql_syntax_column_definition [name type_or_domain ref_scope default_or_identity_or_generation constraints collate] (->hydra_sql_syntax_column_definition name type_or_domain ref_scope default_or_identity_or_generation constraints collate))

(def hydra_sql_syntax_column_definition_type_or_domain_option-variants (list :data_type :domain_name))

(def hydra_sql_syntax_column_definition_default_or_identity_or_generation_option-variants (list :default_clause :identity_column_specification :generation_clause))

(defrecord hydra_sql_syntax_column_name_list [first rest])
(defn make-hydra_sql_syntax_column_name_list [first rest] (->hydra_sql_syntax_column_name_list first rest))

(defrecord hydra_sql_syntax_column_options [value])
(defn make-hydra_sql_syntax_column_options [value] (->hydra_sql_syntax_column_options value))

(defrecord hydra_sql_syntax_column_reference [value])
(defn make-hydra_sql_syntax_column_reference [value] (->hydra_sql_syntax_column_reference value))

(def hydra_sql_syntax_common_value_expression-variants (list :numeric :string :datetime :interval :user_defined :reference :collection))

(def hydra_sql_syntax_contextually_typed_row_value_expression-variants (list :special_case :constructor))

(defrecord hydra_sql_syntax_contextually_typed_row_value_constructor [value])
(defn make-hydra_sql_syntax_contextually_typed_row_value_constructor [value] (->hydra_sql_syntax_contextually_typed_row_value_constructor value))

(defrecord hydra_sql_syntax_contextually_typed_row_value_expression_list [first rest])
(defn make-hydra_sql_syntax_contextually_typed_row_value_expression_list [first rest] (->hydra_sql_syntax_contextually_typed_row_value_expression_list first rest))

(defrecord hydra_sql_syntax_contextually_typed_table_value_constructor [value])
(defn make-hydra_sql_syntax_contextually_typed_table_value_constructor [value] (->hydra_sql_syntax_contextually_typed_table_value_constructor value))

(def hydra_sql_syntax_data_type-variants (list :predefined :row :named :reference :collection))

(defrecord hydra_sql_syntax_date_literal [value])
(defn make-hydra_sql_syntax_date_literal [value] (->hydra_sql_syntax_date_literal value))

(def hydra_sql_syntax_datetime_literal-variants (list :date :time :timestamp))

(defrecord hydra_sql_syntax_datetime_type [value])
(defn make-hydra_sql_syntax_datetime_type [value] (->hydra_sql_syntax_datetime_type value))

(defrecord hydra_sql_syntax_datetime_value_expression [value])
(defn make-hydra_sql_syntax_datetime_value_expression [value] (->hydra_sql_syntax_datetime_value_expression value))

(defrecord hydra_sql_syntax_default_clause [value])
(defn make-hydra_sql_syntax_default_clause [value] (->hydra_sql_syntax_default_clause value))

(def hydra_sql_syntax_exact_numeric_type-variants (list :numeric :decimal :dec :smallint :integer :int :bigint))

(defrecord hydra_sql_syntax_exact_numeric_type_numeric_option [precision sequence])
(defn make-hydra_sql_syntax_exact_numeric_type_numeric_option [precision sequence] (->hydra_sql_syntax_exact_numeric_type_numeric_option precision sequence))

(defrecord hydra_sql_syntax_exact_numeric_type_decimal_option [precision sequence])
(defn make-hydra_sql_syntax_exact_numeric_type_decimal_option [precision sequence] (->hydra_sql_syntax_exact_numeric_type_decimal_option precision sequence))

(defrecord hydra_sql_syntax_exact_numeric_type_dec_option [precision sequence])
(defn make-hydra_sql_syntax_exact_numeric_type_dec_option [precision sequence] (->hydra_sql_syntax_exact_numeric_type_dec_option precision sequence))

(defrecord hydra_sql_syntax_field_reference [value])
(defn make-hydra_sql_syntax_field_reference [value] (->hydra_sql_syntax_field_reference value))

(defrecord hydra_sql_syntax_from_constructor [columns override values])
(defn make-hydra_sql_syntax_from_constructor [columns override values] (->hydra_sql_syntax_from_constructor columns override values))

(defrecord hydra_sql_syntax_from_default [value])
(defn make-hydra_sql_syntax_from_default [value] (->hydra_sql_syntax_from_default value))

(defrecord hydra_sql_syntax_from_subquery [value])
(defn make-hydra_sql_syntax_from_subquery [value] (->hydra_sql_syntax_from_subquery value))

(def hydra_sql_syntax_general_literal-variants (list :string :national_string :unicode :binary :date_time :interval :boolean))

(defrecord hydra_sql_syntax_general_value_specification [value])
(defn make-hydra_sql_syntax_general_value_specification [value] (->hydra_sql_syntax_general_value_specification value))

(defrecord hydra_sql_syntax_generation_clause [value])
(defn make-hydra_sql_syntax_generation_clause [value] (->hydra_sql_syntax_generation_clause value))

(def hydra_sql_syntax_global_or_local-variants (list :global :local))

(defrecord hydra_sql_syntax_identity_column_specification [value])
(defn make-hydra_sql_syntax_identity_column_specification [value] (->hydra_sql_syntax_identity_column_specification value))

(defrecord hydra_sql_syntax_insert_column_list [value])
(defn make-hydra_sql_syntax_insert_column_list [value] (->hydra_sql_syntax_insert_column_list value))

(def hydra_sql_syntax_insert_columns_and_source-variants (list :subquery :constructor :default))

(defrecord hydra_sql_syntax_insert_statement [target columns_and_source])
(defn make-hydra_sql_syntax_insert_statement [target columns_and_source] (->hydra_sql_syntax_insert_statement target columns_and_source))

(defrecord hydra_sql_syntax_insertion_target [value])
(defn make-hydra_sql_syntax_insertion_target [value] (->hydra_sql_syntax_insertion_target value))

(defrecord hydra_sql_syntax_interval_literal [value])
(defn make-hydra_sql_syntax_interval_literal [value] (->hydra_sql_syntax_interval_literal value))

(defrecord hydra_sql_syntax_interval_type [value])
(defn make-hydra_sql_syntax_interval_type [value] (->hydra_sql_syntax_interval_type value))

(defrecord hydra_sql_syntax_interval_value_expression [value])
(defn make-hydra_sql_syntax_interval_value_expression [value] (->hydra_sql_syntax_interval_value_expression value))

(defrecord hydra_sql_syntax_large_object_length [value])
(defn make-hydra_sql_syntax_large_object_length [value] (->hydra_sql_syntax_large_object_length value))

(defrecord hydra_sql_syntax_length [value])
(defn make-hydra_sql_syntax_length [value] (->hydra_sql_syntax_length value))

(defrecord hydra_sql_syntax_like_clause [value])
(defn make-hydra_sql_syntax_like_clause [value] (->hydra_sql_syntax_like_clause value))

(defrecord hydra_sql_syntax_method_invocation [value])
(defn make-hydra_sql_syntax_method_invocation [value] (->hydra_sql_syntax_method_invocation value))

(defrecord hydra_sql_syntax_multiset_element_reference [value])
(defn make-hydra_sql_syntax_multiset_element_reference [value] (->hydra_sql_syntax_multiset_element_reference value))

(defrecord hydra_sql_syntax_multiset_type [value])
(defn make-hydra_sql_syntax_multiset_type [value] (->hydra_sql_syntax_multiset_type value))

(defrecord hydra_sql_syntax_multiset_value_constructor [value])
(defn make-hydra_sql_syntax_multiset_value_constructor [value] (->hydra_sql_syntax_multiset_value_constructor value))

(defrecord hydra_sql_syntax_multiset_value_expression [value])
(defn make-hydra_sql_syntax_multiset_value_expression [value] (->hydra_sql_syntax_multiset_value_expression value))

(defrecord hydra_sql_syntax_national_character_string_type [value])
(defn make-hydra_sql_syntax_national_character_string_type [value] (->hydra_sql_syntax_national_character_string_type value))

(defrecord hydra_sql_syntax_new_specification [value])
(defn make-hydra_sql_syntax_new_specification [value] (->hydra_sql_syntax_new_specification value))

(defrecord hydra_sql_syntax_next_value_expression [value])
(defn make-hydra_sql_syntax_next_value_expression [value] (->hydra_sql_syntax_next_value_expression value))

(def hydra_sql_syntax_numeric_type-variants (list :exact :approximate))

(defrecord hydra_sql_syntax_numeric_value_expression [value])
(defn make-hydra_sql_syntax_numeric_value_expression [value] (->hydra_sql_syntax_numeric_value_expression value))

(def hydra_sql_syntax_override_clause-variants (list :o_v_e_r_r_i_d_i_n_gsp_u_s_e_rsp_v_a_l_u_e :o_v_e_r_r_i_d_i_n_gsp_s_y_s_t_e_msp_v_a_l_u_e))

(defrecord hydra_sql_syntax_parenthesized_value_expression [value])
(defn make-hydra_sql_syntax_parenthesized_value_expression [value] (->hydra_sql_syntax_parenthesized_value_expression value))

(defrecord hydra_sql_syntax_precision [value])
(defn make-hydra_sql_syntax_precision [value] (->hydra_sql_syntax_precision value))

(def hydra_sql_syntax_predefined_type-variants (list :string :national_string :blob :numeric :boolean :datetime :interval))

(defrecord hydra_sql_syntax_predefined_type_string [type characters collate])
(defn make-hydra_sql_syntax_predefined_type_string [type characters collate] (->hydra_sql_syntax_predefined_type_string type characters collate))

(defrecord hydra_sql_syntax_predefined_type_national_string [type collate])
(defn make-hydra_sql_syntax_predefined_type_national_string [type collate] (->hydra_sql_syntax_predefined_type_national_string type collate))

(defrecord hydra_sql_syntax_predicate [value])
(defn make-hydra_sql_syntax_predicate [value] (->hydra_sql_syntax_predicate value))

(defrecord hydra_sql_syntax_query_expression [value])
(defn make-hydra_sql_syntax_query_expression [value] (->hydra_sql_syntax_query_expression value))

(defrecord hydra_sql_syntax_reference_scope_check [value])
(defn make-hydra_sql_syntax_reference_scope_check [value] (->hydra_sql_syntax_reference_scope_check value))

(defrecord hydra_sql_syntax_reference_type [value])
(defn make-hydra_sql_syntax_reference_type [value] (->hydra_sql_syntax_reference_type value))

(defrecord hydra_sql_syntax_row_type [value])
(defn make-hydra_sql_syntax_row_type [value] (->hydra_sql_syntax_row_type value))

(defrecord hydra_sql_syntax_row_value_special_case [value])
(defn make-hydra_sql_syntax_row_value_special_case [value] (->hydra_sql_syntax_row_value_special_case value))

(def hydra_sql_syntax_nonparenthesized_value_expression_primary-variants (list :unsigned :column :set_function :window_function :scalar_subquery :cases :cast :field :subtype :method :static_method :new :attribute_or_method :reference :collection :array_element :multiset_element :routine :next))

(defrecord hydra_sql_syntax_reference_resolution [value])
(defn make-hydra_sql_syntax_reference_resolution [value] (->hydra_sql_syntax_reference_resolution value))

(defrecord hydra_sql_syntax_reference_value_expression [value])
(defn make-hydra_sql_syntax_reference_value_expression [value] (->hydra_sql_syntax_reference_value_expression value))

(defrecord hydra_sql_syntax_row_value_expression [value])
(defn make-hydra_sql_syntax_row_value_expression [value] (->hydra_sql_syntax_row_value_expression value))

(defrecord hydra_sql_syntax_routine_invocation [value])
(defn make-hydra_sql_syntax_routine_invocation [value] (->hydra_sql_syntax_routine_invocation value))

(defrecord hydra_sql_syntax_scalar_subquery [value])
(defn make-hydra_sql_syntax_scalar_subquery [value] (->hydra_sql_syntax_scalar_subquery value))

(defrecord hydra_sql_syntax_scale [value])
(defn make-hydra_sql_syntax_scale [value] (->hydra_sql_syntax_scale value))

(defrecord hydra_sql_syntax_self_referencing_column_specification [value])
(defn make-hydra_sql_syntax_self_referencing_column_specification [value] (->hydra_sql_syntax_self_referencing_column_specification value))

(defrecord hydra_sql_syntax_set_function_specification [value])
(defn make-hydra_sql_syntax_set_function_specification [value] (->hydra_sql_syntax_set_function_specification value))

(defrecord hydra_sql_syntax_static_method_invocation [value])
(defn make-hydra_sql_syntax_static_method_invocation [value] (->hydra_sql_syntax_static_method_invocation value))

(defrecord hydra_sql_syntax_string_value_expression [value])
(defn make-hydra_sql_syntax_string_value_expression [value] (->hydra_sql_syntax_string_value_expression value))

(defrecord hydra_sql_syntax_subquery [value])
(defn make-hydra_sql_syntax_subquery [value] (->hydra_sql_syntax_subquery value))

(defrecord hydra_sql_syntax_subtable_clause [value])
(defn make-hydra_sql_syntax_subtable_clause [value] (->hydra_sql_syntax_subtable_clause value))

(defrecord hydra_sql_syntax_subtype_treatment [value])
(defn make-hydra_sql_syntax_subtype_treatment [value] (->hydra_sql_syntax_subtype_treatment value))

(def hydra_sql_syntax_table_commit_action-variants (list :preserve :delete))

(defrecord hydra_sql_syntax_table_constraint_definition [value])
(defn make-hydra_sql_syntax_table_constraint_definition [value] (->hydra_sql_syntax_table_constraint_definition value))

(def hydra_sql_syntax_table_contents_source-variants (list :list :subtable :subquery))

(defrecord hydra_sql_syntax_table_contents_source_subtable [type subtable elements])
(defn make-hydra_sql_syntax_table_contents_source_subtable [type subtable elements] (->hydra_sql_syntax_table_contents_source_subtable type subtable elements))

(defrecord hydra_sql_syntax_table_definition [scope name source commit_actions])
(defn make-hydra_sql_syntax_table_definition [scope name source commit_actions] (->hydra_sql_syntax_table_definition scope name source commit_actions))

(def hydra_sql_syntax_table_element-variants (list :column :table_constraint :like :self_referencing_column :colum_options))

(defrecord hydra_sql_syntax_table_element_list [first rest])
(defn make-hydra_sql_syntax_table_element_list [first rest] (->hydra_sql_syntax_table_element_list first rest))

(defrecord hydra_sql_syntax_table_scope [value])
(defn make-hydra_sql_syntax_table_scope [value] (->hydra_sql_syntax_table_scope value))

(defrecord hydra_sql_syntax_time_literal [value])
(defn make-hydra_sql_syntax_time_literal [value] (->hydra_sql_syntax_time_literal value))

(def hydra_sql_syntax_truth_value-variants (list :t_r_u_e :f_a_l_s_e :u_n_k_n_o_w_n))

(def hydra_sql_syntax_unsigned_literal-variants (list :numeric :general))

(def hydra_sql_syntax_unsigned_numeric_literal-variants (list :exact :approximate))

(def hydra_sql_syntax_unsigned_value_specification-variants (list :literal :general))

(defrecord hydra_sql_syntax_user_defined_type_value_expression [value])
(defn make-hydra_sql_syntax_user_defined_type_value_expression [value] (->hydra_sql_syntax_user_defined_type_value_expression value))

(def hydra_sql_syntax_value_expression-variants (list :common :boolean :row))

(def hydra_sql_syntax_value_expression_primary-variants (list :parens :noparens))

(defrecord hydra_sql_syntax_window_function [value])
(defn make-hydra_sql_syntax_window_function [value] (->hydra_sql_syntax_window_function value))

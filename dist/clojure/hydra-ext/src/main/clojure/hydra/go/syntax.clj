(ns hydra.go.syntax)

(declare hydra_go_syntax_string_lit-variants hydra_go_syntax_import_alias-variants hydra_go_syntax_declaration-variants hydra_go_syntax_top_level_decl-variants hydra_go_syntax_type_spec-variants hydra_go_syntax_type-variants hydra_go_syntax_type_lit-variants hydra_go_syntax_field_decl-variants hydra_go_syntax_result-variants hydra_go_syntax_interface_elem-variants hydra_go_syntax_channel_direction-variants hydra_go_syntax_expression-variants hydra_go_syntax_unary_expr-variants hydra_go_syntax_binary_op-variants hydra_go_syntax_primary_expr-variants hydra_go_syntax_operand-variants hydra_go_syntax_literal-variants hydra_go_syntax_basic_lit-variants hydra_go_syntax_literal_type-variants hydra_go_syntax_key-variants hydra_go_syntax_element-variants hydra_go_syntax_slice-variants hydra_go_syntax_statement-variants hydra_go_syntax_simple_stmt-variants hydra_go_syntax_assign_op-variants hydra_go_syntax_else_clause-variants hydra_go_syntax_switch_stmt-variants hydra_go_syntax_for_clause_or_range-variants hydra_go_syntax_range_vars-variants hydra_go_syntax_comm_case-variants hydra_go_syntax_unary_op-variants hydra_go_syntax_mul_op-variants hydra_go_syntax_add_op-variants hydra_go_syntax_rel_op-variants)

(defrecord hydra_go_syntax_annotated_declaration [comment declaration])
(defn make-hydra_go_syntax_annotated_declaration [comment declaration] (->hydra_go_syntax_annotated_declaration comment declaration))

(defrecord hydra_go_syntax_module [package imports declarations])
(defn make-hydra_go_syntax_module [package imports declarations] (->hydra_go_syntax_module package imports declarations))

(defrecord hydra_go_syntax_identifier [value])
(defn make-hydra_go_syntax_identifier [value] (->hydra_go_syntax_identifier value))

(defrecord hydra_go_syntax_int_lit [value])
(defn make-hydra_go_syntax_int_lit [value] (->hydra_go_syntax_int_lit value))

(defrecord hydra_go_syntax_float_lit [value])
(defn make-hydra_go_syntax_float_lit [value] (->hydra_go_syntax_float_lit value))

(defrecord hydra_go_syntax_imaginary_lit [value])
(defn make-hydra_go_syntax_imaginary_lit [value] (->hydra_go_syntax_imaginary_lit value))

(defrecord hydra_go_syntax_rune_lit [value])
(defn make-hydra_go_syntax_rune_lit [value] (->hydra_go_syntax_rune_lit value))

(def hydra_go_syntax_string_lit-variants (list :raw :interpreted))

(defrecord hydra_go_syntax_raw_string_lit [value])
(defn make-hydra_go_syntax_raw_string_lit [value] (->hydra_go_syntax_raw_string_lit value))

(defrecord hydra_go_syntax_interpreted_string_lit [value])
(defn make-hydra_go_syntax_interpreted_string_lit [value] (->hydra_go_syntax_interpreted_string_lit value))

(defrecord hydra_go_syntax_source_file [package imports declarations])
(defn make-hydra_go_syntax_source_file [package imports declarations] (->hydra_go_syntax_source_file package imports declarations))

(defrecord hydra_go_syntax_package_clause [value])
(defn make-hydra_go_syntax_package_clause [value] (->hydra_go_syntax_package_clause value))

(defrecord hydra_go_syntax_import_decl [value])
(defn make-hydra_go_syntax_import_decl [value] (->hydra_go_syntax_import_decl value))

(defrecord hydra_go_syntax_import_spec [alias path])
(defn make-hydra_go_syntax_import_spec [alias path] (->hydra_go_syntax_import_spec alias path))

(def hydra_go_syntax_import_alias-variants (list :dot :name))

(defrecord hydra_go_syntax_import_path [value])
(defn make-hydra_go_syntax_import_path [value] (->hydra_go_syntax_import_path value))

(def hydra_go_syntax_declaration-variants (list :const :type :var))

(def hydra_go_syntax_top_level_decl-variants (list :declaration :function :method))

(defrecord hydra_go_syntax_const_decl [value])
(defn make-hydra_go_syntax_const_decl [value] (->hydra_go_syntax_const_decl value))

(defrecord hydra_go_syntax_const_spec [names type values])
(defn make-hydra_go_syntax_const_spec [names type values] (->hydra_go_syntax_const_spec names type values))

(defrecord hydra_go_syntax_var_decl [value])
(defn make-hydra_go_syntax_var_decl [value] (->hydra_go_syntax_var_decl value))

(defrecord hydra_go_syntax_var_spec [names type values])
(defn make-hydra_go_syntax_var_spec [names type values] (->hydra_go_syntax_var_spec names type values))

(defrecord hydra_go_syntax_short_var_decl [names values])
(defn make-hydra_go_syntax_short_var_decl [names values] (->hydra_go_syntax_short_var_decl names values))

(defrecord hydra_go_syntax_type_decl [value])
(defn make-hydra_go_syntax_type_decl [value] (->hydra_go_syntax_type_decl value))

(def hydra_go_syntax_type_spec-variants (list :alias :definition))

(defrecord hydra_go_syntax_alias_decl [name type])
(defn make-hydra_go_syntax_alias_decl [name type] (->hydra_go_syntax_alias_decl name type))

(defrecord hydra_go_syntax_type_def [name type_params type])
(defn make-hydra_go_syntax_type_def [name type_params type] (->hydra_go_syntax_type_def name type_params type))

(defrecord hydra_go_syntax_type_parameters [value])
(defn make-hydra_go_syntax_type_parameters [value] (->hydra_go_syntax_type_parameters value))

(defrecord hydra_go_syntax_type_param_decl [names constraint])
(defn make-hydra_go_syntax_type_param_decl [names constraint] (->hydra_go_syntax_type_param_decl names constraint))

(defrecord hydra_go_syntax_type_constraint [value])
(defn make-hydra_go_syntax_type_constraint [value] (->hydra_go_syntax_type_constraint value))

(defrecord hydra_go_syntax_function_decl [name type_params signature body])
(defn make-hydra_go_syntax_function_decl [name type_params signature body] (->hydra_go_syntax_function_decl name type_params signature body))

(defrecord hydra_go_syntax_function_body [value])
(defn make-hydra_go_syntax_function_body [value] (->hydra_go_syntax_function_body value))

(defrecord hydra_go_syntax_method_decl [receiver name signature body])
(defn make-hydra_go_syntax_method_decl [receiver name signature body] (->hydra_go_syntax_method_decl receiver name signature body))

(defrecord hydra_go_syntax_receiver [name type])
(defn make-hydra_go_syntax_receiver [name type] (->hydra_go_syntax_receiver name type))

(def hydra_go_syntax_type-variants (list :name :literal :paren))

(defrecord hydra_go_syntax_type_name [name type_args])
(defn make-hydra_go_syntax_type_name [name type_args] (->hydra_go_syntax_type_name name type_args))

(defrecord hydra_go_syntax_qualified_ident [package name])
(defn make-hydra_go_syntax_qualified_ident [package name] (->hydra_go_syntax_qualified_ident package name))

(def hydra_go_syntax_type_lit-variants (list :array :struct :pointer :function :interface :slice :map :channel))

(defrecord hydra_go_syntax_array_type [length element])
(defn make-hydra_go_syntax_array_type [length element] (->hydra_go_syntax_array_type length element))

(defrecord hydra_go_syntax_slice_type [value])
(defn make-hydra_go_syntax_slice_type [value] (->hydra_go_syntax_slice_type value))

(defrecord hydra_go_syntax_struct_type [value])
(defn make-hydra_go_syntax_struct_type [value] (->hydra_go_syntax_struct_type value))

(def hydra_go_syntax_field_decl-variants (list :named :embedded))

(defrecord hydra_go_syntax_named_field [names type tag])
(defn make-hydra_go_syntax_named_field [names type tag] (->hydra_go_syntax_named_field names type tag))

(defrecord hydra_go_syntax_embedded_field [pointer type tag])
(defn make-hydra_go_syntax_embedded_field [pointer type tag] (->hydra_go_syntax_embedded_field pointer type tag))

(defrecord hydra_go_syntax_tag [value])
(defn make-hydra_go_syntax_tag [value] (->hydra_go_syntax_tag value))

(defrecord hydra_go_syntax_pointer_type [value])
(defn make-hydra_go_syntax_pointer_type [value] (->hydra_go_syntax_pointer_type value))

(defrecord hydra_go_syntax_function_type [value])
(defn make-hydra_go_syntax_function_type [value] (->hydra_go_syntax_function_type value))

(defrecord hydra_go_syntax_signature [parameters result])
(defn make-hydra_go_syntax_signature [parameters result] (->hydra_go_syntax_signature parameters result))

(def hydra_go_syntax_result-variants (list :parameters :type))

(defrecord hydra_go_syntax_parameters [value])
(defn make-hydra_go_syntax_parameters [value] (->hydra_go_syntax_parameters value))

(defrecord hydra_go_syntax_parameter_decl [names variadic type])
(defn make-hydra_go_syntax_parameter_decl [names variadic type] (->hydra_go_syntax_parameter_decl names variadic type))

(defrecord hydra_go_syntax_interface_type [value])
(defn make-hydra_go_syntax_interface_type [value] (->hydra_go_syntax_interface_type value))

(def hydra_go_syntax_interface_elem-variants (list :method :type))

(defrecord hydra_go_syntax_method_elem [name signature])
(defn make-hydra_go_syntax_method_elem [name signature] (->hydra_go_syntax_method_elem name signature))

(defrecord hydra_go_syntax_type_elem [value])
(defn make-hydra_go_syntax_type_elem [value] (->hydra_go_syntax_type_elem value))

(defrecord hydra_go_syntax_type_term [underlying type])
(defn make-hydra_go_syntax_type_term [underlying type] (->hydra_go_syntax_type_term underlying type))

(defrecord hydra_go_syntax_map_type [key value])
(defn make-hydra_go_syntax_map_type [key value] (->hydra_go_syntax_map_type key value))

(defrecord hydra_go_syntax_channel_type [direction element])
(defn make-hydra_go_syntax_channel_type [direction element] (->hydra_go_syntax_channel_type direction element))

(def hydra_go_syntax_channel_direction-variants (list :bidirectional :send :receive))

(def hydra_go_syntax_expression-variants (list :unary :binary))

(def hydra_go_syntax_unary_expr-variants (list :primary :op))

(defrecord hydra_go_syntax_unary_operation [op operand])
(defn make-hydra_go_syntax_unary_operation [op operand] (->hydra_go_syntax_unary_operation op operand))

(defrecord hydra_go_syntax_binary_expr [left op right])
(defn make-hydra_go_syntax_binary_expr [left op right] (->hydra_go_syntax_binary_expr left op right))

(def hydra_go_syntax_binary_op-variants (list :or :and :equal :not_equal :less :less_equal :greater :greater_equal :add :subtract :bitwise_or :bitwise_xor :multiply :divide :remainder :left_shift :right_shift :bitwise_and :bit_clear))

(def hydra_go_syntax_primary_expr-variants (list :operand :conversion :method_expr :selector :index :slice :type_assertion :call))

(defrecord hydra_go_syntax_selector_expr [expr selector])
(defn make-hydra_go_syntax_selector_expr [expr selector] (->hydra_go_syntax_selector_expr expr selector))

(defrecord hydra_go_syntax_index_expr [expr index])
(defn make-hydra_go_syntax_index_expr [expr index] (->hydra_go_syntax_index_expr expr index))

(defrecord hydra_go_syntax_slice_expr [expr slice])
(defn make-hydra_go_syntax_slice_expr [expr slice] (->hydra_go_syntax_slice_expr expr slice))

(defrecord hydra_go_syntax_type_assertion_expr [expr type])
(defn make-hydra_go_syntax_type_assertion_expr [expr type] (->hydra_go_syntax_type_assertion_expr expr type))

(defrecord hydra_go_syntax_call_expr [function arguments])
(defn make-hydra_go_syntax_call_expr [function arguments] (->hydra_go_syntax_call_expr function arguments))

(def hydra_go_syntax_operand-variants (list :literal :name :paren))

(defrecord hydra_go_syntax_operand_name [name type_args])
(defn make-hydra_go_syntax_operand_name [name type_args] (->hydra_go_syntax_operand_name name type_args))

(def hydra_go_syntax_literal-variants (list :basic :composite :function))

(def hydra_go_syntax_basic_lit-variants (list :int :float :imaginary :rune :string))

(defrecord hydra_go_syntax_composite_lit [type value])
(defn make-hydra_go_syntax_composite_lit [type value] (->hydra_go_syntax_composite_lit type value))

(def hydra_go_syntax_literal_type-variants (list :struct :array :inferred_array :slice :map :name))

(defrecord hydra_go_syntax_literal_value [value])
(defn make-hydra_go_syntax_literal_value [value] (->hydra_go_syntax_literal_value value))

(defrecord hydra_go_syntax_element_list [value])
(defn make-hydra_go_syntax_element_list [value] (->hydra_go_syntax_element_list value))

(defrecord hydra_go_syntax_keyed_element [key element])
(defn make-hydra_go_syntax_keyed_element [key element] (->hydra_go_syntax_keyed_element key element))

(def hydra_go_syntax_key-variants (list :field :expression :literal))

(def hydra_go_syntax_element-variants (list :expression :literal))

(defrecord hydra_go_syntax_function_lit [signature body])
(defn make-hydra_go_syntax_function_lit [signature body] (->hydra_go_syntax_function_lit signature body))

(defrecord hydra_go_syntax_selector [value])
(defn make-hydra_go_syntax_selector [value] (->hydra_go_syntax_selector value))

(defrecord hydra_go_syntax_index [value])
(defn make-hydra_go_syntax_index [value] (->hydra_go_syntax_index value))

(def hydra_go_syntax_slice-variants (list :simple :full))

(defrecord hydra_go_syntax_simple_slice [low high])
(defn make-hydra_go_syntax_simple_slice [low high] (->hydra_go_syntax_simple_slice low high))

(defrecord hydra_go_syntax_full_slice [low high max])
(defn make-hydra_go_syntax_full_slice [low high max] (->hydra_go_syntax_full_slice low high max))

(defrecord hydra_go_syntax_type_assertion [value])
(defn make-hydra_go_syntax_type_assertion [value] (->hydra_go_syntax_type_assertion value))

(defrecord hydra_go_syntax_arguments [type_arg expressions ellipsis])
(defn make-hydra_go_syntax_arguments [type_arg expressions ellipsis] (->hydra_go_syntax_arguments type_arg expressions ellipsis))

(defrecord hydra_go_syntax_method_expr [receiver method])
(defn make-hydra_go_syntax_method_expr [receiver method] (->hydra_go_syntax_method_expr receiver method))

(defrecord hydra_go_syntax_conversion [type expression])
(defn make-hydra_go_syntax_conversion [type expression] (->hydra_go_syntax_conversion type expression))

(def hydra_go_syntax_statement-variants (list :declaration :labeled :simple :go :return :break :continue :goto :fallthrough :block :if :switch :select :for :defer))

(def hydra_go_syntax_simple_stmt-variants (list :empty :expression :send :inc_dec :assignment :short_var_decl))

(defrecord hydra_go_syntax_empty_stmt [value])
(defn make-hydra_go_syntax_empty_stmt [value] (->hydra_go_syntax_empty_stmt value))

(defrecord hydra_go_syntax_labeled_stmt [label statement])
(defn make-hydra_go_syntax_labeled_stmt [label statement] (->hydra_go_syntax_labeled_stmt label statement))

(defrecord hydra_go_syntax_expression_stmt [value])
(defn make-hydra_go_syntax_expression_stmt [value] (->hydra_go_syntax_expression_stmt value))

(defrecord hydra_go_syntax_send_stmt [channel value])
(defn make-hydra_go_syntax_send_stmt [channel value] (->hydra_go_syntax_send_stmt channel value))

(defrecord hydra_go_syntax_inc_dec_stmt [expression increment])
(defn make-hydra_go_syntax_inc_dec_stmt [expression increment] (->hydra_go_syntax_inc_dec_stmt expression increment))

(defrecord hydra_go_syntax_assignment [lhs op rhs])
(defn make-hydra_go_syntax_assignment [lhs op rhs] (->hydra_go_syntax_assignment lhs op rhs))

(def hydra_go_syntax_assign_op-variants (list :simple :add :mul))

(defrecord hydra_go_syntax_if_stmt [init condition then else])
(defn make-hydra_go_syntax_if_stmt [init condition then else] (->hydra_go_syntax_if_stmt init condition then else))

(def hydra_go_syntax_else_clause-variants (list :if :block))

(def hydra_go_syntax_switch_stmt-variants (list :expression :type))

(defrecord hydra_go_syntax_expr_switch_stmt [init expression cases])
(defn make-hydra_go_syntax_expr_switch_stmt [init expression cases] (->hydra_go_syntax_expr_switch_stmt init expression cases))

(defrecord hydra_go_syntax_expr_case_clause [case statements])
(defn make-hydra_go_syntax_expr_case_clause [case statements] (->hydra_go_syntax_expr_case_clause case statements))

(defrecord hydra_go_syntax_type_switch_stmt [init guard cases])
(defn make-hydra_go_syntax_type_switch_stmt [init guard cases] (->hydra_go_syntax_type_switch_stmt init guard cases))

(defrecord hydra_go_syntax_type_switch_guard [name expression])
(defn make-hydra_go_syntax_type_switch_guard [name expression] (->hydra_go_syntax_type_switch_guard name expression))

(defrecord hydra_go_syntax_type_case_clause [case statements])
(defn make-hydra_go_syntax_type_case_clause [case statements] (->hydra_go_syntax_type_case_clause case statements))

(defrecord hydra_go_syntax_for_stmt [clause body])
(defn make-hydra_go_syntax_for_stmt [clause body] (->hydra_go_syntax_for_stmt clause body))

(def hydra_go_syntax_for_clause_or_range-variants (list :condition :clause :range))

(defrecord hydra_go_syntax_for_clause [init condition post])
(defn make-hydra_go_syntax_for_clause [init condition post] (->hydra_go_syntax_for_clause init condition post))

(defrecord hydra_go_syntax_range_clause [vars expression])
(defn make-hydra_go_syntax_range_clause [vars expression] (->hydra_go_syntax_range_clause vars expression))

(def hydra_go_syntax_range_vars-variants (list :assign :declare))

(defrecord hydra_go_syntax_go_stmt [value])
(defn make-hydra_go_syntax_go_stmt [value] (->hydra_go_syntax_go_stmt value))

(defrecord hydra_go_syntax_select_stmt [value])
(defn make-hydra_go_syntax_select_stmt [value] (->hydra_go_syntax_select_stmt value))

(defrecord hydra_go_syntax_comm_clause [case statements])
(defn make-hydra_go_syntax_comm_clause [case statements] (->hydra_go_syntax_comm_clause case statements))

(def hydra_go_syntax_comm_case-variants (list :send :receive :default))

(defrecord hydra_go_syntax_receive_case [vars expression])
(defn make-hydra_go_syntax_receive_case [vars expression] (->hydra_go_syntax_receive_case vars expression))

(defrecord hydra_go_syntax_return_stmt [value])
(defn make-hydra_go_syntax_return_stmt [value] (->hydra_go_syntax_return_stmt value))

(defrecord hydra_go_syntax_break_stmt [value])
(defn make-hydra_go_syntax_break_stmt [value] (->hydra_go_syntax_break_stmt value))

(defrecord hydra_go_syntax_continue_stmt [value])
(defn make-hydra_go_syntax_continue_stmt [value] (->hydra_go_syntax_continue_stmt value))

(defrecord hydra_go_syntax_goto_stmt [value])
(defn make-hydra_go_syntax_goto_stmt [value] (->hydra_go_syntax_goto_stmt value))

(defrecord hydra_go_syntax_fallthrough_stmt [value])
(defn make-hydra_go_syntax_fallthrough_stmt [value] (->hydra_go_syntax_fallthrough_stmt value))

(defrecord hydra_go_syntax_defer_stmt [value])
(defn make-hydra_go_syntax_defer_stmt [value] (->hydra_go_syntax_defer_stmt value))

(defrecord hydra_go_syntax_block [value])
(defn make-hydra_go_syntax_block [value] (->hydra_go_syntax_block value))

(def hydra_go_syntax_unary_op-variants (list :plus :minus :not :xor :deref :address_of :receive))

(def hydra_go_syntax_mul_op-variants (list :multiply :divide :remainder :left_shift :right_shift :bitwise_and :bit_clear))

(def hydra_go_syntax_add_op-variants (list :add :subtract :bitwise_or :bitwise_xor))

(def hydra_go_syntax_rel_op-variants (list :equal :not_equal :less :less_equal :greater :greater_equal))

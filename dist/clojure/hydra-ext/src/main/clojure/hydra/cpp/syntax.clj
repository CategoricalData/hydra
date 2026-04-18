(ns hydra.cpp.syntax)

(declare hydra_cpp_syntax_access_specifier-variants hydra_cpp_syntax_declaration-variants hydra_cpp_syntax_preprocessor_directive-variants hydra_cpp_syntax_class_key-variants hydra_cpp_syntax_member_specification-variants hydra_cpp_syntax_member_declaration-variants hydra_cpp_syntax_function_specifier_prefix-variants hydra_cpp_syntax_function_specifier_suffix-variants hydra_cpp_syntax_function_body-variants hydra_cpp_syntax_container_declaration-variants hydra_cpp_syntax_expression-variants hydra_cpp_syntax_assignment_expression-variants hydra_cpp_syntax_assignment_operator-variants hydra_cpp_syntax_conditional_expression-variants hydra_cpp_syntax_logical_or_expression-variants hydra_cpp_syntax_logical_and_expression-variants hydra_cpp_syntax_inclusive_or_expression-variants hydra_cpp_syntax_exclusive_or_expression-variants hydra_cpp_syntax_and_expression-variants hydra_cpp_syntax_equality_expression-variants hydra_cpp_syntax_relational_expression-variants hydra_cpp_syntax_shift_expression-variants hydra_cpp_syntax_additive_expression-variants hydra_cpp_syntax_multiplicative_expression-variants hydra_cpp_syntax_unary_expression-variants hydra_cpp_syntax_unary_operator-variants hydra_cpp_syntax_postfix_expression-variants hydra_cpp_syntax_primary_expression-variants hydra_cpp_syntax_capture_list-variants hydra_cpp_syntax_visitor-variants hydra_cpp_syntax_function_identifier-variants hydra_cpp_syntax_statement-variants hydra_cpp_syntax_case_statement-variants hydra_cpp_syntax_iteration_statement-variants hydra_cpp_syntax_for_init-variants hydra_cpp_syntax_jump_statement-variants hydra_cpp_syntax_type_expression-variants hydra_cpp_syntax_basic_type-variants hydra_cpp_syntax_type_qualifier-variants hydra_cpp_syntax_template_argument-variants hydra_cpp_syntax_literal-variants hydra_cpp_syntax_integer_literal-variants hydra_cpp_syntax_binary_operator-variants)

(def hydra_cpp_syntax_access_specifier-variants (list :public :protected :private :none))

(defrecord hydra_cpp_syntax_program [preprocessor_directives includes declarations])
(defn make-hydra_cpp_syntax_program [preprocessor_directives includes declarations] (->hydra_cpp_syntax_program preprocessor_directives includes declarations))

(defrecord hydra_cpp_syntax_include_directive [name is_system])
(defn make-hydra_cpp_syntax_include_directive [name is_system] (->hydra_cpp_syntax_include_directive name is_system))

(def hydra_cpp_syntax_declaration-variants (list :preprocessor :class :function :variable :typedef :namespace :template))

(defrecord hydra_cpp_syntax_namespace_declaration [name declarations])
(defn make-hydra_cpp_syntax_namespace_declaration [name declarations] (->hydra_cpp_syntax_namespace_declaration name declarations))

(defrecord hydra_cpp_syntax_typedef_declaration [name type is_using])
(defn make-hydra_cpp_syntax_typedef_declaration [name type is_using] (->hydra_cpp_syntax_typedef_declaration name type is_using))

(defrecord hydra_cpp_syntax_class_declaration [specifier body])
(defn make-hydra_cpp_syntax_class_declaration [specifier body] (->hydra_cpp_syntax_class_declaration specifier body))

(defrecord hydra_cpp_syntax_template_declaration [inline parameters declaration])
(defn make-hydra_cpp_syntax_template_declaration [inline parameters declaration] (->hydra_cpp_syntax_template_declaration inline parameters declaration))

(def hydra_cpp_syntax_preprocessor_directive-variants (list :include :pragma :define :undef :ifdef :ifndef :if :elif :else :endif :line :error :warning))

(defrecord hydra_cpp_syntax_pragma_directive [content])
(defn make-hydra_cpp_syntax_pragma_directive [content] (->hydra_cpp_syntax_pragma_directive content))

(defrecord hydra_cpp_syntax_define_directive [name parameters replacement])
(defn make-hydra_cpp_syntax_define_directive [name parameters replacement] (->hydra_cpp_syntax_define_directive name parameters replacement))

(defrecord hydra_cpp_syntax_undef_directive [name])
(defn make-hydra_cpp_syntax_undef_directive [name] (->hydra_cpp_syntax_undef_directive name))

(defrecord hydra_cpp_syntax_ifdef_directive [identifier])
(defn make-hydra_cpp_syntax_ifdef_directive [identifier] (->hydra_cpp_syntax_ifdef_directive identifier))

(defrecord hydra_cpp_syntax_ifndef_directive [identifier])
(defn make-hydra_cpp_syntax_ifndef_directive [identifier] (->hydra_cpp_syntax_ifndef_directive identifier))

(defrecord hydra_cpp_syntax_if_directive [condition])
(defn make-hydra_cpp_syntax_if_directive [condition] (->hydra_cpp_syntax_if_directive condition))

(defrecord hydra_cpp_syntax_elif_directive [condition])
(defn make-hydra_cpp_syntax_elif_directive [condition] (->hydra_cpp_syntax_elif_directive condition))

(defrecord hydra_cpp_syntax_line_directive [line_number filename])
(defn make-hydra_cpp_syntax_line_directive [line_number filename] (->hydra_cpp_syntax_line_directive line_number filename))

(defrecord hydra_cpp_syntax_error_directive [message])
(defn make-hydra_cpp_syntax_error_directive [message] (->hydra_cpp_syntax_error_directive message))

(defrecord hydra_cpp_syntax_warning_directive [message])
(defn make-hydra_cpp_syntax_warning_directive [message] (->hydra_cpp_syntax_warning_directive message))

(defrecord hydra_cpp_syntax_class_specifier [key name inheritance])
(defn make-hydra_cpp_syntax_class_specifier [key name inheritance] (->hydra_cpp_syntax_class_specifier key name inheritance))

(def hydra_cpp_syntax_class_key-variants (list :class :enum :enum_class :struct))

(defrecord hydra_cpp_syntax_base_specifier [access name])
(defn make-hydra_cpp_syntax_base_specifier [access name] (->hydra_cpp_syntax_base_specifier access name))

(defrecord hydra_cpp_syntax_class_body [value])
(defn make-hydra_cpp_syntax_class_body [value] (->hydra_cpp_syntax_class_body value))

(def hydra_cpp_syntax_member_specification-variants (list :access_label :member))

(def hydra_cpp_syntax_member_declaration-variants (list :function :variable :constructor :destructor :nested_class :template))

(defrecord hydra_cpp_syntax_constructor_declaration [name parameters initializers body])
(defn make-hydra_cpp_syntax_constructor_declaration [name parameters initializers body] (->hydra_cpp_syntax_constructor_declaration name parameters initializers body))

(defrecord hydra_cpp_syntax_mem_initializer [name arguments])
(defn make-hydra_cpp_syntax_mem_initializer [name arguments] (->hydra_cpp_syntax_mem_initializer name arguments))

(defrecord hydra_cpp_syntax_destructor_declaration [prefix_specifiers name suffix_specifiers body])
(defn make-hydra_cpp_syntax_destructor_declaration [prefix_specifiers name suffix_specifiers body] (->hydra_cpp_syntax_destructor_declaration prefix_specifiers name suffix_specifiers body))

(defrecord hydra_cpp_syntax_function_declaration [prefix_specifiers return_type name parameters suffix_specifiers body])
(defn make-hydra_cpp_syntax_function_declaration [prefix_specifiers return_type name parameters suffix_specifiers body] (->hydra_cpp_syntax_function_declaration prefix_specifiers return_type name parameters suffix_specifiers body))

(def hydra_cpp_syntax_function_specifier_prefix-variants (list :inline :virtual :static :explicit))

(def hydra_cpp_syntax_function_specifier_suffix-variants (list :const :noexcept :override :final))

(defrecord hydra_cpp_syntax_parameter [type name unnamed default_value])
(defn make-hydra_cpp_syntax_parameter [type name unnamed default_value] (->hydra_cpp_syntax_parameter type name unnamed default_value))

(def hydra_cpp_syntax_function_body-variants (list :compound :declaration :pure :default))

(defrecord hydra_cpp_syntax_variable_declaration [type name initializer is_auto])
(defn make-hydra_cpp_syntax_variable_declaration [type name initializer is_auto] (->hydra_cpp_syntax_variable_declaration type name initializer is_auto))

(defrecord hydra_cpp_syntax_variant_declaration [types name])
(defn make-hydra_cpp_syntax_variant_declaration [types name] (->hydra_cpp_syntax_variant_declaration types name))

(defrecord hydra_cpp_syntax_product_declaration [name fields])
(defn make-hydra_cpp_syntax_product_declaration [name fields] (->hydra_cpp_syntax_product_declaration name fields))

(def hydra_cpp_syntax_container_declaration-variants (list :list :map :set :optional))

(defrecord hydra_cpp_syntax_list_declaration [element_type name])
(defn make-hydra_cpp_syntax_list_declaration [element_type name] (->hydra_cpp_syntax_list_declaration element_type name))

(defrecord hydra_cpp_syntax_map_declaration [key_type value_type name])
(defn make-hydra_cpp_syntax_map_declaration [key_type value_type name] (->hydra_cpp_syntax_map_declaration key_type value_type name))

(defrecord hydra_cpp_syntax_set_declaration [element_type name])
(defn make-hydra_cpp_syntax_set_declaration [element_type name] (->hydra_cpp_syntax_set_declaration element_type name))

(defrecord hydra_cpp_syntax_optional_declaration [value_type name])
(defn make-hydra_cpp_syntax_optional_declaration [value_type name] (->hydra_cpp_syntax_optional_declaration value_type name))

(def hydra_cpp_syntax_expression-variants (list :assignment :comma))

(defrecord hydra_cpp_syntax_comma_expression [left right])
(defn make-hydra_cpp_syntax_comma_expression [left right] (->hydra_cpp_syntax_comma_expression left right))

(def hydra_cpp_syntax_assignment_expression-variants (list :conditional :assignment))

(defrecord hydra_cpp_syntax_explicit_assignment [left op right])
(defn make-hydra_cpp_syntax_explicit_assignment [left op right] (->hydra_cpp_syntax_explicit_assignment left op right))

(def hydra_cpp_syntax_assignment_operator-variants (list :assign :plus_assign :minus_assign :multiply_assign :divide_assign :modulo_assign :left_shift_assign :right_shift_assign :bitwise_and_assign :bitwise_xor_assign :bitwise_or_assign))

(def hydra_cpp_syntax_conditional_expression-variants (list :logical_or :ternary))

(defrecord hydra_cpp_syntax_ternary_expression [condition true_expr false_expr])
(defn make-hydra_cpp_syntax_ternary_expression [condition true_expr false_expr] (->hydra_cpp_syntax_ternary_expression condition true_expr false_expr))

(def hydra_cpp_syntax_logical_or_expression-variants (list :logical_and :logical_or))

(defrecord hydra_cpp_syntax_logical_or_operation [left right])
(defn make-hydra_cpp_syntax_logical_or_operation [left right] (->hydra_cpp_syntax_logical_or_operation left right))

(def hydra_cpp_syntax_logical_and_expression-variants (list :inclusive_or :logical_and))

(defrecord hydra_cpp_syntax_logical_and_operation [left right])
(defn make-hydra_cpp_syntax_logical_and_operation [left right] (->hydra_cpp_syntax_logical_and_operation left right))

(def hydra_cpp_syntax_inclusive_or_expression-variants (list :exclusive_or :bitwise_or))

(defrecord hydra_cpp_syntax_bitwise_or_operation [left right])
(defn make-hydra_cpp_syntax_bitwise_or_operation [left right] (->hydra_cpp_syntax_bitwise_or_operation left right))

(def hydra_cpp_syntax_exclusive_or_expression-variants (list :and :bitwise_xor))

(defrecord hydra_cpp_syntax_bitwise_xor_operation [left right])
(defn make-hydra_cpp_syntax_bitwise_xor_operation [left right] (->hydra_cpp_syntax_bitwise_xor_operation left right))

(def hydra_cpp_syntax_and_expression-variants (list :equality :bitwise_and))

(defrecord hydra_cpp_syntax_bitwise_and_operation [left right])
(defn make-hydra_cpp_syntax_bitwise_and_operation [left right] (->hydra_cpp_syntax_bitwise_and_operation left right))

(def hydra_cpp_syntax_equality_expression-variants (list :relational :equal :not_equal))

(defrecord hydra_cpp_syntax_equal_operation [left right])
(defn make-hydra_cpp_syntax_equal_operation [left right] (->hydra_cpp_syntax_equal_operation left right))

(defrecord hydra_cpp_syntax_not_equal_operation [left right])
(defn make-hydra_cpp_syntax_not_equal_operation [left right] (->hydra_cpp_syntax_not_equal_operation left right))

(def hydra_cpp_syntax_relational_expression-variants (list :shift :less :greater :less_equal :greater_equal))

(defrecord hydra_cpp_syntax_less_operation [left right])
(defn make-hydra_cpp_syntax_less_operation [left right] (->hydra_cpp_syntax_less_operation left right))

(defrecord hydra_cpp_syntax_greater_operation [left right])
(defn make-hydra_cpp_syntax_greater_operation [left right] (->hydra_cpp_syntax_greater_operation left right))

(defrecord hydra_cpp_syntax_less_equal_operation [left right])
(defn make-hydra_cpp_syntax_less_equal_operation [left right] (->hydra_cpp_syntax_less_equal_operation left right))

(defrecord hydra_cpp_syntax_greater_equal_operation [left right])
(defn make-hydra_cpp_syntax_greater_equal_operation [left right] (->hydra_cpp_syntax_greater_equal_operation left right))

(def hydra_cpp_syntax_shift_expression-variants (list :additive :left_shift :right_shift))

(defrecord hydra_cpp_syntax_left_shift_operation [left right])
(defn make-hydra_cpp_syntax_left_shift_operation [left right] (->hydra_cpp_syntax_left_shift_operation left right))

(defrecord hydra_cpp_syntax_right_shift_operation [left right])
(defn make-hydra_cpp_syntax_right_shift_operation [left right] (->hydra_cpp_syntax_right_shift_operation left right))

(def hydra_cpp_syntax_additive_expression-variants (list :multiplicative :add :subtract))

(defrecord hydra_cpp_syntax_add_operation [left right])
(defn make-hydra_cpp_syntax_add_operation [left right] (->hydra_cpp_syntax_add_operation left right))

(defrecord hydra_cpp_syntax_subtract_operation [left right])
(defn make-hydra_cpp_syntax_subtract_operation [left right] (->hydra_cpp_syntax_subtract_operation left right))

(def hydra_cpp_syntax_multiplicative_expression-variants (list :unary :multiply :divide :modulo))

(defrecord hydra_cpp_syntax_multiply_operation [left right])
(defn make-hydra_cpp_syntax_multiply_operation [left right] (->hydra_cpp_syntax_multiply_operation left right))

(defrecord hydra_cpp_syntax_divide_operation [left right])
(defn make-hydra_cpp_syntax_divide_operation [left right] (->hydra_cpp_syntax_divide_operation left right))

(defrecord hydra_cpp_syntax_modulo_operation [left right])
(defn make-hydra_cpp_syntax_modulo_operation [left right] (->hydra_cpp_syntax_modulo_operation left right))

(def hydra_cpp_syntax_unary_expression-variants (list :postfix :unary_op :sizeof))

(defrecord hydra_cpp_syntax_unary_operation [operator operand])
(defn make-hydra_cpp_syntax_unary_operation [operator operand] (->hydra_cpp_syntax_unary_operation operator operand))

(def hydra_cpp_syntax_unary_operator-variants (list :plus :minus :logical_not :bitwise_not :dereference :address_of :pre_increment :pre_decrement))

(defrecord hydra_cpp_syntax_sizeof_expression [value])
(defn make-hydra_cpp_syntax_sizeof_expression [value] (->hydra_cpp_syntax_sizeof_expression value))

(def hydra_cpp_syntax_postfix_expression-variants (list :primary :subscript :function_call :template_function_call :member_access :pointer_member_access :post_increment :post_decrement))

(defrecord hydra_cpp_syntax_subscript_operation [array index])
(defn make-hydra_cpp_syntax_subscript_operation [array index] (->hydra_cpp_syntax_subscript_operation array index))

(defrecord hydra_cpp_syntax_function_call_operation [function arguments])
(defn make-hydra_cpp_syntax_function_call_operation [function arguments] (->hydra_cpp_syntax_function_call_operation function arguments))

(defrecord hydra_cpp_syntax_member_access_operation [object member])
(defn make-hydra_cpp_syntax_member_access_operation [object member] (->hydra_cpp_syntax_member_access_operation object member))

(defrecord hydra_cpp_syntax_pointer_member_access_operation [pointer member])
(defn make-hydra_cpp_syntax_pointer_member_access_operation [pointer member] (->hydra_cpp_syntax_pointer_member_access_operation pointer member))

(defrecord hydra_cpp_syntax_template_function_call_operation [function template_arguments arguments])
(defn make-hydra_cpp_syntax_template_function_call_operation [function template_arguments arguments] (->hydra_cpp_syntax_template_function_call_operation function template_arguments arguments))

(def hydra_cpp_syntax_primary_expression-variants (list :identifier :literal :parenthesized :lambda))

(defrecord hydra_cpp_syntax_lambda_expression [captures parameters return_type body])
(defn make-hydra_cpp_syntax_lambda_expression [captures parameters return_type body] (->hydra_cpp_syntax_lambda_expression captures parameters return_type body))

(def hydra_cpp_syntax_capture_list-variants (list :capture_by_value :captures))

(defrecord hydra_cpp_syntax_capture [name by_reference])
(defn make-hydra_cpp_syntax_capture [name by_reference] (->hydra_cpp_syntax_capture name by_reference))

(defrecord hydra_cpp_syntax_pattern_match [visitor variant])
(defn make-hydra_cpp_syntax_pattern_match [visitor variant] (->hydra_cpp_syntax_pattern_match visitor variant))

(def hydra_cpp_syntax_visitor-variants (list :lambda :overloaded))

(defrecord hydra_cpp_syntax_overloaded_lambdas [value])
(defn make-hydra_cpp_syntax_overloaded_lambdas [value] (->hydra_cpp_syntax_overloaded_lambdas value))

(defrecord hydra_cpp_syntax_function_application [function arguments])
(defn make-hydra_cpp_syntax_function_application [function arguments] (->hydra_cpp_syntax_function_application function arguments))

(def hydra_cpp_syntax_function_identifier-variants (list :simple :qualified))

(defrecord hydra_cpp_syntax_qualified_identifier [namespace name])
(defn make-hydra_cpp_syntax_qualified_identifier [namespace name] (->hydra_cpp_syntax_qualified_identifier namespace name))

(def hydra_cpp_syntax_statement-variants (list :labeled :compound :selection :switch :iteration :jump :declaration :expression))

(defrecord hydra_cpp_syntax_labeled_statement [label statement])
(defn make-hydra_cpp_syntax_labeled_statement [label statement] (->hydra_cpp_syntax_labeled_statement label statement))

(defrecord hydra_cpp_syntax_compound_statement [value])
(defn make-hydra_cpp_syntax_compound_statement [value] (->hydra_cpp_syntax_compound_statement value))

(defrecord hydra_cpp_syntax_selection_statement [condition then_branch else_branch])
(defn make-hydra_cpp_syntax_selection_statement [condition then_branch else_branch] (->hydra_cpp_syntax_selection_statement condition then_branch else_branch))

(defrecord hydra_cpp_syntax_switch_statement [value cases])
(defn make-hydra_cpp_syntax_switch_statement [value cases] (->hydra_cpp_syntax_switch_statement value cases))

(def hydra_cpp_syntax_case_statement-variants (list :case :default))

(defrecord hydra_cpp_syntax_case_value [value statement])
(defn make-hydra_cpp_syntax_case_value [value statement] (->hydra_cpp_syntax_case_value value statement))

(def hydra_cpp_syntax_iteration_statement-variants (list :while :do :for :range_for))

(defrecord hydra_cpp_syntax_while_statement [condition body])
(defn make-hydra_cpp_syntax_while_statement [condition body] (->hydra_cpp_syntax_while_statement condition body))

(defrecord hydra_cpp_syntax_do_statement [body condition])
(defn make-hydra_cpp_syntax_do_statement [body condition] (->hydra_cpp_syntax_do_statement body condition))

(defrecord hydra_cpp_syntax_for_statement [init condition increment body])
(defn make-hydra_cpp_syntax_for_statement [init condition increment body] (->hydra_cpp_syntax_for_statement init condition increment body))

(def hydra_cpp_syntax_for_init-variants (list :expression :declaration :empty))

(defrecord hydra_cpp_syntax_range_for_statement [type variable range body])
(defn make-hydra_cpp_syntax_range_for_statement [type variable range body] (->hydra_cpp_syntax_range_for_statement type variable range body))

(def hydra_cpp_syntax_jump_statement-variants (list :break :continue :return_value :return_void :throw))

(defrecord hydra_cpp_syntax_expression_statement [value])
(defn make-hydra_cpp_syntax_expression_statement [value] (->hydra_cpp_syntax_expression_statement value))

(def hydra_cpp_syntax_type_expression-variants (list :basic :qualified :template :function :auto))

(def hydra_cpp_syntax_basic_type-variants (list :void :bool :char :int :float :double :string :auto :named))

(defrecord hydra_cpp_syntax_qualified_type [base_type qualifier])
(defn make-hydra_cpp_syntax_qualified_type [base_type qualifier] (->hydra_cpp_syntax_qualified_type base_type qualifier))

(def hydra_cpp_syntax_type_qualifier-variants (list :const :lvalue_ref :rvalue_ref :pointer))

(defrecord hydra_cpp_syntax_template_type [name arguments])
(defn make-hydra_cpp_syntax_template_type [name arguments] (->hydra_cpp_syntax_template_type name arguments))

(def hydra_cpp_syntax_template_argument-variants (list :type :value))

(defrecord hydra_cpp_syntax_function_type [return_type parameters])
(defn make-hydra_cpp_syntax_function_type [return_type parameters] (->hydra_cpp_syntax_function_type return_type parameters))

(def hydra_cpp_syntax_literal-variants (list :integer :floating :character :string :boolean :null))

(def hydra_cpp_syntax_integer_literal-variants (list :decimal :hexadecimal :octal :binary))

(defrecord hydra_cpp_syntax_floating_literal [value])
(defn make-hydra_cpp_syntax_floating_literal [value] (->hydra_cpp_syntax_floating_literal value))

(defrecord hydra_cpp_syntax_character_literal [value])
(defn make-hydra_cpp_syntax_character_literal [value] (->hydra_cpp_syntax_character_literal value))

(defrecord hydra_cpp_syntax_string_literal [value])
(defn make-hydra_cpp_syntax_string_literal [value] (->hydra_cpp_syntax_string_literal value))

(defrecord hydra_cpp_syntax_boolean_literal [value])
(defn make-hydra_cpp_syntax_boolean_literal [value] (->hydra_cpp_syntax_boolean_literal value))

(defrecord hydra_cpp_syntax_vector [element_type elements])
(defn make-hydra_cpp_syntax_vector [element_type elements] (->hydra_cpp_syntax_vector element_type elements))

(defrecord hydra_cpp_syntax_map [key_type value_type entries])
(defn make-hydra_cpp_syntax_map [key_type value_type entries] (->hydra_cpp_syntax_map key_type value_type entries))

(defrecord hydra_cpp_syntax_map_entry [key value])
(defn make-hydra_cpp_syntax_map_entry [key value] (->hydra_cpp_syntax_map_entry key value))

(defrecord hydra_cpp_syntax_set [element_type elements])
(defn make-hydra_cpp_syntax_set [element_type elements] (->hydra_cpp_syntax_set element_type elements))

(defrecord hydra_cpp_syntax_optional [value_type value])
(defn make-hydra_cpp_syntax_optional [value_type value] (->hydra_cpp_syntax_optional value_type value))

(defrecord hydra_cpp_syntax_comment [text is_multiline])
(defn make-hydra_cpp_syntax_comment [text is_multiline] (->hydra_cpp_syntax_comment text is_multiline))

(def hydra_cpp_syntax_binary_operator-variants (list :plus :minus :multiply :divide :modulo :bitwise_and :bitwise_or :bitwise_xor :logical_and :logical_or :equal :not_equal :less :greater :less_equal :greater_equal :left_shift :right_shift))

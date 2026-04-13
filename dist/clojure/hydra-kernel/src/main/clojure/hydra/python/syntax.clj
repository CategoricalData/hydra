(ns hydra.python.syntax)

(declare hydra_python_syntax_quote_style-variants hydra_python_syntax_number-variants hydra_python_syntax_statement-variants hydra_python_syntax_simple_statement-variants hydra_python_syntax_compound_statement-variants hydra_python_syntax_assignment-variants hydra_python_syntax_annotated_rhs-variants hydra_python_syntax_aug_assign-variants hydra_python_syntax_import_statement-variants hydra_python_syntax_relative_import_prefix-variants hydra_python_syntax_import_from_targets-variants hydra_python_syntax_block-variants hydra_python_syntax_parameters-variants hydra_python_syntax_star_etc-variants hydra_python_syntax_if_tail-variants hydra_python_syntax_try_statement-variants hydra_python_syntax_subject_expression-variants hydra_python_syntax_patterns-variants hydra_python_syntax_pattern-variants hydra_python_syntax_closed_pattern-variants hydra_python_syntax_literal_expression-variants hydra_python_syntax_plus_or_minus-variants hydra_python_syntax_signed_number-variants hydra_python_syntax_signed_real_number-variants hydra_python_syntax_sequence_pattern-variants hydra_python_syntax_maybe_star_pattern-variants hydra_python_syntax_star_pattern-variants hydra_python_syntax_literal_expression_or_attribute-variants hydra_python_syntax_type_parameter-variants hydra_python_syntax_expression-variants hydra_python_syntax_yield_expression-variants hydra_python_syntax_star_expression-variants hydra_python_syntax_star_named_expression-variants hydra_python_syntax_named_expression-variants hydra_python_syntax_inversion-variants hydra_python_syntax_compare_op-variants hydra_python_syntax_shift_op-variants hydra_python_syntax_sum_op-variants hydra_python_syntax_term_op-variants hydra_python_syntax_factor-variants hydra_python_syntax_primary-variants hydra_python_syntax_primary_rhs-variants hydra_python_syntax_slice_or_starred_expression-variants hydra_python_syntax_slice-variants hydra_python_syntax_atom-variants hydra_python_syntax_group-variants hydra_python_syntax_lambda_star_etc-variants hydra_python_syntax_double_starred_kvpair-variants hydra_python_syntax_genexp_head-variants hydra_python_syntax_pos_arg-variants hydra_python_syntax_kwarg_or_starred-variants hydra_python_syntax_kwarg_or_double_starred-variants hydra_python_syntax_star_target-variants hydra_python_syntax_target_with_star_atom-variants hydra_python_syntax_star_atom-variants hydra_python_syntax_single_target-variants hydra_python_syntax_single_subscript_attribute_target-variants hydra_python_syntax_t_primary-variants hydra_python_syntax_del_target-variants hydra_python_syntax_del_t_atom-variants hydra_python_syntax_type_expression-variants)

(defrecord hydra_python_syntax_annotated_statement [comment statement])
(defn make-hydra_python_syntax_annotated_statement [comment statement] (->hydra_python_syntax_annotated_statement comment statement))

(defrecord hydra_python_syntax_module [value])
(defn make-hydra_python_syntax_module [value] (->hydra_python_syntax_module value))

(def hydra_python_syntax_quote_style-variants (list :single :double :triple))

(defrecord hydra_python_syntax_name [value])
(defn make-hydra_python_syntax_name [value] (->hydra_python_syntax_name value))

(def hydra_python_syntax_number-variants (list :integer :float))

(defrecord hydra_python_syntax_string [value quote_style])
(defn make-hydra_python_syntax_string [value quote_style] (->hydra_python_syntax_string value quote_style))

(defrecord hydra_python_syntax_type_comment [value])
(defn make-hydra_python_syntax_type_comment [value] (->hydra_python_syntax_type_comment value))

(defrecord hydra_python_syntax_file [value])
(defn make-hydra_python_syntax_file [value] (->hydra_python_syntax_file value))

(defrecord hydra_python_syntax_interactive [value])
(defn make-hydra_python_syntax_interactive [value] (->hydra_python_syntax_interactive value))

(defrecord hydra_python_syntax_eval [value])
(defn make-hydra_python_syntax_eval [value] (->hydra_python_syntax_eval value))

(defrecord hydra_python_syntax_func_type [type body])
(defn make-hydra_python_syntax_func_type [type body] (->hydra_python_syntax_func_type type body))

(def hydra_python_syntax_statement-variants (list :compound :simple :annotated))

(def hydra_python_syntax_simple_statement-variants (list :assignment :type_alias :star_expressions :return :import :raise :pass :del :yield :assert :break :continue :global :nonlocal))

(def hydra_python_syntax_compound_statement-variants (list :function :if :class_def :with :for :try :while :match))

(def hydra_python_syntax_assignment-variants (list :typed :untyped :aug))

(defrecord hydra_python_syntax_typed_assignment [lhs type rhs])
(defn make-hydra_python_syntax_typed_assignment [lhs type rhs] (->hydra_python_syntax_typed_assignment lhs type rhs))

(defrecord hydra_python_syntax_untyped_assignment [targets rhs type_comment])
(defn make-hydra_python_syntax_untyped_assignment [targets rhs type_comment] (->hydra_python_syntax_untyped_assignment targets rhs type_comment))

(defrecord hydra_python_syntax_aug_assignment [lhs augassign rhs])
(defn make-hydra_python_syntax_aug_assignment [lhs augassign rhs] (->hydra_python_syntax_aug_assignment lhs augassign rhs))

(def hydra_python_syntax_annotated_rhs-variants (list :yield :star))

(def hydra_python_syntax_aug_assign-variants (list :plus_equal :minus_equal :times_equal :at_equal :slash_equal :percent_equal :ampersand_equal :bar_equal :caret_equal :left_shift_equal :right_shift_equal :star_star_equal :double_slash_equal))

(defrecord hydra_python_syntax_return_statement [value])
(defn make-hydra_python_syntax_return_statement [value] (->hydra_python_syntax_return_statement value))

(defrecord hydra_python_syntax_raise_statement [value])
(defn make-hydra_python_syntax_raise_statement [value] (->hydra_python_syntax_raise_statement value))

(defrecord hydra_python_syntax_raise_expression [expression from])
(defn make-hydra_python_syntax_raise_expression [expression from] (->hydra_python_syntax_raise_expression expression from))

(defrecord hydra_python_syntax_del_statement [value])
(defn make-hydra_python_syntax_del_statement [value] (->hydra_python_syntax_del_statement value))

(defrecord hydra_python_syntax_yield_statement [value])
(defn make-hydra_python_syntax_yield_statement [value] (->hydra_python_syntax_yield_statement value))

(defrecord hydra_python_syntax_assert_statement [expression1 expression2])
(defn make-hydra_python_syntax_assert_statement [expression1 expression2] (->hydra_python_syntax_assert_statement expression1 expression2))

(def hydra_python_syntax_import_statement-variants (list :name :from))

(defrecord hydra_python_syntax_import_name [value])
(defn make-hydra_python_syntax_import_name [value] (->hydra_python_syntax_import_name value))

(defrecord hydra_python_syntax_import_from [prefixes dotted_name targets])
(defn make-hydra_python_syntax_import_from [prefixes dotted_name targets] (->hydra_python_syntax_import_from prefixes dotted_name targets))

(def hydra_python_syntax_relative_import_prefix-variants (list :dot :ellipsis))

(def hydra_python_syntax_import_from_targets-variants (list :simple :parens :star))

(defrecord hydra_python_syntax_import_from_as_name [name as])
(defn make-hydra_python_syntax_import_from_as_name [name as] (->hydra_python_syntax_import_from_as_name name as))

(defrecord hydra_python_syntax_dotted_as_name [name as])
(defn make-hydra_python_syntax_dotted_as_name [name as] (->hydra_python_syntax_dotted_as_name name as))

(defrecord hydra_python_syntax_dotted_name [value])
(defn make-hydra_python_syntax_dotted_name [value] (->hydra_python_syntax_dotted_name value))

(def hydra_python_syntax_block-variants (list :indented :simple))

(defrecord hydra_python_syntax_decorators [value])
(defn make-hydra_python_syntax_decorators [value] (->hydra_python_syntax_decorators value))

(defrecord hydra_python_syntax_class_definition [decorators name type_params arguments body])
(defn make-hydra_python_syntax_class_definition [decorators name type_params arguments body] (->hydra_python_syntax_class_definition decorators name type_params arguments body))

(defrecord hydra_python_syntax_function_definition [decorators raw])
(defn make-hydra_python_syntax_function_definition [decorators raw] (->hydra_python_syntax_function_definition decorators raw))

(defrecord hydra_python_syntax_function_def_raw [async name type_params params return_type func_type_comment block])
(defn make-hydra_python_syntax_function_def_raw [async name type_params params return_type func_type_comment block] (->hydra_python_syntax_function_def_raw async name type_params params return_type func_type_comment block))

(def hydra_python_syntax_parameters-variants (list :slash_no_default :slash_with_default :param_no_default :param_with_default :star_etc))

(defrecord hydra_python_syntax_slash_no_default_parameters [slash param_no_default param_with_default star_etc])
(defn make-hydra_python_syntax_slash_no_default_parameters [slash param_no_default param_with_default star_etc] (->hydra_python_syntax_slash_no_default_parameters slash param_no_default param_with_default star_etc))

(defrecord hydra_python_syntax_slash_with_default_parameters [param_no_default param_with_default star_etc])
(defn make-hydra_python_syntax_slash_with_default_parameters [param_no_default param_with_default star_etc] (->hydra_python_syntax_slash_with_default_parameters param_no_default param_with_default star_etc))

(defrecord hydra_python_syntax_param_no_default_parameters [param_no_default param_with_default star_etc])
(defn make-hydra_python_syntax_param_no_default_parameters [param_no_default param_with_default star_etc] (->hydra_python_syntax_param_no_default_parameters param_no_default param_with_default star_etc))

(defrecord hydra_python_syntax_param_with_default_parameters [param_with_default star_etc])
(defn make-hydra_python_syntax_param_with_default_parameters [param_with_default star_etc] (->hydra_python_syntax_param_with_default_parameters param_with_default star_etc))

(defrecord hydra_python_syntax_slash_no_default [value])
(defn make-hydra_python_syntax_slash_no_default [value] (->hydra_python_syntax_slash_no_default value))

(defrecord hydra_python_syntax_slash_with_default [param_no_default param_with_default])
(defn make-hydra_python_syntax_slash_with_default [param_no_default param_with_default] (->hydra_python_syntax_slash_with_default param_no_default param_with_default))

(def hydra_python_syntax_star_etc-variants (list :star_no_default :star_no_default_star_annotation :star_comma :keywords))

(defrecord hydra_python_syntax_no_default_star_etc [param_no_default param_maybe_default keywords])
(defn make-hydra_python_syntax_no_default_star_etc [param_no_default param_maybe_default keywords] (->hydra_python_syntax_no_default_star_etc param_no_default param_maybe_default keywords))

(defrecord hydra_python_syntax_no_default_star_annotation_star_etc [param_no_default_star_annotation param_maybe_default keywords])
(defn make-hydra_python_syntax_no_default_star_annotation_star_etc [param_no_default_star_annotation param_maybe_default keywords] (->hydra_python_syntax_no_default_star_annotation_star_etc param_no_default_star_annotation param_maybe_default keywords))

(defrecord hydra_python_syntax_comma_star_etc [param_maybe_default keywords])
(defn make-hydra_python_syntax_comma_star_etc [param_maybe_default keywords] (->hydra_python_syntax_comma_star_etc param_maybe_default keywords))

(defrecord hydra_python_syntax_keywords [value])
(defn make-hydra_python_syntax_keywords [value] (->hydra_python_syntax_keywords value))

(defrecord hydra_python_syntax_param_no_default [param type_comment])
(defn make-hydra_python_syntax_param_no_default [param type_comment] (->hydra_python_syntax_param_no_default param type_comment))

(defrecord hydra_python_syntax_param_no_default_star_annotation [param_star_annotation type_comment])
(defn make-hydra_python_syntax_param_no_default_star_annotation [param_star_annotation type_comment] (->hydra_python_syntax_param_no_default_star_annotation param_star_annotation type_comment))

(defrecord hydra_python_syntax_param_with_default [param default type_comment])
(defn make-hydra_python_syntax_param_with_default [param default type_comment] (->hydra_python_syntax_param_with_default param default type_comment))

(defrecord hydra_python_syntax_param_maybe_default [param default type_comment])
(defn make-hydra_python_syntax_param_maybe_default [param default type_comment] (->hydra_python_syntax_param_maybe_default param default type_comment))

(defrecord hydra_python_syntax_param [name annotation])
(defn make-hydra_python_syntax_param [name annotation] (->hydra_python_syntax_param name annotation))

(defrecord hydra_python_syntax_param_star_annotation [name annotation])
(defn make-hydra_python_syntax_param_star_annotation [name annotation] (->hydra_python_syntax_param_star_annotation name annotation))

(defrecord hydra_python_syntax_annotation [value])
(defn make-hydra_python_syntax_annotation [value] (->hydra_python_syntax_annotation value))

(defrecord hydra_python_syntax_star_annotation [value])
(defn make-hydra_python_syntax_star_annotation [value] (->hydra_python_syntax_star_annotation value))

(defrecord hydra_python_syntax_default [value])
(defn make-hydra_python_syntax_default [value] (->hydra_python_syntax_default value))

(defrecord hydra_python_syntax_if_statement [condition body continuation])
(defn make-hydra_python_syntax_if_statement [condition body continuation] (->hydra_python_syntax_if_statement condition body continuation))

(def hydra_python_syntax_if_tail-variants (list :elif :else))

(defrecord hydra_python_syntax_while_statement [condition body else])
(defn make-hydra_python_syntax_while_statement [condition body else] (->hydra_python_syntax_while_statement condition body else))

(defrecord hydra_python_syntax_for_statement [async targets expressions type_comment body else])
(defn make-hydra_python_syntax_for_statement [async targets expressions type_comment body else] (->hydra_python_syntax_for_statement async targets expressions type_comment body else))

(defrecord hydra_python_syntax_with_statement [async items type_comment body])
(defn make-hydra_python_syntax_with_statement [async items type_comment body] (->hydra_python_syntax_with_statement async items type_comment body))

(defrecord hydra_python_syntax_with_item [expression as])
(defn make-hydra_python_syntax_with_item [expression as] (->hydra_python_syntax_with_item expression as))

(def hydra_python_syntax_try_statement-variants (list :finally :except :except_star))

(defrecord hydra_python_syntax_try_finally_statement [body finally])
(defn make-hydra_python_syntax_try_finally_statement [body finally] (->hydra_python_syntax_try_finally_statement body finally))

(defrecord hydra_python_syntax_try_except_statement [body excepts else finally])
(defn make-hydra_python_syntax_try_except_statement [body excepts else finally] (->hydra_python_syntax_try_except_statement body excepts else finally))

(defrecord hydra_python_syntax_try_except_star_statement [body excepts else finally])
(defn make-hydra_python_syntax_try_except_star_statement [body excepts else finally] (->hydra_python_syntax_try_except_star_statement body excepts else finally))

(defrecord hydra_python_syntax_except_block [expression body])
(defn make-hydra_python_syntax_except_block [expression body] (->hydra_python_syntax_except_block expression body))

(defrecord hydra_python_syntax_except_expression [expression as])
(defn make-hydra_python_syntax_except_expression [expression as] (->hydra_python_syntax_except_expression expression as))

(defrecord hydra_python_syntax_except_star_block [expression as body])
(defn make-hydra_python_syntax_except_star_block [expression as body] (->hydra_python_syntax_except_star_block expression as body))

(defrecord hydra_python_syntax_match_statement [subject cases])
(defn make-hydra_python_syntax_match_statement [subject cases] (->hydra_python_syntax_match_statement subject cases))

(def hydra_python_syntax_subject_expression-variants (list :tuple :simple))

(defrecord hydra_python_syntax_case_block [patterns guard body])
(defn make-hydra_python_syntax_case_block [patterns guard body] (->hydra_python_syntax_case_block patterns guard body))

(defrecord hydra_python_syntax_guard [value])
(defn make-hydra_python_syntax_guard [value] (->hydra_python_syntax_guard value))

(def hydra_python_syntax_patterns-variants (list :sequence :pattern))

(def hydra_python_syntax_pattern-variants (list :as :or))

(defrecord hydra_python_syntax_as_pattern [pattern as])
(defn make-hydra_python_syntax_as_pattern [pattern as] (->hydra_python_syntax_as_pattern pattern as))

(defrecord hydra_python_syntax_or_pattern [value])
(defn make-hydra_python_syntax_or_pattern [value] (->hydra_python_syntax_or_pattern value))

(def hydra_python_syntax_closed_pattern-variants (list :literal :capture :wildcard :value :group :sequence :mapping :class))

(def hydra_python_syntax_literal_expression-variants (list :number :complex :string :none :true :false))

(defrecord hydra_python_syntax_complex_number [real plus_or_minus imaginary])
(defn make-hydra_python_syntax_complex_number [real plus_or_minus imaginary] (->hydra_python_syntax_complex_number real plus_or_minus imaginary))

(def hydra_python_syntax_plus_or_minus-variants (list :plus :minus))

(def hydra_python_syntax_signed_number-variants (list :sign :number))

(def hydra_python_syntax_signed_real_number-variants (list :sign :number))

(defrecord hydra_python_syntax_real_number [value])
(defn make-hydra_python_syntax_real_number [value] (->hydra_python_syntax_real_number value))

(defrecord hydra_python_syntax_imaginary_number [value])
(defn make-hydra_python_syntax_imaginary_number [value] (->hydra_python_syntax_imaginary_number value))

(defrecord hydra_python_syntax_capture_pattern [value])
(defn make-hydra_python_syntax_capture_pattern [value] (->hydra_python_syntax_capture_pattern value))

(defrecord hydra_python_syntax_pattern_capture_target [value])
(defn make-hydra_python_syntax_pattern_capture_target [value] (->hydra_python_syntax_pattern_capture_target value))

(defrecord hydra_python_syntax_value_pattern [value])
(defn make-hydra_python_syntax_value_pattern [value] (->hydra_python_syntax_value_pattern value))

(defrecord hydra_python_syntax_attribute [value])
(defn make-hydra_python_syntax_attribute [value] (->hydra_python_syntax_attribute value))

(defrecord hydra_python_syntax_name_or_attribute [value])
(defn make-hydra_python_syntax_name_or_attribute [value] (->hydra_python_syntax_name_or_attribute value))

(defrecord hydra_python_syntax_group_pattern [value])
(defn make-hydra_python_syntax_group_pattern [value] (->hydra_python_syntax_group_pattern value))

(def hydra_python_syntax_sequence_pattern-variants (list :list :tuple))

(defrecord hydra_python_syntax_open_sequence_pattern [head tail])
(defn make-hydra_python_syntax_open_sequence_pattern [head tail] (->hydra_python_syntax_open_sequence_pattern head tail))

(defrecord hydra_python_syntax_maybe_sequence_pattern [value])
(defn make-hydra_python_syntax_maybe_sequence_pattern [value] (->hydra_python_syntax_maybe_sequence_pattern value))

(def hydra_python_syntax_maybe_star_pattern-variants (list :star :pattern))

(def hydra_python_syntax_star_pattern-variants (list :capture :wildcard))

(defrecord hydra_python_syntax_mapping_pattern [items double_star])
(defn make-hydra_python_syntax_mapping_pattern [items double_star] (->hydra_python_syntax_mapping_pattern items double_star))

(defrecord hydra_python_syntax_items_pattern [value])
(defn make-hydra_python_syntax_items_pattern [value] (->hydra_python_syntax_items_pattern value))

(defrecord hydra_python_syntax_key_value_pattern [key value])
(defn make-hydra_python_syntax_key_value_pattern [key value] (->hydra_python_syntax_key_value_pattern key value))

(def hydra_python_syntax_literal_expression_or_attribute-variants (list :literal :attribute))

(defrecord hydra_python_syntax_double_star_pattern [value])
(defn make-hydra_python_syntax_double_star_pattern [value] (->hydra_python_syntax_double_star_pattern value))

(defrecord hydra_python_syntax_class_pattern [name_or_attribute positional_patterns keyword_patterns])
(defn make-hydra_python_syntax_class_pattern [name_or_attribute positional_patterns keyword_patterns] (->hydra_python_syntax_class_pattern name_or_attribute positional_patterns keyword_patterns))

(defrecord hydra_python_syntax_positional_patterns [value])
(defn make-hydra_python_syntax_positional_patterns [value] (->hydra_python_syntax_positional_patterns value))

(defrecord hydra_python_syntax_keyword_patterns [value])
(defn make-hydra_python_syntax_keyword_patterns [value] (->hydra_python_syntax_keyword_patterns value))

(defrecord hydra_python_syntax_keyword_pattern [name pattern])
(defn make-hydra_python_syntax_keyword_pattern [name pattern] (->hydra_python_syntax_keyword_pattern name pattern))

(defrecord hydra_python_syntax_type_alias [name type_params expression])
(defn make-hydra_python_syntax_type_alias [name type_params expression] (->hydra_python_syntax_type_alias name type_params expression))

(def hydra_python_syntax_type_parameter-variants (list :simple :star :double_star))

(defrecord hydra_python_syntax_simple_type_parameter [name bound default])
(defn make-hydra_python_syntax_simple_type_parameter [name bound default] (->hydra_python_syntax_simple_type_parameter name bound default))

(defrecord hydra_python_syntax_star_type_parameter [name default])
(defn make-hydra_python_syntax_star_type_parameter [name default] (->hydra_python_syntax_star_type_parameter name default))

(defrecord hydra_python_syntax_double_star_type_parameter [name default])
(defn make-hydra_python_syntax_double_star_type_parameter [name default] (->hydra_python_syntax_double_star_type_parameter name default))

(def hydra_python_syntax_expression-variants (list :conditional :simple :lambda))

(defrecord hydra_python_syntax_conditional [body if else])
(defn make-hydra_python_syntax_conditional [body if else] (->hydra_python_syntax_conditional body if else))

(def hydra_python_syntax_yield_expression-variants (list :from :simple))

(def hydra_python_syntax_star_expression-variants (list :star :simple))

(defrecord hydra_python_syntax_star_named_expressions [value])
(defn make-hydra_python_syntax_star_named_expressions [value] (->hydra_python_syntax_star_named_expressions value))

(def hydra_python_syntax_star_named_expression-variants (list :star :simple))

(defrecord hydra_python_syntax_assignment_expression [name expression])
(defn make-hydra_python_syntax_assignment_expression [name expression] (->hydra_python_syntax_assignment_expression name expression))

(def hydra_python_syntax_named_expression-variants (list :assignment :simple))

(defrecord hydra_python_syntax_disjunction [value])
(defn make-hydra_python_syntax_disjunction [value] (->hydra_python_syntax_disjunction value))

(defrecord hydra_python_syntax_conjunction [value])
(defn make-hydra_python_syntax_conjunction [value] (->hydra_python_syntax_conjunction value))

(def hydra_python_syntax_inversion-variants (list :not :simple))

(defrecord hydra_python_syntax_comparison [lhs rhs])
(defn make-hydra_python_syntax_comparison [lhs rhs] (->hydra_python_syntax_comparison lhs rhs))

(defrecord hydra_python_syntax_compare_op_bitwise_or_pair [operator rhs])
(defn make-hydra_python_syntax_compare_op_bitwise_or_pair [operator rhs] (->hydra_python_syntax_compare_op_bitwise_or_pair operator rhs))

(def hydra_python_syntax_compare_op-variants (list :eq :noteq :lte :lt :gte :gt :notin :in :isnot :is))

(defrecord hydra_python_syntax_bitwise_or [lhs rhs])
(defn make-hydra_python_syntax_bitwise_or [lhs rhs] (->hydra_python_syntax_bitwise_or lhs rhs))

(defrecord hydra_python_syntax_bitwise_xor [lhs rhs])
(defn make-hydra_python_syntax_bitwise_xor [lhs rhs] (->hydra_python_syntax_bitwise_xor lhs rhs))

(defrecord hydra_python_syntax_bitwise_and [lhs rhs])
(defn make-hydra_python_syntax_bitwise_and [lhs rhs] (->hydra_python_syntax_bitwise_and lhs rhs))

(defrecord hydra_python_syntax_shift_expression [lhs rhs])
(defn make-hydra_python_syntax_shift_expression [lhs rhs] (->hydra_python_syntax_shift_expression lhs rhs))

(defrecord hydra_python_syntax_shift_lhs [operand operator])
(defn make-hydra_python_syntax_shift_lhs [operand operator] (->hydra_python_syntax_shift_lhs operand operator))

(def hydra_python_syntax_shift_op-variants (list :left :right))

(defrecord hydra_python_syntax_sum [lhs rhs])
(defn make-hydra_python_syntax_sum [lhs rhs] (->hydra_python_syntax_sum lhs rhs))

(defrecord hydra_python_syntax_sum_lhs [operand operator])
(defn make-hydra_python_syntax_sum_lhs [operand operator] (->hydra_python_syntax_sum_lhs operand operator))

(def hydra_python_syntax_sum_op-variants (list :add :sub))

(defrecord hydra_python_syntax_term [lhs rhs])
(defn make-hydra_python_syntax_term [lhs rhs] (->hydra_python_syntax_term lhs rhs))

(defrecord hydra_python_syntax_term_lhs [operand operator])
(defn make-hydra_python_syntax_term_lhs [operand operator] (->hydra_python_syntax_term_lhs operand operator))

(def hydra_python_syntax_term_op-variants (list :mul :div :floordiv :mod :matmul))

(def hydra_python_syntax_factor-variants (list :positive :negative :complement :simple))

(defrecord hydra_python_syntax_power [lhs rhs])
(defn make-hydra_python_syntax_power [lhs rhs] (->hydra_python_syntax_power lhs rhs))

(defrecord hydra_python_syntax_await_primary [await primary])
(defn make-hydra_python_syntax_await_primary [await primary] (->hydra_python_syntax_await_primary await primary))

(def hydra_python_syntax_primary-variants (list :simple :compound))

(defrecord hydra_python_syntax_primary_with_rhs [primary rhs])
(defn make-hydra_python_syntax_primary_with_rhs [primary rhs] (->hydra_python_syntax_primary_with_rhs primary rhs))

(def hydra_python_syntax_primary_rhs-variants (list :project :genexp :call :slices))

(defrecord hydra_python_syntax_slices [head tail])
(defn make-hydra_python_syntax_slices [head tail] (->hydra_python_syntax_slices head tail))

(def hydra_python_syntax_slice_or_starred_expression-variants (list :slice :starred))

(def hydra_python_syntax_slice-variants (list :named :slice_))

(defrecord hydra_python_syntax_slice_expression [start stop step])
(defn make-hydra_python_syntax_slice_expression [start stop step] (->hydra_python_syntax_slice_expression start stop step))

(def hydra_python_syntax_atom-variants (list :name :true :false :none :string :number :tuple :group :genexp :list :listcomp :dict :set :dictcomp :setcomp :ellipsis))

(def hydra_python_syntax_group-variants (list :yield :expression))

(defrecord hydra_python_syntax_lambda [params body])
(defn make-hydra_python_syntax_lambda [params body] (->hydra_python_syntax_lambda params body))

(defrecord hydra_python_syntax_lambda_parameters [slash_no_default param_no_default param_with_default star_etc])
(defn make-hydra_python_syntax_lambda_parameters [slash_no_default param_no_default param_with_default star_etc] (->hydra_python_syntax_lambda_parameters slash_no_default param_no_default param_with_default star_etc))

(defrecord hydra_python_syntax_lambda_slash_no_default [parameters])
(defn make-hydra_python_syntax_lambda_slash_no_default [parameters] (->hydra_python_syntax_lambda_slash_no_default parameters))

(defrecord hydra_python_syntax_lambda_slash_with_default [param_no_default param_with_default])
(defn make-hydra_python_syntax_lambda_slash_with_default [param_no_default param_with_default] (->hydra_python_syntax_lambda_slash_with_default param_no_default param_with_default))

(def hydra_python_syntax_lambda_star_etc-variants (list :star :param_no_default :param_maybe_default :kwds))

(defrecord hydra_python_syntax_lambda_kwds [value])
(defn make-hydra_python_syntax_lambda_kwds [value] (->hydra_python_syntax_lambda_kwds value))

(defrecord hydra_python_syntax_lambda_param_no_default [value])
(defn make-hydra_python_syntax_lambda_param_no_default [value] (->hydra_python_syntax_lambda_param_no_default value))

(defrecord hydra_python_syntax_lambda_param_with_default [param default])
(defn make-hydra_python_syntax_lambda_param_with_default [param default] (->hydra_python_syntax_lambda_param_with_default param default))

(defrecord hydra_python_syntax_lambda_param_maybe_default [param default])
(defn make-hydra_python_syntax_lambda_param_maybe_default [param default] (->hydra_python_syntax_lambda_param_maybe_default param default))

(defrecord hydra_python_syntax_list [value])
(defn make-hydra_python_syntax_list [value] (->hydra_python_syntax_list value))

(defrecord hydra_python_syntax_tuple [value])
(defn make-hydra_python_syntax_tuple [value] (->hydra_python_syntax_tuple value))

(defrecord hydra_python_syntax_set [value])
(defn make-hydra_python_syntax_set [value] (->hydra_python_syntax_set value))

(defrecord hydra_python_syntax_dict [value])
(defn make-hydra_python_syntax_dict [value] (->hydra_python_syntax_dict value))

(def hydra_python_syntax_double_starred_kvpair-variants (list :starred :pair))

(defrecord hydra_python_syntax_kvpair [key value])
(defn make-hydra_python_syntax_kvpair [key value] (->hydra_python_syntax_kvpair key value))

(defrecord hydra_python_syntax_for_if_clauses [value])
(defn make-hydra_python_syntax_for_if_clauses [value] (->hydra_python_syntax_for_if_clauses value))

(defrecord hydra_python_syntax_for_if_clause [async targets in ifs])
(defn make-hydra_python_syntax_for_if_clause [async targets in ifs] (->hydra_python_syntax_for_if_clause async targets in ifs))

(defrecord hydra_python_syntax_listcomp [expression for_if_clauses])
(defn make-hydra_python_syntax_listcomp [expression for_if_clauses] (->hydra_python_syntax_listcomp expression for_if_clauses))

(defrecord hydra_python_syntax_setcomp [expression for_if_clauses])
(defn make-hydra_python_syntax_setcomp [expression for_if_clauses] (->hydra_python_syntax_setcomp expression for_if_clauses))

(defrecord hydra_python_syntax_genexp [head tail])
(defn make-hydra_python_syntax_genexp [head tail] (->hydra_python_syntax_genexp head tail))

(def hydra_python_syntax_genexp_head-variants (list :assignment :expression))

(defrecord hydra_python_syntax_dictcomp [kvpair for_if_clauses])
(defn make-hydra_python_syntax_dictcomp [kvpair for_if_clauses] (->hydra_python_syntax_dictcomp kvpair for_if_clauses))

(defrecord hydra_python_syntax_args [positional kwarg_or_starred kwarg_or_double_starred])
(defn make-hydra_python_syntax_args [positional kwarg_or_starred kwarg_or_double_starred] (->hydra_python_syntax_args positional kwarg_or_starred kwarg_or_double_starred))

(def hydra_python_syntax_pos_arg-variants (list :starred :assignment :expression))

(defrecord hydra_python_syntax_starred_expression [value])
(defn make-hydra_python_syntax_starred_expression [value] (->hydra_python_syntax_starred_expression value))

(def hydra_python_syntax_kwarg_or_starred-variants (list :kwarg :starred))

(defrecord hydra_python_syntax_kwarg [name value])
(defn make-hydra_python_syntax_kwarg [name value] (->hydra_python_syntax_kwarg name value))

(def hydra_python_syntax_kwarg_or_double_starred-variants (list :kwarg :double_starred))

(defrecord hydra_python_syntax_star_targets_list_seq [value])
(defn make-hydra_python_syntax_star_targets_list_seq [value] (->hydra_python_syntax_star_targets_list_seq value))

(defrecord hydra_python_syntax_star_targets_tuple_seq [value])
(defn make-hydra_python_syntax_star_targets_tuple_seq [value] (->hydra_python_syntax_star_targets_tuple_seq value))

(def hydra_python_syntax_star_target-variants (list :starred :unstarred))

(def hydra_python_syntax_target_with_star_atom-variants (list :project :slices :atom))

(defrecord hydra_python_syntax_t_primary_and_name [primary name])
(defn make-hydra_python_syntax_t_primary_and_name [primary name] (->hydra_python_syntax_t_primary_and_name primary name))

(defrecord hydra_python_syntax_t_primary_and_slices [primary slices])
(defn make-hydra_python_syntax_t_primary_and_slices [primary slices] (->hydra_python_syntax_t_primary_and_slices primary slices))

(def hydra_python_syntax_star_atom-variants (list :name :target_with_star_atom :star_targets_tuple_seq :star_targets_list_seq))

(def hydra_python_syntax_single_target-variants (list :subscript_attribute_target :name :parens))

(def hydra_python_syntax_single_subscript_attribute_target-variants (list :primary_and_name :primary_and_slices))

(def hydra_python_syntax_t_primary-variants (list :primary_and_name :primary_and_slices :primary_and_genexp :primary_and_arguments :atom))

(defrecord hydra_python_syntax_t_primary_and_genexp [primary genexp])
(defn make-hydra_python_syntax_t_primary_and_genexp [primary genexp] (->hydra_python_syntax_t_primary_and_genexp primary genexp))

(defrecord hydra_python_syntax_t_primary_and_arguments [primary arguments])
(defn make-hydra_python_syntax_t_primary_and_arguments [primary arguments] (->hydra_python_syntax_t_primary_and_arguments primary arguments))

(defrecord hydra_python_syntax_del_targets [value])
(defn make-hydra_python_syntax_del_targets [value] (->hydra_python_syntax_del_targets value))

(def hydra_python_syntax_del_target-variants (list :primary_and_name :primary_and_slices :del_t_atom))

(def hydra_python_syntax_del_t_atom-variants (list :name :target :targets))

(def hydra_python_syntax_type_expression-variants (list :expression :starred_expression :double_starred_expression))

(defrecord hydra_python_syntax_func_type_comment [value])
(defn make-hydra_python_syntax_func_type_comment [value] (->hydra_python_syntax_func_type_comment value))

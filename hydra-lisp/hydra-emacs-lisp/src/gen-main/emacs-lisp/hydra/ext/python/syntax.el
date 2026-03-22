(require 'cl-lib)

(cl-defstruct hydra_ext_python_syntax_annotated_statement comment statement)

(cl-defstruct hydra_ext_python_syntax_module value)

(defvar hydra_ext_python_syntax_quote_style-variants (list :single :double :triple))

(cl-defstruct hydra_ext_python_syntax_name value)

(defvar hydra_ext_python_syntax_number-variants (list :integer :float))

(cl-defstruct hydra_ext_python_syntax_string value quote_style)

(cl-defstruct hydra_ext_python_syntax_type_comment value)

(cl-defstruct hydra_ext_python_syntax_file value)

(cl-defstruct hydra_ext_python_syntax_interactive value)

(cl-defstruct hydra_ext_python_syntax_eval value)

(cl-defstruct hydra_ext_python_syntax_func_type type body)

(defvar hydra_ext_python_syntax_statement-variants (list :compound :simple :annotated))

(defvar hydra_ext_python_syntax_simple_statement-variants (list :assignment :type_alias :star_expressions :return :import :raise :pass :del :yield :assert :break :continue :global :nonlocal))

(defvar hydra_ext_python_syntax_compound_statement-variants (list :function :if :class_def :with :for :try :while :match))

(defvar hydra_ext_python_syntax_assignment-variants (list :typed :untyped :aug))

(cl-defstruct hydra_ext_python_syntax_typed_assignment lhs type rhs)

(cl-defstruct hydra_ext_python_syntax_untyped_assignment targets rhs type_comment)

(cl-defstruct hydra_ext_python_syntax_aug_assignment lhs augassign rhs)

(defvar hydra_ext_python_syntax_annotated_rhs-variants (list :yield :star))

(defvar hydra_ext_python_syntax_aug_assign-variants (list :plus_equal :minus_equal :times_equal :at_equal :slash_equal :percent_equal :ampersand_equal :bar_equal :caret_equal :left_shift_equal :right_shift_equal :star_star_equal :double_slash_equal))

(cl-defstruct hydra_ext_python_syntax_return_statement value)

(cl-defstruct hydra_ext_python_syntax_raise_statement value)

(cl-defstruct hydra_ext_python_syntax_raise_expression expression from)

(cl-defstruct hydra_ext_python_syntax_del_statement value)

(cl-defstruct hydra_ext_python_syntax_yield_statement value)

(cl-defstruct hydra_ext_python_syntax_assert_statement expression1 expression2)

(defvar hydra_ext_python_syntax_import_statement-variants (list :name :from))

(cl-defstruct hydra_ext_python_syntax_import_name value)

(cl-defstruct hydra_ext_python_syntax_import_from prefixes dotted_name targets)

(defvar hydra_ext_python_syntax_relative_import_prefix-variants (list :dot :ellipsis))

(defvar hydra_ext_python_syntax_import_from_targets-variants (list :simple :parens :star))

(cl-defstruct hydra_ext_python_syntax_import_from_as_name name as)

(cl-defstruct hydra_ext_python_syntax_dotted_as_name name as)

(cl-defstruct hydra_ext_python_syntax_dotted_name value)

(defvar hydra_ext_python_syntax_block-variants (list :indented :simple))

(cl-defstruct hydra_ext_python_syntax_decorators value)

(cl-defstruct hydra_ext_python_syntax_class_definition decorators name type_params arguments body)

(cl-defstruct hydra_ext_python_syntax_function_definition decorators raw)

(cl-defstruct hydra_ext_python_syntax_function_def_raw async name type_params params return_type func_type_comment block)

(defvar hydra_ext_python_syntax_parameters-variants (list :slash_no_default :slash_with_default :param_no_default :param_with_default :star_etc))

(cl-defstruct hydra_ext_python_syntax_slash_no_default_parameters slash param_no_default param_with_default star_etc)

(cl-defstruct hydra_ext_python_syntax_slash_with_default_parameters param_no_default param_with_default star_etc)

(cl-defstruct hydra_ext_python_syntax_param_no_default_parameters param_no_default param_with_default star_etc)

(cl-defstruct hydra_ext_python_syntax_param_with_default_parameters param_with_default star_etc)

(cl-defstruct hydra_ext_python_syntax_slash_no_default value)

(cl-defstruct hydra_ext_python_syntax_slash_with_default param_no_default param_with_default)

(defvar hydra_ext_python_syntax_star_etc-variants (list :star_no_default :star_no_default_star_annotation :star_comma :keywords))

(cl-defstruct hydra_ext_python_syntax_no_default_star_etc param_no_default param_maybe_default keywords)

(cl-defstruct hydra_ext_python_syntax_no_default_star_annotation_star_etc param_no_default_star_annotation param_maybe_default keywords)

(cl-defstruct hydra_ext_python_syntax_comma_star_etc param_maybe_default keywords)

(cl-defstruct hydra_ext_python_syntax_keywords value)

(cl-defstruct hydra_ext_python_syntax_param_no_default param type_comment)

(cl-defstruct hydra_ext_python_syntax_param_no_default_star_annotation param_star_annotation type_comment)

(cl-defstruct hydra_ext_python_syntax_param_with_default param default type_comment)

(cl-defstruct hydra_ext_python_syntax_param_maybe_default param default type_comment)

(cl-defstruct hydra_ext_python_syntax_param name annotation)

(cl-defstruct hydra_ext_python_syntax_param_star_annotation name annotation)

(cl-defstruct hydra_ext_python_syntax_annotation value)

(cl-defstruct hydra_ext_python_syntax_star_annotation value)

(cl-defstruct hydra_ext_python_syntax_default value)

(cl-defstruct hydra_ext_python_syntax_if_statement condition body continuation)

(defvar hydra_ext_python_syntax_if_tail-variants (list :elif :else))

(cl-defstruct hydra_ext_python_syntax_while_statement condition body else)

(cl-defstruct hydra_ext_python_syntax_for_statement async targets expressions type_comment body else)

(cl-defstruct hydra_ext_python_syntax_with_statement async items type_comment body)

(cl-defstruct hydra_ext_python_syntax_with_item expression as)

(defvar hydra_ext_python_syntax_try_statement-variants (list :finally :except :except_star))

(cl-defstruct hydra_ext_python_syntax_try_finally_statement body finally)

(cl-defstruct hydra_ext_python_syntax_try_except_statement body excepts else finally)

(cl-defstruct hydra_ext_python_syntax_try_except_star_statement body excepts else finally)

(cl-defstruct hydra_ext_python_syntax_except_block expression body)

(cl-defstruct hydra_ext_python_syntax_except_expression expression as)

(cl-defstruct hydra_ext_python_syntax_except_star_block expression as body)

(cl-defstruct hydra_ext_python_syntax_match_statement subject cases)

(defvar hydra_ext_python_syntax_subject_expression-variants (list :tuple :simple))

(cl-defstruct hydra_ext_python_syntax_case_block patterns guard body)

(cl-defstruct hydra_ext_python_syntax_guard value)

(defvar hydra_ext_python_syntax_patterns-variants (list :sequence :pattern))

(defvar hydra_ext_python_syntax_pattern-variants (list :as :or))

(cl-defstruct hydra_ext_python_syntax_as_pattern pattern as)

(cl-defstruct hydra_ext_python_syntax_or_pattern value)

(defvar hydra_ext_python_syntax_closed_pattern-variants (list :literal :capture :wildcard :value :group :sequence :mapping :class))

(defvar hydra_ext_python_syntax_literal_expression-variants (list :number :complex :string :none :true :false))

(cl-defstruct hydra_ext_python_syntax_complex_number real plus_or_minus imaginary)

(defvar hydra_ext_python_syntax_plus_or_minus-variants (list :plus :minus))

(defvar hydra_ext_python_syntax_signed_number-variants (list :sign :number))

(defvar hydra_ext_python_syntax_signed_real_number-variants (list :sign :number))

(cl-defstruct hydra_ext_python_syntax_real_number value)

(cl-defstruct hydra_ext_python_syntax_imaginary_number value)

(cl-defstruct hydra_ext_python_syntax_capture_pattern value)

(cl-defstruct hydra_ext_python_syntax_pattern_capture_target value)

(cl-defstruct hydra_ext_python_syntax_value_pattern value)

(cl-defstruct hydra_ext_python_syntax_attribute value)

(cl-defstruct hydra_ext_python_syntax_name_or_attribute value)

(cl-defstruct hydra_ext_python_syntax_group_pattern value)

(defvar hydra_ext_python_syntax_sequence_pattern-variants (list :list :tuple))

(cl-defstruct hydra_ext_python_syntax_open_sequence_pattern head tail)

(cl-defstruct hydra_ext_python_syntax_maybe_sequence_pattern value)

(defvar hydra_ext_python_syntax_maybe_star_pattern-variants (list :star :pattern))

(defvar hydra_ext_python_syntax_star_pattern-variants (list :capture :wildcard))

(cl-defstruct hydra_ext_python_syntax_mapping_pattern items double_star)

(cl-defstruct hydra_ext_python_syntax_items_pattern value)

(cl-defstruct hydra_ext_python_syntax_key_value_pattern key value)

(defvar hydra_ext_python_syntax_literal_expression_or_attribute-variants (list :literal :attribute))

(cl-defstruct hydra_ext_python_syntax_double_star_pattern value)

(cl-defstruct hydra_ext_python_syntax_class_pattern name_or_attribute positional_patterns keyword_patterns)

(cl-defstruct hydra_ext_python_syntax_positional_patterns value)

(cl-defstruct hydra_ext_python_syntax_keyword_patterns value)

(cl-defstruct hydra_ext_python_syntax_keyword_pattern name pattern)

(cl-defstruct hydra_ext_python_syntax_type_alias name type_params expression)

(defvar hydra_ext_python_syntax_type_parameter-variants (list :simple :star :double_star))

(cl-defstruct hydra_ext_python_syntax_simple_type_parameter name bound default)

(cl-defstruct hydra_ext_python_syntax_star_type_parameter name default)

(cl-defstruct hydra_ext_python_syntax_double_star_type_parameter name default)

(defvar hydra_ext_python_syntax_expression-variants (list :conditional :simple :lambda))

(cl-defstruct hydra_ext_python_syntax_conditional body if else)

(defvar hydra_ext_python_syntax_yield_expression-variants (list :from :simple))

(defvar hydra_ext_python_syntax_star_expression-variants (list :star :simple))

(cl-defstruct hydra_ext_python_syntax_star_named_expressions value)

(defvar hydra_ext_python_syntax_star_named_expression-variants (list :star :simple))

(cl-defstruct hydra_ext_python_syntax_assignment_expression name expression)

(defvar hydra_ext_python_syntax_named_expression-variants (list :assignment :simple))

(cl-defstruct hydra_ext_python_syntax_disjunction value)

(cl-defstruct hydra_ext_python_syntax_conjunction value)

(defvar hydra_ext_python_syntax_inversion-variants (list :not :simple))

(cl-defstruct hydra_ext_python_syntax_comparison lhs rhs)

(cl-defstruct hydra_ext_python_syntax_compare_op_bitwise_or_pair operator rhs)

(defvar hydra_ext_python_syntax_compare_op-variants (list :eq :noteq :lte :lt :gte :gt :notin :in :isnot :is))

(cl-defstruct hydra_ext_python_syntax_bitwise_or lhs rhs)

(cl-defstruct hydra_ext_python_syntax_bitwise_xor lhs rhs)

(cl-defstruct hydra_ext_python_syntax_bitwise_and lhs rhs)

(cl-defstruct hydra_ext_python_syntax_shift_expression lhs rhs)

(cl-defstruct hydra_ext_python_syntax_shift_lhs operand operator)

(defvar hydra_ext_python_syntax_shift_op-variants (list :left :right))

(cl-defstruct hydra_ext_python_syntax_sum lhs rhs)

(cl-defstruct hydra_ext_python_syntax_sum_lhs operand operator)

(defvar hydra_ext_python_syntax_sum_op-variants (list :add :sub))

(cl-defstruct hydra_ext_python_syntax_term lhs rhs)

(cl-defstruct hydra_ext_python_syntax_term_lhs operand operator)

(defvar hydra_ext_python_syntax_term_op-variants (list :mul :div :floordiv :mod :matmul))

(defvar hydra_ext_python_syntax_factor-variants (list :positive :negative :complement :simple))

(cl-defstruct hydra_ext_python_syntax_power lhs rhs)

(cl-defstruct hydra_ext_python_syntax_await_primary await primary)

(defvar hydra_ext_python_syntax_primary-variants (list :simple :compound))

(cl-defstruct hydra_ext_python_syntax_primary_with_rhs primary rhs)

(defvar hydra_ext_python_syntax_primary_rhs-variants (list :project :genexp :call :slices))

(cl-defstruct hydra_ext_python_syntax_slices head tail)

(defvar hydra_ext_python_syntax_slice_or_starred_expression-variants (list :slice :starred))

(defvar hydra_ext_python_syntax_slice-variants (list :named :slice_))

(cl-defstruct hydra_ext_python_syntax_slice_expression start stop step)

(defvar hydra_ext_python_syntax_atom-variants (list :name :true :false :none :string :number :tuple :group :genexp :list :listcomp :dict :set :dictcomp :setcomp :ellipsis))

(defvar hydra_ext_python_syntax_group-variants (list :yield :expression))

(cl-defstruct hydra_ext_python_syntax_lambda params body)

(cl-defstruct hydra_ext_python_syntax_lambda_parameters slash_no_default param_no_default param_with_default star_etc)

(cl-defstruct hydra_ext_python_syntax_lambda_slash_no_default parameters)

(cl-defstruct hydra_ext_python_syntax_lambda_slash_with_default param_no_default param_with_default)

(defvar hydra_ext_python_syntax_lambda_star_etc-variants (list :star :param_no_default :param_maybe_default :kwds))

(cl-defstruct hydra_ext_python_syntax_lambda_kwds value)

(cl-defstruct hydra_ext_python_syntax_lambda_param_no_default value)

(cl-defstruct hydra_ext_python_syntax_lambda_param_with_default param default)

(cl-defstruct hydra_ext_python_syntax_lambda_param_maybe_default param default)

(cl-defstruct hydra_ext_python_syntax_list value)

(cl-defstruct hydra_ext_python_syntax_tuple value)

(cl-defstruct hydra_ext_python_syntax_set value)

(cl-defstruct hydra_ext_python_syntax_dict value)

(defvar hydra_ext_python_syntax_double_starred_kvpair-variants (list :starred :pair))

(cl-defstruct hydra_ext_python_syntax_kvpair key value)

(cl-defstruct hydra_ext_python_syntax_for_if_clauses value)

(cl-defstruct hydra_ext_python_syntax_for_if_clause async targets in ifs)

(cl-defstruct hydra_ext_python_syntax_listcomp expression for_if_clauses)

(cl-defstruct hydra_ext_python_syntax_setcomp expression for_if_clauses)

(cl-defstruct hydra_ext_python_syntax_genexp head tail)

(defvar hydra_ext_python_syntax_genexp_head-variants (list :assignment :expression))

(cl-defstruct hydra_ext_python_syntax_dictcomp kvpair for_if_clauses)

(cl-defstruct hydra_ext_python_syntax_args positional kwarg_or_starred kwarg_or_double_starred)

(defvar hydra_ext_python_syntax_pos_arg-variants (list :starred :assignment :expression))

(cl-defstruct hydra_ext_python_syntax_starred_expression value)

(defvar hydra_ext_python_syntax_kwarg_or_starred-variants (list :kwarg :starred))

(cl-defstruct hydra_ext_python_syntax_kwarg name value)

(defvar hydra_ext_python_syntax_kwarg_or_double_starred-variants (list :kwarg :double_starred))

(cl-defstruct hydra_ext_python_syntax_star_targets_list_seq value)

(cl-defstruct hydra_ext_python_syntax_star_targets_tuple_seq value)

(defvar hydra_ext_python_syntax_star_target-variants (list :starred :unstarred))

(defvar hydra_ext_python_syntax_target_with_star_atom-variants (list :project :slices :atom))

(cl-defstruct hydra_ext_python_syntax_t_primary_and_name primary name)

(cl-defstruct hydra_ext_python_syntax_t_primary_and_slices primary slices)

(defvar hydra_ext_python_syntax_star_atom-variants (list :name :target_with_star_atom :star_targets_tuple_seq :star_targets_list_seq))

(defvar hydra_ext_python_syntax_single_target-variants (list :subscript_attribute_target :name :parens))

(defvar hydra_ext_python_syntax_single_subscript_attribute_target-variants (list :primary_and_name :primary_and_slices))

(defvar hydra_ext_python_syntax_t_primary-variants (list :primary_and_name :primary_and_slices :primary_and_genexp :primary_and_arguments :atom))

(cl-defstruct hydra_ext_python_syntax_t_primary_and_genexp primary genexp)

(cl-defstruct hydra_ext_python_syntax_t_primary_and_arguments primary arguments)

(cl-defstruct hydra_ext_python_syntax_del_targets value)

(defvar hydra_ext_python_syntax_del_target-variants (list :primary_and_name :primary_and_slices :del_t_atom))

(defvar hydra_ext_python_syntax_del_t_atom-variants (list :name :target :targets))

(defvar hydra_ext_python_syntax_type_expression-variants (list :expression :starred_expression :double_starred_expression))

(cl-defstruct hydra_ext_python_syntax_func_type_comment value)

(provide 'hydra.ext.python.syntax)

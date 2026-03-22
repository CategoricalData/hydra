(require 'cl-lib)

(require 'hydra.ast)

(require 'hydra.coders)

(require 'hydra.core)

(require 'hydra.error.core)

(require 'hydra.graph)

(require 'hydra.json.model)

(require 'hydra.module)

(require 'hydra.parsing)

(require 'hydra.typing)

(require 'hydra.util)

(cl-defstruct hydra_testing_alpha_conversion_test_case term old_variable new_variable result)

(defvar hydra_testing_evaluation_style-variants (list :eager :lazy))

(cl-defstruct hydra_testing_case_conversion_test_case from_convention to_convention from_string to_string)

(cl-defstruct hydra_testing_delegated_evaluation_test_case input output)

(cl-defstruct hydra_testing_eta_expansion_test_case input output)

(cl-defstruct hydra_testing_deannotate_term_test_case input output)

(cl-defstruct hydra_testing_deannotate_type_test_case input output)

(cl-defstruct hydra_testing_flatten_let_terms_test_case input output)

(defvar hydra_testing_fold_operation-variants (list :sum_int32_literals :collect_list_lengths :collect_labels))

(cl-defstruct hydra_testing_fold_over_term_test_case input traversal_order operation output)

(cl-defstruct hydra_testing_free_variables_test_case input output)

(defvar hydra_testing_hoist_predicate-variants (list :case_statements :applications :lists :nothing))

(cl-defstruct hydra_testing_hoist_let_bindings_test_case input output)

(cl-defstruct hydra_testing_hoist_polymorphic_let_bindings_test_case input output)

(cl-defstruct hydra_testing_hoist_subterms_test_case predicate input output)

(cl-defstruct hydra_testing_hoist_case_statements_test_case input output)

(defvar hydra_testing_term_rewriter-variants (list :replace_foo_with_bar :replace_int32_with_int64))

(cl-defstruct hydra_testing_rewrite_term_test_case input rewriter output)

(defvar hydra_testing_type_rewriter-variants (list :replace_string_with_int32))

(cl-defstruct hydra_testing_rewrite_type_test_case input rewriter output)

(cl-defstruct hydra_testing_evaluation_test_case evaluation_style input output)

(cl-defstruct hydra_testing_inference_failure_test_case input)

(cl-defstruct hydra_testing_inference_test_case input output)

(cl-defstruct hydra_testing_json_decode_test_case type json expected)

(cl-defstruct hydra_testing_json_encode_test_case term expected)

(cl-defstruct hydra_testing_json_roundtrip_test_case type term)

(cl-defstruct hydra_testing_lift_lambda_above_let_test_case input output)

(cl-defstruct hydra_testing_parser_test_case input output)

(cl-defstruct hydra_testing_tag value)

(cl-defstruct hydra_testing_test_codec language file_extension encode_term encode_type format_test_name format_module_name test_case_template test_group_template module_template import_template find_imports)

(cl-defstruct hydra_testing_test_generator namespaces_for_module create_codec generate_test_file aggregator_file)

(defvar hydra_testing_test_case-variants (list :alpha_conversion :case_conversion :deannotate_term :deannotate_type :delegated_evaluation :eta_expansion :flatten_let_terms :free_variables :evaluation :inference :inference_failure :json_decode :json_encode :json_parser :json_roundtrip :json_writer :lift_lambda_above_let :serialization :simplify_term :topological_sort :topological_sort_bindings :topological_sort_s_c_c :type_checking :type_checking_failure :type_reduction :normalize_type_variables :fold_over_term :rewrite_term :rewrite_type :hoist_subterms :hoist_case_statements :hoist_let_bindings :hoist_polymorphic_let_bindings :subst_in_type :variable_occurs_in_type :unify_types :join_types :unshadow_variables :validate_core_term))

(cl-defstruct hydra_testing_test_case_with_metadata name case description tags)

(cl-defstruct hydra_testing_test_group name description subgroups cases)

(cl-defstruct hydra_testing_type_checking_test_case input output_term output_type)

(cl-defstruct hydra_testing_type_checking_failure_test_case input)

(cl-defstruct hydra_testing_topological_sort_bindings_test_case bindings expected)

(cl-defstruct hydra_testing_topological_sort_test_case adjacency_list expected)

(cl-defstruct hydra_testing_topological_sort_s_c_c_test_case adjacency_list expected)

(cl-defstruct hydra_testing_serialization_test_case input output)

(cl-defstruct hydra_testing_simplify_term_test_case input output)

(cl-defstruct hydra_testing_normalize_type_variables_test_case input output)

(cl-defstruct hydra_testing_type_reduction_test_case input output)

(cl-defstruct hydra_testing_writer_test_case input output)

(cl-defstruct hydra_testing_subst_in_type_test_case substitution input output)

(cl-defstruct hydra_testing_variable_occurs_in_type_test_case variable type expected)

(cl-defstruct hydra_testing_unshadow_variables_test_case input output)

(cl-defstruct hydra_testing_unify_types_test_case schema_types left right expected)

(cl-defstruct hydra_testing_join_types_test_case left right expected)

(cl-defstruct hydra_testing_validate_core_term_test_case input output)

(provide 'hydra.testing)

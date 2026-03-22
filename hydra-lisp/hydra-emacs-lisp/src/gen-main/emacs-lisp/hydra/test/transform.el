(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.testing)

(require 'hydra.util)

(defvar hydra_test_transform_add_generation_prefix (lambda (ns_) (funcall (hydra_lib_strings_cat2 "generation.") (funcall (lambda (v) v) ns_))))

(defvar hydra_test_transform_encode_case_convention (lambda (conv) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :lower_snake) (funcall (lambda (_) "lowerSnake") match_value)) ((equal (car match_target) :upper_snake) (funcall (lambda (_) "upperSnake") match_value)) ((equal (car match_target) :camel) (funcall (lambda (_) "camel") match_value)) ((equal (car match_target) :pascal) (funcall (lambda (_) "pascal") match_value)))) (cadr match_target))) conv) (list :unit nil))))))

(defvar hydra_test_transform_build_convert_case_call (lambda (from_conv) (lambda (to_conv) (lambda (input_) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.formatting.convertCase") (hydra_test_transform_encode_case_convention from_conv))) (hydra_test_transform_encode_case_convention to_conv))) (list :literal (list :string input_))))))))

(defvar hydra_test_transform_encode_int (lambda (n) (list :literal (list :integer (list :int32 n)))))

(defvar hydra_test_transform_encode_adjacency_list (lambda (pairs) (list :list (funcall (hydra_lib_lists_map (lambda (p) (list :pair (list (hydra_test_transform_encode_int (hydra_lib_pairs_first p)) (list :list (funcall (hydra_lib_lists_map (lambda (d) (hydra_test_transform_encode_int d))) (hydra_lib_pairs_second p))))))) pairs))))

(defvar hydra_test_transform_build_topological_sort_call (lambda (adj_list) (list :application (make-hydra_core_application (list :variable "hydra.sorting.topologicalSort") (hydra_test_transform_encode_adjacency_list adj_list)))))

(defvar hydra_test_transform_build_topological_sort_s_c_c_call (lambda (adj_list) (list :application (make-hydra_core_application (list :variable "hydra.sorting.topologicalSortComponents") (hydra_test_transform_encode_adjacency_list adj_list)))))

(defvar hydra_test_transform_collect_test_cases (lambda (tg) (funcall (hydra_lib_lists_concat2 (funcall (lambda (v) (hydra_testing_test_group-cases v)) tg)) (hydra_lib_lists_concat (funcall (hydra_lib_lists_map (lambda (sg) (hydra_test_transform_collect_test_cases sg))) (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) tg))))))

(defvar hydra_test_transform_encode_int_list (lambda (ints) (list :list (funcall (hydra_lib_lists_map (lambda (n) (hydra_test_transform_encode_int n))) ints))))

(defvar hydra_test_transform_encode_list_list (lambda (lists) (list :list (funcall (hydra_lib_lists_map (lambda (l) (hydra_test_transform_encode_int_list l))) lists))))

(defvar hydra_test_transform_encode_either_list_list (lambda (e) (list :either (funcall (funcall (hydra_lib_eithers_bimap (lambda (cycles) (hydra_test_transform_encode_list_list cycles))) (lambda (sorted) (hydra_test_transform_encode_int_list sorted))) e))))

(defvar hydra_test_transform_transform_module (lambda (m) (make-hydra_module_module (hydra_test_transform_add_generation_prefix (funcall (lambda (v) (hydra_module_module-namespace v)) m)) (funcall (lambda (v) (hydra_module_module-elements v)) m) (funcall (lambda (v) (hydra_module_module-term_dependencies v)) m) (funcall (lambda (v) (hydra_module_module-type_dependencies v)) m) (funcall (lambda (v) (hydra_module_module-description v)) m))))

(defvar hydra_test_transform_transform_test_case (lambda (tcm) (let* ((desc (funcall (lambda (v) (hydra_testing_test_case_with_metadata-description v)) tcm)) (name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tags_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-tags v)) tcm)) (tc (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :case_conversion) (funcall (lambda (ccase) (let* ((from_conv (funcall (lambda (v) (hydra_testing_case_conversion_test_case-from_convention v)) ccase)) (from_str (funcall (lambda (v) (hydra_testing_case_conversion_test_case-from_string v)) ccase)) (to_conv (funcall (lambda (v) (hydra_testing_case_conversion_test_case-to_convention v)) ccase)) (to_str (funcall (lambda (v) (hydra_testing_case_conversion_test_case-to_string v)) ccase))) (list :just (make-hydra_testing_test_case_with_metadata name_ (list :delegated_evaluation (make-hydra_testing_delegated_evaluation_test_case (funcall (funcall (hydra_test_transform_build_convert_case_call from_conv) to_conv) from_str) (list :literal (list :string to_str)))) desc tags_)))) match_value)) ((equal (car match_target) :evaluation) (funcall (lambda (ecase_) (let* ((input_ (funcall (lambda (v) (hydra_testing_evaluation_test_case-input v)) ecase_)) (output_ (funcall (lambda (v) (hydra_testing_evaluation_test_case-output v)) ecase_))) (list :just (make-hydra_testing_test_case_with_metadata name_ (list :delegated_evaluation (make-hydra_testing_delegated_evaluation_test_case input_ output_)) desc tags_)))) match_value)) ((equal (car match_target) :delegated_evaluation) (funcall (lambda (_) (list :just tcm)) match_value)) ((equal (car match_target) :topological_sort) (funcall (lambda (tscase) (let* ((adj_list (funcall (lambda (v) (hydra_testing_topological_sort_test_case-adjacency_list v)) tscase)) (expected (funcall (lambda (v) (hydra_testing_topological_sort_test_case-expected v)) tscase))) (list :just (make-hydra_testing_test_case_with_metadata name_ (list :delegated_evaluation (make-hydra_testing_delegated_evaluation_test_case (hydra_test_transform_build_topological_sort_call adj_list) (hydra_test_transform_encode_either_list_list expected))) desc tags_)))) match_value)) ((equal (car match_target) :topological_sort_s_c_c) (funcall (lambda (scccase) (let* ((adj_list (funcall (lambda (v) (hydra_testing_topological_sort_s_c_c_test_case-adjacency_list v)) scccase)) (expected (funcall (lambda (v) (hydra_testing_topological_sort_s_c_c_test_case-expected v)) scccase))) (list :just (make-hydra_testing_test_case_with_metadata name_ (list :delegated_evaluation (make-hydra_testing_delegated_evaluation_test_case (hydra_test_transform_build_topological_sort_s_c_c_call adj_list) (hydra_test_transform_encode_list_list expected))) desc tags_)))) match_value)) ((equal (car match_target) :validate_core_term) (funcall (lambda (_) (list :just tcm)) match_value)) (t (list :nothing)))) (cadr match_target))) tc))))

(defvar hydra_test_transform_transform_to_compiled_tests (lambda (tg) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) tg)) (desc (funcall (lambda (v) (hydra_testing_test_group-description v)) tg)) (name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) tg)) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) tg)) (transformed_cases (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (tc) (hydra_test_transform_transform_test_case tc))) cases_))) (transformed_subgroups (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (sg) (hydra_test_transform_transform_to_compiled_tests sg))) subgroups)))) (if (funcall (hydra_lib_logic_and (hydra_lib_lists_null transformed_cases)) (hydra_lib_lists_null transformed_subgroups)) (list :nothing) (list :just (make-hydra_testing_test_group name_ desc transformed_subgroups transformed_cases))))))

(provide 'hydra.test.transform)

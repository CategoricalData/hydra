(defpackage :hydra.test.transform
(:use :cl :hydra.core :hydra.lib.eithers :hydra.lib.lists :hydra.lib.logic :hydra.lib.maybes :hydra.lib.pairs :hydra.lib.strings :hydra.module :hydra.testing :hydra.util)
(:export :hydra_test_transform_add_generation_prefix :hydra_test_transform_encode_case_convention :hydra_test_transform_build_convert_case_call :hydra_test_transform_encode_int :hydra_test_transform_encode_adjacency_list :hydra_test_transform_build_topological_sort_call :hydra_test_transform_build_topological_sort_s_c_c_call :hydra_test_transform_collect_test_cases :hydra_test_transform_encode_int_list :hydra_test_transform_encode_list_list :hydra_test_transform_encode_either_list_list :hydra_test_transform_transform_module :hydra_test_transform_transform_test_case :hydra_test_transform_transform_to_compiled_tests))

(in-package :hydra.test.transform)

(cl:defvar hydra_test_transform_add_generation_prefix (cl:lambda (ns_) ((hydra_lib_strings_cat2 "generation.") ((cl:lambda (v) v) ns_))))

(cl:defvar hydra_test_transform_encode_case_convention (cl:lambda (conv) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :lower_snake) ((cl:lambda (_) "lowerSnake") match_value)) ((equal (car match_target) :upper_snake) ((cl:lambda (_) "upperSnake") match_value)) ((equal (car match_target) :camel) ((cl:lambda (_) "camel") match_value)) ((equal (car match_target) :pascal) ((cl:lambda (_) "pascal") match_value)))) (cadr match_target))) conv) (list :unit cl:nil))))))

(cl:defvar hydra_test_transform_build_convert_case_call (cl:lambda (from_conv) (cl:lambda (to_conv) (cl:lambda (input_) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.formatting.convertCase") (hydra_test_transform_encode_case_convention from_conv))) (hydra_test_transform_encode_case_convention to_conv))) (list :literal (list :string input_))))))))

(cl:defvar hydra_test_transform_encode_int (cl:lambda (n) (list :literal (list :integer (list :int32 n)))))

(cl:defvar hydra_test_transform_encode_adjacency_list (cl:lambda (pairs) (list :list ((hydra_lib_lists_map (cl:lambda (p) (list :pair (cl:list (hydra_test_transform_encode_int (hydra_lib_pairs_first p)) (list :list ((hydra_lib_lists_map (cl:lambda (d) (hydra_test_transform_encode_int d))) (hydra_lib_pairs_second p))))))) pairs))))

(cl:defvar hydra_test_transform_build_topological_sort_call (cl:lambda (adj_list) (list :application (make-hydra_core_application (list :variable "hydra.sorting.topologicalSort") (hydra_test_transform_encode_adjacency_list adj_list)))))

(cl:defvar hydra_test_transform_build_topological_sort_s_c_c_call (cl:lambda (adj_list) (list :application (make-hydra_core_application (list :variable "hydra.sorting.topologicalSortComponents") (hydra_test_transform_encode_adjacency_list adj_list)))))

(cl:defvar hydra_test_transform_collect_test_cases (cl:lambda (tg) ((hydra_lib_lists_concat2 ((cl:lambda (v) (hydra_testing_test_group-cases v)) tg)) (hydra_lib_lists_concat ((hydra_lib_lists_map (cl:lambda (sg) (hydra_test_transform_collect_test_cases sg))) ((cl:lambda (v) (hydra_testing_test_group-subgroups v)) tg))))))

(cl:defvar hydra_test_transform_encode_int_list (cl:lambda (ints) (list :list ((hydra_lib_lists_map (cl:lambda (n) (hydra_test_transform_encode_int n))) ints))))

(cl:defvar hydra_test_transform_encode_list_list (cl:lambda (lists) (list :list ((hydra_lib_lists_map (cl:lambda (l) (hydra_test_transform_encode_int_list l))) lists))))

(cl:defvar hydra_test_transform_encode_either_list_list (cl:lambda (e) (list :either (((hydra_lib_eithers_bimap (cl:lambda (cycles) (hydra_test_transform_encode_list_list cycles))) (cl:lambda (sorted) (hydra_test_transform_encode_int_list sorted))) e))))

(cl:defvar hydra_test_transform_transform_module (cl:lambda (m) (make-hydra_module_module (hydra_test_transform_add_generation_prefix ((cl:lambda (v) (hydra_module_module-namespace v)) m)) ((cl:lambda (v) (hydra_module_module-definitions v)) m) ((cl:lambda (v) (hydra_module_module-term_dependencies v)) m) ((cl:lambda (v) (hydra_module_module-type_dependencies v)) m) ((cl:lambda (v) (hydra_module_module-description v)) m))))

(cl:defvar hydra_test_transform_transform_test_case (cl:lambda (tcm) (list :just tcm)))

(cl:defvar hydra_test_transform_transform_to_compiled_tests (cl:lambda (tg) (let* ((cases_ ((cl:lambda (v) (hydra_testing_test_group-cases v)) tg)) (desc ((cl:lambda (v) (hydra_testing_test_group-description v)) tg)) (name_ ((cl:lambda (v) (hydra_testing_test_group-name v)) tg)) (subgroups ((cl:lambda (v) (hydra_testing_test_group-subgroups v)) tg)) (transformed_cases (hydra_lib_maybes_cat ((hydra_lib_lists_map (cl:lambda (tc) (hydra_test_transform_transform_test_case tc))) cases_))) (transformed_subgroups (hydra_lib_maybes_cat ((hydra_lib_lists_map (cl:lambda (sg) (hydra_test_transform_transform_to_compiled_tests sg))) subgroups)))) (if ((hydra_lib_logic_and (hydra_lib_lists_null transformed_cases)) (hydra_lib_lists_null transformed_subgroups)) (list :nothing) (list :just (make-hydra_testing_test_group name_ desc transformed_subgroups transformed_cases))))))

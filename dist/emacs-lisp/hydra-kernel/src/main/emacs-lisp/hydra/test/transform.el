(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.packaging)

(require 'hydra.testing)

(require 'hydra.util)

(defvar hydra_test_transform_add_generation_prefix (lambda (ns_) (funcall (hydra_lib_strings_cat2 "generation.") (funcall (lambda (v) v) ns_))))

(defvar hydra_test_transform_encode_case_convention (lambda (conv) (list :inject (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :lower_snake) (funcall (lambda (_) "lowerSnake") match_value)) ((equal (car match_target) :upper_snake) (funcall (lambda (_) "upperSnake") match_value)) ((equal (car match_target) :camel) (funcall (lambda (_) "camel") match_value)) ((equal (car match_target) :pascal) (funcall (lambda (_) "pascal") match_value)))) (cadr match_target))) conv) (list :unit nil))))))

(defvar hydra_test_transform_build_convert_case_call (lambda (from_conv) (lambda (to_conv) (lambda (input_) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.formatting.convertCase") (hydra_test_transform_encode_case_convention from_conv))) (hydra_test_transform_encode_case_convention to_conv))) (list :literal (list :string input_))))))))

(defvar hydra_test_transform_encode_int (lambda (n) (list :literal (list :integer (list :int32 n)))))

(defvar hydra_test_transform_encode_adjacency_list (lambda (pairs) (list :list (funcall (hydra_lib_lists_map (lambda (p) (list :pair (list (hydra_test_transform_encode_int (hydra_lib_pairs_first p)) (list :list (funcall (hydra_lib_lists_map (lambda (d) (hydra_test_transform_encode_int d))) (hydra_lib_pairs_second p))))))) pairs))))

(defvar hydra_test_transform_build_topological_sort_call (lambda (adj_list) (list :application (make-hydra_core_application (list :variable "hydra.sorting.topologicalSort") (hydra_test_transform_encode_adjacency_list adj_list)))))

(defvar hydra_test_transform_build_topological_sort_s_c_c_call (lambda (adj_list) (list :application (make-hydra_core_application (list :variable "hydra.sorting.topologicalSortComponents") (hydra_test_transform_encode_adjacency_list adj_list)))))

(defvar hydra_test_transform_collect_test_cases (lambda (tg) (funcall (hydra_lib_lists_concat2 (funcall (lambda (v) (hydra_testing_test_group-cases v)) tg)) (hydra_lib_lists_concat (funcall (hydra_lib_lists_map (lambda (sg) (hydra_test_transform_collect_test_cases sg))) (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) tg))))))

(defvar hydra_test_transform_encode_int_list (lambda (ints) (list :list (funcall (hydra_lib_lists_map (lambda (n) (hydra_test_transform_encode_int n))) ints))))

(defvar hydra_test_transform_encode_list_list (lambda (lists) (list :list (funcall (hydra_lib_lists_map (lambda (l) (hydra_test_transform_encode_int_list l))) lists))))

(defvar hydra_test_transform_encode_either_list_list (lambda (e) (list :either (funcall (funcall (hydra_lib_eithers_bimap (lambda (cycles) (hydra_test_transform_encode_list_list cycles))) (lambda (sorted) (hydra_test_transform_encode_int_list sorted))) e))))

(defvar hydra_test_transform_transform_module (lambda (m) (make-hydra_packaging_module (hydra_test_transform_add_generation_prefix (funcall (lambda (v) (hydra_packaging_module-namespace v)) m)) (funcall (lambda (v) (hydra_packaging_module-definitions v)) m) (funcall (lambda (v) (hydra_packaging_module-term_dependencies v)) m) (funcall (lambda (v) (hydra_packaging_module-type_dependencies v)) m) (funcall (lambda (v) (hydra_packaging_module-description v)) m))))

(defvar hydra_test_transform_transform_test_case (lambda (tcm) (list :just tcm)))

(defvar hydra_test_transform_transform_to_compiled_tests (lambda (tg) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) tg)) (desc (funcall (lambda (v) (hydra_testing_test_group-description v)) tg)) (name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) tg)) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) tg)) (transformed_cases (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (tc) (hydra_test_transform_transform_test_case tc))) cases_))) (transformed_subgroups (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (sg) (hydra_test_transform_transform_to_compiled_tests sg))) subgroups)))) (if (funcall (hydra_lib_logic_and (hydra_lib_lists_null transformed_cases)) (hydra_lib_lists_null transformed_subgroups)) (list :nothing) (list :just (make-hydra_testing_test_group name_ desc transformed_subgroups transformed_cases))))))

(provide 'hydra.test.transform)

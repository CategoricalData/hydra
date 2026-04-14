(ns hydra.test.transform
  (:require [hydra.core :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.strings :refer :all] [hydra.packaging :refer :all] [hydra.testing :refer :all] [hydra.util :refer :all]
))

(declare hydra_test_transform_add_generation_prefix hydra_test_transform_encode_case_convention hydra_test_transform_build_convert_case_call hydra_test_transform_encode_int hydra_test_transform_encode_adjacency_list hydra_test_transform_build_topological_sort_call hydra_test_transform_build_topological_sort_s_c_c_call hydra_test_transform_collect_test_cases hydra_test_transform_encode_int_list hydra_test_transform_encode_list_list hydra_test_transform_encode_either_list_list hydra_test_transform_transform_module hydra_test_transform_transform_test_case hydra_test_transform_transform_to_compiled_tests)

(def hydra_test_transform_add_generation_prefix (fn [ns_] ((hydra_lib_strings_cat2 "generation.") ((fn [v] v) ns_))))

(def hydra_test_transform_encode_case_convention (fn [conv] (list :inject (->hydra_core_injection "hydra.util.CaseConvention" (->hydra_core_field ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :lower_snake) ((fn [_] "lowerSnake") match_value) (= (first match_target) :upper_snake) ((fn [_] "upperSnake") match_value) (= (first match_target) :camel) ((fn [_] "camel") match_value) (= (first match_target) :pascal) ((fn [_] "pascal") match_value))) (second match_target))) conv) (list :unit nil))))))

(def hydra_test_transform_build_convert_case_call (fn [from_conv] (fn [to_conv] (fn [input_] (list :application (->hydra_core_application (list :application (->hydra_core_application (list :application (->hydra_core_application (list :variable "hydra.formatting.convertCase") (hydra_test_transform_encode_case_convention from_conv))) (hydra_test_transform_encode_case_convention to_conv))) (list :literal (list :string input_))))))))

(def hydra_test_transform_encode_int (fn [n] (list :literal (list :integer (list :int32 n)))))

(def hydra_test_transform_encode_adjacency_list (fn [pairs] (list :list ((hydra_lib_lists_map (fn [p] (list :pair (list (hydra_test_transform_encode_int (hydra_lib_pairs_first p)) (list :list ((hydra_lib_lists_map (fn [d] (hydra_test_transform_encode_int d))) (hydra_lib_pairs_second p))))))) pairs))))

(def hydra_test_transform_build_topological_sort_call (fn [adj_list] (list :application (->hydra_core_application (list :variable "hydra.sorting.topologicalSort") (hydra_test_transform_encode_adjacency_list adj_list)))))

(def hydra_test_transform_build_topological_sort_s_c_c_call (fn [adj_list] (list :application (->hydra_core_application (list :variable "hydra.sorting.topologicalSortComponents") (hydra_test_transform_encode_adjacency_list adj_list)))))

(def hydra_test_transform_collect_test_cases (fn [tg] ((hydra_lib_lists_concat2 ((fn [v] (:cases v)) tg)) (hydra_lib_lists_concat ((hydra_lib_lists_map (fn [sg] (hydra_test_transform_collect_test_cases sg))) ((fn [v] (:subgroups v)) tg))))))

(def hydra_test_transform_encode_int_list (fn [ints] (list :list ((hydra_lib_lists_map (fn [n] (hydra_test_transform_encode_int n))) ints))))

(def hydra_test_transform_encode_list_list (fn [lists] (list :list ((hydra_lib_lists_map (fn [l] (hydra_test_transform_encode_int_list l))) lists))))

(def hydra_test_transform_encode_either_list_list (fn [e] (list :either (((hydra_lib_eithers_bimap (fn [cycles] (hydra_test_transform_encode_list_list cycles))) (fn [sorted] (hydra_test_transform_encode_int_list sorted))) e))))

(def hydra_test_transform_transform_module (fn [m] (->hydra_packaging_module (hydra_test_transform_add_generation_prefix ((fn [v] (:namespace v)) m)) ((fn [v] (:definitions v)) m) ((fn [v] (:term_dependencies v)) m) ((fn [v] (:type_dependencies v)) m) ((fn [v] (:description v)) m))))

(def hydra_test_transform_transform_test_case (fn [tcm] (list :just tcm)))

(def hydra_test_transform_transform_to_compiled_tests (fn [tg] (let [cases_ ((fn [v] (:cases v)) tg) desc ((fn [v] (:description v)) tg) name_ ((fn [v] (:name v)) tg) subgroups ((fn [v] (:subgroups v)) tg) transformed_cases (hydra_lib_maybes_cat ((hydra_lib_lists_map (fn [tc] (hydra_test_transform_transform_test_case tc))) cases_)) transformed_subgroups (hydra_lib_maybes_cat ((hydra_lib_lists_map (fn [sg] (hydra_test_transform_transform_to_compiled_tests sg))) subgroups))] (if ((hydra_lib_logic_and (hydra_lib_lists_null transformed_cases)) (hydra_lib_lists_null transformed_subgroups)) (list :nothing) (list :just (->hydra_testing_test_group name_ desc transformed_subgroups transformed_cases))))))

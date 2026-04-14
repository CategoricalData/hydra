(ns hydra.encode.testing
  (:require [hydra.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maybes :refer :all] [hydra.testing :refer :all]
))

(declare hydra_encode_testing_tag hydra_encode_testing_universal_test_case hydra_encode_testing_test_case hydra_encode_testing_test_case_with_metadata hydra_encode_testing_test_group)

(def hydra_encode_testing_tag (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.testing.Tag" ((fn [x2] (list :literal (list :string x2))) ((fn [v] v) x))))))

(def hydra_encode_testing_universal_test_case (fn [x] (list :record (->hydra_core_record "hydra.testing.UniversalTestCase" (list (->hydra_core_field "actual" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:actual v)) x))) (->hydra_core_field "expected" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:expected v)) x))))))))

(def hydra_encode_testing_test_case (fn [match_target] ((fn [match_value] (cond (= (first match_target) :universal) ((fn [y] (list :inject (->hydra_core_injection "hydra.testing.TestCase" (->hydra_core_field "universal" (hydra_encode_testing_universal_test_case y))))) match_value))) (second match_target))))

(def hydra_encode_testing_test_case_with_metadata (fn [x] (list :record (->hydra_core_record "hydra.testing.TestCaseWithMetadata" (list (->hydra_core_field "name" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:name v)) x))) (->hydra_core_field "case" (hydra_encode_testing_test_case ((fn [v] (:case v)) x))) (->hydra_core_field "description" ((fn [opt] (list :maybe ((hydra_lib_maybes_map (fn [x2] (list :literal (list :string x2)))) opt))) ((fn [v] (:description v)) x))) (->hydra_core_field "tags" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_testing_tag) xs))) ((fn [v] (:tags v)) x))))))))

(def hydra_encode_testing_test_group (fn [x] (list :record (->hydra_core_record "hydra.testing.TestGroup" (list (->hydra_core_field "name" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:name v)) x))) (->hydra_core_field "description" ((fn [opt] (list :maybe ((hydra_lib_maybes_map (fn [x2] (list :literal (list :string x2)))) opt))) ((fn [v] (:description v)) x))) (->hydra_core_field "subgroups" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_testing_test_group) xs))) ((fn [v] (:subgroups v)) x))) (->hydra_core_field "cases" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_testing_test_case_with_metadata) xs))) ((fn [v] (:cases v)) x))))))))

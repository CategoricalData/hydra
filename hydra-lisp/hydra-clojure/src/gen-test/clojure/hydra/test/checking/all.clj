(ns hydra.test.checking.all
  (:require [hydra.test.checking.advanced :refer :all] [hydra.test.checking.algebraicTypes :refer :all] [hydra.test.checking.collections :refer :all] [hydra.test.checking.failures :refer :all] [hydra.test.checking.fundamentals :refer :all] [hydra.test.checking.nominalTypes :refer :all] [hydra.testing :refer :all]
))

(declare hydra_test_checking_all_all_tests)

(def hydra_test_checking_all_all_tests (->hydra_testing_test_group "checking" nil (list hydra_test_checking_advanced_all_tests hydra_test_checking_algebraic_types_all_tests hydra_test_checking_collections_all_tests hydra_test_checking_failures_all_tests hydra_test_checking_fundamentals_all_tests hydra_test_checking_nominal_types_all_tests) (list)))

(ns hydra.test.validate.all
  (:require [hydra.test.validate.core :refer :all] [hydra.testing :refer :all]
))

(declare hydra_test_validate_all_all_tests)

(def hydra_test_validate_all_all_tests (->hydra_testing_test_group "validation" (list :nothing) (list hydra_test_validate_core_all_tests) (list)))

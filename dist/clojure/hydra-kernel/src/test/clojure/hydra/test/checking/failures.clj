(ns hydra.test.checking.failures
  (:require [hydra.testing :refer :all]
))

(declare hydra_test_checking_failures_untyped_lambdas_tests hydra_test_checking_failures_fail_on_untyped_tests hydra_test_checking_failures_all_tests)

(def hydra_test_checking_failures_untyped_lambdas_tests (->hydra_testing_test_group "Untyped lambdas" (list :nothing) (list) (list)))

(def hydra_test_checking_failures_fail_on_untyped_tests (->hydra_testing_test_group "Fail on untyped (pre-inference) terms" (list :nothing) (list hydra_test_checking_failures_untyped_lambdas_tests) (list)))

(def hydra_test_checking_failures_all_tests (->hydra_testing_test_group "Failures" (list :nothing) (list hydra_test_checking_failures_fail_on_untyped_tests) (list)))

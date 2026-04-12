(defpackage :hydra.test.checking.failures
(:use :cl :hydra.testing)
(:export :hydra_test_checking_failures_untyped_lambdas_tests :hydra_test_checking_failures_fail_on_untyped_tests :hydra_test_checking_failures_all_tests))

(in-package :hydra.test.checking.failures)

(cl:defvar hydra_test_checking_failures_untyped_lambdas_tests (make-hydra_testing_test_group "Untyped lambdas" (list :nothing) (cl:list) (cl:list)))

(cl:defvar hydra_test_checking_failures_fail_on_untyped_tests (make-hydra_testing_test_group "Fail on untyped (pre-inference) terms" (list :nothing) (cl:list hydra_test_checking_failures_untyped_lambdas_tests) (cl:list)))

(cl:defvar hydra_test_checking_failures_all_tests (make-hydra_testing_test_group "Failures" (list :nothing) (cl:list hydra_test_checking_failures_fail_on_untyped_tests) (cl:list)))

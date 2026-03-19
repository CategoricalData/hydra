(define-library (hydra test checking failures)
(export hydra_test_checking_failures_untyped_lambdas_tests hydra_test_checking_failures_fail_on_untyped_tests hydra_test_checking_failures_all_tests)
(import (scheme base) (hydra testing))
(begin
(define hydra_test_checking_failures_untyped_lambdas_tests (make-hydra_testing_test_group "Untyped lambdas" (list 'nothing) (list) (list)))
(define hydra_test_checking_failures_fail_on_untyped_tests (make-hydra_testing_test_group "Fail on untyped (pre-inference) terms" (list 'nothing) (list hydra_test_checking_failures_untyped_lambdas_tests) (list)))
(define hydra_test_checking_failures_all_tests (make-hydra_testing_test_group "Failures" (list 'nothing) (list hydra_test_checking_failures_fail_on_untyped_tests) (list)))))

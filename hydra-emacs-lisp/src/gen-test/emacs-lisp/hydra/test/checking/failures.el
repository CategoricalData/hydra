(require 'cl-lib)

(require 'hydra.testing)

(defvar hydra_test_checking_failures_untyped_lambdas_tests (make-hydra_testing_test_group "Untyped lambdas" nil (list) (list)))

(defvar hydra_test_checking_failures_fail_on_untyped_tests (make-hydra_testing_test_group "Fail on untyped (pre-inference) terms" nil (list hydra_test_checking_failures_untyped_lambdas_tests) (list)))

(defvar hydra_test_checking_failures_all_tests (make-hydra_testing_test_group "Failures" nil (list hydra_test_checking_failures_fail_on_untyped_tests) (list)))

(provide 'hydra.test.checking.failures)

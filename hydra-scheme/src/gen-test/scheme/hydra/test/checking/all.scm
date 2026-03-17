(define-library (hydra test checking all)
(export hydra_test_checking_all_all_tests)
(import (scheme base) (hydra test checking advanced) (hydra test checking algebraic_types) (hydra test checking collections) (hydra test checking failures) (hydra test checking fundamentals) (hydra test checking nominal_types) (hydra testing))
(begin
(define hydra_test_checking_all_all_tests (make-hydra_testing_test_group "checking" '() (list hydra_test_checking_advanced_all_tests hydra_test_checking_algebraic_types_all_tests hydra_test_checking_collections_all_tests hydra_test_checking_failures_all_tests hydra_test_checking_fundamentals_all_tests hydra_test_checking_nominal_types_all_tests) (list)))))

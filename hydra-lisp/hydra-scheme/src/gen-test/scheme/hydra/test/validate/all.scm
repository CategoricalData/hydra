(define-library (hydra test validate all)
(export hydra_test_validate_all_all_tests)
(import (scheme base) (hydra test validate core) (hydra testing))
(begin
(define hydra_test_validate_all_all_tests (make-hydra_testing_test_group "validation" (list 'nothing) (list hydra_test_validate_core_all_tests) (list)))))

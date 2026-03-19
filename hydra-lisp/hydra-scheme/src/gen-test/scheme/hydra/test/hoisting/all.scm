(define-library (hydra test hoisting all)
(export hydra_test_hoisting_all_all_tests)
(import (scheme base) (hydra test hoisting cases) (hydra test hoisting let) (hydra testing))
(begin
(define hydra_test_hoisting_all_all_tests (make-hydra_testing_test_group "hoisting" (list 'nothing) (list hydra_test_hoisting_cases_all_tests hydra_test_hoisting_let_all_tests) (list)))))

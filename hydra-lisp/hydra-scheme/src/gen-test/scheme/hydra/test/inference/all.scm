(define-library (hydra test inference all)
(export hydra_test_inference_all_all_tests)
(import (scheme base) (hydra test inference algebraic_types) (hydra test inference algorithm_w) (hydra test inference classes) (hydra test inference failures) (hydra test inference fundamentals) (hydra test inference kernel_examples) (hydra test inference nominal_types) (hydra testing))
(begin
(define hydra_test_inference_all_all_tests (make-hydra_testing_test_group "inference" (list 'nothing) (list hydra_test_inference_algebraic_types_all_tests hydra_test_inference_algorithm_w_all_tests hydra_test_inference_classes_all_tests hydra_test_inference_failures_all_tests hydra_test_inference_fundamentals_all_tests hydra_test_inference_kernel_examples_all_tests hydra_test_inference_nominal_types_all_tests) (list)))))

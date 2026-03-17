(defpackage :hydra.test.inference.all
(:use :cl :hydra.test.inference.algebraicTypes :hydra.test.inference.algorithmW :hydra.test.inference.classes :hydra.test.inference.failures :hydra.test.inference.fundamentals :hydra.test.inference.kernelExamples :hydra.test.inference.nominalTypes :hydra.testing)
(:export :hydra_test_inference_all_all_tests))

(in-package :hydra.test.inference.all)

(cl:defvar hydra_test_inference_all_all_tests (make-hydra_testing_test_group "inference" cl:nil (cl:list hydra_test_inference_algebraic_types_all_tests hydra_test_inference_algorithm_w_all_tests hydra_test_inference_classes_all_tests hydra_test_inference_failures_all_tests hydra_test_inference_fundamentals_all_tests hydra_test_inference_kernel_examples_all_tests hydra_test_inference_nominal_types_all_tests) (cl:list)))

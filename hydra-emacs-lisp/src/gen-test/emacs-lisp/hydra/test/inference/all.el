(require 'cl-lib)

(require 'hydra.test.inference.algebraicTypes)

(require 'hydra.test.inference.algorithmW)

(require 'hydra.test.inference.classes)

(require 'hydra.test.inference.failures)

(require 'hydra.test.inference.fundamentals)

(require 'hydra.test.inference.kernelExamples)

(require 'hydra.test.inference.nominalTypes)

(require 'hydra.testing)

(defvar hydra_test_inference_all_all_tests (make-hydra_testing_test_group "inference" nil (list hydra_test_inference_algebraic_types_all_tests hydra_test_inference_algorithm_w_all_tests hydra_test_inference_classes_all_tests hydra_test_inference_failures_all_tests hydra_test_inference_fundamentals_all_tests hydra_test_inference_kernel_examples_all_tests hydra_test_inference_nominal_types_all_tests) (list)))

(provide 'hydra.test.inference.all)

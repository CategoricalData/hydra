(ns hydra.test.inference.all
  (:require [hydra.test.inference.algebraicTypes :refer :all] [hydra.test.inference.algorithmW :refer :all] [hydra.test.inference.classes :refer :all] [hydra.test.inference.failures :refer :all] [hydra.test.inference.fundamentals :refer :all] [hydra.test.inference.kernelExamples :refer :all] [hydra.test.inference.nominalTypes :refer :all] [hydra.testing :refer :all]
))

(declare hydra_test_inference_all_all_tests)

(def hydra_test_inference_all_all_tests (->hydra_testing_test_group "inference" nil (list hydra_test_inference_algebraic_types_all_tests hydra_test_inference_algorithm_w_all_tests hydra_test_inference_classes_all_tests hydra_test_inference_failures_all_tests hydra_test_inference_fundamentals_all_tests hydra_test_inference_kernel_examples_all_tests hydra_test_inference_nominal_types_all_tests) (list)))

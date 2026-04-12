(defpackage :hydra.test.hoisting.all
(:use :cl :hydra.test.hoisting.cases :hydra.test.hoisting.let :hydra.testing)
(:export :hydra_test_hoisting_all_all_tests))

(in-package :hydra.test.hoisting.all)

(cl:defvar hydra_test_hoisting_all_all_tests (make-hydra_testing_test_group "hoisting" (list :nothing) (cl:list hydra_test_hoisting_cases_all_tests hydra_test_hoisting_let_all_tests) (cl:list)))

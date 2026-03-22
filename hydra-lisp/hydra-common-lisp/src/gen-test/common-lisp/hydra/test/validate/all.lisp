(defpackage :hydra.test.validate.all
(:use :cl :hydra.test.validate.core :hydra.testing)
(:export :hydra_test_validate_all_all_tests))

(in-package :hydra.test.validate.all)

(cl:defvar hydra_test_validate_all_all_tests (make-hydra_testing_test_group "validation" (list :nothing) (cl:list hydra_test_validate_core_all_tests) (cl:list)))

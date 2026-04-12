(defpackage :hydra.test.checking.all
(:use :cl :hydra.test.checking.advanced :hydra.test.checking.algebraicTypes :hydra.test.checking.collections :hydra.test.checking.failures :hydra.test.checking.fundamentals :hydra.test.checking.nominalTypes :hydra.testing)
(:export :hydra_test_checking_all_all_tests))

(in-package :hydra.test.checking.all)

(cl:defvar hydra_test_checking_all_all_tests (make-hydra_testing_test_group "checking" (list :nothing) (cl:list hydra_test_checking_advanced_all_tests hydra_test_checking_algebraic_types_all_tests hydra_test_checking_collections_all_tests hydra_test_checking_failures_all_tests hydra_test_checking_fundamentals_all_tests hydra_test_checking_nominal_types_all_tests) (cl:list)))

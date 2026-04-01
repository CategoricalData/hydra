(defpackage :hydra.testing
(:use :cl)
(:export :make-hydra_testing_tag :hydra_testing_tag? :hydra_testing_tag-value :hydra_testing_test_case-variants :make-hydra_testing_test_case_with_metadata :hydra_testing_test_case_with_metadata? :hydra_testing_test_case_with_metadata-name :hydra_testing_test_case_with_metadata-case :hydra_testing_test_case_with_metadata-description :hydra_testing_test_case_with_metadata-tags :make-hydra_testing_test_group :hydra_testing_test_group? :hydra_testing_test_group-name :hydra_testing_test_group-description :hydra_testing_test_group-subgroups :hydra_testing_test_group-cases :make-hydra_testing_universal_test_case :hydra_testing_universal_test_case? :hydra_testing_universal_test_case-actual :hydra_testing_universal_test_case-expected))

(in-package :hydra.testing)

(cl:defstruct hydra_testing_tag value)

(cl:defvar hydra_testing_test_case-variants (cl:list :universal))

(cl:defstruct hydra_testing_test_case_with_metadata name case description tags)

(cl:defstruct hydra_testing_test_group name description subgroups cases)

(cl:defstruct hydra_testing_universal_test_case actual expected)

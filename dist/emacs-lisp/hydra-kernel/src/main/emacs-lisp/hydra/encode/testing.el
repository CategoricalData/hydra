(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maybes)

(require 'hydra.testing)

(defvar hydra_encode_testing_tag (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.testing.Tag" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_testing_universal_test_case (lambda (x) (list :record (make-hydra_core_record "hydra.testing.UniversalTestCase" (list (make-hydra_core_field "actual" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) (hydra_testing_universal_test_case-actual v)) x))) (make-hydra_core_field "expected" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) (hydra_testing_universal_test_case-expected v)) x))))))))

(defvar hydra_encode_testing_test_case (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :universal) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.testing.TestCase" (make-hydra_core_field "universal" (hydra_encode_testing_universal_test_case y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_testing_test_case_with_metadata (lambda (x) (list :record (make-hydra_core_record "hydra.testing.TestCaseWithMetadata" (list (make-hydra_core_field "name" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) x))) (make-hydra_core_field "case" (hydra_encode_testing_test_case (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) x))) (make-hydra_core_field "description" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map (lambda (x2) (list :literal (list :string x2)))) opt))) (funcall (lambda (v) (hydra_testing_test_case_with_metadata-description v)) x))) (make-hydra_core_field "tags" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_testing_tag) xs))) (funcall (lambda (v) (hydra_testing_test_case_with_metadata-tags v)) x))))))))

(defvar hydra_encode_testing_test_group (lambda (x) (list :record (make-hydra_core_record "hydra.testing.TestGroup" (list (make-hydra_core_field "name" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) (hydra_testing_test_group-name v)) x))) (make-hydra_core_field "description" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map (lambda (x2) (list :literal (list :string x2)))) opt))) (funcall (lambda (v) (hydra_testing_test_group-description v)) x))) (make-hydra_core_field "subgroups" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_testing_test_group) xs))) (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) x))) (make-hydra_core_field "cases" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_testing_test_case_with_metadata) xs))) (funcall (lambda (v) (hydra_testing_test_group-cases v)) x))))))))

(provide 'hydra.encode.testing)

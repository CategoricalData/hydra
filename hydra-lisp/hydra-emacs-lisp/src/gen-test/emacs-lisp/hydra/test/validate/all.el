(require 'cl-lib)

(require 'hydra.test.validate.core)

(require 'hydra.testing)

(defvar hydra_test_validate_all_all_tests (make-hydra_testing_test_group "validation" (list :nothing) (list hydra_test_validate_core_all_tests) (list)))

(provide 'hydra.test.validate.all)

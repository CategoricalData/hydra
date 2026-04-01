(require 'cl-lib)

(cl-defstruct hydra_testing_tag value)

(defvar hydra_testing_test_case-variants (list :universal))

(cl-defstruct hydra_testing_test_case_with_metadata name case description tags)

(cl-defstruct hydra_testing_test_group name description subgroups cases)

(cl-defstruct hydra_testing_universal_test_case actual expected)

(provide 'hydra.testing)

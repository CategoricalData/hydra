(require 'cl-lib)

(require 'hydra.test.annotations)

(require 'hydra.test.checking.all)

(require 'hydra.test.etaExpansion)

(require 'hydra.test.formatting)

(require 'hydra.test.hoisting.all)

(require 'hydra.test.inference.all)

(require 'hydra.test.json.roundtrip)

(require 'hydra.test.json.writer)

(require 'hydra.test.lib.chars)

(require 'hydra.test.lib.eithers)

(require 'hydra.test.lib.equality)

(require 'hydra.test.lib.lists)

(require 'hydra.test.lib.literals)

(require 'hydra.test.lib.logic)

(require 'hydra.test.lib.maps)

(require 'hydra.test.lib.math)

(require 'hydra.test.lib.maybes)

(require 'hydra.test.lib.pairs)

(require 'hydra.test.lib.regex)

(require 'hydra.test.lib.sets)

(require 'hydra.test.lib.strings)

(require 'hydra.test.reduction)

(require 'hydra.test.rewriting)

(require 'hydra.test.serialization)

(require 'hydra.test.sorting)

(require 'hydra.test.substitution)

(require 'hydra.test.unification)

(require 'hydra.test.validate.all)

(require 'hydra.testing)

(defvar hydra_test_test_suite_all_tests (make-hydra_testing_test_group "common" (list :nothing) (list hydra_test_lib_chars_all_tests hydra_test_lib_eithers_all_tests hydra_test_lib_equality_all_tests hydra_test_lib_lists_all_tests hydra_test_lib_literals_all_tests hydra_test_lib_logic_all_tests hydra_test_lib_maps_all_tests hydra_test_lib_math_all_tests hydra_test_lib_maybes_all_tests hydra_test_lib_pairs_all_tests hydra_test_lib_regex_all_tests hydra_test_lib_sets_all_tests hydra_test_lib_strings_all_tests hydra_test_annotations_all_tests hydra_test_checking_all_all_tests hydra_test_eta_expansion_all_tests hydra_test_formatting_all_tests hydra_test_hoisting_all_all_tests hydra_test_inference_all_all_tests hydra_test_json_roundtrip_all_tests hydra_test_json_writer_all_tests hydra_test_reduction_all_tests hydra_test_rewriting_all_tests hydra_test_serialization_all_tests hydra_test_sorting_all_tests hydra_test_substitution_all_tests hydra_test_unification_all_tests hydra_test_validate_all_all_tests) (list)))

(provide 'hydra.test.testSuite)

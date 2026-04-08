(defpackage :hydra.test.utils
(:use :cl :hydra.inference :hydra.lexical :hydra.lib.eithers :hydra.show.errors :hydra.testing :hydra.typing)
(:export :hydra_test_utils_infer_term :hydra_test_utils_infer_test_case :hydra_test_utils_infer_test_group_terms))

(in-package :hydra.test.utils)

(cl:defvar hydra_test_utils_infer_term (cl:lambda (g) (cl:lambda (term) (((hydra_lib_eithers_bimap (cl:lambda (e) (hydra_show_errors_error e))) (cl:lambda (x) ((cl:lambda (v) (hydra_typing_inference_result-term v)) x))) (((hydra_inference_infer_in_graph_context hydra_lexical_empty_context) g) term)))))

(cl:defvar hydra_test_utils_infer_test_case (cl:lambda (g) (cl:lambda (tcm) (let* ((desc ((cl:lambda (v) (hydra_testing_test_case_with_metadata-description v)) tcm)) (name_ ((cl:lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tags_ ((cl:lambda (v) (hydra_testing_test_case_with_metadata-tags v)) tcm)) (tcase ((cl:lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) ((hydra_lib_eithers_map (cl:lambda (inferred_case) (make-hydra_testing_test_case_with_metadata name_ inferred_case desc tags_))) (list :right tcase))))))

(cl:defvar hydra_test_utils_infer_test_group_terms (cl:lambda (g) (cl:lambda (tg) (let* ((cases_ ((cl:lambda (v) (hydra_testing_test_group-cases v)) tg)) (desc ((cl:lambda (v) (hydra_testing_test_group-description v)) tg)) (name_ ((cl:lambda (v) (hydra_testing_test_group-name v)) tg)) (subgroups ((cl:lambda (v) (hydra_testing_test_group-subgroups v)) tg))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (cl:lambda (sg) ((hydra_test_utils_infer_test_group_terms g) sg))) subgroups)) (cl:lambda (inferred_subgroups) ((hydra_lib_eithers_map (cl:lambda (inferred_cases) (make-hydra_testing_test_group name_ desc inferred_subgroups inferred_cases))) ((hydra_lib_eithers_map_list (cl:lambda (tc) ((hydra_test_utils_infer_test_case g) tc))) cases_))))))))

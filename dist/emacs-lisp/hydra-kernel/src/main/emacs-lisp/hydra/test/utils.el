(require 'cl-lib)

(require 'hydra.inference)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.show.errors)

(require 'hydra.testing)

(require 'hydra.typing)

(defvar hydra_test_utils_infer_term (lambda (g) (lambda (term) (funcall (funcall (hydra_lib_eithers_bimap (lambda (e) (hydra_show_errors_error e))) (lambda (x) (funcall (lambda (v) (hydra_typing_inference_result-term v)) x))) (funcall (funcall (hydra_inference_infer_in_graph_context hydra_lexical_empty_context) g) term)))))

(defvar hydra_test_utils_infer_test_case (lambda (g) (lambda (tcm) (let* ((desc (funcall (lambda (v) (hydra_testing_test_case_with_metadata-description v)) tcm)) (name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tags_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-tags v)) tcm)) (tcase (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) (funcall (hydra_lib_eithers_map (lambda (inferred_case) (make-hydra_testing_test_case_with_metadata name_ inferred_case desc tags_))) (list :right tcase))))))

(defvar hydra_test_utils_infer_test_group_terms (lambda (g) (lambda (tg) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) tg)) (desc (funcall (lambda (v) (hydra_testing_test_group-description v)) tg)) (name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) tg)) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) tg))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list (lambda (sg) (funcall (hydra_test_utils_infer_test_group_terms g) sg))) subgroups)) (lambda (inferred_subgroups) (funcall (hydra_lib_eithers_map (lambda (inferred_cases) (make-hydra_testing_test_group name_ desc inferred_subgroups inferred_cases))) (funcall (hydra_lib_eithers_map_list (lambda (tc) (funcall (hydra_test_utils_infer_test_case g) tc))) cases_))))))))

(provide 'hydra.test.utils)

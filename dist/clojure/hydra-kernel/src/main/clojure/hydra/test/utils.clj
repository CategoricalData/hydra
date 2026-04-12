(ns hydra.test.utils
  (:require [hydra.inference :refer :all] [hydra.lexical :refer :all] [hydra.lib.eithers :refer :all] [hydra.show.errors :refer :all] [hydra.testing :refer :all] [hydra.typing :refer :all]
))

(declare hydra_test_utils_infer_term hydra_test_utils_infer_test_case hydra_test_utils_infer_test_group_terms)

(def hydra_test_utils_infer_term (fn [g] (fn [term] (((hydra_lib_eithers_bimap (fn [e] (hydra_show_errors_error e))) (fn [x] ((fn [v] (:term v)) x))) (((hydra_inference_infer_in_graph_context hydra_lexical_empty_context) g) term)))))

(def hydra_test_utils_infer_test_case (fn [g] (fn [tcm] (let [desc ((fn [v] (:description v)) tcm) name_ ((fn [v] (:name v)) tcm) tags_ ((fn [v] (:tags v)) tcm) tcase ((fn [v] (:case v)) tcm)] ((hydra_lib_eithers_map (fn [inferred_case] (->hydra_testing_test_case_with_metadata name_ inferred_case desc tags_))) (list :right tcase))))))

(def hydra_test_utils_infer_test_group_terms (fn [g] (fn [tg] (let [cases_ ((fn [v] (:cases v)) tg) desc ((fn [v] (:description v)) tg) name_ ((fn [v] (:name v)) tg) subgroups ((fn [v] (:subgroups v)) tg)] ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (fn [sg] ((hydra_test_utils_infer_test_group_terms g) sg))) subgroups)) (fn [inferred_subgroups] ((hydra_lib_eithers_map (fn [inferred_cases] (->hydra_testing_test_group name_ desc inferred_subgroups inferred_cases))) ((hydra_lib_eithers_map_list (fn [tc] ((hydra_test_utils_infer_test_case g) tc))) cases_))))))))

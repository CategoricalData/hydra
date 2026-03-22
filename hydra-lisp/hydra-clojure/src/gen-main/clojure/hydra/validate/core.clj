(ns hydra.validate.core
  (:require [hydra.accessors :refer :all] [hydra.core :refer :all] [hydra.error.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.sets :refer :all] [hydra.rewriting :refer :all]
))

(declare hydra_validate_core_find_duplicate hydra_validate_core_check_duplicate_bindings hydra_validate_core_check_duplicate_fields hydra_validate_core_check_term hydra_validate_core_term)

(def hydra_validate_core_find_duplicate (fn [names] (let [result (((hydra_lib_lists_foldl (fn [acc] (fn [name] (let [seen (hydra_lib_pairs_first acc)] (let [dup (hydra_lib_pairs_second acc)] (((hydra_lib_maybes_cases dup) (fn [] (if ((hydra_lib_sets_member name) seen) (list seen (list :just name)) (list ((hydra_lib_sets_insert name) seen) (list :nothing))))) (fn [_] acc))))))) (list hydra_lib_sets_empty (list :nothing))) names)] (hydra_lib_pairs_second result))))

(def hydra_validate_core_check_duplicate_bindings (fn [path] (fn [bindings] (let [names ((hydra_lib_lists_map (fn [v] (:name v))) bindings)] (let [dup (hydra_validate_core_find_duplicate names)] ((hydra_lib_maybes_map (fn [name] (list :duplicate_binding (->hydra_error_core_duplicate_binding_error path name)))) dup))))))

(def hydra_validate_core_check_duplicate_fields (fn [path] (fn [names] (let [dup (hydra_validate_core_find_duplicate names)] ((hydra_lib_maybes_map (fn [name] (list :duplicate_field (->hydra_error_core_duplicate_field_error path name)))) dup)))))

(def hydra_validate_core_check_term (fn [path] (fn [term] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :let) ((fn [lt] ((hydra_validate_core_check_duplicate_bindings path) ((fn [v] (:bindings v)) lt))) match_value) (= (first match_target) :record) ((fn [rec] ((hydra_validate_core_check_duplicate_fields path) ((hydra_lib_lists_map (fn [v] (:name v))) ((fn [v] (:fields v)) rec)))) match_value) :else (list :nothing))) (second match_target))) term))))

(def hydra_validate_core_term (fn [g] (fn [t_] ((((hydra_rewriting_fold_term_with_graph_and_path (fn [recurse] (fn [path] (fn [cx] (fn [acc] (fn [trm] (((hydra_lib_maybes_cases acc) (fn [] (let [check_result ((hydra_validate_core_check_term path) trm)] (((hydra_lib_maybes_cases check_result) (fn [] ((recurse (list :nothing)) trm))) (fn [err] (list :just err)))))) (fn [_] acc)))))))) g) (list :nothing)) t_))))

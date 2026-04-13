(ns hydra.eval.lib.pairs
  (:require [hydra.core :refer :all] [hydra.errors :refer :all] [hydra.lib.pairs :refer :all] [hydra.show.core :refer :all]
))

(declare hydra_eval_lib_pairs_bimap hydra_eval_lib_pairs_first hydra_eval_lib_pairs_second)

(def hydra_eval_lib_pairs_bimap (fn [cx] (fn [g] (fn [first_fun] (fn [second_fun] (fn [pair_term] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :pair) ((fn [p] (let [fst (hydra_lib_pairs_first p)] (let [snd (hydra_lib_pairs_second p)] (list :right (list :pair (list (list :application (->hydra_core_application first_fun fst)) (list :application (->hydra_core_application second_fun snd)))))))) match_value) :else (list :left (list :extraction (list :unexpected_shape (->hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term))))))) (second match_target))) pair_term)))))))

(def hydra_eval_lib_pairs_first (fn [cx] (fn [g] (fn [pair_term] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :pair) ((fn [p] (list :right (hydra_lib_pairs_first p))) match_value) :else (list :left (list :extraction (list :unexpected_shape (->hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term))))))) (second match_target))) pair_term)))))

(def hydra_eval_lib_pairs_second (fn [cx] (fn [g] (fn [pair_term] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :pair) ((fn [p] (list :right (hydra_lib_pairs_second p))) match_value) :else (list :left (list :extraction (list :unexpected_shape (->hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term))))))) (second match_target))) pair_term)))))

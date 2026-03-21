(ns hydra.eval.lib.pairs
  (:require [hydra.context :refer :all] [hydra.core :refer :all] [hydra.error :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.strings :refer :all] [hydra.show.core :refer :all]
))

(declare hydra_eval_lib_pairs_bimap)

(def hydra_eval_lib_pairs_bimap (fn [cx] (fn [g] (fn [first_fun] (fn [second_fun] (fn [pair_term] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :pair) ((fn [p] (let [fst (hydra_lib_pairs_first p)] (let [snd (hydra_lib_pairs_second p)] (list :right (list :pair (list (list :application (->hydra_core_application first_fun fst)) (list :application (->hydra_core_application second_fun snd)))))))) match_value) :else (list :left (->hydra_context_in_context (list :other ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "pair value")) " but found ")) (hydra_show_core_term pair_term))) cx)))) (second match_target))) pair_term)))))))

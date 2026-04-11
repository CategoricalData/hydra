(ns hydra.languages
  (:require [hydra.coders :refer :all] [hydra.core :refer :all] [hydra.lib.sets :refer :all] [hydra.reflect :refer :all]
))

(declare hydra_languages_hydra_language)

(def hydra_languages_hydra_language (let [elimination_variants (hydra_lib_sets_from_list hydra_reflect_elimination_variants) float_types (hydra_lib_sets_from_list hydra_reflect_float_types) function_variants (hydra_lib_sets_from_list hydra_reflect_function_variants) integer_types (hydra_lib_sets_from_list hydra_reflect_integer_types) literal_variants (hydra_lib_sets_from_list hydra_reflect_literal_variants) term_variants (hydra_lib_sets_from_list hydra_reflect_term_variants) type_variants (hydra_lib_sets_from_list hydra_reflect_type_variants) types (fn [t_] ((fn [match_target] ((fn [match_value] (cond :else true)) (second match_target))) t_))] (->hydra_coders_language "hydra.core" (->hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types))))

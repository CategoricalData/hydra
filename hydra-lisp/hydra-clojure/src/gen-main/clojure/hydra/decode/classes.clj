(ns hydra.decode.classes
  (:require [hydra.classes :refer :all] [hydra.core :refer :all] [hydra.error :refer :all] [hydra.extract.helpers :refer :all] [hydra.lexical :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.strings :refer :all]
))

(declare hydra_decode_classes_type_class)

(def hydra_decode_classes_type_class (fn [cx] (fn [raw] (((hydra_lib_eithers_either (fn [err] (list :left err))) (fn [stripped] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :union) ((fn [inj] (let [field ((fn [v] (:field v)) inj) fname ((fn [v] (:name v)) field) fterm ((fn [v] (:term v)) field) variant_map (hydra_lib_maps_from_list (list (list "equality" (fn [input] ((hydra_lib_eithers_map (fn [t_] (list :equality t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "ordering" (fn [input] ((hydra_lib_eithers_map (fn [t_] (list :ordering t_))) ((hydra_extract_helpers_decode_unit cx) input))))))] (((hydra_lib_maybes_maybe (fn [] (list :left (hydra_lib_strings_cat (list "no such field " ((fn [v] v) fname) " in union"))))) (fn [f] (f fterm))) ((hydra_lib_maps_lookup fname) variant_map)))) match_value) :else (list :left "expected union"))) (second match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))

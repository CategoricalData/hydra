(ns hydra.show.error.core
  (:require [hydra.core :refer :all] [hydra.error.core :refer :all] [hydra.lib.strings :refer :all] [hydra.show.core :refer :all] [hydra.show.meta :refer :all]
))

(declare hydra_show_error_core_duplicate_binding_error hydra_show_error_core_duplicate_field_error hydra_show_error_core_invalid_term_error hydra_show_error_core_undefined_field_error hydra_show_error_core_undefined_term_error hydra_show_error_core_undefined_type_error hydra_show_error_core_unexpected_term_variant_error hydra_show_error_core_unexpected_type_variant_error)

(def hydra_show_error_core_duplicate_binding_error (fn [e] (hydra_lib_strings_cat (list "duplicate binding: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_duplicate_field_error (fn [e] (hydra_lib_strings_cat (list "duplicate field: " ((fn [v] v) ((fn [v] (:name v)) e))))))

(def hydra_show_error_core_invalid_term_error (fn [e] ((hydra_lib_strings_cat2 "invalid term: ") ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :duplicate_binding) (hydra_show_error_core_duplicate_binding_error match_value) (= (first match_target) :duplicate_field) (hydra_show_error_core_duplicate_field_error match_value))) (second match_target))) e))))

(def hydra_show_error_core_undefined_field_error (fn [e] (let [fname ((fn [v] (:field_name v)) e)] (let [tname ((fn [v] (:type_name v)) e)] (hydra_lib_strings_cat (list "no such field \"" ((fn [v] v) fname) "\" in type \"" ((fn [v] v) tname) "\""))))))

(def hydra_show_error_core_undefined_term_error (fn [e] ((hydra_lib_strings_cat2 "undefined term: ") ((fn [v] v) ((fn [v] (:name v)) e)))))

(def hydra_show_error_core_undefined_type_error (fn [e] ((hydra_lib_strings_cat2 "undefined type: ") ((fn [v] v) ((fn [v] (:name v)) e)))))

(def hydra_show_error_core_unexpected_term_variant_error (fn [e] (let [expected ((fn [v] (:expected_variant v)) e)] (let [actual ((fn [v] (:actual_term v)) e)] (hydra_lib_strings_cat (list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(def hydra_show_error_core_unexpected_type_variant_error (fn [e] (let [expected ((fn [v] (:expected_variant v)) e)] (let [actual ((fn [v] (:actual_type v)) e)] (hydra_lib_strings_cat (list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

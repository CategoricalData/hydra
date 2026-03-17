(ns hydra.extract.json
  (:require [hydra.json.model :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.strings :refer :all]
))

(declare hydra_extract_json_show_value hydra_extract_json_expect_array hydra_extract_json_expect_number hydra_extract_json_expect_object hydra_extract_json_expect_string hydra_extract_json_opt hydra_extract_json_opt_array hydra_extract_json_opt_string hydra_extract_json_require hydra_extract_json_require_array hydra_extract_json_require_number hydra_extract_json_require_string)

(def hydra_extract_json_show_value (fn [value] "TODO: implement showValue"))

(def hydra_extract_json_expect_array (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :array) ((fn [els] (list :right els)) match_value) :else (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON array")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value)))))) (second match_target))) value)))

(def hydra_extract_json_expect_number (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :number) ((fn [d] (list :right d)) match_value) :else (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON number")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value)))))) (second match_target))) value)))

(def hydra_extract_json_expect_object (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :object) ((fn [m] (list :right m)) match_value) :else (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON object")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value)))))) (second match_target))) value)))

(def hydra_extract_json_expect_string (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :string) ((fn [s] (list :right s)) match_value) :else (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON string")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value)))))) (second match_target))) value)))

(def hydra_extract_json_opt (fn [fname] (fn [m] ((hydra_lib_maps_lookup fname) m))))

(def hydra_extract_json_opt_array (fn [fname] (fn [m] (((hydra_lib_maybes_maybe (list :right nil)) (fn [a] ((hydra_lib_eithers_map (fn [x] x)) (hydra_extract_json_expect_array a)))) ((hydra_extract_json_opt fname) m)))))

(def hydra_extract_json_opt_string (fn [fname] (fn [m] (((hydra_lib_maybes_maybe (list :right nil)) (fn [s] ((hydra_lib_eithers_map (fn [x] x)) (hydra_extract_json_expect_string s)))) ((hydra_extract_json_opt fname) m)))))

(def hydra_extract_json_require (fn [fname] (fn [m] (((hydra_lib_maybes_maybe (list :left (hydra_lib_strings_cat (list "required attribute " (hydra_extract_json_show_value fname) " not found")))) (fn [value] (list :right value))) ((hydra_lib_maps_lookup fname) m)))))

(def hydra_extract_json_require_array (fn [fname] (fn [m] ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_array))))

(def hydra_extract_json_require_number (fn [fname] (fn [m] ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_number))))

(def hydra_extract_json_require_string (fn [fname] (fn [m] ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_string))))

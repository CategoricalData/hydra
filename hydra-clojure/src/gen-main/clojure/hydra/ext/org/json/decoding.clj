(ns hydra.ext.org.json.decoding
  (:require [hydra.json.model :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.strings :refer :all]
))

(declare hydra_ext_org_json_decoding_decode_array hydra_ext_org_json_decoding_decode_boolean hydra_ext_org_json_decoding_decode_optional_field hydra_ext_org_json_decoding_decode_field hydra_ext_org_json_decoding_decode_object hydra_ext_org_json_decoding_decode_string)

(def hydra_ext_org_json_decoding_decode_array (fn [decode_elem] (fn [match_target] ((fn [match_value] (cond (= (first match_target) :array) ((fn [a] ((hydra_lib_eithers_map_list decode_elem) a)) match_value) :else (list :left "expected an array"))) (second match_target)))))

(def hydra_ext_org_json_decoding_decode_boolean (fn [match_target] ((fn [match_value] (cond (= (first match_target) :boolean) ((fn [b] (list :right b)) match_value) :else (list :left "expected a boolean"))) (second match_target))))

(def hydra_ext_org_json_decoding_decode_optional_field (fn [decode_value] (fn [name] (fn [m] (((hydra_lib_maybes_maybe (list :right nil)) (fn [v] ((hydra_lib_eithers_map (fn [x] x)) (decode_value v)))) ((hydra_lib_maps_lookup name) m))))))

(def hydra_ext_org_json_decoding_decode_field (fn [decode_value] (fn [name] (fn [m] ((hydra_lib_eithers_bind (((hydra_ext_org_json_decoding_decode_optional_field decode_value) name) m)) ((hydra_lib_maybes_maybe (list :left ((hydra_lib_strings_cat2 "missing field: ") name))) (fn [f] (list :right f))))))))

(def hydra_ext_org_json_decoding_decode_object (fn [match_target] ((fn [match_value] (cond (= (first match_target) :object) ((fn [o] (list :right o)) match_value) :else (list :left "expected an object"))) (second match_target))))

(def hydra_ext_org_json_decoding_decode_string (fn [match_target] ((fn [match_value] (cond (= (first match_target) :string) ((fn [s] (list :right s)) match_value) :else (list :left "expected a string"))) (second match_target))))

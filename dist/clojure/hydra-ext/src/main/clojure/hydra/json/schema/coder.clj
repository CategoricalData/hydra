(ns hydra.json.schema.coder
  (:require [hydra.core :refer :all] [hydra.formatting :refer :all] [hydra.json.schema :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.strings :refer :all] [hydra.strip :refer :all]
))

(declare hydra_json_schema_coder_construct_module hydra_json_schema_coder_encode_type hydra_json_schema_coder_encode_field hydra_json_schema_coder_encode_name hydra_json_schema_coder_encode_named_type hydra_json_schema_coder_is_required_field hydra_json_schema_coder_module_to_json_schema hydra_json_schema_coder_reference_restriction)

(def hydra_json_schema_coder_construct_module (fn [cx] (fn [g] (fn [opts] (fn [mod] (fn [type_defs] (((((hydra_json_schema_coder_construct_module cx) g) opts) mod) type_defs)))))))

(def hydra_json_schema_coder_encode_type (fn [cx] (fn [g] (fn [optional] (fn [typ] ((((hydra_json_schema_coder_encode_type cx) g) optional) typ))))))

(def hydra_json_schema_coder_encode_field (fn [cx] (fn [g] (fn [ft] (let [name ((fn [v] (:name v)) ft) typ ((fn [v] (:type v)) ft)] ((hydra_lib_eithers_map (fn [res] (list ((fn [v] v) name) res))) ((((hydra_json_schema_coder_encode_type cx) g) false) typ)))))))

(def hydra_json_schema_coder_encode_name (fn [name] (hydra_formatting_non_alnum_to_underscores ((fn [v] v) name))))

(def hydra_json_schema_coder_encode_named_type (fn [cx] (fn [g] (fn [name] (fn [typ] ((hydra_lib_eithers_map (fn [res] (hydra_lib_lists_concat (list (list (list :title ((fn [v] v) name))) res)))) ((((hydra_json_schema_coder_encode_type cx) g) false) (hydra_strip_deannotate_type typ))))))))

(def hydra_json_schema_coder_is_required_field (fn [ft] (let [typ ((fn [v] (:type v)) ft)] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :maybe) ((fn [_] false) match_value) :else true)) (second match_target))) (hydra_strip_deannotate_type typ)))))

(def hydra_json_schema_coder_module_to_json_schema (fn [opts] (fn [mod] (fn [defs] (fn [cx] (fn [g] (((((hydra_json_schema_coder_module_to_json_schema opts) mod) defs) cx) g)))))))

(def hydra_json_schema_coder_reference_restriction (fn [name] (list :reference (hydra_lib_strings_cat (list "#/$defs/" (hydra_json_schema_coder_encode_name name))))))

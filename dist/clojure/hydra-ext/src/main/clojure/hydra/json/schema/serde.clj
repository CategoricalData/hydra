(ns hydra.json.schema.serde
  (:require [hydra.json.model :refer :all] [hydra.json.schema :refer :all] [hydra.json.writer :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all]
))

(declare hydra_json_schema_serde_encode_integer hydra_json_schema_serde_key_items hydra_json_schema_serde_key_additional_items hydra_json_schema_serde_key_max_items hydra_json_schema_serde_key_min_items hydra_json_schema_serde_key_unique_items hydra_json_schema_serde_key_all_of hydra_json_schema_serde_key_any_of hydra_json_schema_serde_key_enum hydra_json_schema_serde_key_not hydra_json_schema_serde_key_one_of hydra_json_schema_serde_key_exclusive_maximum hydra_json_schema_serde_key_exclusive_minimum hydra_json_schema_serde_key_maximum hydra_json_schema_serde_key_minimum hydra_json_schema_serde_key_multiple_of hydra_json_schema_serde_encode_numeric_restriction hydra_json_schema_serde_encode_keyword hydra_json_schema_serde_key_additional_properties hydra_json_schema_serde_key_dependencies hydra_json_schema_serde_key_max_properties hydra_json_schema_serde_key_min_properties hydra_json_schema_serde_key_pattern_properties hydra_json_schema_serde_key_properties hydra_json_schema_serde_key_required hydra_json_schema_serde_encode_schema_reference hydra_json_schema_serde_key_max_length hydra_json_schema_serde_key_min_length hydra_json_schema_serde_key_pattern hydra_json_schema_serde_encode_string_restriction hydra_json_schema_serde_encode_type_name hydra_json_schema_serde_encode_type hydra_json_schema_serde_key_description hydra_json_schema_serde_key_ref hydra_json_schema_serde_key_title hydra_json_schema_serde_key_type hydra_json_schema_serde_encode_additional_items hydra_json_schema_serde_encode_array_restriction hydra_json_schema_serde_encode_items hydra_json_schema_serde_encode_keyword_schema_or_array hydra_json_schema_serde_encode_multiple_restriction hydra_json_schema_serde_encode_object_restriction hydra_json_schema_serde_encode_pattern_property hydra_json_schema_serde_encode_property hydra_json_schema_serde_encode_restriction hydra_json_schema_serde_encode_schema hydra_json_schema_serde_encode_schema_or_array hydra_json_schema_serde_from_object hydra_json_schema_serde_key_definitions hydra_json_schema_serde_key_id hydra_json_schema_serde_key_schema hydra_json_schema_serde_to_object hydra_json_schema_serde_json_schema_document_to_json_value hydra_json_schema_serde_json_schema_document_to_string hydra_json_schema_serde_key_label)

(def hydra_json_schema_serde_encode_integer (fn [n] (list :number (hydra_lib_literals_bigint_to_decimal (hydra_lib_literals_int32_to_bigint n)))))

(def hydra_json_schema_serde_key_items "items")

(def hydra_json_schema_serde_key_additional_items "additionalItems")

(def hydra_json_schema_serde_key_max_items "maxItems")

(def hydra_json_schema_serde_key_min_items "minItems")

(def hydra_json_schema_serde_key_unique_items "uniqueItems")

(def hydra_json_schema_serde_key_all_of "allOf")

(def hydra_json_schema_serde_key_any_of "anyOf")

(def hydra_json_schema_serde_key_enum "enum")

(def hydra_json_schema_serde_key_not "not")

(def hydra_json_schema_serde_key_one_of "oneOf")

(def hydra_json_schema_serde_key_exclusive_maximum "exclusiveMaximum")

(def hydra_json_schema_serde_key_exclusive_minimum "exclusiveMinimum")

(def hydra_json_schema_serde_key_maximum "maximum")

(def hydra_json_schema_serde_key_minimum "minimum")

(def hydra_json_schema_serde_key_multiple_of "multipleOf")

(def hydra_json_schema_serde_encode_numeric_restriction (fn [r] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :minimum) ((fn [lim] (let [excl ((fn [v] (:exclusive v)) lim) value ((fn [v] (:value v)) lim)] (hydra_lib_lists_concat (list (list (list hydra_json_schema_serde_key_minimum (hydra_json_schema_serde_encode_integer value))) (if excl (list (list hydra_json_schema_serde_key_exclusive_minimum (list :boolean true))) (list)))))) match_value) (= (first match_target) :maximum) ((fn [lim] (let [excl ((fn [v] (:exclusive v)) lim) value ((fn [v] (:value v)) lim)] (hydra_lib_lists_concat (list (list (list hydra_json_schema_serde_key_maximum (hydra_json_schema_serde_encode_integer value))) (if excl (list (list hydra_json_schema_serde_key_exclusive_maximum (list :boolean true))) (list)))))) match_value) (= (first match_target) :multiple_of) ((fn [n] (list (list hydra_json_schema_serde_key_multiple_of (hydra_json_schema_serde_encode_integer n)))) match_value))) (second match_target))) r)))

(def hydra_json_schema_serde_encode_keyword (fn [k] (list :string ((fn [v] v) k))))

(def hydra_json_schema_serde_key_additional_properties "additionalProperties")

(def hydra_json_schema_serde_key_dependencies "dependencies")

(def hydra_json_schema_serde_key_max_properties "maxProperties")

(def hydra_json_schema_serde_key_min_properties "minProperties")

(def hydra_json_schema_serde_key_pattern_properties "patternProperties")

(def hydra_json_schema_serde_key_properties "properties")

(def hydra_json_schema_serde_key_required "required")

(def hydra_json_schema_serde_encode_schema_reference (fn [sr] (list :string ((fn [v] v) sr))))

(def hydra_json_schema_serde_key_max_length "maxLength")

(def hydra_json_schema_serde_key_min_length "minLength")

(def hydra_json_schema_serde_key_pattern "pattern")

(def hydra_json_schema_serde_encode_string_restriction (fn [r] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :max_length) ((fn [n] (list hydra_json_schema_serde_key_max_length (list :number (hydra_lib_literals_bigint_to_decimal (hydra_lib_literals_int32_to_bigint n))))) match_value) (= (first match_target) :min_length) ((fn [n] (list hydra_json_schema_serde_key_min_length (list :number (hydra_lib_literals_bigint_to_decimal (hydra_lib_literals_int32_to_bigint n))))) match_value) (= (first match_target) :pattern) ((fn [re] (list hydra_json_schema_serde_key_pattern (list :string ((fn [v] v) re)))) match_value))) (second match_target))) r)))

(def hydra_json_schema_serde_encode_type_name (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :string) ((fn [_] (list :string "string")) match_value) (= (first match_target) :integer) ((fn [_] (list :string "integer")) match_value) (= (first match_target) :number) ((fn [_] (list :string "number")) match_value) (= (first match_target) :boolean) ((fn [_] (list :string "boolean")) match_value) (= (first match_target) :null) ((fn [_] (list :string "null")) match_value) (= (first match_target) :array) ((fn [_] (list :string "array")) match_value) (= (first match_target) :object) ((fn [_] (list :string "object")) match_value))) (second match_target))) t_)))

(def hydra_json_schema_serde_encode_type (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :single) ((fn [name] (hydra_json_schema_serde_encode_type_name name)) match_value) (= (first match_target) :multiple) ((fn [names] (list :array ((hydra_lib_lists_map hydra_json_schema_serde_encode_type_name) names))) match_value))) (second match_target))) t_)))

(def hydra_json_schema_serde_key_description "description")

(def hydra_json_schema_serde_key_ref "$ref")

(def hydra_json_schema_serde_key_title "title")

(def hydra_json_schema_serde_key_type "type")

(def hydra_json_schema_serde_encode_additional_items (fn [ai] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :any) ((fn [b] (list :boolean b)) match_value) (= (first match_target) :schema) ((fn [schema] (hydra_json_schema_serde_encode_schema schema)) match_value))) (second match_target))) ai)))

(def hydra_json_schema_serde_encode_array_restriction (fn [r] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :items) ((fn [items] (hydra_json_schema_serde_encode_items items)) match_value) (= (first match_target) :additional_items) ((fn [ai] (list hydra_json_schema_serde_key_additional_items (hydra_json_schema_serde_encode_additional_items ai))) match_value) (= (first match_target) :min_items) ((fn [n] (list hydra_json_schema_serde_key_min_items (hydra_json_schema_serde_encode_integer n))) match_value) (= (first match_target) :max_items) ((fn [n] (list hydra_json_schema_serde_key_max_items (hydra_json_schema_serde_encode_integer n))) match_value) (= (first match_target) :unique_items) ((fn [b] (list hydra_json_schema_serde_key_unique_items (list :boolean b))) match_value))) (second match_target))) r)))

(def hydra_json_schema_serde_encode_items (fn [items] (list hydra_json_schema_serde_key_items ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :same_items) ((fn [schema] (hydra_json_schema_serde_encode_schema schema)) match_value) (= (first match_target) :var_items) ((fn [schemas] (list :array ((hydra_lib_lists_map hydra_json_schema_serde_encode_schema) schemas))) match_value))) (second match_target))) items))))

(def hydra_json_schema_serde_encode_keyword_schema_or_array (fn [p] (let [k (hydra_lib_pairs_first p) s (hydra_lib_pairs_second p)] (list ((fn [v] v) k) (hydra_json_schema_serde_encode_schema_or_array s)))))

(def hydra_json_schema_serde_encode_multiple_restriction (fn [r] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :all_of) ((fn [schemas] (list hydra_json_schema_serde_key_all_of (list :array ((hydra_lib_lists_map hydra_json_schema_serde_encode_schema) schemas)))) match_value) (= (first match_target) :any_of) ((fn [schemas] (list hydra_json_schema_serde_key_any_of (list :array ((hydra_lib_lists_map hydra_json_schema_serde_encode_schema) schemas)))) match_value) (= (first match_target) :one_of) ((fn [schemas] (list hydra_json_schema_serde_key_one_of (list :array ((hydra_lib_lists_map hydra_json_schema_serde_encode_schema) schemas)))) match_value) (= (first match_target) :not) ((fn [schema] (list hydra_json_schema_serde_key_not (hydra_json_schema_serde_encode_schema schema))) match_value) (= (first match_target) :enum) ((fn [values_] (list hydra_json_schema_serde_key_enum (list :array values_))) match_value))) (second match_target))) r)))

(def hydra_json_schema_serde_encode_object_restriction (fn [r] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :properties) ((fn [props] (list hydra_json_schema_serde_key_properties (list :object (hydra_lib_maps_from_list ((hydra_lib_lists_map hydra_json_schema_serde_encode_property) (hydra_lib_maps_to_list props)))))) match_value) (= (first match_target) :additional_properties) ((fn [ai] (list hydra_json_schema_serde_key_additional_properties (hydra_json_schema_serde_encode_additional_items ai))) match_value) (= (first match_target) :required) ((fn [keys] (list hydra_json_schema_serde_key_required (list :array ((hydra_lib_lists_map hydra_json_schema_serde_encode_keyword) keys)))) match_value) (= (first match_target) :min_properties) ((fn [n] (list hydra_json_schema_serde_key_min_properties (hydra_json_schema_serde_encode_integer n))) match_value) (= (first match_target) :max_properties) ((fn [n] (list hydra_json_schema_serde_key_max_properties (hydra_json_schema_serde_encode_integer n))) match_value) (= (first match_target) :dependencies) ((fn [deps] (list hydra_json_schema_serde_key_dependencies (list :object (hydra_lib_maps_from_list ((hydra_lib_lists_map hydra_json_schema_serde_encode_keyword_schema_or_array) (hydra_lib_maps_to_list deps)))))) match_value) (= (first match_target) :pattern_properties) ((fn [props] (list hydra_json_schema_serde_key_pattern_properties (list :object (hydra_lib_maps_from_list ((hydra_lib_lists_map hydra_json_schema_serde_encode_pattern_property) (hydra_lib_maps_to_list props)))))) match_value))) (second match_target))) r)))

(def hydra_json_schema_serde_encode_pattern_property (fn [p] (let [pat (hydra_lib_pairs_first p) s (hydra_lib_pairs_second p)] (list ((fn [v] v) pat) (hydra_json_schema_serde_encode_schema s)))))

(def hydra_json_schema_serde_encode_property (fn [p] (let [k (hydra_lib_pairs_first p) s (hydra_lib_pairs_second p)] (list ((fn [v] v) k) (hydra_json_schema_serde_encode_schema s)))))

(def hydra_json_schema_serde_encode_restriction (fn [r] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :type) ((fn [t_] (list (list hydra_json_schema_serde_key_type (hydra_json_schema_serde_encode_type t_)))) match_value) (= (first match_target) :string) ((fn [sr] (list (hydra_json_schema_serde_encode_string_restriction sr))) match_value) (= (first match_target) :number) ((fn [nr] (hydra_json_schema_serde_encode_numeric_restriction nr)) match_value) (= (first match_target) :array) ((fn [ar] (list (hydra_json_schema_serde_encode_array_restriction ar))) match_value) (= (first match_target) :object) ((fn [or_] (list (hydra_json_schema_serde_encode_object_restriction or_))) match_value) (= (first match_target) :multiple) ((fn [mr] (list (hydra_json_schema_serde_encode_multiple_restriction mr))) match_value) (= (first match_target) :reference) ((fn [sr] (list (list hydra_json_schema_serde_key_ref (hydra_json_schema_serde_encode_schema_reference sr)))) match_value) (= (first match_target) :title) ((fn [s] (list (list hydra_json_schema_serde_key_title (list :string s)))) match_value) (= (first match_target) :description) ((fn [s] (list (list hydra_json_schema_serde_key_description (list :string s)))) match_value))) (second match_target))) r)))

(def hydra_json_schema_serde_encode_schema (fn [s] (list :object (hydra_lib_maps_from_list (hydra_lib_lists_concat ((hydra_lib_lists_map hydra_json_schema_serde_encode_restriction) ((fn [v] v) s)))))))

(def hydra_json_schema_serde_encode_schema_or_array (fn [soa] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :schema) ((fn [s] (hydra_json_schema_serde_encode_schema s)) match_value) (= (first match_target) :array) ((fn [keys] (list :array ((hydra_lib_lists_map hydra_json_schema_serde_encode_keyword) keys))) match_value))) (second match_target))) soa)))

(def hydra_json_schema_serde_from_object (fn [v] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :object) ((fn [mp] mp) match_value))) (second match_target))) v)))

(def hydra_json_schema_serde_key_definitions "$defs")

(def hydra_json_schema_serde_key_id "$id")

(def hydra_json_schema_serde_key_schema "$schema")

(def hydra_json_schema_serde_to_object (fn [pairs] (list :object (hydra_lib_maps_from_list (hydra_lib_maybes_cat ((hydra_lib_lists_map (fn [p] (let [k (hydra_lib_pairs_first p) mv (hydra_lib_pairs_second p)] ((hydra_lib_maybes_map (fn [v] (list k v))) mv)))) pairs))))))

(def hydra_json_schema_serde_json_schema_document_to_json_value (fn [doc] (let [mdefs ((fn [v] (:definitions v)) doc) mid ((fn [v] (:id v)) doc) rest_map (hydra_json_schema_serde_from_object (hydra_json_schema_serde_to_object (list (list hydra_json_schema_serde_key_id ((hydra_lib_maybes_map (fn [i] (list :string i))) mid)) (list hydra_json_schema_serde_key_schema (hydra_lib_maybes_pure (list :string "http://json-schema.org/2020-12/schema"))) (list hydra_json_schema_serde_key_definitions ((hydra_lib_maybes_map (fn [mp] (list :object (hydra_lib_maps_from_list ((hydra_lib_lists_map (fn [p] (let [k (hydra_lib_pairs_first p) schema (hydra_lib_pairs_second p)] (list ((fn [v] v) k) (hydra_json_schema_serde_encode_schema schema))))) (hydra_lib_maps_to_list mp)))))) mdefs))))) root ((fn [v] (:root v)) doc) schema_map (hydra_json_schema_serde_from_object (hydra_json_schema_serde_encode_schema root))] (list :object ((hydra_lib_maps_union schema_map) rest_map)))))

(def hydra_json_schema_serde_json_schema_document_to_string (fn [doc] (hydra_json_writer_print_json (hydra_json_schema_serde_json_schema_document_to_json_value doc))))

(def hydra_json_schema_serde_key_label "label")

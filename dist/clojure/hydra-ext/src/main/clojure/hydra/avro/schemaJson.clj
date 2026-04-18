(ns hydra.avro.schemaJson
  (:require [hydra.avro.schema :refer :all] [hydra.coders :refer :all] [hydra.errors :refer :all] [hydra.json.model :refer :all] [hydra.json.parser :refer :all] [hydra.json.writer :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.strings :refer :all] [hydra.parsing :refer :all]
))

(declare hydra_avro_schema_json_avro_type hydra_avro_schema_json_avro_items hydra_avro_schema_json_err hydra_avro_schema_json_require_e hydra_avro_schema_json_avro_default hydra_avro_schema_json_avro_symbols hydra_avro_schema_json_expect_string_e hydra_avro_schema_json_opt_string_e hydra_avro_schema_json_expect_array_e hydra_avro_schema_json_require_array_e hydra_avro_schema_json_decode_enum hydra_avro_schema_json_avro_size hydra_avro_schema_json_expect_number_e hydra_avro_schema_json_require_number_e hydra_avro_schema_json_decode_fixed hydra_avro_schema_json_avro_values hydra_avro_schema_json_avro_doc hydra_avro_schema_json_avro_name hydra_avro_schema_json_avro_namespace hydra_avro_schema_json_avro_aliases hydra_avro_schema_json_opt_array_e hydra_avro_schema_json_decode_aliases hydra_avro_schema_json_get_annotations hydra_avro_schema_json_require_string_e hydra_avro_schema_json_decode_named_schema hydra_avro_schema_json_decode_primitive_name hydra_avro_schema_json_avro_fields hydra_avro_schema_json_avro_order hydra_avro_schema_json_decode_order hydra_avro_schema_json_opt_e hydra_avro_schema_json_expect_object_e hydra_avro_schema_json_show_json_value hydra_avro_schema_json_decode_array_schema hydra_avro_schema_json_decode_field hydra_avro_schema_json_decode_map_schema hydra_avro_schema_json_decode_object_schema hydra_avro_schema_json_decode_record hydra_avro_schema_json_decode_schema hydra_avro_schema_json_encode_annotations hydra_avro_schema_json_encode_enum hydra_avro_schema_json_encode_fixed hydra_avro_schema_json_encode_order hydra_avro_schema_json_encode_primitive hydra_avro_schema_json_encode_array hydra_avro_schema_json_encode_field hydra_avro_schema_json_encode_map hydra_avro_schema_json_encode_named hydra_avro_schema_json_encode_named_type hydra_avro_schema_json_encode_record hydra_avro_schema_json_encode_schema hydra_avro_schema_json_encode_union hydra_avro_schema_json_avro_schema_json_coder hydra_avro_schema_json_string_to_json_value hydra_avro_schema_json_avro_schema_string_coder hydra_avro_schema_json_avro_array hydra_avro_schema_json_avro_ascending hydra_avro_schema_json_avro_boolean hydra_avro_schema_json_avro_bytes hydra_avro_schema_json_avro_descending hydra_avro_schema_json_avro_double hydra_avro_schema_json_avro_enum hydra_avro_schema_json_avro_fixed hydra_avro_schema_json_avro_float hydra_avro_schema_json_avro_ignore hydra_avro_schema_json_avro_int hydra_avro_schema_json_avro_long hydra_avro_schema_json_avro_map hydra_avro_schema_json_avro_null hydra_avro_schema_json_avro_record hydra_avro_schema_json_avro_string hydra_avro_schema_json_unexpected_e)

(def hydra_avro_schema_json_avro_type "type")

(def hydra_avro_schema_json_avro_items "items")

(def hydra_avro_schema_json_err (fn [cx] (fn [msg] (list :left (list :other msg)))))

(def hydra_avro_schema_json_require_e (fn [cx] (fn [fname] (fn [m] (((hydra_lib_maybes_maybe (fn [] ((hydra_avro_schema_json_err cx) (hydra_lib_strings_cat (list "required attribute " (hydra_lib_literals_show_string fname) " not found"))))) (fn [v] (list :right v))) ((hydra_lib_maps_lookup fname) m))))))

(def hydra_avro_schema_json_avro_default "default")

(def hydra_avro_schema_json_avro_symbols "symbols")

(def hydra_avro_schema_json_expect_string_e (fn [cx] (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :string) ((fn [v] (list :right v)) match_value))) (second match_target))) value))))

(def hydra_avro_schema_json_opt_string_e (fn [cx] (fn [fname] (fn [m] (((hydra_lib_maybes_maybe (fn [] (list :right (list :nothing)))) (fn [v] ((hydra_lib_eithers_map (fn [s] (hydra_lib_maybes_pure s))) ((hydra_avro_schema_json_expect_string_e cx) v)))) ((hydra_lib_maps_lookup fname) m))))))

(def hydra_avro_schema_json_expect_array_e (fn [cx] (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :array) ((fn [v] (list :right v)) match_value))) (second match_target))) value))))

(def hydra_avro_schema_json_require_array_e (fn [cx] (fn [fname] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_e cx) fname) m)) (fn [v] ((hydra_avro_schema_json_expect_array_e cx) v)))))))

(def hydra_avro_schema_json_decode_enum (fn [cx] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_array_e cx) hydra_avro_schema_json_avro_symbols) m)) (fn [syms] ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (hydra_avro_schema_json_expect_string_e cx)) syms)) (fn [symbols] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_opt_string_e cx) hydra_avro_schema_json_avro_default) m)) (fn [def_val] (list :right (list :enum (->hydra_avro_schema_enum symbols def_val))))))))))))

(def hydra_avro_schema_json_avro_size "size")

(def hydra_avro_schema_json_expect_number_e (fn [cx] (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :number) ((fn [v] (list :right v)) match_value))) (second match_target))) value))))

(def hydra_avro_schema_json_require_number_e (fn [cx] (fn [fname] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_e cx) fname) m)) (fn [v] ((hydra_avro_schema_json_expect_number_e cx) v)))))))

(def hydra_avro_schema_json_decode_fixed (fn [cx] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_number_e cx) hydra_avro_schema_json_avro_size) m)) (fn [n] (let [size (hydra_lib_literals_bigint_to_int32 (hydra_lib_literals_decimal_to_bigint n))] (list :right (list :fixed (->hydra_avro_schema_fixed size)))))))))

(def hydra_avro_schema_json_avro_values "values")

(def hydra_avro_schema_json_avro_doc "doc")

(def hydra_avro_schema_json_avro_name "name")

(def hydra_avro_schema_json_avro_namespace "namespace")

(def hydra_avro_schema_json_avro_aliases "aliases")

(def hydra_avro_schema_json_opt_array_e (fn [cx] (fn [fname] (fn [m] (((hydra_lib_maybes_maybe (fn [] (list :right (list :nothing)))) (fn [v] ((hydra_lib_eithers_map (fn [a] (hydra_lib_maybes_pure a))) ((hydra_avro_schema_json_expect_array_e cx) v)))) ((hydra_lib_maps_lookup fname) m))))))

(def hydra_avro_schema_json_decode_aliases (fn [cx] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_opt_array_e cx) hydra_avro_schema_json_avro_aliases) m)) (fn [m_arr] (((hydra_lib_maybes_maybe (fn [] (list :right (list :nothing)))) (fn [arr] ((hydra_lib_eithers_map (fn [strs] (hydra_lib_maybes_pure strs))) ((hydra_lib_eithers_map_list (hydra_avro_schema_json_expect_string_e cx)) arr)))) m_arr))))))

(def hydra_avro_schema_json_get_annotations (fn [m] (hydra_lib_maps_from_list (hydra_lib_maybes_cat ((hydra_lib_lists_map (fn [entry] (let [k (hydra_lib_pairs_first entry) v (hydra_lib_pairs_second entry)] (if ((hydra_lib_equality_equal ((hydra_lib_maybes_from_maybe (fn [] 0)) ((hydra_lib_strings_maybe_char_at 0) k))) 64) (hydra_lib_maybes_pure (list (hydra_lib_strings_from_list ((hydra_lib_lists_drop 1) (hydra_lib_strings_to_list k))) v)) (list :nothing))))) (hydra_lib_maps_to_list m))))))

(def hydra_avro_schema_json_require_string_e (fn [cx] (fn [fname] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_e cx) fname) m)) (fn [v] ((hydra_avro_schema_json_expect_string_e cx) v)))))))

(def hydra_avro_schema_json_decode_named_schema (fn [cx] (fn [m] (fn [named_type_result] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_string_e cx) hydra_avro_schema_json_avro_name) m)) (fn [name] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_opt_string_e cx) hydra_avro_schema_json_avro_namespace) m)) (fn [ns_] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_opt_string_e cx) hydra_avro_schema_json_avro_doc) m)) (fn [sdoc] ((hydra_lib_eithers_bind ((hydra_avro_schema_json_decode_aliases cx) m)) (fn [aliases] ((hydra_lib_eithers_bind named_type_result) (fn [named_type] (list :right (list :named (->hydra_avro_schema_named name ns_ aliases sdoc named_type (hydra_avro_schema_json_get_annotations m))))))))))))))))))

(def hydra_avro_schema_json_decode_primitive_name (fn [s] (if ((hydra_lib_equality_equal s) "null") (list :just (list :null nil)) (if ((hydra_lib_equality_equal s) "boolean") (list :just (list :boolean nil)) (if ((hydra_lib_equality_equal s) "int") (list :just (list :int nil)) (if ((hydra_lib_equality_equal s) "long") (list :just (list :long nil)) (if ((hydra_lib_equality_equal s) "float") (list :just (list :float nil)) (if ((hydra_lib_equality_equal s) "double") (list :just (list :double nil)) (if ((hydra_lib_equality_equal s) "bytes") (list :just (list :bytes nil)) (if ((hydra_lib_equality_equal s) "string") (list :just (list :string nil)) (list :nothing)))))))))))

(def hydra_avro_schema_json_avro_fields "fields")

(def hydra_avro_schema_json_avro_order "order")

(def hydra_avro_schema_json_decode_order (fn [cx] (fn [o] (if ((hydra_lib_equality_equal o) "ascending") (list :right (list :ascending nil)) (if ((hydra_lib_equality_equal o) "descending") (list :right (list :descending nil)) (if ((hydra_lib_equality_equal o) "ignore") (list :right (list :ignore nil)) ((hydra_avro_schema_json_err cx) (hydra_lib_strings_cat (list "unknown order: " o)))))))))

(def hydra_avro_schema_json_opt_e (fn [k] (fn [m] ((hydra_lib_maps_lookup k) m))))

(def hydra_avro_schema_json_expect_object_e (fn [cx] (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :object) ((fn [v] (list :right v)) match_value))) (second match_target))) value))))

(def hydra_avro_schema_json_show_json_value (fn [v] (hydra_json_writer_print_json v)))

(def hydra_avro_schema_json_decode_array_schema (fn [cx] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_e cx) hydra_avro_schema_json_avro_items) m)) (fn [items] ((hydra_lib_eithers_map (fn [s] (list :array (->hydra_avro_schema_array s)))) ((hydra_avro_schema_json_decode_schema cx) items)))))))

(def hydra_avro_schema_json_decode_field (fn [cx] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_string_e cx) hydra_avro_schema_json_avro_name) m)) (fn [name] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_opt_string_e cx) hydra_avro_schema_json_avro_doc) m)) (fn [fdoc] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_e cx) hydra_avro_schema_json_avro_type) m)) (fn [type_json] ((hydra_lib_eithers_bind ((hydra_avro_schema_json_decode_schema cx) type_json)) (fn [field_type] ((hydra_lib_eithers_bind ((hydra_lib_eithers_bind (((hydra_avro_schema_json_opt_string_e cx) hydra_avro_schema_json_avro_order) m)) (fn [m_ord] ((hydra_lib_eithers_map_maybe (hydra_avro_schema_json_decode_order cx)) m_ord)))) (fn [order] ((hydra_lib_eithers_bind ((hydra_avro_schema_json_decode_aliases cx) m)) (fn [aliases] (list :right (->hydra_avro_schema_field name fdoc field_type ((hydra_avro_schema_json_opt_e hydra_avro_schema_json_avro_default) m) order aliases (hydra_avro_schema_json_get_annotations m))))))))))))))))))

(def hydra_avro_schema_json_decode_map_schema (fn [cx] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_e cx) hydra_avro_schema_json_avro_values) m)) (fn [values_] ((hydra_lib_eithers_map (fn [s] (list :map (->hydra_avro_schema_map s)))) ((hydra_avro_schema_json_decode_schema cx) values_)))))))

(def hydra_avro_schema_json_decode_object_schema (fn [cx] (fn [m] (fn [type_name] (if ((hydra_lib_equality_equal type_name) "array") ((hydra_avro_schema_json_decode_array_schema cx) m) (if ((hydra_lib_equality_equal type_name) "map") ((hydra_avro_schema_json_decode_map_schema cx) m) (if ((hydra_lib_equality_equal type_name) "record") (((hydra_avro_schema_json_decode_named_schema cx) m) ((hydra_avro_schema_json_decode_record cx) m)) (if ((hydra_lib_equality_equal type_name) "enum") (((hydra_avro_schema_json_decode_named_schema cx) m) ((hydra_avro_schema_json_decode_enum cx) m)) (if ((hydra_lib_equality_equal type_name) "fixed") (((hydra_avro_schema_json_decode_named_schema cx) m) ((hydra_avro_schema_json_decode_fixed cx) m)) (((hydra_lib_maybes_maybe (fn [] ((hydra_avro_schema_json_err cx) (hydra_lib_strings_cat (list "unknown type: " type_name))))) (fn [p] (list :right (list :primitive p)))) (hydra_avro_schema_json_decode_primitive_name type_name)))))))))))

(def hydra_avro_schema_json_decode_record (fn [cx] (fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_array_e cx) hydra_avro_schema_json_avro_fields) m)) (fn [field_jsons] ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (fn [fj] ((hydra_lib_eithers_bind ((hydra_avro_schema_json_expect_object_e cx) fj)) (fn [fm] ((hydra_avro_schema_json_decode_field cx) fm))))) field_jsons)) (fn [fields] (list :right (list :record (->hydra_avro_schema_record fields))))))))))

(def hydra_avro_schema_json_decode_schema (fn [cx] (fn [v] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :string) ((fn [s] (((hydra_lib_maybes_maybe (fn [] (list :right (list :reference s)))) (fn [p] (list :right (list :primitive p)))) (hydra_avro_schema_json_decode_primitive_name s))) match_value) (= (first match_target) :array) ((fn [schemas] ((hydra_lib_eithers_map (fn [decoded] (list :union decoded))) ((hydra_lib_eithers_map_list (hydra_avro_schema_json_decode_schema cx)) schemas))) match_value) (= (first match_target) :object) ((fn [m] ((hydra_lib_eithers_bind (((hydra_avro_schema_json_require_string_e cx) hydra_avro_schema_json_avro_type) m)) (fn [type_name] (((hydra_avro_schema_json_decode_object_schema cx) m) type_name)))) match_value) :else ((hydra_avro_schema_json_err cx) (hydra_lib_strings_cat (list "unexpected JSON value for schema: " (hydra_avro_schema_json_show_json_value v)))))) (second match_target))) v))))

(def hydra_avro_schema_json_encode_annotations (fn [m] ((hydra_lib_lists_map (fn [entry] (list ((hydra_lib_strings_cat2 "@") (hydra_lib_pairs_first entry)) (hydra_lib_pairs_second entry)))) (hydra_lib_maps_to_list m))))

(def hydra_avro_schema_json_encode_enum (fn [e] (hydra_lib_lists_concat (list (list (list "type" (list :string "enum"))) (list (list "symbols" (list :array ((hydra_lib_lists_map (fn [s] (list :string s))) ((fn [v] (:symbols v)) e))))) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [d] (list (list "default" (list :string d))))) ((fn [v] (:default v)) e))))))

(def hydra_avro_schema_json_encode_fixed (fn [f] (list (list "type" (list :string "fixed")) (list "size" (list :number (hydra_lib_literals_bigint_to_decimal (hydra_lib_literals_int32_to_bigint ((fn [v] (:size v)) f))))))))

(def hydra_avro_schema_json_encode_order (fn [o] (list "order" (list :string ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :ascending) ((fn [_] "ascending") match_value) (= (first match_target) :descending) ((fn [_] "descending") match_value) (= (first match_target) :ignore) ((fn [_] "ignore") match_value))) (second match_target))) o)))))

(def hydra_avro_schema_json_encode_primitive (fn [p] (list :string ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :null) ((fn [_] "null") match_value) (= (first match_target) :boolean) ((fn [_] "boolean") match_value) (= (first match_target) :int) ((fn [_] "int") match_value) (= (first match_target) :long) ((fn [_] "long") match_value) (= (first match_target) :float) ((fn [_] "float") match_value) (= (first match_target) :double) ((fn [_] "double") match_value) (= (first match_target) :bytes) ((fn [_] "bytes") match_value) (= (first match_target) :string) ((fn [_] "string") match_value))) (second match_target))) p))))

(def hydra_avro_schema_json_encode_array (fn [arr] (list :object (hydra_lib_maps_from_list (list (list "type" (list :string "array")) (list "items" (hydra_avro_schema_json_encode_schema ((fn [v] (:items v)) arr))))))))

(def hydra_avro_schema_json_encode_field (fn [f] (list :object (hydra_lib_maps_from_list (hydra_lib_lists_concat (list (list (list "name" (list :string ((fn [v] (:name v)) f)))) (list (list "type" (hydra_avro_schema_json_encode_schema ((fn [v] (:type v)) f)))) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [d] (list (list "doc" (list :string d))))) ((fn [v] (:doc v)) f)) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [d] (list (list "default" d)))) ((fn [v] (:default v)) f)) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [o] (list (hydra_avro_schema_json_encode_order o)))) ((fn [v] (:order v)) f)) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [als] (list (list "aliases" (list :array ((hydra_lib_lists_map (fn [a] (list :string a))) als)))))) ((fn [v] (:aliases v)) f)) (hydra_avro_schema_json_encode_annotations ((fn [v] (:annotations v)) f))))))))

(def hydra_avro_schema_json_encode_map (fn [mp] (list :object (hydra_lib_maps_from_list (list (list "type" (list :string "map")) (list "values" (hydra_avro_schema_json_encode_schema ((fn [v] (:values v)) mp))))))))

(def hydra_avro_schema_json_encode_named (fn [n] (list :object (hydra_lib_maps_from_list (hydra_lib_lists_concat (list (list (list "name" (list :string ((fn [v] (:name v)) n)))) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [ns_] (list (list "namespace" (list :string ns_))))) ((fn [v] (:namespace v)) n)) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [d] (list (list "doc" (list :string d))))) ((fn [v] (:doc v)) n)) (((hydra_lib_maybes_maybe (fn [] (list))) (fn [als] (list (list "aliases" (list :array ((hydra_lib_lists_map (fn [a] (list :string a))) als)))))) ((fn [v] (:aliases v)) n)) (hydra_avro_schema_json_encode_named_type ((fn [v] (:type v)) n)) (hydra_avro_schema_json_encode_annotations ((fn [v] (:annotations v)) n))))))))

(def hydra_avro_schema_json_encode_named_type (fn [nt] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :enum) ((fn [e] (hydra_avro_schema_json_encode_enum e)) match_value) (= (first match_target) :fixed) ((fn [f] (hydra_avro_schema_json_encode_fixed f)) match_value) (= (first match_target) :record) ((fn [r] (hydra_avro_schema_json_encode_record r)) match_value))) (second match_target))) nt)))

(def hydra_avro_schema_json_encode_record (fn [r] (list (list "type" (list :string "record")) (list "fields" (list :array ((hydra_lib_lists_map hydra_avro_schema_json_encode_field) ((fn [v] (:fields v)) r)))))))

(def hydra_avro_schema_json_encode_schema (fn [schema] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :primitive) ((fn [p] (hydra_avro_schema_json_encode_primitive p)) match_value) (= (first match_target) :array) ((fn [arr] (hydra_avro_schema_json_encode_array arr)) match_value) (= (first match_target) :map) ((fn [mp] (hydra_avro_schema_json_encode_map mp)) match_value) (= (first match_target) :named) ((fn [n] (hydra_avro_schema_json_encode_named n)) match_value) (= (first match_target) :reference) ((fn [ref] (list :string ref)) match_value) (= (first match_target) :union) ((fn [u] (hydra_avro_schema_json_encode_union u)) match_value))) (second match_target))) schema)))

(def hydra_avro_schema_json_encode_union (fn [u] (list :array ((hydra_lib_lists_map hydra_avro_schema_json_encode_schema) ((fn [v] v) u)))))

(def hydra_avro_schema_json_avro_schema_json_coder (fn [cx] (->hydra_coders_coder (fn [_cx] (fn [schema] (list :right (hydra_avro_schema_json_encode_schema schema)))) (fn [cx2] (fn [json] ((hydra_avro_schema_json_decode_schema cx2) json))))))

(def hydra_avro_schema_json_string_to_json_value (fn [s] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :success) ((fn [success] (list :right ((fn [v] (:value v)) success))) match_value) (= (first match_target) :failure) ((fn [failure] (list :left ((fn [v] (:message v)) failure))) match_value))) (second match_target))) (hydra_json_parser_parse_json s))))

(def hydra_avro_schema_json_avro_schema_string_coder (fn [cx] (->hydra_coders_coder (fn [_cx] (fn [schema] (list :right (hydra_avro_schema_json_show_json_value (hydra_avro_schema_json_encode_schema schema))))) (fn [cx2] (fn [s] ((hydra_lib_eithers_bind (((hydra_lib_eithers_either (fn [e] ((hydra_avro_schema_json_err cx2) e))) (fn [v] (list :right v))) (hydra_avro_schema_json_string_to_json_value s))) (fn [json] ((hydra_avro_schema_json_decode_schema cx2) json))))))))

(def hydra_avro_schema_json_avro_array "array")

(def hydra_avro_schema_json_avro_ascending "ascending")

(def hydra_avro_schema_json_avro_boolean "boolean")

(def hydra_avro_schema_json_avro_bytes "bytes")

(def hydra_avro_schema_json_avro_descending "descending")

(def hydra_avro_schema_json_avro_double "double")

(def hydra_avro_schema_json_avro_enum "enum")

(def hydra_avro_schema_json_avro_fixed "fixed")

(def hydra_avro_schema_json_avro_float "float")

(def hydra_avro_schema_json_avro_ignore "ignore")

(def hydra_avro_schema_json_avro_int "int")

(def hydra_avro_schema_json_avro_long "long")

(def hydra_avro_schema_json_avro_map "map")

(def hydra_avro_schema_json_avro_null "null")

(def hydra_avro_schema_json_avro_record "record")

(def hydra_avro_schema_json_avro_string "string")

(def hydra_avro_schema_json_unexpected_e (fn [cx] (fn [expected] (fn [found] ((hydra_avro_schema_json_err cx) (hydra_lib_strings_cat (list "Expected " expected ", found: " found)))))))

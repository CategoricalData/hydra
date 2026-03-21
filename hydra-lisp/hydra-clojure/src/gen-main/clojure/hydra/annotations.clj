(ns hydra.annotations
  (:require [hydra.classes :refer :all] [hydra.constants :refer :all] [hydra.context :refer :all] [hydra.core :refer :all] [hydra.decode.core :refer :all] [hydra.encode.core :refer :all] [hydra.error :refer :all] [hydra.extract.core :refer :all] [hydra.graph :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.math :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.sets :refer :all] [hydra.lib.strings :refer :all] [hydra.rewriting :refer :all] [hydra.show.core :refer :all]
))

(declare hydra_annotations_aggregate_annotations hydra_annotations_get_attr hydra_annotations_get_debug_id hydra_annotations_debug_if hydra_annotations_get_attr_with_default hydra_annotations_has_flag hydra_annotations_fail_on_flag hydra_annotations_get_count hydra_annotations_get_description hydra_annotations_term_annotation_internal hydra_annotations_get_term_annotation hydra_annotations_get_term_description hydra_annotations_get_type hydra_annotations_type_annotation_internal hydra_annotations_get_type_annotation hydra_annotations_get_type_classes hydra_annotations_get_type_description hydra_annotations_has_description hydra_annotations_has_type_description hydra_annotations_is_native_type hydra_annotations_put_attr hydra_annotations_put_count hydra_annotations_next_count hydra_annotations_normalize_term_annotations hydra_annotations_normalize_type_annotations hydra_annotations_reset_count hydra_annotations_set_annotation hydra_annotations_set_description hydra_annotations_set_term_annotation hydra_annotations_set_term_description hydra_annotations_set_type hydra_annotations_set_type_annotation hydra_annotations_set_type_classes hydra_annotations_set_type_description hydra_annotations_type_element hydra_annotations_when_flag)

(def hydra_annotations_aggregate_annotations (fn [get_value] (fn [get_x] (fn [get_anns] (fn [t_] (let [to_pairs (fn to_pairs [rest] (fn [t_] (((hydra_lib_maybes_maybe (fn [] rest)) (fn [yy] ((to_pairs ((hydra_lib_lists_cons (hydra_lib_maps_to_list (get_anns yy))) rest)) (get_x yy)))) (get_value t_))))] (hydra_lib_maps_from_list (hydra_lib_lists_concat ((to_pairs (list)) t_)))))))))

(def hydra_annotations_get_attr (fn [key] (fn [cx] ((hydra_lib_maps_lookup key) ((fn [v] (:other v)) cx)))))

(def hydra_annotations_get_debug_id (fn [cx] (((hydra_lib_maybes_maybe (fn [] (list :right (list :nothing)))) (fn [term] ((hydra_lib_eithers_map hydra_lib_maybes_pure) (((hydra_extract_core_string cx) (->hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))) ((hydra_annotations_get_attr hydra_constants_key_debug_id) cx))))

(def hydra_annotations_debug_if (fn [cx] (fn [debug_id] (fn [message] ((hydra_lib_eithers_bind (hydra_annotations_get_debug_id cx)) (fn [mid] (if ((hydra_lib_equality_equal mid) (list :just debug_id)) (list :left (->hydra_context_in_context (list :other message) cx)) (list :right nil))))))))

(def hydra_annotations_get_attr_with_default (fn [key] (fn [def_] (fn [cx] ((hydra_lib_maybes_from_maybe (fn [] def_)) ((hydra_annotations_get_attr key) cx))))))

(def hydra_annotations_has_flag (fn [cx] (fn [flag] (let [term (((hydra_annotations_get_attr_with_default flag) (list :literal (list :boolean false))) cx)] (((hydra_extract_core_boolean cx) (->hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))))

(def hydra_annotations_fail_on_flag (fn [cx] (fn [flag] (fn [msg] ((hydra_lib_eithers_bind ((hydra_annotations_has_flag cx) flag)) (fn [val] (if val (list :left (->hydra_context_in_context (list :other msg) cx)) (list :right nil))))))))

(def hydra_annotations_get_count (fn [key] (fn [cx] (((hydra_lib_maybes_maybe (fn [] 0)) (fn [term] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :literal) ((fn [lit] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :integer) ((fn [iv] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :int32) ((fn [i] i) match_value) :else 0)) (second match_target))) iv)) match_value) :else 0)) (second match_target))) lit)) match_value) :else 0)) (second match_target))) term))) ((hydra_lib_maps_lookup key) ((fn [v] (:other v)) cx))))))

(def hydra_annotations_get_description (fn [cx] (fn [graph] (fn [anns] (((hydra_lib_maybes_maybe (fn [] (list :right (list :nothing)))) (fn [term] ((hydra_lib_eithers_map hydra_lib_maybes_pure) (((hydra_extract_core_string cx) graph) term)))) ((hydra_lib_maps_lookup "description") anns))))))

(def hydra_annotations_term_annotation_internal (fn [term] (let [get_ann (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :annotated) ((fn [a] (list :just a)) match_value) :else (list :nothing))) (second match_target))) t_))] ((((hydra_annotations_aggregate_annotations get_ann) (fn [at] ((fn [v] (:body v)) at))) (fn [at] ((fn [v] (:annotation v)) at))) term))))

(def hydra_annotations_get_term_annotation (fn [key] (fn [term] ((hydra_lib_maps_lookup key) (hydra_annotations_term_annotation_internal term)))))

(def hydra_annotations_get_term_description (fn [cx] (fn [graph] (fn [term] (let [peel (fn peel [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :type_lambda) ((fn [tl] (peel ((fn [v] (:body v)) tl))) match_value) (= (first match_target) :type_application) ((fn [ta] (peel ((fn [v] (:body v)) ta))) match_value) :else t_)) (second match_target))) t_))] (((hydra_annotations_get_description cx) graph) (hydra_annotations_term_annotation_internal (peel term))))))))

(def hydra_annotations_get_type (fn [graph] (fn [anns] (((hydra_lib_maybes_maybe (fn [] (list :right (list :nothing)))) (fn [dat] ((hydra_lib_eithers_map hydra_lib_maybes_pure) ((hydra_decode_core_type graph) dat)))) ((hydra_lib_maps_lookup hydra_constants_key_type) anns)))))

(def hydra_annotations_type_annotation_internal (fn [typ] (let [get_ann (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :annotated) ((fn [a] (list :just a)) match_value) :else (list :nothing))) (second match_target))) t_))] ((((hydra_annotations_aggregate_annotations get_ann) (fn [at] ((fn [v] (:body v)) at))) (fn [at] ((fn [v] (:annotation v)) at))) typ))))

(def hydra_annotations_get_type_annotation (fn [key] (fn [typ] ((hydra_lib_maps_lookup key) (hydra_annotations_type_annotation_internal typ)))))

(def hydra_annotations_get_type_classes (fn [cx] (fn [graph] (fn [term] (let [decode_class (fn [term] (let [by_name (hydra_lib_maps_from_list (list (list "equality" (list :equality nil)) (list "ordering" (list :ordering nil))))] ((hydra_lib_eithers_bind ((((hydra_extract_core_unit_variant cx) "hydra.classes.TypeClass") graph) term)) (fn [fn_] (((hydra_lib_maybes_maybe (fn [] (list :left (->hydra_context_in_context (list :other ((hydra_lib_strings_cat2 "unexpected: expected type class, got ") (hydra_show_core_term term))) cx)))) (fn [x] (list :right x))) ((hydra_lib_maps_lookup fn_) by_name))))))] (((hydra_lib_maybes_maybe (fn [] (list :right hydra_lib_maps_empty))) (fn [term] (((((hydra_extract_core_map cx) (fn [t_] (((hydra_lib_eithers_bimap (fn [de] (->hydra_context_in_context (list :other ((fn [v] v) de)) cx))) (fn [x] x)) ((hydra_decode_core_name graph) t_)))) (((hydra_extract_core_set_of cx) decode_class) graph)) graph) term))) ((hydra_annotations_get_term_annotation hydra_constants_key_classes) term)))))))

(def hydra_annotations_get_type_description (fn [cx] (fn [graph] (fn [typ] (((hydra_annotations_get_description cx) graph) (hydra_annotations_type_annotation_internal typ))))))

(def hydra_annotations_has_description (fn [anns] (hydra_lib_maybes_is_just ((hydra_lib_maps_lookup hydra_constants_key_description) anns))))

(def hydra_annotations_has_type_description (fn [typ] (hydra_annotations_has_description (hydra_annotations_type_annotation_internal typ))))

(def hydra_annotations_is_native_type (fn [el] (let [is_flagged_as_first_class_type ((hydra_lib_maybes_from_maybe (fn [] false)) ((hydra_lib_maybes_map (fn [_] true)) ((hydra_annotations_get_term_annotation hydra_constants_key_first_class_type) ((fn [v] (:term v)) el))))] (((hydra_lib_maybes_maybe (fn [] false)) (fn [ts] ((hydra_lib_logic_and ((hydra_lib_equality_equal ts) (->hydra_core_type_scheme (list) (list :variable "hydra.core.Type") (list :nothing)))) (hydra_lib_logic_not is_flagged_as_first_class_type)))) ((fn [v] (:type v)) el)))))

(def hydra_annotations_put_attr (fn [key] (fn [val] (fn [cx] (->hydra_context_context ((fn [v] (:trace v)) cx) ((fn [v] (:messages v)) cx) (((hydra_lib_maps_insert key) val) ((fn [v] (:other v)) cx)))))))

(def hydra_annotations_put_count (fn [key] (fn [count] (fn [cx] (((hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 count)))) cx)))))

(def hydra_annotations_next_count (fn [key] (fn [cx] (let [count ((hydra_annotations_get_count key) cx)] (list count (((hydra_annotations_put_count key) ((hydra_lib_math_add count) 1)) cx))))))

(def hydra_annotations_normalize_term_annotations (fn [term] (let [anns (hydra_annotations_term_annotation_internal term)] (let [stripped (hydra_rewriting_deannotate_term term)] (if (hydra_lib_maps_null anns) stripped (list :annotated (->hydra_core_annotated_term stripped anns)))))))

(def hydra_annotations_normalize_type_annotations (fn [typ] (let [anns (hydra_annotations_type_annotation_internal typ)] (let [stripped (hydra_rewriting_deannotate_type typ)] (if (hydra_lib_maps_null anns) stripped (list :annotated (->hydra_core_annotated_type stripped anns)))))))

(def hydra_annotations_reset_count (fn [key] (fn [cx] (((hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 0)))) cx))))

(def hydra_annotations_set_annotation (fn [key] (fn [val] (fn [m] (((hydra_lib_maps_alter (fn [_] val)) key) m)))))

(def hydra_annotations_set_description (fn [d] ((hydra_annotations_set_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (fn [arg_] ((fn [x] (list :literal x)) ((fn [x] (list :string x)) arg_)))) d))))

(def hydra_annotations_set_term_annotation (fn [key] (fn [val] (fn [term] (let [term_ (hydra_rewriting_deannotate_term term)] (let [anns (((hydra_annotations_set_annotation key) val) (hydra_annotations_term_annotation_internal term))] (if (hydra_lib_maps_null anns) term_ (list :annotated (->hydra_core_annotated_term term_ anns)))))))))

(def hydra_annotations_set_term_description (fn [d] ((hydra_annotations_set_term_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (fn [s] (list :literal (list :string s)))) d))))

(def hydra_annotations_set_type (fn [mt] ((hydra_annotations_set_annotation hydra_constants_key_type) ((hydra_lib_maybes_map hydra_encode_core_type) mt))))

(def hydra_annotations_set_type_annotation (fn [key] (fn [val] (fn [typ] (let [typ_ (hydra_rewriting_deannotate_type typ)] (let [anns (((hydra_annotations_set_annotation key) val) (hydra_annotations_type_annotation_internal typ))] (if (hydra_lib_maps_null anns) typ_ (list :annotated (->hydra_core_annotated_type typ_ anns)))))))))

(def hydra_annotations_set_type_classes (fn [m] (fn [term] (let [encode_class (fn [tc] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :equality) ((fn [_] (list :union (->hydra_core_injection "hydra.classes.TypeClass" (->hydra_core_field "equality" (list :unit nil))))) match_value) (= (first match_target) :ordering) ((fn [_] (list :union (->hydra_core_injection "hydra.classes.TypeClass" (->hydra_core_field "ordering" (list :unit nil))))) match_value))) (second match_target))) tc))] (let [encode_pair (fn [name_classes] (let [name (hydra_lib_pairs_first name_classes)] (let [classes (hydra_lib_pairs_second name_classes)] (list (hydra_encode_core_name name) (list :set (hydra_lib_sets_from_list ((hydra_lib_lists_map encode_class) (hydra_lib_sets_to_list classes))))))))] (let [encoded (if (hydra_lib_maps_null m) (list :nothing) (list :just (list :map (hydra_lib_maps_from_list ((hydra_lib_lists_map encode_pair) (hydra_lib_maps_to_list m))))))] (((hydra_annotations_set_term_annotation hydra_constants_key_classes) encoded) term)))))))

(def hydra_annotations_set_type_description (fn [d] ((hydra_annotations_set_type_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (fn [arg_] ((fn [x] (list :literal x)) ((fn [x] (list :string x)) arg_)))) d))))

(def hydra_annotations_type_element (fn [name] (fn [typ] (let [schema_term (list :variable "hydra.core.Type")] (let [data_term (hydra_annotations_normalize_term_annotations (list :annotated (->hydra_core_annotated_term (hydra_encode_core_type typ) (hydra_lib_maps_from_list (list (list hydra_constants_key_type schema_term))))))] (->hydra_core_binding name data_term (list :just (->hydra_core_type_scheme (list) (list :variable "hydra.core.Type") (list :nothing)))))))))

(def hydra_annotations_when_flag (fn [cx] (fn [flag] (fn [ethen] (fn [eelse] ((hydra_lib_eithers_bind ((hydra_annotations_has_flag cx) flag)) (fn [b] (if b ethen eelse))))))))

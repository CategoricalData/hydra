(defpackage :hydra.annotations
(:use :cl :hydra.classes :hydra.constants :hydra.context :hydra.core :hydra.decode.core :hydra.encode.core :hydra.errors :hydra.extract.core :hydra.graph :hydra.lib.eithers :hydra.lib.equality :hydra.lib.lists :hydra.lib.logic :hydra.lib.maps :hydra.lib.math :hydra.lib.maybes :hydra.lib.pairs :hydra.lib.sets :hydra.show.core :hydra.strip)
(:export :hydra_annotations_aggregate_annotations :hydra_annotations_get_description :hydra_annotations_term_annotation_internal :hydra_annotations_get_term_description :hydra_annotations_comments_from_binding :hydra_annotations_type_annotation_internal :hydra_annotations_get_type_description :hydra_annotations_comments_from_field_type :hydra_annotations_get_attr :hydra_annotations_get_debug_id :hydra_annotations_debug_if :hydra_annotations_get_attr_with_default :hydra_annotations_has_flag :hydra_annotations_fail_on_flag :hydra_annotations_get_count :hydra_annotations_get_term_annotation :hydra_annotations_get_type :hydra_annotations_get_type_annotation :hydra_annotations_get_type_classes :hydra_annotations_has_description :hydra_annotations_has_type_description :hydra_annotations_is_native_type :hydra_annotations_put_attr :hydra_annotations_put_count :hydra_annotations_next_count :hydra_annotations_normalize_term_annotations :hydra_annotations_normalize_type_annotations :hydra_annotations_reset_count :hydra_annotations_set_annotation :hydra_annotations_set_description :hydra_annotations_set_term_annotation :hydra_annotations_set_term_description :hydra_annotations_set_type :hydra_annotations_set_type_annotation :hydra_annotations_set_type_classes :hydra_annotations_set_type_description :hydra_annotations_when_flag))

(in-package :hydra.annotations)

(cl:defvar hydra_annotations_aggregate_annotations (cl:lambda (get_value) (cl:lambda (get_x) (cl:lambda (get_anns) (cl:lambda (t_) (letrec ((to_pairs (cl:lambda (rest) (cl:lambda (t2) (((hydra_lib_maybes_maybe (cl:lambda () rest)) (cl:lambda (yy) ((to_pairs ((hydra_lib_lists_cons (hydra_lib_maps_to_list (get_anns yy))) rest)) (get_x yy)))) (get_value t2)))))) (hydra_lib_maps_from_list (hydra_lib_lists_concat ((to_pairs (cl:list)) t_)))))))))

(cl:defvar hydra_annotations_get_description (cl:lambda (cx) (cl:lambda (graph) (cl:lambda (anns) (((hydra_lib_maybes_maybe (cl:lambda () (list :right (list :nothing)))) (cl:lambda (term) ((hydra_lib_eithers_map hydra_lib_maybes_pure) ((hydra_extract_core_string graph) term)))) ((hydra_lib_maps_lookup "description") anns))))))

(cl:defvar hydra_annotations_term_annotation_internal (cl:lambda (term) (let ((get_ann (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :annotated) ((cl:lambda (a) (list :just a)) match_value)) (t (list :nothing)))) (cadr match_target))) t_)))) ((((hydra_annotations_aggregate_annotations get_ann) (cl:lambda (at) ((cl:lambda (v) (hydra_core_annotated_term-body v)) at))) (cl:lambda (at) ((cl:lambda (v) (hydra_core_annotated_term-annotation v)) at))) term))))

(cl:defvar hydra_annotations_get_term_description (cl:lambda (cx) (cl:lambda (graph) (cl:lambda (term) (letrec ((peel (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :type_lambda) ((cl:lambda (tl) (peel ((cl:lambda (v) (hydra_core_type_lambda-body v)) tl))) match_value)) ((equal (car match_target) :type_application) ((cl:lambda (ta) (peel ((cl:lambda (v) (hydra_core_type_application_term-body v)) ta))) match_value)) (t t_))) (cadr match_target))) t_)))) (((hydra_annotations_get_description cx) graph) (hydra_annotations_term_annotation_internal (peel term))))))))

(cl:defvar hydra_annotations_comments_from_binding (cl:lambda (cx) (cl:lambda (g) (cl:lambda (b) (((hydra_annotations_get_term_description cx) g) ((cl:lambda (v) (hydra_core_binding-term v)) b))))))

(cl:defvar hydra_annotations_type_annotation_internal (cl:lambda (typ) (let ((get_ann (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :annotated) ((cl:lambda (a) (list :just a)) match_value)) (t (list :nothing)))) (cadr match_target))) t_)))) ((((hydra_annotations_aggregate_annotations get_ann) (cl:lambda (at) ((cl:lambda (v) (hydra_core_annotated_type-body v)) at))) (cl:lambda (at) ((cl:lambda (v) (hydra_core_annotated_type-annotation v)) at))) typ))))

(cl:defvar hydra_annotations_get_type_description (cl:lambda (cx) (cl:lambda (graph) (cl:lambda (typ) (((hydra_annotations_get_description cx) graph) (hydra_annotations_type_annotation_internal typ))))))

(cl:defvar hydra_annotations_comments_from_field_type (cl:lambda (cx) (cl:lambda (g) (cl:lambda (ft) (((hydra_annotations_get_type_description cx) g) ((cl:lambda (v) (hydra_core_field_type-type v)) ft))))))

(cl:defvar hydra_annotations_get_attr (cl:lambda (key) (cl:lambda (cx) ((hydra_lib_maps_lookup key) ((cl:lambda (v) (hydra_context_context-other v)) cx)))))

(cl:defvar hydra_annotations_get_debug_id (cl:lambda (cx) (((hydra_lib_maybes_maybe (cl:lambda () (list :right (list :nothing)))) (cl:lambda (term) ((hydra_lib_eithers_map hydra_lib_maybes_pure) ((hydra_extract_core_string (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))) ((hydra_annotations_get_attr hydra_constants_key_debug_id) cx))))

(cl:defvar hydra_annotations_debug_if (cl:lambda (cx) (cl:lambda (debug_id) (cl:lambda (message) ((hydra_lib_eithers_bind (hydra_annotations_get_debug_id cx)) (cl:lambda (mid) (if ((hydra_lib_equality_equal mid) (list :just debug_id)) (list :left (list :other message)) (list :right cl:nil))))))))

(cl:defvar hydra_annotations_get_attr_with_default (cl:lambda (key) (cl:lambda (def_) (cl:lambda (cx) ((hydra_lib_maybes_from_maybe (cl:lambda () def_)) ((hydra_annotations_get_attr key) cx))))))

(cl:defvar hydra_annotations_has_flag (cl:lambda (cx) (cl:lambda (flag) (let ((term (((hydra_annotations_get_attr_with_default flag) (list :literal (list :boolean cl:nil))) cx))) ((hydra_extract_core_boolean (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))))

(cl:defvar hydra_annotations_fail_on_flag (cl:lambda (cx) (cl:lambda (flag) (cl:lambda (msg) ((hydra_lib_eithers_bind ((hydra_annotations_has_flag cx) flag)) (cl:lambda (val) (if val (list :left (list :other msg)) (list :right cl:nil))))))))

(cl:defvar hydra_annotations_get_count (cl:lambda (key) (cl:lambda (cx) (((hydra_lib_maybes_maybe (cl:lambda () 0)) (cl:lambda (term) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :literal) ((cl:lambda (lit) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :integer) ((cl:lambda (iv) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :int32) ((cl:lambda (i) i) match_value)) (t 0))) (cadr match_target))) iv)) match_value)) (t 0))) (cadr match_target))) lit)) match_value)) (t 0))) (cadr match_target))) term))) ((hydra_lib_maps_lookup key) ((cl:lambda (v) (hydra_context_context-other v)) cx))))))

(cl:defvar hydra_annotations_get_term_annotation (cl:lambda (key) (cl:lambda (term) ((hydra_lib_maps_lookup key) (hydra_annotations_term_annotation_internal term)))))

(cl:defvar hydra_annotations_get_type (cl:lambda (graph) (cl:lambda (anns) (((hydra_lib_maybes_maybe (cl:lambda () (list :right (list :nothing)))) (cl:lambda (dat) ((hydra_lib_eithers_map hydra_lib_maybes_pure) ((hydra_decode_core_type graph) dat)))) ((hydra_lib_maps_lookup hydra_constants_key_type) anns)))))

(cl:defvar hydra_annotations_get_type_annotation (cl:lambda (key) (cl:lambda (typ) ((hydra_lib_maps_lookup key) (hydra_annotations_type_annotation_internal typ)))))

(cl:defvar hydra_annotations_get_type_classes (cl:lambda (cx) (cl:lambda (graph) (cl:lambda (term) (let ((decode_class (cl:lambda (term2) (let ((by_name (hydra_lib_maps_from_list (cl:list (cl:list "equality" (list :equality cl:nil)) (cl:list "ordering" (list :ordering cl:nil)))))) ((hydra_lib_eithers_bind (((hydra_extract_core_unit_variant "hydra.classes.TypeClass") graph) term2)) (cl:lambda (fn_) (((hydra_lib_maybes_maybe (cl:lambda () (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "type class" (hydra_show_core_term term2))))))) (cl:lambda (x) (list :right x))) ((hydra_lib_maps_lookup fn_) by_name)))))))) (((hydra_lib_maybes_maybe (cl:lambda () (list :right hydra_lib_maps_empty))) (cl:lambda (term2) ((((hydra_extract_core_map (cl:lambda (t_) (((hydra_lib_eithers_bimap (cl:lambda (de) (list :decoding de))) (cl:lambda (x) x)) ((hydra_decode_core_name graph) t_)))) ((hydra_extract_core_set_of decode_class) graph)) graph) term2))) ((hydra_annotations_get_term_annotation hydra_constants_key_classes) term)))))))

(cl:defvar hydra_annotations_has_description (cl:lambda (anns) (hydra_lib_maybes_is_just ((hydra_lib_maps_lookup hydra_constants_key_description) anns))))

(cl:defvar hydra_annotations_has_type_description (cl:lambda (typ) (hydra_annotations_has_description (hydra_annotations_type_annotation_internal typ))))

(cl:defvar hydra_annotations_is_native_type (cl:lambda (el) (let ((is_flagged_as_first_class_type ((hydra_lib_maybes_from_maybe (cl:lambda () cl:nil)) ((hydra_lib_maybes_map (cl:lambda (_) cl:t)) ((hydra_annotations_get_term_annotation hydra_constants_key_first_class_type) ((cl:lambda (v) (hydra_core_binding-term v)) el)))))) (((hydra_lib_maybes_maybe (cl:lambda () cl:nil)) (cl:lambda (ts) ((hydra_lib_logic_and ((hydra_lib_equality_equal ts) (make-hydra_core_type_scheme (cl:list) (list :variable "hydra.core.Type") (list :nothing)))) (hydra_lib_logic_not is_flagged_as_first_class_type)))) ((cl:lambda (v) (hydra_core_binding-type v)) el)))))

(cl:defvar hydra_annotations_put_attr (cl:lambda (key) (cl:lambda (val) (cl:lambda (cx) (make-hydra_context_context ((cl:lambda (v) (hydra_context_context-trace v)) cx) ((cl:lambda (v) (hydra_context_context-messages v)) cx) (((hydra_lib_maps_insert key) val) ((cl:lambda (v) (hydra_context_context-other v)) cx)))))))

(cl:defvar hydra_annotations_put_count (cl:lambda (key) (cl:lambda (count) (cl:lambda (cx) (((hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 count)))) cx)))))

(cl:defvar hydra_annotations_next_count (cl:lambda (key) (cl:lambda (cx) (let ((count ((hydra_annotations_get_count key) cx))) (cl:list count (((hydra_annotations_put_count key) ((hydra_lib_math_add count) 1)) cx))))))

(cl:defvar hydra_annotations_normalize_term_annotations (cl:lambda (term) (let ((anns (hydra_annotations_term_annotation_internal term))) (let ((stripped (hydra_strip_deannotate_term term))) (if (hydra_lib_maps_null anns) stripped (list :annotated (make-hydra_core_annotated_term stripped anns)))))))

(cl:defvar hydra_annotations_normalize_type_annotations (cl:lambda (typ) (let ((anns (hydra_annotations_type_annotation_internal typ))) (let ((stripped (hydra_strip_deannotate_type typ))) (if (hydra_lib_maps_null anns) stripped (list :annotated (make-hydra_core_annotated_type stripped anns)))))))

(cl:defvar hydra_annotations_reset_count (cl:lambda (key) (cl:lambda (cx) (((hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 0)))) cx))))

(cl:defvar hydra_annotations_set_annotation (cl:lambda (key) (cl:lambda (val) (cl:lambda (m) (((hydra_lib_maps_alter (cl:lambda (_) val)) key) m)))))

(cl:defvar hydra_annotations_set_description (cl:lambda (d) ((hydra_annotations_set_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (cl:lambda (arg_) ((cl:lambda (x) (list :literal x)) ((cl:lambda (x) (list :string x)) arg_)))) d))))

(cl:defvar hydra_annotations_set_term_annotation (cl:lambda (key) (cl:lambda (val) (cl:lambda (term) (let ((term_ (hydra_strip_deannotate_term term))) (let ((anns (((hydra_annotations_set_annotation key) val) (hydra_annotations_term_annotation_internal term)))) (if (hydra_lib_maps_null anns) term_ (list :annotated (make-hydra_core_annotated_term term_ anns)))))))))

(cl:defvar hydra_annotations_set_term_description (cl:lambda (d) ((hydra_annotations_set_term_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (cl:lambda (s) (list :literal (list :string s)))) d))))

(cl:defvar hydra_annotations_set_type (cl:lambda (mt) ((hydra_annotations_set_annotation hydra_constants_key_type) ((hydra_lib_maybes_map hydra_encode_core_type) mt))))

(cl:defvar hydra_annotations_set_type_annotation (cl:lambda (key) (cl:lambda (val) (cl:lambda (typ) (let ((typ_ (hydra_strip_deannotate_type typ))) (let ((anns (((hydra_annotations_set_annotation key) val) (hydra_annotations_type_annotation_internal typ)))) (if (hydra_lib_maps_null anns) typ_ (list :annotated (make-hydra_core_annotated_type typ_ anns)))))))))

(cl:defvar hydra_annotations_set_type_classes (cl:lambda (m) (cl:lambda (term) (let ((encode_class (cl:lambda (tc) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :equality) ((cl:lambda (_) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "equality" (list :unit cl:nil))))) match_value)) ((equal (car match_target) :ordering) ((cl:lambda (_) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "ordering" (list :unit cl:nil))))) match_value)))) (cadr match_target))) tc)))) (let ((encode_pair (cl:lambda (name_classes) (let ((name (hydra_lib_pairs_first name_classes))) (let ((classes (hydra_lib_pairs_second name_classes))) (cl:list (hydra_encode_core_name name) (list :set (hydra_lib_sets_from_list ((hydra_lib_lists_map encode_class) (hydra_lib_sets_to_list classes)))))))))) (let ((encoded (if (hydra_lib_maps_null m) (list :nothing) (list :just (list :map (hydra_lib_maps_from_list ((hydra_lib_lists_map encode_pair) (hydra_lib_maps_to_list m)))))))) (((hydra_annotations_set_term_annotation hydra_constants_key_classes) encoded) term)))))))

(cl:defvar hydra_annotations_set_type_description (cl:lambda (d) ((hydra_annotations_set_type_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (cl:lambda (arg_) ((cl:lambda (x) (list :literal x)) ((cl:lambda (x) (list :string x)) arg_)))) d))))

(cl:defvar hydra_annotations_when_flag (cl:lambda (cx) (cl:lambda (flag) (cl:lambda (ethen) (cl:lambda (eelse) ((hydra_lib_eithers_bind ((hydra_annotations_has_flag cx) flag)) (cl:lambda (b) (if b ethen eelse))))))))

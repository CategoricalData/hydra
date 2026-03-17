(require 'cl-lib)

(require 'hydra.classes)

(require 'hydra.constants)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.encode.core)

(require 'hydra.error)

(require 'hydra.extract.core)

(require 'hydra.graph)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.rewriting)

(require 'hydra.show.core)

(defvar hydra_annotations_aggregate_annotations (lambda (get_value) (lambda (get_x) (lambda (get_anns) (lambda (t_) (letrec ((to_pairs (lambda (rest) (lambda (t_) (((hydra_lib_maybes_maybe rest) (lambda (yy) ((to_pairs ((hydra_lib_lists_cons (hydra_lib_maps_to_list (get_anns yy))) rest)) (get_x yy)))) (get_value t_)))))) (hydra_lib_maps_from_list (hydra_lib_lists_concat ((to_pairs (list)) t_)))))))))

(defvar hydra_annotations_get_attr (lambda (key) (lambda (cx) ((hydra_lib_maps_lookup key) ((lambda (v) (hydra_context_context-other v)) cx)))))

(defvar hydra_annotations_get_debug_id (lambda (cx) (((hydra_lib_maybes_maybe (list :right nil)) (lambda (term) ((hydra_lib_eithers_map hydra_lib_maybes_pure) (((hydra_extract_core_string cx) (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))) ((hydra_annotations_get_attr hydra_constants_key_debug_id) cx))))

(defvar hydra_annotations_debug_if (lambda (cx) (lambda (debug_id) (lambda (message) ((hydra_lib_eithers_bind (hydra_annotations_get_debug_id cx)) (lambda (mid) (if ((hydra_lib_equality_equal mid) debug_id) (list :left (make-hydra_context_in_context message cx)) (list :right nil))))))))

(defvar hydra_annotations_get_attr_with_default (lambda (key) (lambda (def_) (lambda (cx) ((hydra_lib_maybes_from_maybe def_) ((hydra_annotations_get_attr key) cx))))))

(defvar hydra_annotations_has_flag (lambda (cx) (lambda (flag) (let ((term (((hydra_annotations_get_attr_with_default flag) (list :literal (list :boolean nil))) cx))) (((hydra_extract_core_boolean cx) (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))))

(defvar hydra_annotations_fail_on_flag (lambda (cx) (lambda (flag) (lambda (msg) ((hydra_lib_eithers_bind ((hydra_annotations_has_flag cx) flag)) (lambda (val) (if val (list :left (make-hydra_context_in_context msg cx)) (list :right nil))))))))

(defvar hydra_annotations_get_count (lambda (key) (lambda (cx) (((hydra_lib_maybes_maybe 0) (lambda (term) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :literal) ((lambda (lit) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :integer) ((lambda (iv) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :int32) ((lambda (i) i) match_value)) (t 0))) (cadr match_target))) iv)) match_value)) (t 0))) (cadr match_target))) lit)) match_value)) (t 0))) (cadr match_target))) term))) ((hydra_lib_maps_lookup key) ((lambda (v) (hydra_context_context-other v)) cx))))))

(defvar hydra_annotations_get_description (lambda (cx) (lambda (graph) (lambda (anns) (((hydra_lib_maybes_maybe (list :right nil)) (lambda (term) ((hydra_lib_eithers_map hydra_lib_maybes_pure) (((hydra_extract_core_string cx) graph) term)))) ((hydra_lib_maps_lookup "description") anns))))))

(defvar hydra_annotations_term_annotation_internal (lambda (term) (let ((get_ann (lambda (t_) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :annotated) ((lambda (a) a) match_value)) (t nil))) (cadr match_target))) t_)))) ((((hydra_annotations_aggregate_annotations get_ann) (lambda (at) ((lambda (v) (hydra_core_annotated_term-body v)) at))) (lambda (at) ((lambda (v) (hydra_core_annotated_term-annotation v)) at))) term))))

(defvar hydra_annotations_get_term_annotation (lambda (key) (lambda (term) ((hydra_lib_maps_lookup key) (hydra_annotations_term_annotation_internal term)))))

(defvar hydra_annotations_get_term_description (lambda (cx) (lambda (graph) (lambda (term) (letrec ((peel (lambda (t_) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :type_lambda) ((lambda (tl) (peel ((lambda (v) (hydra_core_type_lambda-body v)) tl))) match_value)) ((equal (car match_target) :type_application) ((lambda (ta) (peel ((lambda (v) (hydra_core_type_application_term-body v)) ta))) match_value)) (t t_))) (cadr match_target))) t_)))) (((hydra_annotations_get_description cx) graph) (hydra_annotations_term_annotation_internal (peel term))))))))

(defvar hydra_annotations_get_type (lambda (graph) (lambda (anns) (((hydra_lib_maybes_maybe (list :right nil)) (lambda (dat) ((hydra_lib_eithers_map hydra_lib_maybes_pure) ((hydra_decode_core_type graph) dat)))) ((hydra_lib_maps_lookup hydra_constants_key_type) anns)))))

(defvar hydra_annotations_type_annotation_internal (lambda (typ) (let ((get_ann (lambda (t_) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :annotated) ((lambda (a) a) match_value)) (t nil))) (cadr match_target))) t_)))) ((((hydra_annotations_aggregate_annotations get_ann) (lambda (at) ((lambda (v) (hydra_core_annotated_type-body v)) at))) (lambda (at) ((lambda (v) (hydra_core_annotated_type-annotation v)) at))) typ))))

(defvar hydra_annotations_get_type_annotation (lambda (key) (lambda (typ) ((hydra_lib_maps_lookup key) (hydra_annotations_type_annotation_internal typ)))))

(defvar hydra_annotations_get_type_classes (lambda (cx) (lambda (graph) (lambda (term) (let ((decode_class (lambda (term) (let ((by_name (hydra_lib_maps_from_list (list (list "equality" (list :equality nil)) (list "ordering" (list :ordering nil)))))) ((hydra_lib_eithers_bind ((((hydra_extract_core_unit_variant cx) "hydra.classes.TypeClass") graph) term)) (lambda (fn_) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context ((hydra_lib_strings_cat2 "unexpected: expected type class, got ") (hydra_show_core_term term)) cx))) (lambda (x) (list :right x))) ((hydra_lib_maps_lookup fn_) by_name)))))))) (((hydra_lib_maybes_maybe (list :right hydra_lib_maps_empty)) (lambda (term) (((((hydra_extract_core_map cx) (lambda (t_) (((hydra_lib_eithers_bimap (lambda (de) (make-hydra_context_in_context ((lambda (v) v) de) cx))) (lambda (x) x)) ((hydra_decode_core_name graph) t_)))) (((hydra_extract_core_set_of cx) decode_class) graph)) graph) term))) ((hydra_annotations_get_term_annotation hydra_constants_key_classes) term)))))))

(defvar hydra_annotations_get_type_description (lambda (cx) (lambda (graph) (lambda (typ) (((hydra_annotations_get_description cx) graph) (hydra_annotations_type_annotation_internal typ))))))

(defvar hydra_annotations_has_description (lambda (anns) (hydra_lib_maybes_is_just ((hydra_lib_maps_lookup hydra_constants_key_description) anns))))

(defvar hydra_annotations_has_type_description (lambda (typ) (hydra_annotations_has_description (hydra_annotations_type_annotation_internal typ))))

(defvar hydra_annotations_is_native_type (lambda (el) (let ((is_flagged_as_first_class_type ((hydra_lib_maybes_from_maybe nil) ((hydra_lib_maybes_map (lambda (_) t)) ((hydra_annotations_get_term_annotation hydra_constants_key_first_class_type) ((lambda (v) (hydra_core_binding-term v)) el)))))) (((hydra_lib_maybes_maybe nil) (lambda (ts) ((hydra_lib_logic_and ((hydra_lib_equality_equal ts) (make-hydra_core_type_scheme (list) (list :variable "hydra.core.Type") nil))) (hydra_lib_logic_not is_flagged_as_first_class_type)))) ((lambda (v) (hydra_core_binding-type v)) el)))))

(defvar hydra_annotations_put_attr (lambda (key) (lambda (val) (lambda (cx) (make-hydra_context_context ((lambda (v) (hydra_context_context-trace v)) cx) ((lambda (v) (hydra_context_context-messages v)) cx) (((hydra_lib_maps_insert key) val) ((lambda (v) (hydra_context_context-other v)) cx)))))))

(defvar hydra_annotations_put_count (lambda (key) (lambda (count) (lambda (cx) (((hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 count)))) cx)))))

(defvar hydra_annotations_next_count (lambda (key) (lambda (cx) (let ((count ((hydra_annotations_get_count key) cx))) (list count (((hydra_annotations_put_count key) ((hydra_lib_math_add count) 1)) cx))))))

(defvar hydra_annotations_normalize_term_annotations (lambda (term) (let ((anns (hydra_annotations_term_annotation_internal term))) (let ((stripped (hydra_rewriting_deannotate_term term))) (if (hydra_lib_maps_null anns) stripped (list :annotated (make-hydra_core_annotated_term stripped anns)))))))

(defvar hydra_annotations_normalize_type_annotations (lambda (typ) (let ((anns (hydra_annotations_type_annotation_internal typ))) (let ((stripped (hydra_rewriting_deannotate_type typ))) (if (hydra_lib_maps_null anns) stripped (list :annotated (make-hydra_core_annotated_type stripped anns)))))))

(defvar hydra_annotations_reset_count (lambda (key) (lambda (cx) (((hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 0)))) cx))))

(defvar hydra_annotations_set_annotation (lambda (key) (lambda (val) (lambda (m) (((hydra_lib_maps_alter (lambda (_) val)) key) m)))))

(defvar hydra_annotations_set_description (lambda (d) ((hydra_annotations_set_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (lambda (arg_) ((lambda (x) (list :literal x)) ((lambda (x) (list :string x)) arg_)))) d))))

(defvar hydra_annotations_set_term_annotation (lambda (key) (lambda (val) (lambda (term) (let ((term_ (hydra_rewriting_deannotate_term term))) (let ((anns (((hydra_annotations_set_annotation key) val) (hydra_annotations_term_annotation_internal term)))) (if (hydra_lib_maps_null anns) term_ (list :annotated (make-hydra_core_annotated_term term_ anns)))))))))

(defvar hydra_annotations_set_term_description (lambda (d) ((hydra_annotations_set_term_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (lambda (s) (list :literal (list :string s)))) d))))

(defvar hydra_annotations_set_type (lambda (mt) ((hydra_annotations_set_annotation hydra_constants_key_type) ((hydra_lib_maybes_map hydra_encode_core_type) mt))))

(defvar hydra_annotations_set_type_annotation (lambda (key) (lambda (val) (lambda (typ) (let ((typ_ (hydra_rewriting_deannotate_type typ))) (let ((anns (((hydra_annotations_set_annotation key) val) (hydra_annotations_type_annotation_internal typ)))) (if (hydra_lib_maps_null anns) typ_ (list :annotated (make-hydra_core_annotated_type typ_ anns)))))))))

(defvar hydra_annotations_set_type_classes (lambda (m) (lambda (term) (let ((encode_class (lambda (tc) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :equality) ((lambda (_) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "equality" (list :unit nil))))) match_value)) ((equal (car match_target) :ordering) ((lambda (_) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "ordering" (list :unit nil))))) match_value)))) (cadr match_target))) tc)))) (let ((encode_pair (lambda (name_classes) (let ((name (hydra_lib_pairs_first name_classes))) (let ((classes (hydra_lib_pairs_second name_classes))) (list (hydra_encode_core_name name) (list :set (hydra_lib_sets_from_list ((hydra_lib_lists_map encode_class) (hydra_lib_sets_to_list classes)))))))))) (let ((encoded (if (hydra_lib_maps_null m) nil (list :map (hydra_lib_maps_from_list ((hydra_lib_lists_map encode_pair) (hydra_lib_maps_to_list m))))))) (((hydra_annotations_set_term_annotation hydra_constants_key_classes) encoded) term)))))))

(defvar hydra_annotations_set_type_description (lambda (d) ((hydra_annotations_set_type_annotation hydra_constants_key_description) ((hydra_lib_maybes_map (lambda (arg_) ((lambda (x) (list :literal x)) ((lambda (x) (list :string x)) arg_)))) d))))

(defvar hydra_annotations_type_element (lambda (name) (lambda (typ) (let ((schema_term (list :variable "hydra.core.Type"))) (let ((data_term (hydra_annotations_normalize_term_annotations (list :annotated (make-hydra_core_annotated_term (hydra_encode_core_type typ) (hydra_lib_maps_from_list (list (list hydra_constants_key_type schema_term)))))))) (make-hydra_core_binding name data_term (make-hydra_core_type_scheme (list) (list :variable "hydra.core.Type") nil)))))))

(defvar hydra_annotations_when_flag (lambda (cx) (lambda (flag) (lambda (ethen) (lambda (eelse) ((hydra_lib_eithers_bind ((hydra_annotations_has_flag cx) flag)) (lambda (b) (if b ethen eelse))))))))

(provide 'hydra.annotations)

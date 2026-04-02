(require 'cl-lib)

(require 'hydra.classes)

(require 'hydra.constants)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.encode.core)

(require 'hydra.errors)

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

(require 'hydra.show.core)

(require 'hydra.strip)

(defvar hydra_annotations_aggregate_annotations (lambda (get_value) (lambda (get_x) (lambda (get_anns) (lambda (t_) (letrec ((to_pairs (lambda (rest) (lambda (t_) (funcall (funcall (hydra_lib_maybes_maybe (lambda () rest)) (lambda (yy) (funcall (to_pairs (funcall (hydra_lib_lists_cons (hydra_lib_maps_to_list (get_anns yy))) rest)) (get_x yy)))) (get_value t_)))))) (hydra_lib_maps_from_list (hydra_lib_lists_concat (funcall (to_pairs (list)) t_)))))))))

(defvar hydra_annotations_get_attr (lambda (key) (lambda (cx) (funcall (hydra_lib_maps_lookup key) (funcall (lambda (v) (hydra_context_context-other v)) cx)))))

(defvar hydra_annotations_get_debug_id (lambda (cx) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (term) (funcall (hydra_lib_eithers_map hydra_lib_maybes_pure) (funcall (funcall (hydra_extract_core_string cx) (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))) (funcall (hydra_annotations_get_attr hydra_constants_key_debug_id) cx))))

(defvar hydra_annotations_debug_if (lambda (cx) (lambda (debug_id) (lambda (message) (funcall (hydra_lib_eithers_bind (hydra_annotations_get_debug_id cx)) (lambda (mid) (if (funcall (hydra_lib_equality_equal mid) (list :just debug_id)) (list :left (make-hydra_context_in_context (list :other message) cx)) (list :right nil))))))))

(defvar hydra_annotations_get_attr_with_default (lambda (key) (lambda (def_) (lambda (cx) (funcall (hydra_lib_maybes_from_maybe (lambda () def_)) (funcall (hydra_annotations_get_attr key) cx))))))

(defvar hydra_annotations_has_flag (lambda (cx) (lambda (flag) (let ((term (funcall (funcall (hydra_annotations_get_attr_with_default flag) (list :literal (list :boolean nil))) cx))) (funcall (funcall (hydra_extract_core_boolean cx) (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty)) term)))))

(defvar hydra_annotations_fail_on_flag (lambda (cx) (lambda (flag) (lambda (msg) (funcall (hydra_lib_eithers_bind (funcall (hydra_annotations_has_flag cx) flag)) (lambda (val) (if val (list :left (make-hydra_context_in_context (list :other msg) cx)) (list :right nil))))))))

(defvar hydra_annotations_get_count (lambda (key) (lambda (cx) (funcall (funcall (hydra_lib_maybes_maybe (lambda () 0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (lit) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :integer) (funcall (lambda (iv) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :int32) (funcall (lambda (i) i) match_value)) (t 0))) (cadr match_target))) iv)) match_value)) (t 0))) (cadr match_target))) lit)) match_value)) (t 0))) (cadr match_target))) term))) (funcall (hydra_lib_maps_lookup key) (funcall (lambda (v) (hydra_context_context-other v)) cx))))))

(defvar hydra_annotations_get_description (lambda (cx) (lambda (graph) (lambda (anns) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (term) (funcall (hydra_lib_eithers_map hydra_lib_maybes_pure) (funcall (funcall (hydra_extract_core_string cx) graph) term)))) (funcall (hydra_lib_maps_lookup "description") anns))))))

(defvar hydra_annotations_term_annotation_internal (lambda (term) (let ((get_ann (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :annotated) (funcall (lambda (a) (list :just a)) match_value)) (t (list :nothing)))) (cadr match_target))) t_)))) (funcall (funcall (funcall (hydra_annotations_aggregate_annotations get_ann) (lambda (at) (funcall (lambda (v) (hydra_core_annotated_term-body v)) at))) (lambda (at) (funcall (lambda (v) (hydra_core_annotated_term-annotation v)) at))) term))))

(defvar hydra_annotations_get_term_annotation (lambda (key) (lambda (term) (funcall (hydra_lib_maps_lookup key) (hydra_annotations_term_annotation_internal term)))))

(defvar hydra_annotations_get_term_description (lambda (cx) (lambda (graph) (lambda (term) (letrec ((peel (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :type_lambda) (funcall (lambda (tl) (peel (funcall (lambda (v) (hydra_core_type_lambda-body v)) tl))) match_value)) ((equal (car match_target) :type_application) (funcall (lambda (ta) (peel (funcall (lambda (v) (hydra_core_type_application_term-body v)) ta))) match_value)) (t t_))) (cadr match_target))) t_)))) (funcall (funcall (hydra_annotations_get_description cx) graph) (hydra_annotations_term_annotation_internal (peel term))))))))

(defvar hydra_annotations_get_type (lambda (graph) (lambda (anns) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (dat) (funcall (hydra_lib_eithers_map hydra_lib_maybes_pure) (funcall (hydra_decode_core_type graph) dat)))) (funcall (hydra_lib_maps_lookup hydra_constants_key_type) anns)))))

(defvar hydra_annotations_type_annotation_internal (lambda (typ) (let ((get_ann (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :annotated) (funcall (lambda (a) (list :just a)) match_value)) (t (list :nothing)))) (cadr match_target))) t_)))) (funcall (funcall (funcall (hydra_annotations_aggregate_annotations get_ann) (lambda (at) (funcall (lambda (v) (hydra_core_annotated_type-body v)) at))) (lambda (at) (funcall (lambda (v) (hydra_core_annotated_type-annotation v)) at))) typ))))

(defvar hydra_annotations_get_type_annotation (lambda (key) (lambda (typ) (funcall (hydra_lib_maps_lookup key) (hydra_annotations_type_annotation_internal typ)))))

(defvar hydra_annotations_get_type_classes (lambda (cx) (lambda (graph) (lambda (term) (let ((decode_class (lambda (term) (let ((by_name (hydra_lib_maps_from_list (list (list "equality" (list :equality nil)) (list "ordering" (list :ordering nil)))))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_unit_variant cx) "hydra.classes.TypeClass") graph) term)) (lambda (fn_) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "unexpected: expected type class, got ") (hydra_show_core_term term))) cx)))) (lambda (x) (list :right x))) (funcall (hydra_lib_maps_lookup fn_) by_name)))))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right hydra_lib_maps_empty))) (lambda (term) (funcall (funcall (funcall (funcall (hydra_extract_core_map cx) (lambda (t_) (funcall (funcall (hydra_lib_eithers_bimap (lambda (de) (make-hydra_context_in_context (list :other (funcall (lambda (v) v) de)) cx))) (lambda (x) x)) (funcall (hydra_decode_core_name graph) t_)))) (funcall (funcall (hydra_extract_core_set_of cx) decode_class) graph)) graph) term))) (funcall (hydra_annotations_get_term_annotation hydra_constants_key_classes) term)))))))

(defvar hydra_annotations_get_type_description (lambda (cx) (lambda (graph) (lambda (typ) (funcall (funcall (hydra_annotations_get_description cx) graph) (hydra_annotations_type_annotation_internal typ))))))

(defvar hydra_annotations_has_description (lambda (anns) (hydra_lib_maybes_is_just (funcall (hydra_lib_maps_lookup hydra_constants_key_description) anns))))

(defvar hydra_annotations_has_type_description (lambda (typ) (hydra_annotations_has_description (hydra_annotations_type_annotation_internal typ))))

(defvar hydra_annotations_is_native_type (lambda (el) (let ((is_flagged_as_first_class_type (funcall (hydra_lib_maybes_from_maybe (lambda () nil)) (funcall (hydra_lib_maybes_map (lambda (_) t)) (funcall (hydra_annotations_get_term_annotation hydra_constants_key_first_class_type) (funcall (lambda (v) (hydra_core_binding-term v)) el)))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () nil)) (lambda (ts) (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_equal ts) (make-hydra_core_type_scheme (list) (list :variable "hydra.core.Type") (list :nothing)))) (hydra_lib_logic_not is_flagged_as_first_class_type)))) (funcall (lambda (v) (hydra_core_binding-type v)) el)))))

(defvar hydra_annotations_put_attr (lambda (key) (lambda (val) (lambda (cx) (make-hydra_context_context (funcall (lambda (v) (hydra_context_context-trace v)) cx) (funcall (lambda (v) (hydra_context_context-messages v)) cx) (funcall (funcall (hydra_lib_maps_insert key) val) (funcall (lambda (v) (hydra_context_context-other v)) cx)))))))

(defvar hydra_annotations_put_count (lambda (key) (lambda (count) (lambda (cx) (funcall (funcall (hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 count)))) cx)))))

(defvar hydra_annotations_next_count (lambda (key) (lambda (cx) (let ((count (funcall (hydra_annotations_get_count key) cx))) (list count (funcall (funcall (hydra_annotations_put_count key) (funcall (hydra_lib_math_add count) 1)) cx))))))

(defvar hydra_annotations_normalize_term_annotations (lambda (term) (let ((anns (hydra_annotations_term_annotation_internal term))) (let ((stripped (hydra_strip_deannotate_term term))) (if (hydra_lib_maps_null anns) stripped (list :annotated (make-hydra_core_annotated_term stripped anns)))))))

(defvar hydra_annotations_normalize_type_annotations (lambda (typ) (let ((anns (hydra_annotations_type_annotation_internal typ))) (let ((stripped (hydra_strip_deannotate_type typ))) (if (hydra_lib_maps_null anns) stripped (list :annotated (make-hydra_core_annotated_type stripped anns)))))))

(defvar hydra_annotations_reset_count (lambda (key) (lambda (cx) (funcall (funcall (hydra_annotations_put_attr key) (list :literal (list :integer (list :int32 0)))) cx))))

(defvar hydra_annotations_set_annotation (lambda (key) (lambda (val) (lambda (m) (funcall (funcall (hydra_lib_maps_alter (lambda (_) val)) key) m)))))

(defvar hydra_annotations_set_description (lambda (d) (funcall (hydra_annotations_set_annotation hydra_constants_key_description) (funcall (hydra_lib_maybes_map (lambda (arg_) (funcall (lambda (x) (list :literal x)) (funcall (lambda (x) (list :string x)) arg_)))) d))))

(defvar hydra_annotations_set_term_annotation (lambda (key) (lambda (val) (lambda (term) (let ((term_ (hydra_strip_deannotate_term term))) (let ((anns (funcall (funcall (hydra_annotations_set_annotation key) val) (hydra_annotations_term_annotation_internal term)))) (if (hydra_lib_maps_null anns) term_ (list :annotated (make-hydra_core_annotated_term term_ anns)))))))))

(defvar hydra_annotations_set_term_description (lambda (d) (funcall (hydra_annotations_set_term_annotation hydra_constants_key_description) (funcall (hydra_lib_maybes_map (lambda (s) (list :literal (list :string s)))) d))))

(defvar hydra_annotations_set_type (lambda (mt) (funcall (hydra_annotations_set_annotation hydra_constants_key_type) (funcall (hydra_lib_maybes_map hydra_encode_core_type) mt))))

(defvar hydra_annotations_set_type_annotation (lambda (key) (lambda (val) (lambda (typ) (let ((typ_ (hydra_strip_deannotate_type typ))) (let ((anns (funcall (funcall (hydra_annotations_set_annotation key) val) (hydra_annotations_type_annotation_internal typ)))) (if (hydra_lib_maps_null anns) typ_ (list :annotated (make-hydra_core_annotated_type typ_ anns)))))))))

(defvar hydra_annotations_set_type_classes (lambda (m) (lambda (term) (let ((encode_class (lambda (tc) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :equality) (funcall (lambda (_) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "equality" (list :unit nil))))) match_value)) ((equal (car match_target) :ordering) (funcall (lambda (_) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "ordering" (list :unit nil))))) match_value)))) (cadr match_target))) tc)))) (let ((encode_pair (lambda (name_classes) (let ((name (hydra_lib_pairs_first name_classes))) (let ((classes (hydra_lib_pairs_second name_classes))) (list (hydra_encode_core_name name) (list :set (hydra_lib_sets_from_list (funcall (hydra_lib_lists_map encode_class) (hydra_lib_sets_to_list classes)))))))))) (let ((encoded (if (hydra_lib_maps_null m) (list :nothing) (list :just (list :map (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map encode_pair) (hydra_lib_maps_to_list m)))))))) (funcall (funcall (hydra_annotations_set_term_annotation hydra_constants_key_classes) encoded) term)))))))

(defvar hydra_annotations_set_type_description (lambda (d) (funcall (hydra_annotations_set_type_annotation hydra_constants_key_description) (funcall (hydra_lib_maybes_map (lambda (arg_) (funcall (lambda (x) (list :literal x)) (funcall (lambda (x) (list :string x)) arg_)))) d))))

(defvar hydra_annotations_type_element (lambda (name) (lambda (typ) (let ((schema_term (list :variable "hydra.core.Type"))) (let ((data_term (hydra_annotations_normalize_term_annotations (list :annotated (make-hydra_core_annotated_term (hydra_encode_core_type typ) (hydra_lib_maps_from_list (list (list hydra_constants_key_type schema_term)))))))) (make-hydra_core_binding name data_term (list :just (make-hydra_core_type_scheme (list) (list :variable "hydra.core.Type") (list :nothing)))))))))

(defvar hydra_annotations_when_flag (lambda (cx) (lambda (flag) (lambda (ethen) (lambda (eelse) (funcall (hydra_lib_eithers_bind (funcall (hydra_annotations_has_flag cx) flag)) (lambda (b) (if b ethen eelse))))))))

(provide 'hydra.annotations)

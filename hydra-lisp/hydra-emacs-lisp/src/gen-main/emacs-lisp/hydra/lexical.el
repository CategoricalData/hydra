(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.graph)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(require 'hydra.strip)

(defvar hydra_lexical_build_graph (lambda (elements) (lambda (environment) (lambda (primitives) (let ((element_terms (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (b) (list (funcall (lambda (v) (hydra_core_binding-name v)) b) (funcall (lambda (v) (hydra_core_binding-term v)) b)))) elements)))) (let ((let_terms (funcall (hydra_lib_maps_map (lambda (mt) (hydra_lib_maybes_from_just mt))) (funcall (hydra_lib_maps_filter (lambda (mt) (hydra_lib_maybes_is_just mt))) environment)))) (let ((element_types (hydra_lib_maps_from_list (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (b) (funcall (hydra_lib_maybes_map (lambda (ts) (list (funcall (lambda (v) (hydra_core_binding-name v)) b) ts))) (funcall (lambda (v) (hydra_core_binding-type v)) b)))) elements))))) (make-hydra_graph_graph (funcall (hydra_lib_maps_union element_terms) let_terms) element_types hydra_lib_maps_empty (hydra_lib_sets_from_list (hydra_lib_maps_keys (funcall (hydra_lib_maps_filter (lambda (mt) (hydra_lib_maybes_is_nothing mt))) environment))) hydra_lib_maps_empty primitives hydra_lib_maps_empty hydra_lib_sets_empty))))))))

(defvar hydra_lexical_choose_unique_name (lambda (reserved) (lambda (name) (letrec ((try_name (lambda (index) (let ((candidate (if (funcall (hydra_lib_equality_equal index) 1) name (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) name)) (hydra_lib_literals_show_int32 index))))) (if (funcall (hydra_lib_sets_member candidate) reserved) (try_name (funcall (hydra_lib_math_add index) 1)) candidate))))) (try_name 1)))))

(defvar hydra_lexical_dereference_schema_type (lambda (name) (lambda (types) (letrec ((for_type (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :annotated) (funcall (lambda (at) (for_type (funcall (lambda (v) (hydra_core_annotated_type-body v)) at))) match_value)) ((equal (car match_target) :forall) (funcall (lambda (ft) (funcall (hydra_lib_maybes_map (lambda (ts) (make-hydra_core_type_scheme (funcall (hydra_lib_lists_cons (funcall (lambda (v) (hydra_core_forall_type-parameter v)) ft)) (funcall (lambda (v) (hydra_core_type_scheme-variables v)) ts)) (funcall (lambda (v) (hydra_core_type_scheme-type v)) ts) (funcall (lambda (v) (hydra_core_type_scheme-constraints v)) ts)))) (for_type (funcall (lambda (v) (hydra_core_forall_type-body v)) ft)))) match_value)) ((equal (car match_target) :variable) (funcall (lambda (v) (funcall (hydra_lexical_dereference_schema_type v) types)) match_value)) (t (list :just (make-hydra_core_type_scheme (list) t_ (list :nothing)))))) (cadr match_target))) t_)))) (funcall (hydra_lib_maybes_bind (funcall (hydra_lib_maps_lookup name) types)) (lambda (ts) (funcall (hydra_lib_maybes_map (lambda (ts2) (make-hydra_core_type_scheme (funcall (hydra_lib_lists_concat2 (funcall (lambda (v) (hydra_core_type_scheme-variables v)) ts)) (funcall (lambda (v) (hydra_core_type_scheme-variables v)) ts2)) (funcall (lambda (v) (hydra_core_type_scheme-type v)) ts2) (funcall (lambda (v) (hydra_core_type_scheme-constraints v)) ts2)))) (for_type (funcall (lambda (v) (hydra_core_type_scheme-type v)) ts)))))))))

(defvar hydra_lexical_lookup_binding (lambda (graph) (lambda (name) (funcall (hydra_lib_maybes_map (lambda (term) (make-hydra_core_binding name term (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-bound_types v)) graph))))) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-bound_terms v)) graph))))))

(defvar hydra_lexical_dereference_variable (lambda (graph) (lambda (name) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (funcall (hydra_lib_strings_cat2 "no such element: ") (funcall (lambda (v) v) name))))) (lambda (right_) (list :right right_))) (funcall (hydra_lexical_lookup_binding graph) name)))))

(defvar hydra_lexical_elements_to_graph (lambda (parent) (lambda (schema_types) (lambda (elements) (let ((prims (funcall (lambda (v) (hydra_graph_graph-primitives v)) parent))) (let ((g (funcall (funcall (hydra_lexical_build_graph elements) hydra_lib_maps_empty) prims))) (make-hydra_graph_graph (funcall (lambda (v) (hydra_graph_graph-bound_terms v)) g) (funcall (lambda (v) (hydra_graph_graph-bound_types v)) g) (funcall (lambda (v) (hydra_graph_graph-class_constraints v)) g) (funcall (lambda (v) (hydra_graph_graph-lambda_variables v)) g) (funcall (lambda (v) (hydra_graph_graph-metadata v)) g) (funcall (lambda (v) (hydra_graph_graph-primitives v)) g) schema_types (funcall (lambda (v) (hydra_graph_graph-type_variables v)) g))))))))

(defvar hydra_lexical_empty_context (make-hydra_context_context (list) (list) hydra_lib_maps_empty))

(defvar hydra_lexical_empty_graph (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty))

(defvar hydra_lexical_fields_of (lambda (t_) (let ((stripped (hydra_strip_deannotate_type t_))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :forall) (funcall (lambda (forall_type) (hydra_lexical_fields_of (funcall (lambda (v) (hydra_core_forall_type-body v)) forall_type))) match_value)) ((equal (car match_target) :record) (funcall (lambda (rt) rt) match_value)) ((equal (car match_target) :union) (funcall (lambda (rt) rt) match_value)) (t (list)))) (cadr match_target))) stripped))))

(defvar hydra_lexical_get_field (lambda (cx) (lambda (m) (lambda (fname) (lambda (decode) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected field ") (funcall (lambda (v) v) fname))) " not found")) cx)))) decode) (funcall (hydra_lib_maps_lookup fname) m)))))))

(defvar hydra_lexical_graph_to_bindings (lambda (g) (funcall (hydra_lib_lists_map (lambda (p) (let ((name (hydra_lib_pairs_first p))) (let ((term (hydra_lib_pairs_second p))) (make-hydra_core_binding name term (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-bound_types v)) g))))))) (hydra_lib_maps_to_list (funcall (lambda (v) (hydra_graph_graph-bound_terms v)) g)))))

(defvar hydra_lexical_lookup_primitive (lambda (graph) (lambda (name) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-primitives v)) graph)))))

(defvar hydra_lexical_lookup_term (lambda (graph) (lambda (name) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-bound_terms v)) graph)))))

(defvar hydra_lexical_require_binding (lambda (cx) (lambda (graph) (lambda (name) (let ((show_all nil)) (let ((ellipsis (lambda (strings) (if (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gt (hydra_lib_lists_length strings)) 3)) (hydra_lib_logic_not show_all)) (funcall (hydra_lib_lists_concat2 (funcall (hydra_lib_lists_take 3) strings)) (list "...")) strings)))) (let ((err_msg (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "no such element: ") (funcall (lambda (v) v) name))) ". Available elements: {")) (funcall (hydra_lib_strings_intercalate ", ") (ellipsis (funcall (hydra_lib_lists_map (lambda (v) v)) (hydra_lib_maps_keys (funcall (lambda (v) (hydra_graph_graph-bound_terms v)) graph))))))) "}"))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other err_msg) cx)))) (lambda (x) (list :right x))) (funcall (hydra_lexical_lookup_binding graph) name)))))))))

(defvar hydra_lexical_match_union (lambda (cx) (lambda (graph) (lambda (tname) (lambda (pairs) (lambda (term) (let ((stripped (hydra_strip_deannotate_and_detype_term term))) (let ((mapping (hydra_lib_maps_from_list pairs))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :variable) (funcall (lambda (name) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lexical_require_binding cx) graph) name)) (lambda (el) (funcall (funcall (funcall (funcall (hydra_lexical_match_union cx) graph) tname) pairs) (funcall (lambda (v) (hydra_core_binding-term v)) el))))) match_value)) ((equal (car match_target) :union) (funcall (lambda (injection) (let ((exp (let ((fname (funcall (lambda (v) (hydra_core_field-name v)) (funcall (lambda (v) (hydra_core_injection-field v)) injection)))) (let ((val (funcall (lambda (v) (hydra_core_field-term v)) (funcall (lambda (v) (hydra_core_injection-field v)) injection)))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "no matching case for field \"") (funcall (lambda (v) v) fname))) "\" in union type ")) (funcall (lambda (v) v) tname))) cx)))) (lambda (f) (f val))) (funcall (hydra_lib_maps_lookup fname) mapping)))))) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_injection-type_name v)) injection))) (funcall (lambda (v) v) tname)) exp (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected injection for type ") (funcall (lambda (v) v) tname))) ", got ")) (hydra_show_core_term term))) cx))))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (hydra_lib_strings_cat (list "expected inject(" (funcall (lambda (v) v) tname) ") with one of {" (funcall (hydra_lib_strings_intercalate ", ") (funcall (hydra_lib_lists_map (lambda (pair) (funcall (lambda (v) v) (hydra_lib_pairs_first pair)))) pairs)) "}, got " (hydra_show_core_term stripped)))) cx))))) (cadr match_target))) stripped)))))))))

(defvar hydra_lexical_match_unit_field (lambda (fname) (lambda (x) (list fname (lambda (ignored) (list :right x))))))

(defvar hydra_lexical_match_enum (lambda (cx) (lambda (graph) (lambda (tname) (lambda (pairs) (funcall (funcall (funcall (hydra_lexical_match_union cx) graph) tname) (funcall (hydra_lib_lists_map (lambda (pair) (funcall (hydra_lexical_match_unit_field (hydra_lib_pairs_first pair)) (hydra_lib_pairs_second pair)))) pairs)))))))

(defvar hydra_lexical_match_record (lambda (cx) (lambda (graph) (lambda (decode) (lambda (term) (let ((stripped (hydra_strip_deannotate_and_detype_term term))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (decode (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (field) (list (funcall (lambda (v) (hydra_core_field-name v)) field) (funcall (lambda (v) (hydra_core_field-term v)) field)))) (funcall (lambda (v) (hydra_core_record-fields v)) record))))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "expected a record, got ") (hydra_show_core_term term))) cx))))) (cadr match_target))) stripped)))))))

(defvar hydra_lexical_require_primitive (lambda (cx) (lambda (graph) (lambda (name) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "no such primitive function: ") (funcall (lambda (v) v) name))) cx)))) (lambda (x) (list :right x))) (funcall (hydra_lexical_lookup_primitive graph) name))))))

(defvar hydra_lexical_require_primitive_type (lambda (cx) (lambda (tx) (lambda (name) (let ((mts (funcall (hydra_lib_maybes_map (lambda (_p) (funcall (lambda (v) (hydra_graph_primitive-type v)) _p))) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-primitives v)) tx))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "no such primitive function: ") (funcall (lambda (v) v) name))) cx)))) (lambda (ts) (list :right ts))) mts))))))

(defvar hydra_lexical_resolve_term (lambda (graph) (lambda (name) (let ((recurse (lambda (term) (let ((stripped (hydra_strip_deannotate_term term))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :variable) (funcall (lambda (name_) (funcall (hydra_lexical_resolve_term graph) name_)) match_value)) (t (list :just term)))) (cadr match_target))) stripped))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :nothing))) recurse) (funcall (hydra_lexical_lookup_term graph) name))))))

(defvar hydra_lexical_require_term (lambda (cx) (lambda (graph) (lambda (name) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "no such element: ") (funcall (lambda (v) v) name))) cx)))) (lambda (x) (list :right x))) (funcall (hydra_lexical_resolve_term graph) name))))))

(defvar hydra_lexical_strip_and_dereference_term (lambda (cx) (lambda (graph) (lambda (term) (let ((stripped (hydra_strip_deannotate_and_detype_term term))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :variable) (funcall (lambda (v) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lexical_require_term cx) graph) v)) (lambda (t_) (funcall (funcall (hydra_lexical_strip_and_dereference_term cx) graph) t_)))) match_value)) (t (list :right stripped)))) (cadr match_target))) stripped))))))

(defvar hydra_lexical_strip_and_dereference_term_either (lambda (graph) (lambda (term) (let ((stripped (hydra_strip_deannotate_and_detype_term term))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :variable) (funcall (lambda (v) (funcall (funcall (hydra_lib_eithers_either (lambda (left_) (list :left left_))) (lambda (binding) (funcall (hydra_lexical_strip_and_dereference_term_either graph) (funcall (lambda (v) (hydra_core_binding-term v)) binding)))) (funcall (hydra_lexical_dereference_variable graph) v))) match_value)) (t (list :right stripped)))) (cadr match_target))) stripped)))))

(provide 'hydra.lexical)

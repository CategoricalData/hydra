(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.error)

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

(require 'hydra.rewriting)

(require 'hydra.show.core)

(defvar hydra_lexical_build_graph (lambda (elements) (lambda (environment) (lambda (primitives) (let ((element_terms (hydra_lib_maps_from_list ((hydra_lib_lists_map (lambda (b) (list ((lambda (v) (hydra_core_binding-name v)) b) ((lambda (v) (hydra_core_binding-term v)) b)))) elements)))) (let ((let_terms ((hydra_lib_maps_map (lambda (mt) (hydra_lib_maybes_from_just mt))) ((hydra_lib_maps_filter (lambda (mt) (hydra_lib_maybes_is_just mt))) environment)))) (let ((element_types (hydra_lib_maps_from_list (hydra_lib_maybes_cat ((hydra_lib_lists_map (lambda (b) ((hydra_lib_maybes_map (lambda (ts) (list ((lambda (v) (hydra_core_binding-name v)) b) ts))) ((lambda (v) (hydra_core_binding-type v)) b)))) elements))))) (make-hydra_graph_graph ((hydra_lib_maps_union element_terms) let_terms) element_types hydra_lib_maps_empty (hydra_lib_sets_from_list (hydra_lib_maps_keys ((hydra_lib_maps_filter (lambda (mt) (hydra_lib_maybes_is_nothing mt))) environment))) hydra_lib_maps_empty primitives hydra_lib_maps_empty hydra_lib_sets_empty))))))))

(defvar hydra_lexical_choose_unique_name (lambda (reserved) (lambda (name) (letrec ((try_name (lambda (index) (let ((candidate (if ((hydra_lib_equality_equal index) 1) name ((hydra_lib_strings_cat2 ((lambda (v) v) name)) (hydra_lib_literals_show_int32 index))))) (if ((hydra_lib_sets_member candidate) reserved) (try_name ((hydra_lib_math_add index) 1)) candidate))))) (try_name 1)))))

(defvar hydra_lexical_lookup_element (lambda (graph) (lambda (name) ((hydra_lib_maybes_map (lambda (term) (make-hydra_core_binding name term ((hydra_lib_maps_lookup name) ((lambda (v) (hydra_graph_graph-bound_types v)) graph))))) ((hydra_lib_maps_lookup name) ((lambda (v) (hydra_graph_graph-bound_terms v)) graph))))))

(defvar hydra_lexical_dereference_element (lambda (graph) (lambda (name) ((hydra_lexical_lookup_element graph) name))))

(defvar hydra_lexical_dereference_schema_type (lambda (name) (lambda (types) (letrec ((for_type (lambda (t_) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :annotated) ((lambda (at) (for_type ((lambda (v) (hydra_core_annotated_type-body v)) at))) match_value)) ((equal (car match_target) :forall) ((lambda (ft) ((hydra_lib_maybes_map (lambda (ts) (make-hydra_core_type_scheme ((hydra_lib_lists_cons ((lambda (v) (hydra_core_forall_type-parameter v)) ft)) ((lambda (v) (hydra_core_type_scheme-variables v)) ts)) ((lambda (v) (hydra_core_type_scheme-type v)) ts) ((lambda (v) (hydra_core_type_scheme-constraints v)) ts)))) (for_type ((lambda (v) (hydra_core_forall_type-body v)) ft)))) match_value)) ((equal (car match_target) :variable) ((lambda (v) ((hydra_lexical_dereference_schema_type v) types)) match_value)) (t (make-hydra_core_type_scheme (list) t_ nil)))) (cadr match_target))) t_)))) ((hydra_lib_maybes_bind ((hydra_lib_maps_lookup name) types)) (lambda (ts) ((hydra_lib_maybes_map (lambda (ts2) (make-hydra_core_type_scheme ((hydra_lib_lists_concat2 ((lambda (v) (hydra_core_type_scheme-variables v)) ts)) ((lambda (v) (hydra_core_type_scheme-variables v)) ts2)) ((lambda (v) (hydra_core_type_scheme-type v)) ts2) ((lambda (v) (hydra_core_type_scheme-constraints v)) ts2)))) (for_type ((lambda (v) (hydra_core_type_scheme-type v)) ts)))))))))

(defvar hydra_lexical_dereference_variable (lambda (graph) (lambda (name) (((hydra_lib_maybes_maybe (list :left ((hydra_lib_strings_cat2 "no such element: ") ((lambda (v) v) name)))) (lambda (right_) (list :right right_))) ((hydra_lexical_lookup_element graph) name)))))

(defvar hydra_lexical_elements_to_graph (lambda (parent) (lambda (schema_types) (lambda (elements) (let ((prims ((lambda (v) (hydra_graph_graph-primitives v)) parent))) (let ((g (((hydra_lexical_build_graph elements) hydra_lib_maps_empty) prims))) (make-hydra_graph_graph ((lambda (v) (hydra_graph_graph-bound_terms v)) g) ((lambda (v) (hydra_graph_graph-bound_types v)) g) ((lambda (v) (hydra_graph_graph-class_constraints v)) g) ((lambda (v) (hydra_graph_graph-lambda_variables v)) g) ((lambda (v) (hydra_graph_graph-metadata v)) g) ((lambda (v) (hydra_graph_graph-primitives v)) g) schema_types ((lambda (v) (hydra_graph_graph-type_variables v)) g))))))))

(defvar hydra_lexical_empty_context (make-hydra_context_context (list) (list) hydra_lib_maps_empty))

(defvar hydra_lexical_empty_graph (make-hydra_graph_graph hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_maps_empty hydra_lib_sets_empty))

(defvar hydra_lexical_extend_graph_with_bindings (lambda (bindings) (lambda (g) (let ((new_terms (hydra_lib_maps_from_list ((hydra_lib_lists_map (lambda (b) (list ((lambda (v) (hydra_core_binding-name v)) b) ((lambda (v) (hydra_core_binding-term v)) b)))) bindings)))) (let ((new_types (hydra_lib_maps_from_list (hydra_lib_maybes_cat ((hydra_lib_lists_map (lambda (b) ((hydra_lib_maybes_map (lambda (ts) (list ((lambda (v) (hydra_core_binding-name v)) b) ts))) ((lambda (v) (hydra_core_binding-type v)) b)))) bindings))))) (make-hydra_graph_graph ((hydra_lib_maps_union new_terms) ((lambda (v) (hydra_graph_graph-bound_terms v)) g)) ((hydra_lib_maps_union new_types) ((lambda (v) (hydra_graph_graph-bound_types v)) g)) ((lambda (v) (hydra_graph_graph-class_constraints v)) g) ((lambda (v) (hydra_graph_graph-lambda_variables v)) g) ((lambda (v) (hydra_graph_graph-metadata v)) g) ((lambda (v) (hydra_graph_graph-primitives v)) g) ((lambda (v) (hydra_graph_graph-schema_types v)) g) ((lambda (v) (hydra_graph_graph-type_variables v)) g)))))))

(defvar hydra_lexical_fields_of (lambda (t_) (let ((stripped (hydra_rewriting_deannotate_type t_))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :forall) ((lambda (forall_type) (hydra_lexical_fields_of ((lambda (v) (hydra_core_forall_type-body v)) forall_type))) match_value)) ((equal (car match_target) :record) ((lambda (rt) rt) match_value)) ((equal (car match_target) :union) ((lambda (rt) rt) match_value)) (t (list)))) (cadr match_target))) stripped))))

(defvar hydra_lexical_get_field (lambda (cx) (lambda (m) (lambda (fname) (lambda (decode) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected field ") ((lambda (v) v) fname))) " not found")) cx))) decode) ((hydra_lib_maps_lookup fname) m)))))))

(defvar hydra_lexical_graph_to_bindings (lambda (g) ((hydra_lib_lists_map (lambda (p) (let ((name (hydra_lib_pairs_first p))) (let ((term (hydra_lib_pairs_second p))) (make-hydra_core_binding name term ((hydra_lib_maps_lookup name) ((lambda (v) (hydra_graph_graph-bound_types v)) g))))))) (hydra_lib_maps_to_list ((lambda (v) (hydra_graph_graph-bound_terms v)) g)))))

(defvar hydra_lexical_lookup_primitive (lambda (graph) (lambda (name) ((hydra_lib_maps_lookup name) ((lambda (v) (hydra_graph_graph-primitives v)) graph)))))

(defvar hydra_lexical_lookup_term (lambda (graph) (lambda (name) ((hydra_lib_maps_lookup name) ((lambda (v) (hydra_graph_graph-bound_terms v)) graph)))))

(defvar hydra_lexical_require_element (lambda (cx) (lambda (graph) (lambda (name) (let ((show_all nil)) (let ((ellipsis (lambda (strings) (if ((hydra_lib_logic_and ((hydra_lib_equality_gt (hydra_lib_lists_length strings)) 3)) (hydra_lib_logic_not show_all)) ((hydra_lib_lists_concat2 ((hydra_lib_lists_take 3) strings)) (list "...")) strings)))) (let ((err_msg ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "no such element: ") ((lambda (v) v) name))) ". Available elements: {")) ((hydra_lib_strings_intercalate ", ") (ellipsis ((hydra_lib_lists_map (lambda (v) v)) (hydra_lib_maps_keys ((lambda (v) (hydra_graph_graph-bound_terms v)) graph))))))) "}"))) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context (list :other err_msg) cx))) (lambda (x) (list :right x))) ((hydra_lexical_dereference_element graph) name)))))))))

(defvar hydra_lexical_match_union (lambda (cx) (lambda (graph) (lambda (tname) (lambda (pairs) (lambda (term) (let ((stripped (hydra_rewriting_deannotate_and_detype_term term))) (let ((mapping (hydra_lib_maps_from_list pairs))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :variable) ((lambda (name) ((hydra_lib_eithers_bind (((hydra_lexical_require_element cx) graph) name)) (lambda (el) (((((hydra_lexical_match_union cx) graph) tname) pairs) ((lambda (v) (hydra_core_binding-term v)) el))))) match_value)) ((equal (car match_target) :union) ((lambda (injection) (let ((exp (let ((fname ((lambda (v) (hydra_core_field-name v)) ((lambda (v) (hydra_core_injection-field v)) injection)))) (let ((val ((lambda (v) (hydra_core_field-term v)) ((lambda (v) (hydra_core_injection-field v)) injection)))) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "no matching case for field \"") ((lambda (v) v) fname))) "\" in union type ")) ((lambda (v) v) tname))) cx))) (lambda (f) (f val))) ((hydra_lib_maps_lookup fname) mapping)))))) (if ((hydra_lib_equality_equal ((lambda (v) v) ((lambda (v) (hydra_core_injection-type_name v)) injection))) ((lambda (v) v) tname)) exp (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected injection for type ") ((lambda (v) v) tname))) ", got ")) (hydra_show_core_term term))) cx))))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (hydra_lib_strings_cat (list "expected inject(" ((lambda (v) v) tname) ") with one of {" ((hydra_lib_strings_intercalate ", ") ((hydra_lib_lists_map (lambda (pair) ((lambda (v) v) (hydra_lib_pairs_first pair)))) pairs)) "}, got " (hydra_show_core_term stripped)))) cx))))) (cadr match_target))) stripped)))))))))

(defvar hydra_lexical_match_unit_field (lambda (fname) (lambda (x) (list fname (lambda (ignored) (list :right x))))))

(defvar hydra_lexical_match_enum (lambda (cx) (lambda (graph) (lambda (tname) (lambda (pairs) ((((hydra_lexical_match_union cx) graph) tname) ((hydra_lib_lists_map (lambda (pair) ((hydra_lexical_match_unit_field (hydra_lib_pairs_first pair)) (hydra_lib_pairs_second pair)))) pairs)))))))

(defvar hydra_lexical_match_record (lambda (cx) (lambda (graph) (lambda (decode) (lambda (term) (let ((stripped (hydra_rewriting_deannotate_and_detype_term term))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :record) ((lambda (record) (decode (hydra_lib_maps_from_list ((hydra_lib_lists_map (lambda (field) (list ((lambda (v) (hydra_core_field-name v)) field) ((lambda (v) (hydra_core_field-term v)) field)))) ((lambda (v) (hydra_core_record-fields v)) record))))) match_value)) (t (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 "expected a record, got ") (hydra_show_core_term term))) cx))))) (cadr match_target))) stripped)))))))

(defvar hydra_lexical_require_primitive (lambda (cx) (lambda (graph) (lambda (name) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 "no such primitive function: ") ((lambda (v) v) name))) cx))) (lambda (x) (list :right x))) ((hydra_lexical_lookup_primitive graph) name))))))

(defvar hydra_lexical_require_primitive_type (lambda (cx) (lambda (tx) (lambda (name) (let ((mts ((hydra_lib_maps_lookup name) (hydra_lib_maps_from_list ((hydra_lib_lists_map (lambda (_gpt_p) (list ((lambda (v) (hydra_graph_primitive-name v)) _gpt_p) ((lambda (v) (hydra_graph_primitive-type v)) _gpt_p)))) (hydra_lib_maps_elems ((lambda (v) (hydra_graph_graph-primitives v)) tx))))))) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 "no such primitive function: ") ((lambda (v) v) name))) cx))) (lambda (ts) (list :right ts))) mts))))))

(defvar hydra_lexical_resolve_term (lambda (graph) (lambda (name) (let ((recurse (lambda (term) (let ((stripped (hydra_rewriting_deannotate_term term))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :variable) ((lambda (name_) ((hydra_lexical_resolve_term graph) name_)) match_value)) (t term))) (cadr match_target))) stripped))))) (((hydra_lib_maybes_maybe nil) recurse) ((hydra_lexical_lookup_term graph) name))))))

(defvar hydra_lexical_require_term (lambda (cx) (lambda (graph) (lambda (name) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 "no such element: ") ((lambda (v) v) name))) cx))) (lambda (x) (list :right x))) ((hydra_lexical_resolve_term graph) name))))))

(defvar hydra_lexical_strip_and_dereference_term (lambda (cx) (lambda (graph) (lambda (term) (let ((stripped (hydra_rewriting_deannotate_and_detype_term term))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :variable) ((lambda (v) ((hydra_lib_eithers_bind (((hydra_lexical_require_term cx) graph) v)) (lambda (t_) (((hydra_lexical_strip_and_dereference_term cx) graph) t_)))) match_value)) (t (list :right stripped)))) (cadr match_target))) stripped))))))

(defvar hydra_lexical_strip_and_dereference_term_either (lambda (graph) (lambda (term) (let ((stripped (hydra_rewriting_deannotate_and_detype_term term))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :variable) ((lambda (v) (((hydra_lib_eithers_either (lambda (left_) (list :left left_))) (lambda (binding) ((hydra_lexical_strip_and_dereference_term_either graph) ((lambda (v) (hydra_core_binding-term v)) binding)))) ((hydra_lexical_dereference_variable graph) v))) match_value)) (t (list :right stripped)))) (cadr match_target))) stripped)))))

(provide 'hydra.lexical)

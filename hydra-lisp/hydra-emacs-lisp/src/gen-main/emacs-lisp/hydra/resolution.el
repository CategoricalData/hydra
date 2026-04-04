(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.errors)

(require 'hydra.graph)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.names)

(require 'hydra.scoping)

(require 'hydra.show.core)

(require 'hydra.strip)

(require 'hydra.substitution)

(require 'hydra.typing)

(require 'hydra.variables)

(defvar hydra_resolution_dereference_type (lambda (cx) (lambda (graph) (lambda (name) (let ((mel (funcall (hydra_lexical_lookup_binding graph) name))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (el) (funcall (hydra_lib_eithers_map hydra_lib_maybes_pure) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e cx))) (lambda (_wc_a) _wc_a)) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (list :other (funcall (lambda (v) v) _e)))) (lambda (_a) _a)) (funcall (hydra_decode_core_type graph) (funcall (lambda (v) (hydra_core_binding-term v)) el))))))) mel))))))

(defvar hydra_resolution_f_type_is_polymorphic (lambda (typ) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :annotated) (funcall (lambda (at) (hydra_resolution_f_type_is_polymorphic (funcall (lambda (v) (hydra_core_annotated_type-body v)) at))) match_value)) ((equal (car match_target) :forall) (funcall (lambda (ft) t) match_value)) (t nil))) (cadr match_target))) typ)))

(defvar hydra_resolution_field_map (lambda (fields) (let ((to_pair (lambda (f) (list (funcall (lambda (v) (hydra_core_field-name v)) f) (funcall (lambda (v) (hydra_core_field-term v)) f))))) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map to_pair) fields)))))

(defvar hydra_resolution_field_type_map (lambda (fields) (let ((to_pair (lambda (f) (list (funcall (lambda (v) (hydra_core_field_type-name v)) f) (funcall (lambda (v) (hydra_core_field_type-type v)) f))))) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map to_pair) fields)))))

(defvar hydra_resolution_field_types (lambda (cx) (lambda (graph) (lambda (t_) (let ((to_map (lambda (fields) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (ft) (list (funcall (lambda (v) (hydra_core_field_type-name v)) ft) (funcall (lambda (v) (hydra_core_field_type-type v)) ft)))) fields))))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :forall) (funcall (lambda (ft) (funcall (funcall (hydra_resolution_field_types cx) graph) (funcall (lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) ((equal (car match_target) :record) (funcall (lambda (rt) (list :right (to_map rt))) match_value)) ((equal (car match_target) :union) (funcall (lambda (rt) (list :right (to_map rt))) match_value)) ((equal (car match_target) :variable) (funcall (lambda (name) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lexical_require_binding cx) graph) name)) (lambda (el) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e cx))) (lambda (_wc_a) _wc_a)) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (list :other (funcall (lambda (v) v) _e)))) (lambda (_a) _a)) (funcall (hydra_decode_core_type graph) (funcall (lambda (v) (hydra_core_binding-term v)) el))))) (lambda (decoded_type) (funcall (funcall (hydra_resolution_field_types cx) graph) decoded_type))))))) (lambda (ts) (funcall (funcall (hydra_resolution_field_types cx) graph) (funcall (lambda (v) (hydra_core_type_scheme-type v)) ts)))) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-schema_types v)) graph)))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (hydra_lib_strings_cat (list "expected record or union type but found " (hydra_show_core_type t_)))) cx))))) (cadr match_target))) (hydra_strip_deannotate_type t_)))))))

(defvar hydra_resolution_find_field_type (lambda (cx) (lambda (fname) (lambda (fields) (let ((matching_fields (funcall (hydra_lib_lists_filter (lambda (ft) (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_field_type-name v)) ft))) (funcall (lambda (v) v) fname)))) fields))) (if (hydra_lib_lists_null matching_fields) (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "No such field: ") (funcall (lambda (v) v) fname))) cx)) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length matching_fields)) 1) (list :right (funcall (lambda (v) (hydra_core_field_type-type v)) (hydra_lib_lists_head matching_fields))) (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "Multiple fields named ") (funcall (lambda (v) v) fname))) cx)))))))))

(defvar hydra_resolution_fully_strip_and_normalize_type (lambda (typ) (letrec ((go_ (lambda (depth) (lambda (subst) (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :forall) (funcall (lambda (ft) (let ((old_var (funcall (lambda (v) (hydra_core_forall_type-parameter v)) ft))) (let ((new_var (funcall (hydra_lib_strings_cat2 "_") (hydra_lib_literals_show_int32 depth)))) (funcall (funcall (go_ (funcall (hydra_lib_math_add depth) 1)) (funcall (funcall (hydra_lib_maps_insert old_var) new_var) subst)) (funcall (lambda (v) (hydra_core_forall_type-body v)) ft))))) match_value)) (t (list subst t_)))) (cadr match_target))) (hydra_strip_deannotate_type t_))))))) (let ((result (funcall (funcall (go_ 0) hydra_lib_maps_empty) typ))) (let ((subst (hydra_lib_pairs_first result))) (let ((body (hydra_lib_pairs_second result))) (funcall (hydra_variables_substitute_type_variables subst) body)))))))

(defvar hydra_resolution_fully_strip_type (lambda (typ) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :forall) (funcall (lambda (ft) (hydra_resolution_fully_strip_type (funcall (lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) (t typ))) (cadr match_target))) (hydra_strip_deannotate_type typ))))

(defvar hydra_resolution_instantiate_type_scheme (lambda (cx) (lambda (scheme) (let ((old_vars (funcall (lambda (v) (hydra_core_type_scheme-variables v)) scheme))) (let ((result (funcall (hydra_names_fresh_names (hydra_lib_lists_length old_vars)) cx))) (let ((new_vars (hydra_lib_pairs_first result))) (let ((cx2 (hydra_lib_pairs_second result))) (let ((subst (hydra_lib_maps_from_list (funcall (hydra_lib_lists_zip old_vars) (funcall (hydra_lib_lists_map (lambda (x) (list :variable x))) new_vars))))) (let ((name_subst (hydra_lib_maps_from_list (funcall (hydra_lib_lists_zip old_vars) new_vars)))) (let ((renamed_constraints (funcall (hydra_lib_maybes_map (lambda (old_constraints) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (kv) (list (funcall (hydra_lib_maybes_from_maybe (lambda () (hydra_lib_pairs_first kv))) (funcall (hydra_lib_maps_lookup (hydra_lib_pairs_first kv)) name_subst)) (hydra_lib_pairs_second kv)))) (hydra_lib_maps_to_list old_constraints))))) (funcall (lambda (v) (hydra_core_type_scheme-constraints v)) scheme)))) (list (make-hydra_core_type_scheme new_vars (funcall (hydra_substitution_subst_in_type subst) (funcall (lambda (v) (hydra_core_type_scheme-type v)) scheme)) renamed_constraints) cx2)))))))))))

(defvar hydra_resolution_type_to_type_scheme (lambda (t0) (letrec ((helper (lambda (vars) (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :forall) (funcall (lambda (ft) (funcall (helper (funcall (hydra_lib_lists_cons (funcall (lambda (v) (hydra_core_forall_type-parameter v)) ft)) vars)) (funcall (lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) (t (make-hydra_core_type_scheme (hydra_lib_lists_reverse vars) t_ (list :nothing))))) (cadr match_target))) (hydra_strip_deannotate_type t_)))))) (funcall (helper (list)) t0))))

(defvar hydra_resolution_instantiate_type (lambda (cx) (lambda (typ) (let ((result (funcall (hydra_resolution_instantiate_type_scheme cx) (hydra_resolution_type_to_type_scheme typ)))) (list (hydra_scoping_type_scheme_to_f_type (hydra_lib_pairs_first result)) (hydra_lib_pairs_second result))))))

(defvar hydra_resolution_nominal_application (lambda (tname) (lambda (args) (funcall (funcall (hydra_lib_lists_foldl (lambda (t_) (lambda (a) (list :application (make-hydra_core_application_type t_ a))))) (list :variable tname)) args))))

(defvar hydra_resolution_require_type (lambda (cx) (lambda (graph) (lambda (name) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "no such type: ") (funcall (lambda (v) v) name))) cx)))) (lambda (ts) (list :right (hydra_scoping_type_scheme_to_f_type ts)))) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-bound_types v)) graph))))) (lambda (ts) (list :right (hydra_scoping_type_scheme_to_f_type ts)))) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-schema_types v)) graph)))))))

(defvar hydra_resolution_require_row_type (lambda (cx) (lambda (label) (lambda (getter) (lambda (graph) (lambda (name) (letrec ((raw_type (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :annotated) (funcall (lambda (at) (raw_type (funcall (lambda (v) (hydra_core_annotated_type-body v)) at))) match_value)) ((equal (car match_target) :forall) (funcall (lambda (ft) (raw_type (funcall (lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) (t t_))) (cadr match_target))) t_)))) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_resolution_require_type cx) graph) name)) (lambda (t_) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (hydra_lib_strings_cat (list (funcall (lambda (v) v) name) " does not resolve to a " label " type: " (hydra_show_core_type t_)))) cx)))) (lambda (x) (list :right x))) (getter (raw_type t_))))))))))))

(defvar hydra_resolution_require_record_type (lambda (cx) (lambda (graph) (lambda (name) (let ((to_record (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (rt) (list :just rt)) match_value)) (t (list :nothing)))) (cadr match_target))) t_)))) (funcall (funcall (funcall (funcall (hydra_resolution_require_row_type cx) "record type") to_record) graph) name))))))

(defvar hydra_resolution_require_schema_type (lambda (cx) (lambda (types) (lambda (tname) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other (hydra_lib_strings_cat (list "No such schema type: " (funcall (lambda (v) v) tname) ". Available types are: " (funcall (hydra_lib_strings_intercalate ", ") (funcall (hydra_lib_lists_map (lambda (v) v)) (hydra_lib_maps_keys types)))))) cx)))) (lambda (ts) (list :right (funcall (hydra_resolution_instantiate_type_scheme cx) (hydra_strip_deannotate_type_scheme_recursive ts))))) (funcall (hydra_lib_maps_lookup tname) types))))))

(defvar hydra_resolution_require_union_type (lambda (cx) (lambda (graph) (lambda (name) (let ((to_union (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (rt) (list :just rt)) match_value)) (t (list :nothing)))) (cadr match_target))) t_)))) (funcall (funcall (funcall (funcall (hydra_resolution_require_row_type cx) "union") to_union) graph) name))))))

(defvar hydra_resolution_require_union_field (lambda (cx) (lambda (graph) (lambda (tname) (lambda (fname) (let ((with_row_type (lambda (rt) (let ((matches (funcall (hydra_lib_lists_filter (lambda (ft) (funcall (hydra_lib_equality_equal (funcall (lambda (v) (hydra_core_field_type-name v)) ft)) fname))) rt))) (if (hydra_lib_lists_null matches) (list :left (make-hydra_context_in_context (list :other (hydra_lib_strings_cat (list "no field \"" (funcall (lambda (v) v) fname) "\" in union type \"" (funcall (lambda (v) v) tname)))) cx)) (list :right (funcall (lambda (v) (hydra_core_field_type-type v)) (hydra_lib_lists_head matches)))))))) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_resolution_require_union_type cx) graph) tname)) with_row_type)))))))

(defvar hydra_resolution_resolve_type (lambda (graph) (lambda (typ) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :variable) (funcall (lambda (name) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (funcall (hydra_lib_maybes_map (lambda (ts) (hydra_scoping_type_scheme_to_f_type ts))) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-bound_types v)) graph))))) (lambda (ts) (list :just (hydra_scoping_type_scheme_to_f_type ts)))) (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-schema_types v)) graph)))) match_value)) (t (list :just typ)))) (cadr match_target))) (hydra_strip_deannotate_type typ)))))

(provide 'hydra.resolution)

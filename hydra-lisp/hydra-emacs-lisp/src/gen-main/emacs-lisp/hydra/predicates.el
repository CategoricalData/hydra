(require 'cl-lib)

(require 'hydra.arity)

(require 'hydra.coders)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.dependencies)

(require 'hydra.errors)

(require 'hydra.graph)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.reflect)

(require 'hydra.rewriting)

(require 'hydra.strip)

(require 'hydra.variants)

(defvar hydra_predicates_is_complex_variable (lambda (tc) (lambda (name) (let ((meta_lookup (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-metadata v)) tc)))) (if (hydra_lib_maybes_is_just meta_lookup) t (if (funcall (hydra_lib_sets_member name) (funcall (lambda (v) (hydra_graph_graph-lambda_variables v)) tc)) t (let ((type_lookup (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_graph_graph-bound_types v)) tc)))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () t)) (lambda (ts) (funcall (hydra_lib_equality_gt (hydra_arity_type_scheme_arity ts)) 0))) type_lookup))))))))

(defvar hydra_predicates_is_complex_term (lambda (tc) (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :let) (funcall (lambda (_) t) match_value)) ((equal (car match_target) :type_application) (funcall (lambda (_) t) match_value)) ((equal (car match_target) :type_lambda) (funcall (lambda (_) t) match_value)) ((equal (car match_target) :variable) (funcall (lambda (name) (funcall (hydra_predicates_is_complex_variable tc) name)) match_value)) (t (funcall (funcall (hydra_lib_lists_foldl (lambda (b) (lambda (sub) (funcall (hydra_lib_logic_or b) (funcall (hydra_predicates_is_complex_term tc) sub))))) nil) (hydra_rewriting_subterms t_))))) (cadr match_target))) t_))))

(defvar hydra_predicates_is_complex_binding (lambda (tc) (lambda (b) (let ((term (funcall (lambda (v) (hydra_core_binding-term v)) b))) (let ((mts (funcall (lambda (v) (hydra_core_binding-type v)) b))) (funcall (funcall (hydra_lib_maybes_cases mts) (lambda () (funcall (hydra_predicates_is_complex_term tc) term))) (lambda (ts) (let ((is_polymorphic (hydra_lib_logic_not (hydra_lib_lists_null (funcall (lambda (v) (hydra_core_type_scheme-variables v)) ts))))) (let ((is_non_nullary (funcall (hydra_lib_equality_gt (hydra_arity_type_arity (funcall (lambda (v) (hydra_core_type_scheme-type v)) ts))) 0))) (let ((is_complex (funcall (hydra_predicates_is_complex_term tc) term))) (funcall (hydra_lib_logic_or (funcall (hydra_lib_logic_or is_polymorphic) is_non_nullary)) is_complex)))))))))))

(defvar hydra_predicates_is_encoded_term (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :application) (funcall (lambda (a) (hydra_predicates_is_encoded_term (funcall (lambda (v) (hydra_core_application-function v)) a))) match_value)) ((equal (car match_target) :union) (funcall (lambda (i) (funcall (hydra_lib_equality_equal "hydra.core.Term") (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_injection-type_name v)) i)))) match_value)) (t nil))) (cadr match_target))) (hydra_strip_deannotate_term t_))))

(defvar hydra_predicates_is_encoded_type (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :application) (funcall (lambda (a) (hydra_predicates_is_encoded_type (funcall (lambda (v) (hydra_core_application-function v)) a))) match_value)) ((equal (car match_target) :union) (funcall (lambda (i) (funcall (hydra_lib_equality_equal "hydra.core.Type") (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_injection-type_name v)) i)))) match_value)) (t nil))) (cadr match_target))) (hydra_strip_deannotate_term t_))))

(defvar hydra_predicates_is_unit_type (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unit) (funcall (lambda (_) t) match_value)) (t nil))) (cadr match_target))))

(defvar hydra_predicates_is_enum_row_type (lambda (rt) (funcall (funcall (hydra_lib_lists_foldl hydra_lib_logic_and) t) (funcall (hydra_lib_lists_map (lambda (f) (hydra_predicates_is_unit_type (hydra_strip_deannotate_type (funcall (lambda (v) (hydra_core_field_type-type v)) f))))) rt))))

(defvar hydra_predicates_is_enum_type (lambda (typ) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (rt) (hydra_predicates_is_enum_row_type rt)) match_value)) (t nil))) (cadr match_target))) (hydra_strip_deannotate_type typ))))

(defvar hydra_predicates_is_nominal_type (lambda (typ) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (rt) t) match_value)) ((equal (car match_target) :union) (funcall (lambda (rt) t) match_value)) ((equal (car match_target) :wrap) (funcall (lambda (wt) t) match_value)) ((equal (car match_target) :forall) (funcall (lambda (fa) (hydra_predicates_is_nominal_type (funcall (lambda (v) (hydra_core_forall_type-body v)) fa))) match_value)) (t nil))) (cadr match_target))) (hydra_strip_deannotate_type typ))))

(defvar hydra_predicates_type_dependencies (lambda (cx) (lambda (graph) (lambda (with_schema) (lambda (transform) (lambda (name) (let ((require_type (lambda (name2) (let ((cx1 (make-hydra_context_context (funcall (hydra_lib_lists_cons (funcall (hydra_lib_strings_cat2 "type dependencies of ") (funcall (lambda (v) v) name2))) (funcall (lambda (v) (hydra_context_context-trace v)) cx)) (funcall (lambda (v) (hydra_context_context-messages v)) cx) (funcall (lambda (v) (hydra_context_context-other v)) cx)))) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lexical_require_binding cx1) graph) name2)) (lambda (el) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e cx1))) (lambda (_wc_a) _wc_a)) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (list :other (funcall (lambda (v) v) _e)))) (lambda (_a) _a)) (funcall (hydra_decode_core_type graph) (funcall (lambda (v) (hydra_core_binding-term v)) el)))))))))) (let ((to_pair (lambda (name2) (funcall (hydra_lib_eithers_map (lambda (typ) (list name2 (transform typ)))) (require_type name2))))) (letrec ((deps (lambda (seeds) (lambda (names) (if (hydra_lib_sets_null seeds) (list :right names) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list to_pair) (hydra_lib_sets_to_list seeds))) (lambda (pairs) (let ((new_names (funcall (hydra_lib_maps_union names) (hydra_lib_maps_from_list pairs)))) (let ((refs (funcall (funcall (hydra_lib_lists_foldl hydra_lib_sets_union) hydra_lib_sets_empty) (funcall (hydra_lib_lists_map (lambda (pair) (funcall (hydra_dependencies_type_dependency_names with_schema) (hydra_lib_pairs_second pair)))) pairs)))) (let ((visited (hydra_lib_sets_from_list (hydra_lib_maps_keys names)))) (let ((new_seeds (funcall (hydra_lib_sets_difference refs) visited))) (funcall (deps new_seeds) new_names)))))))))))) (funcall (deps (hydra_lib_sets_singleton name)) hydra_lib_maps_empty))))))))))

(defvar hydra_predicates_is_serializable (lambda (cx) (lambda (graph) (lambda (el) (let ((variants (lambda (typ) (funcall (hydra_lib_lists_map hydra_reflect_type_variant) (funcall (funcall (funcall (hydra_rewriting_fold_over_type (list :pre nil)) (lambda (m) (lambda (t_) (funcall (hydra_lib_lists_cons t_) m)))) (list)) typ))))) (funcall (hydra_lib_eithers_map (lambda (deps) (let ((all_variants (hydra_lib_sets_from_list (hydra_lib_lists_concat (funcall (hydra_lib_lists_map variants) (hydra_lib_maps_elems deps)))))) (hydra_lib_logic_not (funcall (hydra_lib_sets_member (list :function nil)) all_variants))))) (funcall (funcall (funcall (funcall (hydra_predicates_type_dependencies cx) graph) nil) hydra_lib_equality_identity) (funcall (lambda (v) (hydra_core_binding-name v)) el))))))))

(defvar hydra_predicates_is_serializable_by_name (lambda (cx) (lambda (graph) (lambda (name) (let ((variants (lambda (typ) (funcall (hydra_lib_lists_map hydra_reflect_type_variant) (funcall (funcall (funcall (hydra_rewriting_fold_over_type (list :pre nil)) (lambda (m) (lambda (t_) (funcall (hydra_lib_lists_cons t_) m)))) (list)) typ))))) (funcall (hydra_lib_eithers_map (lambda (deps) (let ((all_variants (hydra_lib_sets_from_list (hydra_lib_lists_concat (funcall (hydra_lib_lists_map variants) (hydra_lib_maps_elems deps)))))) (hydra_lib_logic_not (funcall (hydra_lib_sets_member (list :function nil)) all_variants))))) (funcall (funcall (funcall (funcall (hydra_predicates_type_dependencies cx) graph) nil) hydra_lib_equality_identity) name)))))))

(defvar hydra_predicates_is_serializable_type (lambda (typ) (let ((all_variants (hydra_lib_sets_from_list (funcall (hydra_lib_lists_map hydra_reflect_type_variant) (funcall (funcall (funcall (hydra_rewriting_fold_over_type (list :pre nil)) (lambda (m) (lambda (t_) (funcall (hydra_lib_lists_cons t_) m)))) (list)) typ))))) (hydra_lib_logic_not (funcall (hydra_lib_sets_member (list :function nil)) all_variants)))))

(defvar hydra_predicates_is_trivial_term (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (_) t) match_value)) ((equal (car match_target) :variable) (funcall (lambda (_) t) match_value)) ((equal (car match_target) :unit) (funcall (lambda (_) t) match_value)) ((equal (car match_target) :application) (funcall (lambda (app) (let ((fun (funcall (lambda (v) (hydra_core_application-function v)) app))) (let ((arg (funcall (lambda (v) (hydra_core_application-argument v)) app))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :function) (funcall (lambda (f) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :elimination) (funcall (lambda (e) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (_) (hydra_predicates_is_trivial_term arg)) match_value)) ((equal (car match_target) :wrap) (funcall (lambda (_) (hydra_predicates_is_trivial_term arg)) match_value)) (t nil))) (cadr match_target))) e)) match_value)) (t nil))) (cadr match_target))) f)) match_value)) (t nil))) (cadr match_target))) fun)))) match_value)) ((equal (car match_target) :maybe) (funcall (lambda (opt) (funcall (funcall (hydra_lib_maybes_maybe (lambda () t)) (lambda (inner) (hydra_predicates_is_trivial_term inner))) opt)) match_value)) ((equal (car match_target) :record) (funcall (lambda (rec) (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (fld) (funcall (hydra_lib_logic_and acc) (hydra_predicates_is_trivial_term (funcall (lambda (v) (hydra_core_field-term v)) fld)))))) t) (funcall (lambda (v) (hydra_core_record-fields v)) rec))) match_value)) ((equal (car match_target) :wrap) (funcall (lambda (wt) (hydra_predicates_is_trivial_term (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wt))) match_value)) ((equal (car match_target) :type_application) (funcall (lambda (ta) (hydra_predicates_is_trivial_term (funcall (lambda (v) (hydra_core_type_application_term-body v)) ta))) match_value)) ((equal (car match_target) :type_lambda) (funcall (lambda (tl) (hydra_predicates_is_trivial_term (funcall (lambda (v) (hydra_core_type_lambda-body v)) tl))) match_value)) (t nil))) (cadr match_target))) (hydra_strip_deannotate_term t_))))

(defvar hydra_predicates_is_type (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :application) (funcall (lambda (a) (hydra_predicates_is_type (funcall (lambda (v) (hydra_core_application_type-function v)) a))) match_value)) ((equal (car match_target) :forall) (funcall (lambda (l) (hydra_predicates_is_type (funcall (lambda (v) (hydra_core_forall_type-body v)) l))) match_value)) ((equal (car match_target) :union) (funcall (lambda (rt) nil) match_value)) ((equal (car match_target) :variable) (funcall (lambda (v) (funcall (hydra_lib_equality_equal v) "hydra.core.Type")) match_value)) (t nil))) (cadr match_target))) (hydra_strip_deannotate_type t_))))

(defvar hydra_predicates_is_unit_term (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unit) (funcall (lambda (_) t) match_value)) (t nil))) (cadr match_target))))

(provide 'hydra.predicates)

(require 'cl-lib)

(require 'hydra.annotations)

(require 'hydra.coders)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.dependencies)

(require 'hydra.errors)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.sets)

(require 'hydra.module)

(require 'hydra.names)

(require 'hydra.predicates)

(require 'hydra.rewriting)

(require 'hydra.strip)

(defvar hydra_analysis_add_names_to_namespaces (lambda (encode_namespace) (lambda (names) (lambda (ns0) (let ((nss (hydra_lib_sets_from_list (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map hydra_names_namespace_of) (hydra_lib_sets_to_list names)))))) (let ((to_pair (lambda (ns_) (list ns_ (encode_namespace ns_))))) (make-hydra_module_namespaces (funcall (lambda (v) (hydra_module_namespaces-focus v)) ns0) (funcall (hydra_lib_maps_union (funcall (lambda (v) (hydra_module_namespaces-mapping v)) ns0)) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map to_pair) (hydra_lib_sets_to_list nss)))))))))))

(defvar hydra_analysis_definition_dependency_namespaces (lambda (defs) (let ((def_names (lambda (def_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :type) (funcall (lambda (type_def) (funcall (hydra_dependencies_type_dependency_names t) (funcall (lambda (v) (hydra_module_type_definition-type v)) type_def))) match_value)) ((equal (car match_target) :term) (funcall (lambda (term_def) (funcall (funcall (funcall (hydra_dependencies_term_dependency_names t) t) t) (funcall (lambda (v) (hydra_module_term_definition-term v)) term_def))) match_value)))) (cadr match_target))) def_)))) (let ((all_names (hydra_lib_sets_unions (funcall (hydra_lib_lists_map def_names) defs)))) (hydra_lib_sets_from_list (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map hydra_names_namespace_of) (hydra_lib_sets_to_list all_names))))))))

(defvar hydra_analysis_dependency_namespaces (lambda (cx) (lambda (graph) (lambda (binds) (lambda (with_prims) (lambda (with_noms) (lambda (with_schema) (lambda (els) (let ((dep_names (lambda (el) (let ((term (funcall (lambda (v) (hydra_core_binding-term v)) el))) (let ((deannotated_term (hydra_strip_deannotate_term term))) (let ((data_names (funcall (funcall (funcall (hydra_dependencies_term_dependency_names binds) with_prims) with_noms) term))) (let ((schema_names (if with_schema (funcall (funcall (hydra_lib_maybes_maybe (lambda () hydra_lib_sets_empty)) (lambda (ts) (funcall (hydra_dependencies_type_dependency_names t) (funcall (lambda (v) (hydra_core_type_scheme-type v)) ts)))) (funcall (lambda (v) (hydra_core_binding-type v)) el)) hydra_lib_sets_empty))) (if (hydra_predicates_is_encoded_type deannotated_term) (funcall (hydra_lib_eithers_map (lambda (typ) (hydra_lib_sets_unions (list data_names schema_names (funcall (hydra_dependencies_type_dependency_names t) typ))))) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e (make-hydra_context_context (funcall (hydra_lib_lists_cons "dependency namespace (type)") (funcall (lambda (v) (hydra_context_context-trace v)) cx)) (funcall (lambda (v) (hydra_context_context-messages v)) cx) (funcall (lambda (v) (hydra_context_context-other v)) cx))))) (lambda (_wc_a) _wc_a)) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (list :other (funcall (lambda (v) v) _e)))) (lambda (_a) _a)) (funcall (hydra_decode_core_type graph) term)))) (if (hydra_predicates_is_encoded_term deannotated_term) (funcall (hydra_lib_eithers_map (lambda (decoded_term) (hydra_lib_sets_unions (list data_names schema_names (funcall (funcall (funcall (hydra_dependencies_term_dependency_names binds) with_prims) with_noms) decoded_term))))) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e (make-hydra_context_context (funcall (hydra_lib_lists_cons "dependency namespace (term)") (funcall (lambda (v) (hydra_context_context-trace v)) cx)) (funcall (lambda (v) (hydra_context_context-messages v)) cx) (funcall (lambda (v) (hydra_context_context-other v)) cx))))) (lambda (_wc_a) _wc_a)) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (list :other (funcall (lambda (v) v) _e)))) (lambda (_a) _a)) (funcall (hydra_decode_core_term graph) term)))) (list :right (hydra_lib_sets_unions (list data_names schema_names)))))))))))) (funcall (hydra_lib_eithers_map (lambda (names_list) (hydra_lib_sets_from_list (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map hydra_names_namespace_of) (hydra_lib_sets_to_list (hydra_lib_sets_unions names_list))))))) (funcall (hydra_lib_eithers_map_list dep_names) els)))))))))))

(defvar hydra_analysis_module_contains_binary_literals (lambda (mod) (let ((check_term (lambda (found) (lambda (term) (funcall (hydra_lib_logic_or found) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (lit) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :binary) (funcall (lambda (_) t) match_value)) (t nil))) (cadr match_target))) lit)) match_value)) (t nil))) (cadr match_target))) term)))))) (let ((term_contains_binary (lambda (term) (funcall (funcall (funcall (hydra_rewriting_fold_over_term (list :pre nil)) check_term) nil) term)))) (let ((def_terms (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (d) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :term) (funcall (lambda (td) (list :just (funcall (lambda (v) (hydra_module_term_definition-term v)) td))) match_value)) (t (list :nothing)))) (cadr match_target))) d))) (funcall (lambda (v) (hydra_module_module-definitions v)) mod))))) (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (t_) (funcall (hydra_lib_logic_or acc) (term_contains_binary t_))))) nil) def_terms))))))

(defvar hydra_analysis_module_dependency_namespaces (lambda (cx) (lambda (graph) (lambda (binds) (lambda (with_prims) (lambda (with_noms) (lambda (with_schema) (lambda (mod) (let ((all_bindings (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (d) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :type) (funcall (lambda (td) (list :just (funcall (hydra_annotations_type_element (funcall (lambda (v) (hydra_module_type_definition-name v)) td)) (funcall (lambda (v) (hydra_module_type_definition-type v)) td)))) match_value)) ((equal (car match_target) :term) (funcall (lambda (td) (list :just (make-hydra_core_binding (funcall (lambda (v) (hydra_module_term_definition-name v)) td) (funcall (lambda (v) (hydra_module_term_definition-term v)) td) (funcall (lambda (v) (hydra_module_term_definition-type v)) td)))) match_value)) (t (list :nothing)))) (cadr match_target))) d))) (funcall (lambda (v) (hydra_module_module-definitions v)) mod))))) (funcall (hydra_lib_eithers_map (lambda (deps) (funcall (hydra_lib_sets_delete (funcall (lambda (v) (hydra_module_module-namespace v)) mod)) deps))) (funcall (funcall (funcall (funcall (funcall (funcall (hydra_analysis_dependency_namespaces cx) graph) binds) with_prims) with_noms) with_schema) all_bindings)))))))))))

(defvar hydra_analysis_namespaces_for_definitions (lambda (encode_namespace) (lambda (focus_ns) (lambda (defs) (let ((nss (funcall (hydra_lib_sets_delete focus_ns) (hydra_analysis_definition_dependency_namespaces defs)))) (let ((to_pair (lambda (ns_) (list ns_ (encode_namespace ns_))))) (make-hydra_module_namespaces (to_pair focus_ns) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map to_pair) (hydra_lib_sets_to_list nss))))))))))

(provide 'hydra.analysis)

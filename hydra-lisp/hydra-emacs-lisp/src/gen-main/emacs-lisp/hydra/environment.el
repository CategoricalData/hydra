(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.encode.core)

(require 'hydra.errors)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.module)

(require 'hydra.scoping)

(require 'hydra.strip)

(defvar hydra_environment_element_as_type_application_term (lambda (cx) (lambda (el) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (make-hydra_context_in_context (list :other "missing element type") cx)))) (lambda (ts) (list :right (make-hydra_core_type_application_term (funcall (lambda (v) (hydra_core_binding-term v)) el) (funcall (lambda (v) (hydra_core_type_scheme-type v)) ts))))) (funcall (lambda (v) (hydra_core_binding-type v)) el)))))

(defvar hydra_environment_graph_as_let (lambda (bindings) (lambda (body) (make-hydra_core_let bindings body))))

(defvar hydra_environment_graph_as_term (lambda (bindings) (lambda (body) (list :let (funcall (hydra_environment_graph_as_let bindings) body)))))

(defvar hydra_environment_graph_as_types (lambda (cx) (lambda (graph) (lambda (els) (let ((to_pair (lambda (el) (funcall (hydra_lib_eithers_map (lambda (typ) (list (funcall (lambda (v) (hydra_core_binding-name v)) el) typ))) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e cx))) (lambda (_wc_a) _wc_a)) (funcall (hydra_decode_core_type graph) (funcall (lambda (v) (hydra_core_binding-term v)) el))))))) (funcall (hydra_lib_eithers_map hydra_lib_maps_from_list) (funcall (hydra_lib_eithers_map_list to_pair) els)))))))

(defvar hydra_environment_partition_definitions (lambda (defs) (let ((get_type (lambda (def_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :type) (funcall (lambda (td) (list :just td)) match_value)) ((equal (car match_target) :term) (funcall (lambda (_) (list :nothing)) match_value)))) (cadr match_target))) def_)))) (let ((get_term (lambda (def_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :type) (funcall (lambda (_) (list :nothing)) match_value)) ((equal (car match_target) :term) (funcall (lambda (td) (list :just td)) match_value)))) (cadr match_target))) def_)))) (list (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map get_type) defs)) (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map get_term) defs)))))))

(defvar hydra_environment_schema_graph_to_typing_environment (lambda (cx) (lambda (g) (letrec ((to_type_scheme (lambda (vars) (lambda (typ) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :forall) (funcall (lambda (ft) (funcall (to_type_scheme (funcall (hydra_lib_lists_cons (funcall (lambda (v) (hydra_core_forall_type-parameter v)) ft)) vars)) (funcall (lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) (t (make-hydra_core_type_scheme (hydra_lib_lists_reverse vars) typ (list :nothing))))) (cadr match_target))) (hydra_strip_deannotate_type typ)))))) (let ((decode_type (lambda (term) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e cx))) (lambda (_wc_a) _wc_a)) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (list :other (funcall (lambda (v) v) _e)))) (lambda (_a) _a)) (funcall (hydra_decode_core_type g) term)))))) (let ((decode_type_scheme (lambda (term) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e cx))) (lambda (_wc_a) _wc_a)) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (list :other (funcall (lambda (v) v) _e)))) (lambda (_a) _a)) (funcall (hydra_decode_core_type_scheme g) term)))))) (let ((to_pair (lambda (el) (let ((for_term (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (r) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) (hydra_core_record-type_name v)) r)) "hydra.core.TypeScheme") (funcall (hydra_lib_eithers_map hydra_lib_maybes_pure) (decode_type_scheme (funcall (lambda (v) (hydra_core_binding-term v)) el))) (list :right (list :nothing)))) match_value)) ((equal (car match_target) :union) (funcall (lambda (i) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) (hydra_core_injection-type_name v)) i)) "hydra.core.Type") (funcall (hydra_lib_eithers_map (lambda (decoded) (list :just (funcall (to_type_scheme (list)) decoded)))) (decode_type (funcall (lambda (v) (hydra_core_binding-term v)) el))) (list :right (list :nothing)))) match_value)) (t (list :right (list :nothing))))) (cadr match_target))) term)))) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_maybes_maybe (lambda () (funcall (hydra_lib_eithers_map (lambda (typ) (list :just (hydra_scoping_f_type_to_type_scheme typ)))) (decode_type (funcall (lambda (v) (hydra_core_binding-term v)) el))))) (lambda (ts) (if (funcall (hydra_lib_equality_equal ts) (make-hydra_core_type_scheme (list) (list :variable "hydra.core.TypeScheme") (list :nothing))) (funcall (hydra_lib_eithers_map hydra_lib_maybes_pure) (decode_type_scheme (funcall (lambda (v) (hydra_core_binding-term v)) el))) (if (funcall (hydra_lib_equality_equal ts) (make-hydra_core_type_scheme (list) (list :variable "hydra.core.Type") (list :nothing))) (funcall (hydra_lib_eithers_map (lambda (decoded) (list :just (funcall (to_type_scheme (list)) decoded)))) (decode_type (funcall (lambda (v) (hydra_core_binding-term v)) el))) (for_term (hydra_strip_deannotate_term (funcall (lambda (v) (hydra_core_binding-term v)) el))))))) (funcall (lambda (v) (hydra_core_binding-type v)) el))) (lambda (mts) (list :right (funcall (hydra_lib_maybes_map (lambda (ts) (list (funcall (lambda (v) (hydra_core_binding-name v)) el) ts))) mts)))))))) (funcall (hydra_lib_eithers_map (lambda (mpairs) (hydra_lib_maps_from_list (hydra_lib_maybes_cat mpairs)))) (funcall (hydra_lib_eithers_map_list to_pair) (hydra_lexical_graph_to_bindings g))))))))))

(defvar hydra_environment_term_as_bindings (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :let) (funcall (lambda (lt) (funcall (lambda (v) (hydra_core_let-bindings v)) lt)) match_value)) (t (list)))) (cadr match_target))) (hydra_strip_deannotate_term term))))

(defvar hydra_environment_types_to_elements (lambda (type_map) (let ((to_element (lambda (pair) (let ((name (hydra_lib_pairs_first pair))) (make-hydra_core_binding name (hydra_encode_core_type (hydra_lib_pairs_second pair)) (list :nothing)))))) (funcall (hydra_lib_lists_map to_element) (hydra_lib_maps_to_list type_map)))))

(defvar hydra_environment_with_lambda_context (lambda (get_context) (lambda (set_context) (lambda (env) (lambda (lam) (lambda (body) (let ((new_context (funcall (hydra_scoping_extend_graph_for_lambda (get_context env)) lam))) (body (funcall (set_context new_context) env)))))))))

(defvar hydra_environment_with_let_context (lambda (get_context) (lambda (set_context) (lambda (for_binding) (lambda (env) (lambda (letrec_) (lambda (body) (let ((new_context (funcall (funcall (hydra_scoping_extend_graph_for_let for_binding) (get_context env)) letrec_))) (body (funcall (set_context new_context) env))))))))))

(defvar hydra_environment_with_type_lambda_context (lambda (get_context) (lambda (set_context) (lambda (env) (lambda (tlam) (lambda (body) (let ((new_context (funcall (hydra_scoping_extend_graph_for_type_lambda (get_context env)) tlam))) (body (funcall (set_context new_context) env)))))))))

(provide 'hydra.environment)

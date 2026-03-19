(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.error)

(require 'hydra.lib.eithers)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(defvar hydra_templates_graph_to_schema (lambda (cx) (lambda (graph) (lambda (els) (let ((to_pair (lambda (el) (let ((name ((lambda (v) (hydra_core_binding-name v)) el))) ((hydra_lib_eithers_bind (((hydra_lib_eithers_bimap (lambda (_wc_e) (make-hydra_context_in_context _wc_e cx))) (lambda (_wc_a) _wc_a)) ((hydra_decode_core_type graph) ((lambda (v) (hydra_core_binding-term v)) el)))) (lambda (t_) (list :right (list name t_)))))))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list to_pair) els)) (lambda (pairs) (list :right (hydra_lib_maps_from_list pairs)))))))))

(defvar hydra_templates_instantiate_template (lambda (cx) (lambda (minimal) (lambda (schema) (lambda (tname) (lambda (t_) (let ((inst (lambda (tn) ((((hydra_templates_instantiate_template cx) minimal) schema) tn)))) (let ((no_poly (list :left (make-hydra_context_in_context (list :other "Polymorphic and function types are not currently supported") cx)))) (let ((for_float (lambda (ft) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :bigfloat) ((lambda (_) (list :bigfloat 0.0)) match_value)) ((equal (car match_target) :float32) ((lambda (_) (list :float32 0.0)) match_value)) ((equal (car match_target) :float64) ((lambda (_) (list :float64 0.0)) match_value)))) (cadr match_target))) ft)))) (let ((for_integer (lambda (it) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :bigint) ((lambda (_) (list :bigint 0)) match_value)) ((equal (car match_target) :int8) ((lambda (_) (list :int8 0)) match_value)) ((equal (car match_target) :int16) ((lambda (_) (list :int16 0)) match_value)) ((equal (car match_target) :int32) ((lambda (_) (list :int32 0)) match_value)) ((equal (car match_target) :int64) ((lambda (_) (list :int64 0)) match_value)) ((equal (car match_target) :uint8) ((lambda (_) (list :uint8 0)) match_value)) ((equal (car match_target) :uint16) ((lambda (_) (list :uint16 0)) match_value)) ((equal (car match_target) :uint32) ((lambda (_) (list :uint32 0)) match_value)) ((equal (car match_target) :uint64) ((lambda (_) (list :uint64 0)) match_value)))) (cadr match_target))) it)))) (let ((for_literal (lambda (lt) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :binary) ((lambda (_) (list :string "")) match_value)) ((equal (car match_target) :boolean) ((lambda (_) (list :boolean nil)) match_value)) ((equal (car match_target) :integer) ((lambda (it) (list :integer (for_integer it))) match_value)) ((equal (car match_target) :float) ((lambda (ft) (list :float (for_float ft))) match_value)) ((equal (car match_target) :string) ((lambda (_) (list :string "")) match_value)))) (cadr match_target))) lt)))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :annotated) ((lambda (at) ((inst tname) ((lambda (v) (hydra_core_annotated_type-body v)) at))) match_value)) ((equal (car match_target) :application) ((lambda (_) no_poly) match_value)) ((equal (car match_target) :function) ((lambda (_) no_poly) match_value)) ((equal (car match_target) :forall) ((lambda (_) no_poly) match_value)) ((equal (car match_target) :list) ((lambda (et) (if minimal (list :right (list :list (list))) ((hydra_lib_eithers_bind ((inst tname) et)) (lambda (e) (list :right (list :list (list e))))))) match_value)) ((equal (car match_target) :literal) ((lambda (lt) (list :right (list :literal (for_literal lt)))) match_value)) ((equal (car match_target) :map) ((lambda (mt) (let ((kt ((lambda (v) (hydra_core_map_type-keys v)) mt))) (let ((vt ((lambda (v) (hydra_core_map_type-values v)) mt))) (if minimal (list :right (list :map hydra_lib_maps_empty)) ((hydra_lib_eithers_bind ((inst tname) kt)) (lambda (ke) ((hydra_lib_eithers_bind ((inst tname) vt)) (lambda (ve) (list :right (list :map ((hydra_lib_maps_singleton ke) ve))))))))))) match_value)) ((equal (car match_target) :maybe) ((lambda (ot) (if minimal (list :right (list :maybe (list :nothing))) ((hydra_lib_eithers_bind ((inst tname) ot)) (lambda (e) (list :right (list :maybe (list :just e))))))) match_value)) ((equal (car match_target) :record) ((lambda (rt) (let ((to_field (lambda (ft) ((hydra_lib_eithers_bind ((inst tname) ((lambda (v) (hydra_core_field_type-type v)) ft))) (lambda (e) (list :right (make-hydra_core_field ((lambda (v) (hydra_core_field_type-name v)) ft) e))))))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list to_field) rt)) (lambda (dfields) (list :right (list :record (make-hydra_core_record tname dfields))))))) match_value)) ((equal (car match_target) :set) ((lambda (et) (if minimal (list :right (list :set hydra_lib_sets_empty)) ((hydra_lib_eithers_bind ((inst tname) et)) (lambda (e) (list :right (list :set (hydra_lib_sets_from_list (list e)))))))) match_value)) ((equal (car match_target) :variable) ((lambda (vname) (((hydra_lib_maybes_maybe (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 "Type variable ") ((hydra_lib_strings_cat2 (hydra_show_core_term (list :variable vname))) " not found in schema"))) cx))) (inst vname)) ((hydra_lib_maps_lookup vname) schema))) match_value)) ((equal (car match_target) :wrap) ((lambda (wt) ((hydra_lib_eithers_bind ((inst tname) wt)) (lambda (e) (list :right (list :wrap (make-hydra_core_wrapped_term tname e)))))) match_value)))) (cadr match_target))) t_))))))))))))

(provide 'hydra.templates)

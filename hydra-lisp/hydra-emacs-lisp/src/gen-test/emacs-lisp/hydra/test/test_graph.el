(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lexical)

(require 'hydra.lib.maps)

(require 'hydra.module)

(require 'hydra.test.testTerms)

(require 'hydra.test.testTypes)

(defvar hydra_test_test_graph_test_context hydra_lexical_empty_context)

(defvar hydra_test_test_graph_test_graph hydra_lexical_empty_graph)

(defvar hydra_test_test_graph_test_namespace "testGraph")

(defvar hydra_test_test_graph_test_schema_namespace "testSchemaGraph")

(defvar hydra_test_test_graph_test_terms (hydra_lib_maps_from_list (list (list "testDataArthur" hydra_test_test_terms_test_data_arthur))))

(defvar hydra_test_test_graph_test_types (hydra_lib_maps_from_list (list (list hydra_test_test_types_test_type_buddy_list_a_name hydra_test_test_types_test_type_buddy_list_a) (list hydra_test_test_types_test_type_buddy_list_b_name hydra_test_test_types_test_type_buddy_list_b) (list hydra_test_test_types_test_type_comparison_name hydra_test_test_types_test_type_comparison) (list hydra_test_test_types_test_type_either_name hydra_test_test_types_test_type_either) (list hydra_test_test_types_test_type_hydra_literal_type_name hydra_test_test_types_test_type_hydra_literal_type) (list hydra_test_test_types_test_type_hydra_type_name hydra_test_test_types_test_type_hydra_type) (list hydra_test_test_types_test_type_int_list_name hydra_test_test_types_test_type_int_list) (list hydra_test_test_types_test_type_lat_lon_name hydra_test_test_types_test_type_lat_lon) (list hydra_test_test_types_test_type_lat_lon_poly_name hydra_test_test_types_test_type_lat_lon_poly) (list hydra_test_test_types_test_type_list_name hydra_test_test_types_test_type_list) (list hydra_test_test_types_test_type_number_name hydra_test_test_types_test_type_number) (list hydra_test_test_types_test_type_person_name hydra_test_test_types_test_type_person) (list hydra_test_test_types_test_type_person_or_something_name hydra_test_test_types_test_type_person_or_something) (list hydra_test_test_types_test_type_polymorphic_wrapper_name hydra_test_test_types_test_type_polymorphic_wrapper) (list hydra_test_test_types_test_type_simple_number_name hydra_test_test_types_test_type_simple_number) (list hydra_test_test_types_test_type_string_alias_name hydra_test_test_types_test_type_string_alias) (list hydra_test_test_types_test_type_symmetric_triple_name hydra_test_test_types_test_type_symmetric_triple) (list hydra_test_test_types_test_type_timestamp_name hydra_test_test_types_test_type_timestamp) (list hydra_test_test_types_test_type_triple_name hydra_test_test_types_test_type_triple) (list hydra_test_test_types_test_type_union_monomorphic_name hydra_test_test_types_test_type_union_monomorphic) (list hydra_test_test_types_test_type_union_polymorphic_recursive_name hydra_test_test_types_test_type_union_polymorphic_recursive) (list hydra_test_test_types_test_type_unit_name hydra_test_test_types_test_type_unit))))

(provide 'hydra.test.testGraph)

(setq hydra_test_test_graph_test_context (list (cons :functions nil) (cons :annotations nil) (cons :variable_types nil)))

(setq hydra_test_test_graph_test_graph
  (let* ((std-prims (standard-library))
         (type-to-ts (lambda (t) (funcall hydra_rewriting_f_type_to_type_scheme t)))
         (boot-types-raw hydra_json_bootstrap_types_by_name)
         (kernel-schemas (mapcar (lambda (entry) (list (car entry) (funcall type-to-ts (cdr entry)))) (hydra_lib_maps_to_list boot-types-raw)))
         (test-schemas (mapcar (lambda (entry) (list (car entry) (funcall type-to-ts (cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
         (schema-types (hydra_lib_maps_from_list (append kernel-schemas test-schemas)))
         (prim-map (hydra_lib_maps_from_list (mapcar (lambda (p) (list (car p) (cdr p))) std-prims)))
         (bound-terms (hydra_lib_maps_from_list (append (mapcar (lambda (p) (list (car p) (list :function (list :primitive (car p))))) std-prims) (hydra-annotation-bindings) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))))
    (list (cons :bound_terms bound-terms) (cons :bound_types nil) (cons :class_constraints nil) (cons :lambda_variables nil) (cons :metadata nil) (cons :primitives prim-map) (cons :schema_types schema-types) (cons :type_variables nil))))

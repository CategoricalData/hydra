(defpackage :hydra.test.testGraph
(:use :cl :hydra.core :hydra.lexical :hydra.lib.maps :hydra.packaging :hydra.test.testTerms :hydra.test.testTypes)
(:export :hydra_test_test_graph_test_context :hydra_test_test_graph_test_graph :hydra_test_test_graph_test_namespace :hydra_test_test_graph_test_schema_namespace :hydra_test_test_graph_test_terms :hydra_test_test_graph_test_types))

(in-package :hydra.test.testGraph)



(cl:defvar hydra_test_test_graph_test_namespace "testGraph")

(cl:defvar hydra_test_test_graph_test_schema_namespace "testSchemaGraph")

(cl:defvar hydra_test_test_graph_test_terms (hydra_lib_maps_from_list (cl:list (cl:list "testDataArthur" hydra_test_test_terms_test_data_arthur))))

(cl:defvar hydra_test_test_graph_test_types (hydra_lib_maps_from_list (cl:list (cl:list hydra_test_test_types_test_type_buddy_list_a_name hydra_test_test_types_test_type_buddy_list_a) (cl:list hydra_test_test_types_test_type_buddy_list_b_name hydra_test_test_types_test_type_buddy_list_b) (cl:list hydra_test_test_types_test_type_comparison_name hydra_test_test_types_test_type_comparison) (cl:list hydra_test_test_types_test_type_either_name hydra_test_test_types_test_type_either) (cl:list hydra_test_test_types_test_type_hydra_literal_type_name hydra_test_test_types_test_type_hydra_literal_type) (cl:list hydra_test_test_types_test_type_hydra_type_name hydra_test_test_types_test_type_hydra_type) (cl:list hydra_test_test_types_test_type_int_list_name hydra_test_test_types_test_type_int_list) (cl:list hydra_test_test_types_test_type_lat_lon_name hydra_test_test_types_test_type_lat_lon) (cl:list hydra_test_test_types_test_type_lat_lon_poly_name hydra_test_test_types_test_type_lat_lon_poly) (cl:list hydra_test_test_types_test_type_list_name hydra_test_test_types_test_type_list) (cl:list hydra_test_test_types_test_type_number_name hydra_test_test_types_test_type_number) (cl:list hydra_test_test_types_test_type_person_name hydra_test_test_types_test_type_person) (cl:list hydra_test_test_types_test_type_person_or_something_name hydra_test_test_types_test_type_person_or_something) (cl:list hydra_test_test_types_test_type_polymorphic_wrapper_name hydra_test_test_types_test_type_polymorphic_wrapper) (cl:list hydra_test_test_types_test_type_simple_number_name hydra_test_test_types_test_type_simple_number) (cl:list hydra_test_test_types_test_type_string_alias_name hydra_test_test_types_test_type_string_alias) (cl:list hydra_test_test_types_test_type_symmetric_triple_name hydra_test_test_types_test_type_symmetric_triple) (cl:list hydra_test_test_types_test_type_timestamp_name hydra_test_test_types_test_type_timestamp) (cl:list hydra_test_test_types_test_type_triple_name hydra_test_test_types_test_type_triple) (cl:list hydra_test_test_types_test_type_union_monomorphic_name hydra_test_test_types_test_type_union_monomorphic) (cl:list hydra_test_test_types_test_type_union_polymorphic_recursive_name hydra_test_test_types_test_type_union_polymorphic_recursive) (cl:list hydra_test_test_types_test_type_unit_name hydra_test_test_types_test_type_unit))))

(cl:defvar hydra_test_test_graph_test_context (make-hydra_context_context (cl:list) (cl:list) hydra_lib_maps_empty))

(cl:defvar hydra_test_test_graph_test_graph
  (cl:let* ((std-prims (standard-library))
            (type-to-ts hydra_scoping_f_type_to_type_scheme)
            (boot-types-raw hydra_json_bootstrap_types_by_name)
            (kernel-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cdr entry)))) (hydra_lib_maps_to_list boot-types-raw)))
            (test-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
            (schema-types (hydra_lib_maps_from_list (cl:append kernel-schemas test-schemas)))
            (prim-map (hydra_lib_maps_from_list (cl:mapcar (cl:lambda (p) (cl:list (cl:car p) (cl:cdr p))) std-prims)))
            (bound-terms (hydra_lib_maps_from_list (cl:append (annotation-bindings) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))))
    (make-hydra_graph_graph
      bound-terms
      hydra_lib_maps_empty
      hydra_lib_maps_empty
      hydra_lib_sets_empty
      hydra_lib_maps_empty
      prim-map
      schema-types
      hydra_lib_sets_empty)))

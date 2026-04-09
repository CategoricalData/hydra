(define-library (hydra test test_graph)
(export hydra_test_test_graph_test_context hydra_test_test_graph_test_graph hydra_test_test_graph_test_namespace hydra_test_test_graph_test_schema_namespace hydra_test_test_graph_test_terms hydra_test_test_graph_test_types)
(import (scheme base) (hydra core) (hydra context) (hydra graph) (hydra lexical) (hydra lib libraries) (hydra lib maps) (hydra packaging) (hydra rewriting) (hydra scoping) (hydra json bootstrap) (hydra test test_terms) (hydra test test_types))
(begin
(define hydra_test_test_graph_test_namespace "testGraph")
(define hydra_test_test_graph_test_schema_namespace "testSchemaGraph")
(define hydra_test_test_graph_test_terms (hydra_lib_maps_from_list (list (list "testDataArthur" hydra_test_test_terms_test_data_arthur))))
(define hydra_test_test_graph_test_types (hydra_lib_maps_from_list (list (list hydra_test_test_types_test_type_buddy_list_a_name hydra_test_test_types_test_type_buddy_list_a) (list hydra_test_test_types_test_type_buddy_list_b_name hydra_test_test_types_test_type_buddy_list_b) (list hydra_test_test_types_test_type_comparison_name hydra_test_test_types_test_type_comparison) (list hydra_test_test_types_test_type_either_name hydra_test_test_types_test_type_either) (list hydra_test_test_types_test_type_hydra_literal_type_name hydra_test_test_types_test_type_hydra_literal_type) (list hydra_test_test_types_test_type_hydra_type_name hydra_test_test_types_test_type_hydra_type) (list hydra_test_test_types_test_type_int_list_name hydra_test_test_types_test_type_int_list) (list hydra_test_test_types_test_type_lat_lon_name hydra_test_test_types_test_type_lat_lon) (list hydra_test_test_types_test_type_lat_lon_poly_name hydra_test_test_types_test_type_lat_lon_poly) (list hydra_test_test_types_test_type_list_name hydra_test_test_types_test_type_list) (list hydra_test_test_types_test_type_number_name hydra_test_test_types_test_type_number) (list hydra_test_test_types_test_type_person_name hydra_test_test_types_test_type_person) (list hydra_test_test_types_test_type_person_or_something_name hydra_test_test_types_test_type_person_or_something) (list hydra_test_test_types_test_type_polymorphic_wrapper_name hydra_test_test_types_test_type_polymorphic_wrapper) (list hydra_test_test_types_test_type_simple_number_name hydra_test_test_types_test_type_simple_number) (list hydra_test_test_types_test_type_string_alias_name hydra_test_test_types_test_type_string_alias) (list hydra_test_test_types_test_type_symmetric_triple_name hydra_test_test_types_test_type_symmetric_triple) (list hydra_test_test_types_test_type_timestamp_name hydra_test_test_types_test_type_timestamp) (list hydra_test_test_types_test_type_triple_name hydra_test_test_types_test_type_triple) (list hydra_test_test_types_test_type_union_monomorphic_name hydra_test_test_types_test_type_union_monomorphic) (list hydra_test_test_types_test_type_union_polymorphic_recursive_name hydra_test_test_types_test_type_union_polymorphic_recursive) (list hydra_test_test_types_test_type_unit_name hydra_test_test_types_test_type_unit))))
;; Include annotation term-level bindings (shared with test runner).
(include "/Users/josh/projects/github/CategoricalData/hydra-branches/feature_292_error_context/hydra-lisp/hydra-scheme/src/test/scheme/hydra/annotation_bindings.scm")

(define hydra_test_test_graph_test_context (make-hydra_context_context (list) (list) hydra_lib_maps_empty))
(define hydra_test_test_graph_test_graph
  (let* ((all-prims (standard-library))
         (type-to-ts hydra_scoping_f_type_to_type_scheme)
         (kernel-schemas (map (lambda (entry) (list (car entry) (type-to-ts (cdr entry)))) hydra_json_bootstrap_types_by_name))
         (test-schemas (map (lambda (entry) (list (car entry) (type-to-ts (cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
         (schema-types (hydra_lib_maps_from_list (append kernel-schemas test-schemas)))
         (test-terms (map (lambda (entry) (list (car entry) (cdr entry))) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))
         (bound-terms (append
           ;; Primitives are resolved via graphPrimitives, not boundTerms.
           (annotation-bindings)
           (list (list "hydra.monads.emptyContext" (list (quote unit) (list)))
                 (list "hydra.lexical.emptyGraph" (list (quote unit) (list))))
           test-terms)))
    (make-hydra_graph_graph
      (hydra_lib_maps_from_list bound-terms)
      hydra_lib_maps_empty (list) (list) hydra_lib_maps_empty
      (hydra_lib_maps_from_list (map (lambda (p) (list (car p) (cdr p))) all-prims))
      schema-types (list))))
))

(ns hydra.test.testGraph
  (:require [hydra.core :refer :all] [hydra.lexical :refer :all] [hydra.lib.libraries :refer :all] [hydra.rewriting :refer :all] [hydra.scoping :refer :all] [hydra.json.bootstrap :refer :all] [hydra.graph :refer :all] [hydra.context :refer :all] [hydra.annotation-bindings :refer [annotation-bindings]] [hydra.lib.maps :refer :all] [hydra.module :refer :all] [hydra.test.testTerms :refer :all] [hydra.test.testTypes :refer :all]
))

(declare hydra_test_test_graph_test_context hydra_test_test_graph_test_graph hydra_test_test_graph_test_namespace hydra_test_test_graph_test_schema_namespace hydra_test_test_graph_test_terms hydra_test_test_graph_test_types)



(def hydra_test_test_graph_test_namespace "testGraph")

(def hydra_test_test_graph_test_schema_namespace "testSchemaGraph")

(def hydra_test_test_graph_test_terms (hydra_lib_maps_from_list (list (list "testDataArthur" hydra_test_test_terms_test_data_arthur))))

(def hydra_test_test_graph_test_types (hydra_lib_maps_from_list (list (list hydra_test_test_types_test_type_buddy_list_a_name hydra_test_test_types_test_type_buddy_list_a) (list hydra_test_test_types_test_type_buddy_list_b_name hydra_test_test_types_test_type_buddy_list_b) (list hydra_test_test_types_test_type_comparison_name hydra_test_test_types_test_type_comparison) (list hydra_test_test_types_test_type_either_name hydra_test_test_types_test_type_either) (list hydra_test_test_types_test_type_hydra_literal_type_name hydra_test_test_types_test_type_hydra_literal_type) (list hydra_test_test_types_test_type_hydra_type_name hydra_test_test_types_test_type_hydra_type) (list hydra_test_test_types_test_type_int_list_name hydra_test_test_types_test_type_int_list) (list hydra_test_test_types_test_type_lat_lon_name hydra_test_test_types_test_type_lat_lon) (list hydra_test_test_types_test_type_lat_lon_poly_name hydra_test_test_types_test_type_lat_lon_poly) (list hydra_test_test_types_test_type_list_name hydra_test_test_types_test_type_list) (list hydra_test_test_types_test_type_number_name hydra_test_test_types_test_type_number) (list hydra_test_test_types_test_type_person_name hydra_test_test_types_test_type_person) (list hydra_test_test_types_test_type_person_or_something_name hydra_test_test_types_test_type_person_or_something) (list hydra_test_test_types_test_type_polymorphic_wrapper_name hydra_test_test_types_test_type_polymorphic_wrapper) (list hydra_test_test_types_test_type_simple_number_name hydra_test_test_types_test_type_simple_number) (list hydra_test_test_types_test_type_string_alias_name hydra_test_test_types_test_type_string_alias) (list hydra_test_test_types_test_type_symmetric_triple_name hydra_test_test_types_test_type_symmetric_triple) (list hydra_test_test_types_test_type_timestamp_name hydra_test_test_types_test_type_timestamp) (list hydra_test_test_types_test_type_triple_name hydra_test_test_types_test_type_triple) (list hydra_test_test_types_test_type_union_monomorphic_name hydra_test_test_types_test_type_union_monomorphic) (list hydra_test_test_types_test_type_union_polymorphic_recursive_name hydra_test_test_types_test_type_union_polymorphic_recursive) (list hydra_test_test_types_test_type_unit_name hydra_test_test_types_test_type_unit))))

(def hydra_test_test_graph_test_context {:functions () :annotations () :variable_types {}})

(def hydra_test_test_graph_test_graph
  (let [std-prims (standard-library)
        type-to-ts hydra_scoping_f_type_to_type_scheme
        boot-types-raw hydra_json_bootstrap_types_by_name
        kernel-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) boot-types-raw))
        test-types-list (seq hydra_test_test_graph_test_types)
        test-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) test-types-list))
        schema-types (merge kernel-schemas test-schemas)
        bound-terms (merge
          (into {} (map (fn [[k _]] [k (list :function (list :primitive k))]) std-prims))
          (into {} (annotation-bindings))
          (into {} (seq hydra_test_test_graph_test_terms)))]
    {:bound_terms bound-terms
     :bound_types {}
     :class_constraints {}
     :lambda_variables #{}
     :metadata {}
     :primitives std-prims
     :schema_types schema-types
     :type_variables #{}}))

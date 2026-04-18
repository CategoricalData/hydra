(ns hydra.dsl.paths
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_paths_subterm_edge hydra_dsl_paths_subterm_edge_path hydra_dsl_paths_subterm_edge_source hydra_dsl_paths_subterm_edge_target hydra_dsl_paths_subterm_edge_with_path hydra_dsl_paths_subterm_edge_with_source hydra_dsl_paths_subterm_edge_with_target hydra_dsl_paths_subterm_graph hydra_dsl_paths_subterm_graph_edges hydra_dsl_paths_subterm_graph_nodes hydra_dsl_paths_subterm_graph_with_edges hydra_dsl_paths_subterm_graph_with_nodes hydra_dsl_paths_subterm_node hydra_dsl_paths_subterm_node_id hydra_dsl_paths_subterm_node_label hydra_dsl_paths_subterm_node_name hydra_dsl_paths_subterm_node_with_id hydra_dsl_paths_subterm_node_with_label hydra_dsl_paths_subterm_node_with_name hydra_dsl_paths_subterm_path hydra_dsl_paths_subterm_step_annotated_body hydra_dsl_paths_subterm_step_application_argument hydra_dsl_paths_subterm_step_application_function hydra_dsl_paths_subterm_step_injection_term hydra_dsl_paths_subterm_step_lambda_body hydra_dsl_paths_subterm_step_let_binding hydra_dsl_paths_subterm_step_let_body hydra_dsl_paths_subterm_step_list_element hydra_dsl_paths_subterm_step_map_key hydra_dsl_paths_subterm_step_map_value hydra_dsl_paths_subterm_step_maybe_term hydra_dsl_paths_subterm_step_product_term hydra_dsl_paths_subterm_step_record_field hydra_dsl_paths_subterm_step_set_element hydra_dsl_paths_subterm_step_sum_term hydra_dsl_paths_subterm_step_type_application_term hydra_dsl_paths_subterm_step_type_lambda_body hydra_dsl_paths_subterm_step_union_cases_branch hydra_dsl_paths_subterm_step_union_cases_default hydra_dsl_paths_subterm_step_wrapped_term hydra_dsl_paths_subtype_edge hydra_dsl_paths_subtype_edge_path hydra_dsl_paths_subtype_edge_source hydra_dsl_paths_subtype_edge_target hydra_dsl_paths_subtype_edge_with_path hydra_dsl_paths_subtype_edge_with_source hydra_dsl_paths_subtype_edge_with_target hydra_dsl_paths_subtype_graph hydra_dsl_paths_subtype_graph_edges hydra_dsl_paths_subtype_graph_nodes hydra_dsl_paths_subtype_graph_with_edges hydra_dsl_paths_subtype_graph_with_nodes hydra_dsl_paths_subtype_node hydra_dsl_paths_subtype_node_id hydra_dsl_paths_subtype_node_label hydra_dsl_paths_subtype_node_name hydra_dsl_paths_subtype_node_with_id hydra_dsl_paths_subtype_node_with_label hydra_dsl_paths_subtype_node_with_name hydra_dsl_paths_subtype_path hydra_dsl_paths_subtype_step_annotated_body hydra_dsl_paths_subtype_step_application_argument hydra_dsl_paths_subtype_step_application_function hydra_dsl_paths_subtype_step_either_left hydra_dsl_paths_subtype_step_either_right hydra_dsl_paths_subtype_step_forall_body hydra_dsl_paths_subtype_step_function_codomain hydra_dsl_paths_subtype_step_function_domain hydra_dsl_paths_subtype_step_list_element hydra_dsl_paths_subtype_step_map_keys hydra_dsl_paths_subtype_step_map_values hydra_dsl_paths_subtype_step_maybe_element hydra_dsl_paths_subtype_step_pair_first hydra_dsl_paths_subtype_step_pair_second hydra_dsl_paths_subtype_step_record_field hydra_dsl_paths_subtype_step_set_element hydra_dsl_paths_subtype_step_union_field hydra_dsl_paths_subtype_step_wrapped_type hydra_dsl_paths_un_subterm_path hydra_dsl_paths_un_subtype_path)

(def hydra_dsl_paths_subterm_edge (fn [source] (fn [path] (fn [target] (list :record (->hydra_core_record "hydra.paths.SubtermEdge" (list (->hydra_core_field "source" ((fn [v] v) source)) (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "target" ((fn [v] v) target)))))))))

(def hydra_dsl_paths_subterm_edge_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "path")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_edge_source (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "source")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_edge_target (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "target")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_edge_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermEdge" (list (->hydra_core_field "source" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "source")) ((fn [v] v) original)))) (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "target" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "target")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subterm_edge_with_source (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermEdge" (list (->hydra_core_field "source" ((fn [v] v) new_val)) (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "path")) ((fn [v] v) original)))) (->hydra_core_field "target" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "target")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subterm_edge_with_target (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermEdge" (list (->hydra_core_field "source" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "source")) ((fn [v] v) original)))) (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermEdge" "path")) ((fn [v] v) original)))) (->hydra_core_field "target" ((fn [v] v) new_val))))))))

(def hydra_dsl_paths_subterm_graph (fn [nodes] (fn [edges] (list :record (->hydra_core_record "hydra.paths.SubtermGraph" (list (->hydra_core_field "nodes" ((fn [v] v) nodes)) (->hydra_core_field "edges" ((fn [v] v) edges))))))))

(def hydra_dsl_paths_subterm_graph_edges (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermGraph" "edges")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_graph_nodes (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermGraph" "nodes")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_graph_with_edges (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermGraph" (list (->hydra_core_field "nodes" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermGraph" "nodes")) ((fn [v] v) original)))) (->hydra_core_field "edges" ((fn [v] v) new_val))))))))

(def hydra_dsl_paths_subterm_graph_with_nodes (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermGraph" (list (->hydra_core_field "nodes" ((fn [v] v) new_val)) (->hydra_core_field "edges" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermGraph" "edges")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subterm_node (fn [name] (fn [label] (fn [id] (list :record (->hydra_core_record "hydra.paths.SubtermNode" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "label" ((fn [v] v) label)) (->hydra_core_field "id" ((fn [v] v) id)))))))))

(def hydra_dsl_paths_subterm_node_id (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "id")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_node_label (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "label")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_node_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "name")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_node_with_id (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermNode" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "name")) ((fn [v] v) original)))) (->hydra_core_field "label" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "label")) ((fn [v] v) original)))) (->hydra_core_field "id" ((fn [v] v) new_val))))))))

(def hydra_dsl_paths_subterm_node_with_label (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermNode" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "name")) ((fn [v] v) original)))) (->hydra_core_field "label" ((fn [v] v) new_val)) (->hydra_core_field "id" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "id")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subterm_node_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtermNode" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "label" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "label")) ((fn [v] v) original)))) (->hydra_core_field "id" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtermNode" "id")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subterm_path (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.paths.SubtermPath" ((fn [v] v) x)))))

(def hydra_dsl_paths_subterm_step_annotated_body (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "annotatedBody" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_application_argument (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "applicationArgument" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_application_function (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "applicationFunction" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_injection_term (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "injectionTerm" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_lambda_body (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "lambdaBody" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_let_binding (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "letBinding" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_let_body (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "letBody" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_list_element (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "listElement" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_map_key (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "mapKey" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_map_value (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "mapValue" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_maybe_term (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "maybeTerm" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_product_term (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "productTerm" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_record_field (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "recordField" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_set_element (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "setElement" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_sum_term (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "sumTerm" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_type_application_term (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "typeApplicationTerm" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_type_lambda_body (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "typeLambdaBody" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_union_cases_branch (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "unionCasesBranch" ((fn [v] v) x))))))

(def hydra_dsl_paths_subterm_step_union_cases_default (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "unionCasesDefault" (list :unit nil)))))

(def hydra_dsl_paths_subterm_step_wrapped_term (list :inject (->hydra_core_injection "hydra.paths.SubtermStep" (->hydra_core_field "wrappedTerm" (list :unit nil)))))

(def hydra_dsl_paths_subtype_edge (fn [source] (fn [path] (fn [target] (list :record (->hydra_core_record "hydra.paths.SubtypeEdge" (list (->hydra_core_field "source" ((fn [v] v) source)) (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "target" ((fn [v] v) target)))))))))

(def hydra_dsl_paths_subtype_edge_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "path")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_edge_source (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "source")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_edge_target (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "target")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_edge_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeEdge" (list (->hydra_core_field "source" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "source")) ((fn [v] v) original)))) (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "target" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "target")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subtype_edge_with_source (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeEdge" (list (->hydra_core_field "source" ((fn [v] v) new_val)) (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "path")) ((fn [v] v) original)))) (->hydra_core_field "target" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "target")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subtype_edge_with_target (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeEdge" (list (->hydra_core_field "source" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "source")) ((fn [v] v) original)))) (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeEdge" "path")) ((fn [v] v) original)))) (->hydra_core_field "target" ((fn [v] v) new_val))))))))

(def hydra_dsl_paths_subtype_graph (fn [nodes] (fn [edges] (list :record (->hydra_core_record "hydra.paths.SubtypeGraph" (list (->hydra_core_field "nodes" ((fn [v] v) nodes)) (->hydra_core_field "edges" ((fn [v] v) edges))))))))

(def hydra_dsl_paths_subtype_graph_edges (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeGraph" "edges")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_graph_nodes (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeGraph" "nodes")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_graph_with_edges (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeGraph" (list (->hydra_core_field "nodes" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeGraph" "nodes")) ((fn [v] v) original)))) (->hydra_core_field "edges" ((fn [v] v) new_val))))))))

(def hydra_dsl_paths_subtype_graph_with_nodes (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeGraph" (list (->hydra_core_field "nodes" ((fn [v] v) new_val)) (->hydra_core_field "edges" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeGraph" "edges")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subtype_node (fn [name] (fn [label] (fn [id] (list :record (->hydra_core_record "hydra.paths.SubtypeNode" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "label" ((fn [v] v) label)) (->hydra_core_field "id" ((fn [v] v) id)))))))))

(def hydra_dsl_paths_subtype_node_id (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "id")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_node_label (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "label")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_node_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "name")) ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_node_with_id (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeNode" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "name")) ((fn [v] v) original)))) (->hydra_core_field "label" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "label")) ((fn [v] v) original)))) (->hydra_core_field "id" ((fn [v] v) new_val))))))))

(def hydra_dsl_paths_subtype_node_with_label (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeNode" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "name")) ((fn [v] v) original)))) (->hydra_core_field "label" ((fn [v] v) new_val)) (->hydra_core_field "id" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "id")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subtype_node_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.paths.SubtypeNode" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "label" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "label")) ((fn [v] v) original)))) (->hydra_core_field "id" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.paths.SubtypeNode" "id")) ((fn [v] v) original))))))))))

(def hydra_dsl_paths_subtype_path (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.paths.SubtypePath" ((fn [v] v) x)))))

(def hydra_dsl_paths_subtype_step_annotated_body (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "annotatedBody" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_application_argument (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "applicationArgument" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_application_function (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "applicationFunction" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_either_left (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "eitherLeft" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_either_right (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "eitherRight" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_forall_body (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "forallBody" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_function_codomain (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "functionCodomain" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_function_domain (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "functionDomain" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_list_element (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "listElement" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_map_keys (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "mapKeys" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_map_values (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "mapValues" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_maybe_element (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "maybeElement" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_pair_first (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "pairFirst" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_pair_second (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "pairSecond" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_record_field (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "recordField" ((fn [v] v) x))))))

(def hydra_dsl_paths_subtype_step_set_element (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "setElement" (list :unit nil)))))

(def hydra_dsl_paths_subtype_step_union_field (fn [x] (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "unionField" ((fn [v] v) x))))))

(def hydra_dsl_paths_subtype_step_wrapped_type (list :inject (->hydra_core_injection "hydra.paths.SubtypeStep" (->hydra_core_field "wrappedType" (list :unit nil)))))

(def hydra_dsl_paths_un_subterm_path (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.paths.SubtermPath") ((fn [v] v) x)))))

(def hydra_dsl_paths_un_subtype_path (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.paths.SubtypePath") ((fn [v] v) x)))))

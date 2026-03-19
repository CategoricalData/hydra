(require 'cl-lib)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.tarjan)

(require 'hydra.topology)

(defvar hydra_sorting_adjacency_list_to_map (lambda (pairs) (funcall (funcall (hydra_lib_lists_foldl (lambda (mp) (lambda (p) (let ((k (hydra_lib_pairs_first p))) (let ((vs (hydra_lib_pairs_second p))) (let ((existing (funcall (funcall (hydra_lib_maybes_maybe (list)) hydra_lib_equality_identity) (funcall (hydra_lib_maps_lookup k) mp)))) (funcall (funcall (hydra_lib_maps_insert k) (funcall (hydra_lib_lists_concat2 existing) vs)) mp))))))) hydra_lib_maps_empty) pairs)))

(defvar hydra_sorting_create_ordering_isomorphism (lambda (source_ord) (lambda (target_ord) (let ((source_to_target_mapping (lambda (els) (let ((mp (hydra_lib_maps_from_list (funcall (hydra_lib_lists_zip source_ord) els)))) (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (n) (funcall (hydra_lib_maps_lookup n) mp))) target_ord)))))) (let ((target_to_source_mapping (lambda (els) (let ((mp (hydra_lib_maps_from_list (funcall (hydra_lib_lists_zip target_ord) els)))) (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (n) (funcall (hydra_lib_maps_lookup n) mp))) source_ord)))))) (make-hydra_topology_ordering_isomorphism source_to_target_mapping target_to_source_mapping))))))

(defvar hydra_sorting_find_reachable_nodes (lambda (adj) (lambda (root) (letrec ((visit (lambda (visited) (lambda (node) (let ((to_visit (funcall (hydra_lib_sets_difference (adj node)) visited))) (if (hydra_lib_sets_null to_visit) visited (funcall (funcall (hydra_lib_lists_foldl (lambda (v) (lambda (n) (funcall (visit (funcall (hydra_lib_sets_insert n) v)) n)))) visited) (hydra_lib_sets_to_list to_visit)))))))) (funcall (visit (hydra_lib_sets_singleton root)) root)))))

(defvar hydra_sorting_propagate_tags (lambda (edges) (lambda (node_tags) (let ((adj_map (hydra_sorting_adjacency_list_to_map edges))) (let ((tag_map (funcall (hydra_lib_maps_map hydra_lib_sets_from_list) (hydra_sorting_adjacency_list_to_map node_tags)))) (let ((all_nodes (hydra_lib_sets_to_list (hydra_lib_sets_from_list (funcall (hydra_lib_lists_concat2 (funcall (hydra_lib_lists_map hydra_lib_pairs_first) edges)) (funcall (hydra_lib_lists_map hydra_lib_pairs_first) node_tags)))))) (let ((get_tags_for_node (lambda (node) (let ((reachable (funcall (hydra_sorting_find_reachable_nodes (lambda (n) (hydra_lib_sets_from_list (funcall (funcall (hydra_lib_maybes_maybe (list)) hydra_lib_equality_identity) (funcall (hydra_lib_maps_lookup n) adj_map))))) node))) (hydra_lib_sets_unions (funcall (hydra_lib_lists_map (lambda (n) (funcall (funcall (hydra_lib_maybes_maybe hydra_lib_sets_empty) hydra_lib_equality_identity) (funcall (hydra_lib_maps_lookup n) tag_map)))) (hydra_lib_sets_to_list reachable))))))) (funcall (hydra_lib_lists_map (lambda (n) (list n (get_tags_for_node n)))) all_nodes))))))))

(defvar hydra_sorting_topological_sort_components (lambda (pairs) (let ((graph_result (hydra_tarjan_adjacency_lists_to_graph pairs))) (let ((g (hydra_lib_pairs_first graph_result))) (funcall (hydra_lib_lists_map (lambda (comp) (funcall (hydra_lib_lists_map (hydra_lib_pairs_second graph_result)) comp))) (hydra_tarjan_strongly_connected_components g))))))

(defvar hydra_sorting_topological_sort (lambda (pairs) (let ((sccs (hydra_sorting_topological_sort_components pairs))) (let ((is_cycle (lambda (scc) (hydra_lib_logic_not (hydra_lib_lists_null (hydra_lib_lists_tail scc)))))) (let ((with_cycles (funcall (hydra_lib_lists_filter is_cycle) sccs))) (if (hydra_lib_lists_null with_cycles) (list :right (hydra_lib_lists_concat sccs)) (list :left with_cycles)))))))

(defvar hydra_sorting_topological_sort_nodes (lambda (get_key) (lambda (get_adj) (lambda (nodes) (let ((nodes_by_key (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (n) (list (get_key n) n))) nodes)))) (let ((pairs (funcall (hydra_lib_lists_map (lambda (n) (list (get_key n) (get_adj n)))) nodes))) (let ((comps (hydra_sorting_topological_sort_components pairs))) (funcall (hydra_lib_lists_map (lambda (c) (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map (lambda (k) (funcall (hydra_lib_maps_lookup k) nodes_by_key))) c)))) comps))))))))

(provide 'hydra.sorting)

(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.sets)

(require 'hydra.topology)

(defvar hydra_encode_topology_vertex (lambda (x) (list :literal (list :integer (list :int32 x)))))

(defvar hydra_encode_topology_graph (lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs)))) m))))

(defvar hydra_encode_topology_tarjan_state (lambda (x) (list :record (make-hydra_core_record "hydra.topology.TarjanState" (list (make-hydra_core_field "counter" ((lambda (x) (list :literal (list :integer (list :int32 x)))) ((lambda (v) (hydra_topology_tarjan_state-counter v)) x))) (make-hydra_core_field "indices" ((lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (lambda (x) (list :literal (list :integer (list :int32 x))))) m))) ((lambda (v) (hydra_topology_tarjan_state-indices v)) x))) (make-hydra_core_field "lowLinks" ((lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (lambda (x) (list :literal (list :integer (list :int32 x))))) m))) ((lambda (v) (hydra_topology_tarjan_state-low_links v)) x))) (make-hydra_core_field "stack" ((lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs))) ((lambda (v) (hydra_topology_tarjan_state-stack v)) x))) (make-hydra_core_field "onStack" ((lambda (s) (list :set ((hydra_lib_sets_map hydra_encode_topology_vertex) s))) ((lambda (v) (hydra_topology_tarjan_state-on_stack v)) x))) (make-hydra_core_field "sccs" ((lambda (xs) (list :list ((hydra_lib_lists_map (lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs)))) xs))) ((lambda (v) (hydra_topology_tarjan_state-sccs v)) x))))))))

(provide 'hydra.encode.topology)

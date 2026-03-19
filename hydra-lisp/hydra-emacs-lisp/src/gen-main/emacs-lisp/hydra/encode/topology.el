(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.sets)

(require 'hydra.topology)

(defvar hydra_encode_topology_vertex (lambda (x) (list :literal (list :integer (list :int32 x)))))

(defvar hydra_encode_topology_graph (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_topology_vertex) (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_topology_vertex) xs)))) m))))

(defvar hydra_encode_topology_tarjan_state (lambda (x) (list :record (make-hydra_core_record "hydra.topology.TarjanState" (list (make-hydra_core_field "counter" (funcall (lambda (x) (list :literal (list :integer (list :int32 x)))) (funcall (lambda (v) (hydra_topology_tarjan_state-counter v)) x))) (make-hydra_core_field "indices" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_topology_vertex) (lambda (x) (list :literal (list :integer (list :int32 x))))) m))) (funcall (lambda (v) (hydra_topology_tarjan_state-indices v)) x))) (make-hydra_core_field "lowLinks" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_topology_vertex) (lambda (x) (list :literal (list :integer (list :int32 x))))) m))) (funcall (lambda (v) (hydra_topology_tarjan_state-low_links v)) x))) (make-hydra_core_field "stack" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_topology_vertex) xs))) (funcall (lambda (v) (hydra_topology_tarjan_state-stack v)) x))) (make-hydra_core_field "onStack" (funcall (lambda (s) (list :set (funcall (hydra_lib_sets_map hydra_encode_topology_vertex) s))) (funcall (lambda (v) (hydra_topology_tarjan_state-on_stack v)) x))) (make-hydra_core_field "sccs" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_topology_vertex) xs)))) xs))) (funcall (lambda (v) (hydra_topology_tarjan_state-sccs v)) x))))))))

(provide 'hydra.encode.topology)

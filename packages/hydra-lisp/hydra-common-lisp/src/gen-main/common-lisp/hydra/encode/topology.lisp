(defpackage :hydra.encode.topology
(:use :cl :hydra.core :hydra.lib.lists :hydra.lib.maps :hydra.lib.sets :hydra.topology)
(:export :hydra_encode_topology_vertex :hydra_encode_topology_graph :hydra_encode_topology_tarjan_state))

(in-package :hydra.encode.topology)

(cl:defvar hydra_encode_topology_vertex (cl:lambda (x) (list :literal (list :integer (list :int32 x)))))

(cl:defvar hydra_encode_topology_graph (cl:lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (cl:lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs)))) m))))

(cl:defvar hydra_encode_topology_tarjan_state (cl:lambda (x) (list :record (make-hydra_core_record "hydra.topology.TarjanState" (cl:list (make-hydra_core_field "counter" ((cl:lambda (x2) (list :literal (list :integer (list :int32 x2)))) ((cl:lambda (v) (hydra_topology_tarjan_state-counter v)) x))) (make-hydra_core_field "indices" ((cl:lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (cl:lambda (x2) (list :literal (list :integer (list :int32 x2))))) m))) ((cl:lambda (v) (hydra_topology_tarjan_state-indices v)) x))) (make-hydra_core_field "lowLinks" ((cl:lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (cl:lambda (x2) (list :literal (list :integer (list :int32 x2))))) m))) ((cl:lambda (v) (hydra_topology_tarjan_state-low_links v)) x))) (make-hydra_core_field "stack" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs))) ((cl:lambda (v) (hydra_topology_tarjan_state-stack v)) x))) (make-hydra_core_field "onStack" ((cl:lambda (s) (list :set ((hydra_lib_sets_map hydra_encode_topology_vertex) s))) ((cl:lambda (v) (hydra_topology_tarjan_state-on_stack v)) x))) (make-hydra_core_field "sccs" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (cl:lambda (xs2) (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs2)))) xs))) ((cl:lambda (v) (hydra_topology_tarjan_state-sccs v)) x))))))))

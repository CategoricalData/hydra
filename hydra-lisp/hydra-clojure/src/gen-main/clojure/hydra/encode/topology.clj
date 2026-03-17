(ns hydra.encode.topology
  (:require [hydra.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.sets :refer :all] [hydra.topology :refer :all]
))

(declare hydra_encode_topology_vertex hydra_encode_topology_graph hydra_encode_topology_tarjan_state)

(def hydra_encode_topology_vertex (fn [x] (list :literal (list :integer (list :int32 x)))))

(def hydra_encode_topology_graph (fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs)))) m))))

(def hydra_encode_topology_tarjan_state (fn [x] (list :record (->hydra_core_record "hydra.topology.TarjanState" (list (->hydra_core_field "counter" ((fn [x] (list :literal (list :integer (list :int32 x)))) ((fn [v] (:counter v)) x))) (->hydra_core_field "indices" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (fn [x] (list :literal (list :integer (list :int32 x))))) m))) ((fn [v] (:indices v)) x))) (->hydra_core_field "lowLinks" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_topology_vertex) (fn [x] (list :literal (list :integer (list :int32 x))))) m))) ((fn [v] (:low_links v)) x))) (->hydra_core_field "stack" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs))) ((fn [v] (:stack v)) x))) (->hydra_core_field "onStack" ((fn [s] (list :set ((hydra_lib_sets_map hydra_encode_topology_vertex) s))) ((fn [v] (:on_stack v)) x))) (->hydra_core_field "sccs" ((fn [xs] (list :list ((hydra_lib_lists_map (fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_topology_vertex) xs)))) xs))) ((fn [v] (:sccs v)) x))))))))

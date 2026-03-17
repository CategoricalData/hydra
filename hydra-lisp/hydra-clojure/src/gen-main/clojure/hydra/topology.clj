(ns hydra.topology)

(defrecord hydra_topology_ordering_isomorphism [encode decode])
(defn make-hydra_topology_ordering_isomorphism [encode decode] (->hydra_topology_ordering_isomorphism encode decode))

(defrecord hydra_topology_tarjan_state [counter indices low_links stack on_stack sccs])
(defn make-hydra_topology_tarjan_state [counter indices low_links stack on_stack sccs] (->hydra_topology_tarjan_state counter indices low_links stack on_stack sccs))

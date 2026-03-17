(defpackage :hydra.topology
(:use :cl)
(:export :make-hydra_topology_ordering_isomorphism :hydra_topology_ordering_isomorphism? :hydra_topology_ordering_isomorphism-encode :hydra_topology_ordering_isomorphism-decode :make-hydra_topology_tarjan_state :hydra_topology_tarjan_state? :hydra_topology_tarjan_state-counter :hydra_topology_tarjan_state-indices :hydra_topology_tarjan_state-low_links :hydra_topology_tarjan_state-stack :hydra_topology_tarjan_state-on_stack :hydra_topology_tarjan_state-sccs))

(in-package :hydra.topology)

(cl:defstruct hydra_topology_ordering_isomorphism encode decode)

(cl:defstruct hydra_topology_tarjan_state counter indices low_links stack on_stack sccs)

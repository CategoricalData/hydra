(require 'cl-lib)

(cl-defstruct hydra_topology_ordering_isomorphism encode decode)

(cl-defstruct hydra_topology_tarjan_state counter indices low_links stack on_stack sccs)

(provide 'hydra.topology)

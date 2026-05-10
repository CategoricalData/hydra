"""Internal collection classes and utilities for Hydra-Python.

Mirrors Hydra-Java's hydra.util package. Persistent (structurally-shared)
collection classes that satisfy collections.abc.{Sequence, Mapping, Set} so
they can be used wherever the abstract types are accepted, while providing
sub-linear updates suitable for inference-style workloads.
"""

from hydra.python.util.cons_list import ConsList
from hydra.python.util.lazy import Lazy
from hydra.python.util.persistent_map import PersistentMap
from hydra.python.util.persistent_set import PersistentSet

__all__ = ["ConsList", "Lazy", "PersistentMap", "PersistentSet"]

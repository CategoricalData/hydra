# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.graph types."""

from __future__ import annotations
from functools import lru_cache
import hydra.core
import hydra.graph
import hydra.lib.lists
import hydra.lib.strings
import hydra.show.core

def graph(graph: hydra.graph.Graph) -> str:
    r"""Show a graph as a string."""
    
    @lru_cache(1)
    def elements() -> frozenlist[hydra.core.Binding]:
        return graph.elements
    @lru_cache(1)
    def element_strs() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda x1: hydra.show.core.binding(x1)), elements())
    return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", element_strs()), "}"))

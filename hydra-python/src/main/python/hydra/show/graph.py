# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.graph types."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
import hydra.core
import hydra.graph
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.strings
import hydra.show.core

def graph(graph: hydra.graph.Graph) -> str:
    r"""Show a graph as a string."""
    
    def elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.maps.elems(graph.elements)
    def element_strs() -> frozenlist[str]:
        return hydra.lib.lists.map(hydra.show.core.binding, elements())
    return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", element_strs()), "}"))

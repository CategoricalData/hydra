# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Set functions for the Hydra interpreter."""

from __future__ import annotations
from typing import cast
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.sets

def map(fun: hydra.core.Term, set_term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Interpreter-friendly map for Set terms."""
    
    return hydra.lib.flows.bind(hydra.extract.core.set(set_term), (lambda elements: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.sets.fromList"))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun, el)))), hydra.lib.sets.to_list(elements))))))))))

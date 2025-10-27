# Note: this is an automatically generated file. Do not edit.

"""A DSL for decoding and validating Hydra terms at runtime. This module provides functions to extract typed values from Hydra terms with appropriate error handling."""

from __future__ import annotations
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.logic
import hydra.mantle
import hydra.monads

def comparison(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.mantle.Comparison]:
    """Extract a comparison from a term."""
    
    return hydra.lib.flows.bind(hydra.extract.core.unit_variant(hydra.core.Name("hydra.mantle.Comparison"), term), (lambda fname: hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "equalTo"), hydra.lib.flows.pure(hydra.mantle.Comparison.EQUAL_TO), hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "lessThan"), hydra.lib.flows.pure(hydra.mantle.Comparison.LESS_THAN), hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "greaterThan"), hydra.lib.flows.pure(hydra.mantle.Comparison.GREATER_THAN), hydra.monads.unexpected("comparison", fname.value))))))

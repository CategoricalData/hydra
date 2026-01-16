# Note: this is an automatically generated file. Do not edit.

r"""Extraction and validation for hydra.util types."""

from __future__ import annotations
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.logic
import hydra.monads
import hydra.util

def comparison(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.util.Comparison]:
    r"""Extract a comparison from a term."""
    
    return hydra.lib.flows.bind(hydra.extract.core.unit_variant(hydra.core.Name("hydra.util.Comparison"), term), (lambda fname: hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "equalTo"), (lambda : hydra.lib.flows.pure(hydra.util.Comparison.EQUAL_TO)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "lessThan"), (lambda : hydra.lib.flows.pure(hydra.util.Comparison.LESS_THAN)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "greaterThan"), (lambda : hydra.lib.flows.pure(hydra.util.Comparison.GREATER_THAN)), (lambda : hydra.monads.unexpected("comparison", fname.value)))))))))

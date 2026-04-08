# Note: this is an automatically generated file. Do not edit.

r"""Extraction and validation for hydra.util types."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.logic
import hydra.util

T0 = TypeVar("T0")

def comparison(cx: T0, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.util.Comparison]:
    r"""Extract a comparison from a term."""

    return hydra.lib.eithers.bind(hydra.extract.core.unit_variant(hydra.core.Name("hydra.util.Comparison"), graph, term), (lambda fname: hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "equalTo"), (lambda : Right(hydra.util.Comparison.EQUAL_TO)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "lessThan"), (lambda : Right(hydra.util.Comparison.LESS_THAN)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, "greaterThan"), (lambda : Right(hydra.util.Comparison.GREATER_THAN)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("comparison", fname.value))))))))))))))

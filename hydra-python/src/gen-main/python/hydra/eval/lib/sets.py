# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Set functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right
from typing import cast
import hydra.core
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.sets

def map(cx: hydra.context.Context, g: hydra.graph.Graph, fun: hydra.core.Term, set_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Term]:
    r"""Interpreter-friendly map for Set terms."""

    return hydra.lib.eithers.bind(hydra.extract.core.set(cx, g, set_term), (lambda elements: Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.sets.fromList"))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun, el)))), hydra.lib.sets.to_list(elements))))))))))

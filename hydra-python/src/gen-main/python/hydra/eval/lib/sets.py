# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Set functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.sets

T0 = TypeVar("T0")

def difference(cx: T0, g: hydra.graph.Graph, set1_term: hydra.core.Term, set2_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly set difference."""

    return hydra.lib.eithers.bind(hydra.extract.core.set(g, set1_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.logic.ifElse"))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.member"))), el))), set2_term)))))), acc))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.insert"))), el))), acc))))))), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(()))), hydra.lib.sets.to_list(elements)))))

def intersection(cx: T0, g: hydra.graph.Graph, set1_term: hydra.core.Term, set2_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly set intersection."""

    return hydra.lib.eithers.bind(hydra.extract.core.set(g, set1_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.logic.ifElse"))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.member"))), el))), set2_term)))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.insert"))), el))), acc)))))), acc)))), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(()))), hydra.lib.sets.to_list(elements)))))

def map(cx: T0, g: hydra.graph.Graph, fun: hydra.core.Term, set_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly map for Set terms."""

    return hydra.lib.eithers.bind(hydra.extract.core.set(g, set_term), (lambda elements: Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.fromList"))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun, el)))), hydra.lib.sets.to_list(elements))))))))))

def union(cx: T0, g: hydra.graph.Graph, set1_term: hydra.core.Term, set2_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly set union."""

    return hydra.lib.eithers.bind(hydra.extract.core.set(g, set1_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.insert"))), el))), acc)))), set2_term, hydra.lib.sets.to_list(elements)))))

def unions(cx: T0, g: hydra.graph.Graph, list_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly unions for list of Set terms."""

    return hydra.lib.eithers.bind(hydra.extract.core.list(g, list_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, s: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.union"))), acc))), s)))), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(()))), elements))))

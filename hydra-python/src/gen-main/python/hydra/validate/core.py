# Note: this is an automatically generated file. Do not edit.

r"""Validation functions for core terms."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.accessors
import hydra.core
import hydra.error.core
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.rewriting

T0 = TypeVar("T0")

def find_duplicate(names: frozenlist[T0]) -> Maybe[T0]:
    r"""Find the first duplicate name in a list."""

    @lru_cache(1)
    def result() -> tuple[frozenset[T0], Maybe[T0]]:
        return hydra.lib.lists.foldl((lambda acc, name: (seen := hydra.lib.pairs.first(acc), dup := hydra.lib.pairs.second(acc), hydra.lib.maybes.cases(dup, (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, seen), (lambda : (seen, Just(name))), (lambda : (hydra.lib.sets.insert(name, seen), Nothing())))), (lambda _: acc)))[2]), (hydra.lib.sets.empty(), Nothing()), names)
    return hydra.lib.pairs.second(result())

def check_duplicate_bindings(path: hydra.accessors.AccessorPath, bindings: frozenlist[hydra.core.Binding]) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Check for duplicate binding names in a list of bindings."""

    @lru_cache(1)
    def names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bindings)
    @lru_cache(1)
    def dup() -> Maybe[hydra.core.Name]:
        return find_duplicate(names())
    return hydra.lib.maybes.map((lambda name: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateBinding(hydra.error.core.DuplicateBindingError(path, name)))), dup())

def check_duplicate_fields(path: hydra.accessors.AccessorPath, names: frozenlist[hydra.core.Name]) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Check for duplicate field names in a list of fields."""

    @lru_cache(1)
    def dup() -> Maybe[hydra.core.Name]:
        return find_duplicate(names)
    return hydra.lib.maybes.map((lambda name: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateField(hydra.error.core.DuplicateFieldError(path, name)))), dup())

def check_term(path: hydra.accessors.AccessorPath, term: hydra.core.Term) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Check a single term node for duplicate bindings or fields."""

    match term:
        case hydra.core.TermLet(value=lt):
            return check_duplicate_bindings(path, lt.bindings)

        case hydra.core.TermRecord(value=rec):
            return check_duplicate_fields(path, hydra.lib.lists.map((lambda v1: v1.name), rec.fields))

        case _:
            return Nothing()

def term(g: hydra.graph.Graph, t: hydra.core.Term) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Validate a term, returning the first error found or nothing if valid."""

    return hydra.rewriting.fold_term_with_graph_and_path((lambda recurse, path, cx, acc, trm: hydra.lib.maybes.cases(acc, (lambda : (check_result := check_term(hydra.accessors.AccessorPath(path), trm), hydra.lib.maybes.cases(check_result, (lambda : recurse(Nothing(), trm)), (lambda err: Just(err))))[1]), (lambda _: acc))), g, Nothing(), t)

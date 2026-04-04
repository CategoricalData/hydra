# Note: this is an automatically generated file. Do not edit.

r"""Transform test cases for code generation, filtering to tests that can be compiled to target languages."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.packaging
import hydra.testing
import hydra.util

T0 = TypeVar("T0")

def add_generation_prefix(ns_: hydra.packaging.Namespace) -> hydra.packaging.Namespace:
    r"""Add generation namespace prefix."""

    return hydra.packaging.Namespace(hydra.lib.strings.cat2("generation.", ns_.value))

def encode_case_convention(conv: hydra.util.CaseConvention) -> hydra.core.Term:
    def _hoist_hydra_test_transform_encode_case_convention_1(v1):
        match v1:
            case hydra.util.CaseConvention.LOWER_SNAKE:
                return hydra.core.Name("lowerSnake")

            case hydra.util.CaseConvention.UPPER_SNAKE:
                return hydra.core.Name("upperSnake")

            case hydra.util.CaseConvention.CAMEL:
                return hydra.core.Name("camel")

            case hydra.util.CaseConvention.PASCAL:
                return hydra.core.Name("pascal")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(_hoist_hydra_test_transform_encode_case_convention_1(conv), cast(hydra.core.Term, hydra.core.TermUnit())))))

def build_convert_case_call(from_conv: hydra.util.CaseConvention, to_conv: hydra.util.CaseConvention, input_: str) -> hydra.core.Term:
    r"""Build a Term representing a convertCase function call."""

    return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.formatting.convertCase"))), encode_case_convention(from_conv)))), encode_case_convention(to_conv)))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(input_)))))))

def encode_int(n: int) -> hydra.core.Term:
    r"""Encode an Int as a Term."""

    return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(n))))))

def encode_adjacency_list(pairs: frozenlist[tuple[int, frozenlist[int]]]) -> hydra.core.Term:
    r"""Encode an adjacency list as a Term."""

    return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda p: cast(hydra.core.Term, hydra.core.TermPair((encode_int(hydra.lib.pairs.first(p)), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda d: encode_int(d)), hydra.lib.pairs.second(p)))))))), pairs)))

def build_topological_sort_call(adj_list: frozenlist[tuple[int, frozenlist[int]]]) -> hydra.core.Term:
    r"""Build a Term representing a topologicalSort function call."""

    return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.sorting.topologicalSort"))), encode_adjacency_list(adj_list))))

def build_topological_sort_s_c_c_call(adj_list: frozenlist[tuple[int, frozenlist[int]]]) -> hydra.core.Term:
    r"""Build a Term representing a topologicalSortComponents function call."""

    return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.sorting.topologicalSortComponents"))), encode_adjacency_list(adj_list))))

def collect_test_cases(tg: hydra.testing.TestGroup) -> frozenlist[hydra.testing.TestCaseWithMetadata]:
    r"""Collect all test cases from a test group (flattening hierarchy)."""

    return hydra.lib.lists.concat2(tg.cases, hydra.lib.lists.concat(hydra.lib.lists.map((lambda sg: collect_test_cases(sg)), tg.subgroups)))

def encode_int_list(ints: frozenlist[int]) -> hydra.core.Term:
    r"""Encode [Int] as a Term."""

    return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda n: encode_int(n)), ints)))

def encode_list_list(lists: frozenlist[frozenlist[int]]) -> hydra.core.Term:
    r"""Encode [[Int]] as a Term."""

    return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda l: encode_int_list(l)), lists)))

def encode_either_list_list(e: Either[frozenlist[frozenlist[int]], frozenlist[int]]) -> hydra.core.Term:
    r"""Encode Either [[Int]] [Int] as a Term."""

    return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.bimap((lambda cycles: encode_list_list(cycles)), (lambda sorted: encode_int_list(sorted)), e)))

def transform_module(m: hydra.packaging.Module) -> hydra.packaging.Module:
    r"""Transform module with generation namespace."""

    return hydra.packaging.Module(add_generation_prefix(m.namespace), m.definitions, m.term_dependencies, m.type_dependencies, m.description)

def transform_test_case(tcm: T0) -> Maybe[T0]:
    r"""Pass through test cases unchanged."""

    return Just(tcm)

def transform_to_compiled_tests(tg: hydra.testing.TestGroup) -> Maybe[hydra.testing.TestGroup]:
    r"""Transform test group hierarchy to only include delegated evaluation tests."""

    name_ = tg.name
    desc = tg.description
    subgroups = tg.subgroups
    cases_ = tg.cases
    @lru_cache(1)
    def transformed_cases() -> frozenlist[hydra.testing.TestCaseWithMetadata]:
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda tc: transform_test_case(tc)), cases_))
    @lru_cache(1)
    def transformed_subgroups() -> frozenlist[hydra.testing.TestGroup]:
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda sg: transform_to_compiled_tests(sg)), subgroups))
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.lists.null(transformed_cases()), hydra.lib.lists.null(transformed_subgroups())), (lambda : Nothing()), (lambda : Just(hydra.testing.TestGroup(name_, desc, transformed_subgroups(), transformed_cases()))))

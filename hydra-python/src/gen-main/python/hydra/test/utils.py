# Note: this is an automatically generated file. Do not edit.

r"""Shared utility functions for test code generation codecs."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.inference
import hydra.lexical
import hydra.lib.eithers
import hydra.show.errors
import hydra.testing
import hydra.typing

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def infer_term(g: hydra.graph.Graph, term: hydra.core.Term) -> Either[str, hydra.core.Term]:
    r"""Run type inference on a single term."""

    return hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda x: x.term), hydra.inference.infer_in_graph_context(hydra.lexical.empty_context(), g, term))

def infer_test_case(g: T0, tcm: hydra.testing.TestCaseWithMetadata) -> Either[T1, hydra.testing.TestCaseWithMetadata]:
    r"""Run type inference on the terms in a test case."""

    name_ = tcm.name
    tcase = tcm.case
    desc = tcm.description
    tags_ = tcm.tags
    return hydra.lib.eithers.map((lambda inferred_case: hydra.testing.TestCaseWithMetadata(name_, inferred_case, desc, tags_)), Right(tcase))

def infer_test_group_terms(g: T0, tg: hydra.testing.TestGroup) -> Either[frozenlist[hydra.testing.TestGroup], hydra.testing.TestGroup]:
    r"""Run type inference on all terms in a TestGroup to ensure lambdas have domain types."""

    name_ = tg.name
    desc = tg.description
    subgroups = tg.subgroups
    cases_ = tg.cases
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda sg: infer_test_group_terms(g, sg)), subgroups), (lambda inferred_subgroups: hydra.lib.eithers.map((lambda inferred_cases: hydra.testing.TestGroup(name_, desc, inferred_subgroups, inferred_cases)), hydra.lib.eithers.map_list((lambda tc: infer_test_case(g, tc)), cases_))))

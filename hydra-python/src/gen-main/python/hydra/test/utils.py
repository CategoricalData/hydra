# Note: this is an automatically generated file. Do not edit.

r"""Shared utility functions for test code generation codecs."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right, frozenlist
from typing import cast
import hydra.context
import hydra.core
import hydra.inference
import hydra.lexical
import hydra.lib.eithers
import hydra.show.error
import hydra.testing
import hydra.typing

def infer_term(g: hydra.graph.Graph, term: hydra.core.Term) -> Either[str, hydra.core.Term]:
    r"""Run type inference on a single term."""

    return hydra.lib.eithers.bimap((lambda ic: hydra.show.error.error(ic.object)), (lambda x: x.term), hydra.inference.infer_in_graph_context(hydra.lexical.empty_context(), g, term))

def infer_test_case(g: hydra.graph.Graph, tcm: hydra.testing.TestCaseWithMetadata):
    r"""Run type inference on the terms in a test case."""

    name_ = tcm.name
    tcase = tcm.case
    desc = tcm.description
    tags_ = tcm.tags
    def _hoist_body_1(v1):
        match v1:
            case hydra.testing.TestCaseDelegatedEvaluation(value=del_case):
                input_ = del_case.input
                output_ = del_case.output
                return hydra.lib.eithers.bind(infer_term(g, input_), (lambda inferred_input: hydra.lib.eithers.map((lambda inferred_output: cast(hydra.testing.TestCase, hydra.testing.TestCaseDelegatedEvaluation(hydra.testing.DelegatedEvaluationTestCase(inferred_input, inferred_output)))), infer_term(g, output_))))

            case _:
                return Right(tcase)
    return hydra.lib.eithers.map((lambda inferred_case: hydra.testing.TestCaseWithMetadata(name_, inferred_case, desc, tags_)), _hoist_body_1(tcase))

def infer_test_group_terms(g: hydra.graph.Graph, tg: hydra.testing.TestGroup) -> Either[str, hydra.testing.TestGroup]:
    r"""Run type inference on all terms in a TestGroup to ensure lambdas have domain types."""

    name_ = tg.name
    desc = tg.description
    subgroups = tg.subgroups
    cases_ = tg.cases
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda sg: infer_test_group_terms(g, sg)), subgroups), (lambda inferred_subgroups: hydra.lib.eithers.map((lambda inferred_cases: hydra.testing.TestGroup(name_, desc, inferred_subgroups, inferred_cases)), hydra.lib.eithers.map_list((lambda tc: infer_test_case(g, tc)), cases_))))

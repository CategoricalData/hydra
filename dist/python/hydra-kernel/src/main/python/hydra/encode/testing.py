# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.testing."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.lib.maybes
import hydra.testing

def tag(x: hydra.testing.Tag) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.testing.Tag"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def universal_test_case(x: hydra.testing.UniversalTestCase) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.UniversalTestCase"), (hydra.core.Field(hydra.core.Name("actual"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.actual))))), hydra.core.Field(hydra.core.Name("expected"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.expected)))))))))

def test_case(v1: hydra.testing.TestCase) -> hydra.core.Term:
    match v1:
        case hydra.testing.TestCaseUniversal(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("universal"), universal_test_case(y)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def test_case_with_metadata(x: hydra.testing.TestCaseWithMetadata) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.name))))), hydra.core.Field(hydra.core.Name("case"), test_case(x.case)), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.description)))), hydra.core.Field(hydra.core.Name("tags"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: tag(x1)), x.tags))))))))

def test_group(x: hydra.testing.TestGroup) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestGroup"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.name))))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.description)))), hydra.core.Field(hydra.core.Name("subgroups"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: test_group(x1)), x.subgroups)))), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: test_case_with_metadata(x1)), x.cases))))))))

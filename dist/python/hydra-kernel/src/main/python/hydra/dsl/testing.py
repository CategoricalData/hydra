# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.testing."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def tag(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.testing.Tag"), x.value))))

def test_case_universal(x: hydra.phantoms.TTerm[hydra.testing.UniversalTestCase]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("universal"), x.value)))))

def test_case_with_metadata(name: hydra.phantoms.TTerm[str], case: hydra.phantoms.TTerm[hydra.testing.TestCase], description: hydra.phantoms.TTerm[Maybe[str]], tags: hydra.phantoms.TTerm[frozenlist[hydra.testing.Tag]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("case"), case.value), hydra.core.Field(hydra.core.Name("description"), description.value), hydra.core.Field(hydra.core.Name("tags"), tags.value))))))

def test_case_with_metadata_case(x: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("case")))), x.value))))

def test_case_with_metadata_description(x: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("description")))), x.value))))

def test_case_with_metadata_name(x: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("name")))), x.value))))

def test_case_with_metadata_tags(x: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("tags")))), x.value))))

def test_case_with_metadata_with_case(original: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata], new_val: hydra.phantoms.TTerm[hydra.testing.TestCase]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("case"), new_val.value), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("description")))), original.value)))), hydra.core.Field(hydra.core.Name("tags"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("tags")))), original.value)))))))))

def test_case_with_metadata_with_description(original: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata], new_val: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("case"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("case")))), original.value)))), hydra.core.Field(hydra.core.Name("description"), new_val.value), hydra.core.Field(hydra.core.Name("tags"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("tags")))), original.value)))))))))

def test_case_with_metadata_with_name(original: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("case"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("case")))), original.value)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("description")))), original.value)))), hydra.core.Field(hydra.core.Name("tags"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("tags")))), original.value)))))))))

def test_case_with_metadata_with_tags(original: hydra.phantoms.TTerm[hydra.testing.TestCaseWithMetadata], new_val: hydra.phantoms.TTerm[frozenlist[hydra.testing.Tag]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("case"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("case")))), original.value)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.core.Name("description")))), original.value)))), hydra.core.Field(hydra.core.Name("tags"), new_val.value))))))

def test_group(name: hydra.phantoms.TTerm[str], description: hydra.phantoms.TTerm[Maybe[str]], subgroups: hydra.phantoms.TTerm[frozenlist[hydra.testing.TestGroup]], cases: hydra.phantoms.TTerm[frozenlist[hydra.testing.TestCaseWithMetadata]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestGroup"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("description"), description.value), hydra.core.Field(hydra.core.Name("subgroups"), subgroups.value), hydra.core.Field(hydra.core.Name("cases"), cases.value))))))

def test_group_cases(x: hydra.phantoms.TTerm[hydra.testing.TestGroup]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("cases")))), x.value))))

def test_group_description(x: hydra.phantoms.TTerm[hydra.testing.TestGroup]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("description")))), x.value))))

def test_group_name(x: hydra.phantoms.TTerm[hydra.testing.TestGroup]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("name")))), x.value))))

def test_group_subgroups(x: hydra.phantoms.TTerm[hydra.testing.TestGroup]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("subgroups")))), x.value))))

def test_group_with_cases(original: hydra.phantoms.TTerm[hydra.testing.TestGroup], new_val: hydra.phantoms.TTerm[frozenlist[hydra.testing.TestCaseWithMetadata]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestGroup"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("description")))), original.value)))), hydra.core.Field(hydra.core.Name("subgroups"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("subgroups")))), original.value)))), hydra.core.Field(hydra.core.Name("cases"), new_val.value))))))

def test_group_with_description(original: hydra.phantoms.TTerm[hydra.testing.TestGroup], new_val: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestGroup"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("description"), new_val.value), hydra.core.Field(hydra.core.Name("subgroups"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("subgroups")))), original.value)))), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("cases")))), original.value)))))))))

def test_group_with_name(original: hydra.phantoms.TTerm[hydra.testing.TestGroup], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestGroup"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("description")))), original.value)))), hydra.core.Field(hydra.core.Name("subgroups"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("subgroups")))), original.value)))), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("cases")))), original.value)))))))))

def test_group_with_subgroups(original: hydra.phantoms.TTerm[hydra.testing.TestGroup], new_val: hydra.phantoms.TTerm[frozenlist[hydra.testing.TestGroup]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestGroup"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("description")))), original.value)))), hydra.core.Field(hydra.core.Name("subgroups"), new_val.value), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.TestGroup"), hydra.core.Name("cases")))), original.value)))))))))

def un_tag(x: hydra.phantoms.TTerm[hydra.testing.Tag]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.testing.Tag"))), x.value))))

def universal_test_case(actual: hydra.phantoms.TTerm[str], expected: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.UniversalTestCase"), (hydra.core.Field(hydra.core.Name("actual"), actual.value), hydra.core.Field(hydra.core.Name("expected"), expected.value))))))

def universal_test_case_actual(x: hydra.phantoms.TTerm[hydra.testing.UniversalTestCase]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.UniversalTestCase"), hydra.core.Name("actual")))), x.value))))

def universal_test_case_expected(x: hydra.phantoms.TTerm[hydra.testing.UniversalTestCase]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.UniversalTestCase"), hydra.core.Name("expected")))), x.value))))

def universal_test_case_with_actual(original: hydra.phantoms.TTerm[hydra.testing.UniversalTestCase], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.UniversalTestCase"), (hydra.core.Field(hydra.core.Name("actual"), new_val.value), hydra.core.Field(hydra.core.Name("expected"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.UniversalTestCase"), hydra.core.Name("expected")))), original.value)))))))))

def universal_test_case_with_expected(original: hydra.phantoms.TTerm[hydra.testing.UniversalTestCase], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.UniversalTestCase"), (hydra.core.Field(hydra.core.Name("actual"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.testing.UniversalTestCase"), hydra.core.Name("actual")))), original.value)))), hydra.core.Field(hydra.core.Name("expected"), new_val.value))))))

# Note: this is an automatically generated file. Do not edit.

r"""A model for unit testing."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

class Tag(Node[str]):
    r"""A tag for test cases."""

Tag.TYPE_ = hydra.core.Name("hydra.testing.Tag")

class TestCaseUniversal(Node["UniversalTestCase"]):
    r"""A universal test case (string comparison)"""

class _TestCaseMeta(type):
    def __getitem__(cls, item):
        return object

# A test case with an actual and expected string for comparison.
class TestCase(metaclass=_TestCaseMeta):
    r"""TestCaseUniversal"""

    TYPE_ = hydra.core.Name("hydra.testing.TestCase")
    UNIVERSAL = hydra.core.Name("universal")

@dataclass(frozen=True)
class TestCaseWithMetadata:
    r"""A test case together with metadata."""

    name: Annotated[str, "A short name for the test case"]
    case: Annotated[TestCase, "The test case itself"]
    description: Annotated[Maybe[str], "An optional longer description of the test case"]
    tags: Annotated[frozenlist[Tag], "Zero or more tags for the test case"]

    TYPE_ = hydra.core.Name("hydra.testing.TestCaseWithMetadata")
    NAME = hydra.core.Name("name")
    CASE = hydra.core.Name("case")
    DESCRIPTION = hydra.core.Name("description")
    TAGS = hydra.core.Name("tags")

@dataclass(frozen=True)
class TestGroup:
    r"""A collection of test cases with a name and optional description."""

    name: Annotated[str, "A short name for the test group"]
    description: Annotated[Maybe[str], "An optional longer description of the test group"]
    subgroups: Annotated[frozenlist[TestGroup], "Zero or more subgroups"]
    cases: Annotated[frozenlist[TestCaseWithMetadata], "Zero or more test cases"]

    TYPE_ = hydra.core.Name("hydra.testing.TestGroup")
    NAME = hydra.core.Name("name")
    DESCRIPTION = hydra.core.Name("description")
    SUBGROUPS = hydra.core.Name("subgroups")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class UniversalTestCase:
    r"""A universal test case: the actual and expected values are both strings."""

    actual: Annotated[str, "The actual result (a string produced by evaluating and showing the test expression)"]
    expected: Annotated[str, "The expected result (a string produced by showing the expected value)"]

    TYPE_ = hydra.core.Name("hydra.testing.UniversalTestCase")
    ACTUAL = hydra.core.Name("actual")
    EXPECTED = hydra.core.Name("expected")

# Note: this is an automatically generated file. Do not edit.

r"""Test cases for hydra.lib.pairs primitives."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Nothing
from typing import cast
import hydra.core
import hydra.lib.literals
import hydra.lib.math
import hydra.lib.pairs
import hydra.lib.strings
import hydra.testing

@lru_cache(1)
def all_tests() -> hydra.testing.TestGroup:
    r"""Test cases for hydra.lib.pairs primitives."""

    return hydra.testing.TestGroup("hydra.lib.pairs primitives", Nothing(), (hydra.testing.TestGroup("bimap", Nothing(), (), (hydra.testing.TestCaseWithMetadata("transform both elements", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.strings.cat(("(", hydra.lib.literals.show_int32(hydra.lib.pairs.first(hydra.lib.pairs.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), (5, "ab")))), ", ", hydra.lib.literals.show_int32(hydra.lib.pairs.second(hydra.lib.pairs.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), (5, "ab")))), ")")), hydra.lib.strings.cat(("(", hydra.lib.literals.show_int32(hydra.lib.pairs.first((10, 2))), ", ", hydra.lib.literals.show_int32(hydra.lib.pairs.second((10, 2))), ")"))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("with zero", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.strings.cat(("(", hydra.lib.literals.show_int32(hydra.lib.pairs.first(hydra.lib.pairs.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), (0, "hello")))), ", ", hydra.lib.literals.show_int32(hydra.lib.pairs.second(hydra.lib.pairs.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), (0, "hello")))), ")")), hydra.lib.strings.cat(("(", hydra.lib.literals.show_int32(hydra.lib.pairs.first((0, 5))), ", ", hydra.lib.literals.show_int32(hydra.lib.pairs.second((0, 5))), ")"))))), Nothing(), ()))), hydra.testing.TestGroup("first", Nothing(), (), (hydra.testing.TestCaseWithMetadata("extract first element", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_int32(hydra.lib.pairs.first((42, "hello"))), hydra.lib.literals.show_int32(42)))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("with zero", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_int32(hydra.lib.pairs.first((0, "world"))), hydra.lib.literals.show_int32(0)))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("negative number", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_int32(hydra.lib.pairs.first((-5, "test"))), hydra.lib.literals.show_int32(-5)))), Nothing(), ()))), hydra.testing.TestGroup("second", Nothing(), (), (hydra.testing.TestCaseWithMetadata("extract second element", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.pairs.second((42, "hello")), "hello"))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty string", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.pairs.second((0, "")), ""))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("long string", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.pairs.second((123, "testing")), "testing"))), Nothing(), ())))), ())

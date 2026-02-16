# Note: this is an automatically generated file. Do not edit.

r"""Hydra's type checking test suite."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Nothing
import hydra.core
import hydra.test.checking.advanced
import hydra.test.checking.algebraic_types
import hydra.test.checking.collections
import hydra.test.checking.failures
import hydra.test.checking.fundamentals
import hydra.test.checking.nominal_types
import hydra.testing

@lru_cache(1)
def all_tests() -> hydra.testing.TestGroup:
    r"""The group of all type checking tests."""
    
    return hydra.testing.TestGroup("checking", Nothing(), (hydra.test.checking.advanced.all_tests(), hydra.test.checking.algebraic_types.all_tests(), hydra.test.checking.collections.all_tests(), hydra.test.checking.failures.all_tests(), hydra.test.checking.fundamentals.all_tests(), hydra.test.checking.nominal_types.all_tests()), ())

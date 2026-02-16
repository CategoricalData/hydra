# Note: this is an automatically generated file. Do not edit.

r"""Hydra's inference test suite."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Nothing
import hydra.core
import hydra.test.inference.algebraic_types
import hydra.test.inference.algorithm_w
import hydra.test.inference.failures
import hydra.test.inference.fundamentals
import hydra.test.inference.kernel_examples
import hydra.test.inference.nominal_types
import hydra.testing

@lru_cache(1)
def all_tests() -> hydra.testing.TestGroup:
    r"""The group of all inference tests."""
    
    return hydra.testing.TestGroup("inference", Nothing(), (hydra.test.inference.algebraic_types.all_tests(), hydra.test.inference.algorithm_w.all_tests(), hydra.test.inference.failures.all_tests(), hydra.test.inference.fundamentals.all_tests(), hydra.test.inference.kernel_examples.all_tests(), hydra.test.inference.nominal_types.all_tests()), ())

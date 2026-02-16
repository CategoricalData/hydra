# Note: this is an automatically generated file. Do not edit.

r"""Type checking failure test cases."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Nothing
import hydra.core
import hydra.testing

@lru_cache(1)
def untyped_lambdas_tests() -> hydra.testing.TestGroup:
    return hydra.testing.TestGroup("Untyped lambdas", Nothing(), (), ())

@lru_cache(1)
def fail_on_untyped_tests() -> hydra.testing.TestGroup:
    return hydra.testing.TestGroup("Fail on untyped (pre-inference) terms", Nothing(), (untyped_lambdas_tests(),), ())

@lru_cache(1)
def all_tests() -> hydra.testing.TestGroup:
    r"""Type checking failure test cases."""
    
    return hydra.testing.TestGroup("Failures", Nothing(), (fail_on_untyped_tests(),), ())

# Note: this is an automatically generated file. Do not edit.

r"""Hydra's validation test suite."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Nothing
from typing import cast
import hydra.core
import hydra.test.validate.core
import hydra.testing

@lru_cache(1)
def all_tests() -> hydra.testing.TestGroup:
    r"""The group of all validation tests."""

    return hydra.testing.TestGroup("validation", Nothing(), (hydra.test.validate.core.all_tests(),), ())

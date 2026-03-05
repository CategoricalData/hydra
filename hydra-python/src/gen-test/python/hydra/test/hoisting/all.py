# Note: this is an automatically generated file. Do not edit.

r"""Hydra's hoisting test suite."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Nothing
from typing import cast
import hydra.core
import hydra.test.hoisting.cases
import hydra.test.hoisting.let
import hydra.testing

@lru_cache(1)
def all_tests() -> hydra.testing.TestGroup:
    r"""The group of all hoisting tests."""
    
    return hydra.testing.TestGroup("hoisting", Nothing(), (hydra.test.hoisting.cases.all_tests(), hydra.test.hoisting.let.all_tests()), ())

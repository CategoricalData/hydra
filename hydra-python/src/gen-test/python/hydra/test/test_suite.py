# Note: this is an automatically generated file. Do not edit.

r"""Hydra's common test suite, which is designed to run identically in each Hydra implementation; the criterion for a true Hydra implementation is that all test cases pass."""

from __future__ import annotations
from hydra.dsl.python import Nothing
import hydra.core
import hydra.test.annotations
import hydra.test.checking.all
import hydra.test.eta_expansion
import hydra.test.formatting
import hydra.test.hoisting
import hydra.test.inference.all
import hydra.test.json.coder
import hydra.test.json.parser
import hydra.test.json.roundtrip
import hydra.test.json.writer
import hydra.test.lib.chars
import hydra.test.lib.eithers
import hydra.test.lib.equality
import hydra.test.lib.flows
import hydra.test.lib.lists
import hydra.test.lib.literals
import hydra.test.lib.logic
import hydra.test.lib.maps
import hydra.test.lib.math
import hydra.test.lib.maybes
import hydra.test.lib.pairs
import hydra.test.lib.sets
import hydra.test.lib.strings
import hydra.test.monads
import hydra.test.reduction
import hydra.test.rewriting
import hydra.test.serialization
import hydra.test.sorting
import hydra.testing

def all_tests() -> hydra.testing.TestGroup:
    r"""The group of all common tests."""
    
    return hydra.testing.TestGroup("common", Nothing(), (hydra.test.lib.chars.all_tests(), hydra.test.lib.eithers.all_tests(), hydra.test.lib.equality.all_tests(), hydra.test.lib.flows.all_tests(), hydra.test.lib.lists.all_tests(), hydra.test.lib.literals.all_tests(), hydra.test.lib.logic.all_tests(), hydra.test.lib.maps.all_tests(), hydra.test.lib.math.all_tests(), hydra.test.lib.maybes.all_tests(), hydra.test.lib.pairs.all_tests(), hydra.test.lib.sets.all_tests(), hydra.test.lib.strings.all_tests(), hydra.test.annotations.all_tests(), hydra.test.checking.all.all_tests(), hydra.test.eta_expansion.all_tests(), hydra.test.formatting.all_tests(), hydra.test.hoisting.all_tests(), hydra.test.inference.all.all_tests(), hydra.test.json.coder.all_tests(), hydra.test.json.parser.all_tests(), hydra.test.json.roundtrip.all_tests(), hydra.test.json.writer.all_tests(), hydra.test.monads.all_tests(), hydra.test.reduction.all_tests(), hydra.test.rewriting.all_tests(), hydra.test.serialization.all_tests(), hydra.test.sorting.all_tests()), ())

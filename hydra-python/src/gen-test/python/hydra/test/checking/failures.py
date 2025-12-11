# Note: this is an automatically generated file. Do not edit.

r"""Type checking failure test cases."""

from __future__ import annotations
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import cast
import hydra.core
import hydra.testing

untyped_lambdas_tests = hydra.testing.TestGroup("Untyped lambdas", cast(Maybe[str], Nothing()), cast(frozenlist[hydra.testing.TestGroup], ()), cast(frozenlist[hydra.testing.TestCaseWithMetadata], ()))

fail_on_untyped_tests = hydra.testing.TestGroup("Fail on untyped (pre-inference) terms", cast(Maybe[str], Nothing()), (untyped_lambdas_tests,), cast(frozenlist[hydra.testing.TestCaseWithMetadata], ()))

# Type checking failure test cases.
all_tests = hydra.testing.TestGroup("Failures", cast(Maybe[str], Nothing()), (fail_on_untyped_tests,), cast(frozenlist[hydra.testing.TestCaseWithMetadata], ()))

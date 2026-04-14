# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.util."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.phantoms

case_convention_camel = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("camel"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

case_convention_lower_snake = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("lowerSnake"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

case_convention_pascal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("pascal"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

case_convention_upper_snake = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("upperSnake"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_equal_to = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("equalTo"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_greater_than = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("greaterThan"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_less_than = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("lessThan"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

precision_arbitrary = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("arbitrary"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def precision_bits(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("bits"), x.value)))))

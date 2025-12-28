# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.constraints."""

from __future__ import annotations
from typing import cast
import hydra.constraints
import hydra.core
import hydra.encode.query

def path_equation(x: hydra.constraints.PathEquation) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.constraints.PathEquation"), (hydra.core.Field(hydra.core.Name("left"), hydra.encode.query.path(x.left)), hydra.core.Field(hydra.core.Name("right"), hydra.encode.query.path(x.right))))))

def pattern_implication(x: hydra.constraints.PatternImplication) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.constraints.PatternImplication"), (hydra.core.Field(hydra.core.Name("antecedent"), hydra.encode.query.pattern(x.antecedent)), hydra.core.Field(hydra.core.Name("consequent"), hydra.encode.query.pattern(x.consequent))))))

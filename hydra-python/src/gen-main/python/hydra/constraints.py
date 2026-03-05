# Note: this is an automatically generated file. Do not edit.

r"""A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.query

@dataclass(frozen=True)
class PathEquation:
    r"""A declared equivalence between two abstract paths in a graph."""
    
    left: Annotated[hydra.query.Path, "The left-hand side of the equation"]
    right: Annotated[hydra.query.Path, "The right-hand side of the equation"]
    
    TYPE_ = hydra.core.Name("hydra.constraints.PathEquation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class PatternImplication:
    r"""A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns."""
    
    antecedent: Annotated[hydra.query.Pattern, "The pattern which, if it matches, triggers the constraint"]
    consequent: Annotated[hydra.query.Pattern, "The pattern which must also match when the antecedent matches"]
    
    TYPE_ = hydra.core.Name("hydra.constraints.PatternImplication")
    ANTECEDENT = hydra.core.Name("antecedent")
    CONSEQUENT = hydra.core.Name("consequent")

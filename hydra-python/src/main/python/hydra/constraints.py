# Note: this is an automatically generated file. Do not edit.

r"""A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph."""

from __future__ import annotations
from dataclasses import dataclass
from typing import Annotated
import hydra.core
import hydra.query

@dataclass
class PathEquation:
    r"""A declared equivalence between two abstract paths in a graph."""
    
    left: Annotated[hydra.query.Path, "The left-hand side of the equation"]
    right: Annotated[hydra.query.Path, "The right-hand side of the equation"]

PATH_EQUATION__NAME = hydra.core.Name("hydra.constraints.PathEquation")
PATH_EQUATION__LEFT__NAME = hydra.core.Name("left")
PATH_EQUATION__RIGHT__NAME = hydra.core.Name("right")

@dataclass
class PatternImplication:
    r"""A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns."""
    
    antecedent: Annotated[hydra.query.Pattern, "The pattern which, if it matches, triggers the constraint"]
    consequent: Annotated[hydra.query.Pattern, "The pattern which must also match when the antecedent matches"]

PATTERN_IMPLICATION__NAME = hydra.core.Name("hydra.constraints.PatternImplication")
PATTERN_IMPLICATION__ANTECEDENT__NAME = hydra.core.Name("antecedent")
PATTERN_IMPLICATION__CONSEQUENT__NAME = hydra.core.Name("consequent")

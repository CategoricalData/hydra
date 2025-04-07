"""A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph."""

from __future__ import annotations
from dataclasses import dataclass
import hydra.core
import hydra.query

@dataclass
class PathEquation:
    """A declared equivalence between two abstract paths in a graph."""
    
    left: hydra.query.Path
    right: hydra.query.Path

PATH_EQUATION__NAME = hydra.core.Name("hydra.constraints.PathEquation")
PATH_EQUATION__LEFT__NAME = hydra.core.Name("left")
PATH_EQUATION__RIGHT__NAME = hydra.core.Name("right")

@dataclass
class PatternImplication:
    """A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns."""
    
    antecedent: hydra.query.Pattern
    consequent: hydra.query.Pattern

PATTERN_IMPLICATION__NAME = hydra.core.Name("hydra.constraints.PatternImplication")
PATTERN_IMPLICATION__ANTECEDENT__NAME = hydra.core.Name("antecedent")
PATTERN_IMPLICATION__CONSEQUENT__NAME = hydra.core.Name("consequent")

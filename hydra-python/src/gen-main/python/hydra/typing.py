"""Types supporting type inference."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Node
from typing import Annotated
import hydra.core

@dataclass
class InferenceContext:
    """The context provided to type inference, including various typing enviroments."""
    
    schema_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A fixed typing environment which is derived from the schema of the graph."]
    primitive_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A fixed typing environment which is derived from the set of primitives in the graph."]
    data_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A mutable typing environment which is specific to the current graph being processed. This environment is (usually) smaller than the schema and primitive typing environments, and is subject to global substitutions."]
    debug: bool

@dataclass
class InferenceResult:
    """The result of applying inference rules to a term."""
    
    term: hydra.core.Term
    type: hydra.core.Type
    subst: TypeSubst

class TermSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Term]"]):
    """A substitution of term variables for terms."""

@dataclass
class TypeConstraint:
    """An assertion that two types can be unified into a single type."""
    
    left: hydra.core.Type
    right: hydra.core.Type
    comment: Annotated[str, "A description of the type constraint which may be used for tracing or debugging"]

class TypeSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Type]"]):
    """A substitution of type variables for types."""

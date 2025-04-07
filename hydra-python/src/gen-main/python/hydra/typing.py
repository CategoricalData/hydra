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

INFERENCE_CONTEXT__NAME = hydra.core.Name("hydra.typing.InferenceContext")
INFERENCE_CONTEXT__SCHEMA_TYPES__NAME = hydra.core.Name("schemaTypes")
INFERENCE_CONTEXT__PRIMITIVE_TYPES__NAME = hydra.core.Name("primitiveTypes")
INFERENCE_CONTEXT__DATA_TYPES__NAME = hydra.core.Name("dataTypes")
INFERENCE_CONTEXT__DEBUG__NAME = hydra.core.Name("debug")

@dataclass
class InferenceResult:
    """The result of applying inference rules to a term."""
    
    term: hydra.core.Term
    type: hydra.core.Type
    subst: TypeSubst

INFERENCE_RESULT__NAME = hydra.core.Name("hydra.typing.InferenceResult")
INFERENCE_RESULT__TERM__NAME = hydra.core.Name("term")
INFERENCE_RESULT__TYPE__NAME = hydra.core.Name("type")
INFERENCE_RESULT__SUBST__NAME = hydra.core.Name("subst")

class TermSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Term]"]):
    """A substitution of term variables for terms."""

TERM_SUBST__NAME = hydra.core.Name("hydra.typing.TermSubst")

@dataclass
class TypeConstraint:
    """An assertion that two types can be unified into a single type."""
    
    left: hydra.core.Type
    right: hydra.core.Type
    comment: Annotated[str, "A description of the type constraint which may be used for tracing or debugging"]

TYPE_CONSTRAINT__NAME = hydra.core.Name("hydra.typing.TypeConstraint")
TYPE_CONSTRAINT__LEFT__NAME = hydra.core.Name("left")
TYPE_CONSTRAINT__RIGHT__NAME = hydra.core.Name("right")
TYPE_CONSTRAINT__COMMENT__NAME = hydra.core.Name("comment")

class TypeSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Type]"]):
    """A substitution of type variables for types."""

TYPE_SUBST__NAME = hydra.core.Name("hydra.typing.TypeSubst")

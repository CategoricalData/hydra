"""Types supporting type inference."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Annotated

import hydra.gen.core
from hydra.dsl.python import FrozenDict, Node


@dataclass
class InferenceContext:
    """The context provided to type inference, including various typing enviroments."""
    
    schema_types: Annotated[FrozenDict[hydra.gen.core.Name, hydra.gen.core.TypeScheme], "A fixed typing environment which is derived from the schema of the graph."]
    primitive_types: Annotated[FrozenDict[hydra.gen.core.Name, hydra.gen.core.TypeScheme], "A fixed typing environment which is derived from the set of primitives in the graph."]
    data_types: Annotated[FrozenDict[hydra.gen.core.Name, hydra.gen.core.TypeScheme], "A mutable typing environment which is specific to the current graph being processed. This environment is (usually) smaller than the schema and primitive typing environments, and is subject to global substitutions."]
    debug: bool

INFERENCE_CONTEXT__NAME = hydra.gen.core.Name("hydra.typing.InferenceContext")
INFERENCE_CONTEXT__SCHEMA_TYPES__NAME = hydra.gen.core.Name("schemaTypes")
INFERENCE_CONTEXT__PRIMITIVE_TYPES__NAME = hydra.gen.core.Name("primitiveTypes")
INFERENCE_CONTEXT__DATA_TYPES__NAME = hydra.gen.core.Name("dataTypes")
INFERENCE_CONTEXT__DEBUG__NAME = hydra.gen.core.Name("debug")

@dataclass
class InferenceResult:
    """The result of applying inference rules to a term."""
    
    term: hydra.gen.core.Term
    type: hydra.gen.core.Type
    subst: TypeSubst

INFERENCE_RESULT__NAME = hydra.gen.core.Name("hydra.typing.InferenceResult")
INFERENCE_RESULT__TERM__NAME = hydra.gen.core.Name("term")
INFERENCE_RESULT__TYPE__NAME = hydra.gen.core.Name("type")
INFERENCE_RESULT__SUBST__NAME = hydra.gen.core.Name("subst")

class TermSubst(Node["FrozenDict[hydra.gen.core.Name, hydra.gen.core.Term]"]):
    """A substitution of term variables for terms."""

TERM_SUBST__NAME = hydra.gen.core.Name("hydra.typing.TermSubst")

@dataclass
class TypeConstraint:
    """An assertion that two types can be unified into a single type."""
    
    left: hydra.gen.core.Type
    right: hydra.gen.core.Type
    comment: Annotated[str, "A description of the type constraint which may be used for tracing or debugging"]

TYPE_CONSTRAINT__NAME = hydra.gen.core.Name("hydra.typing.TypeConstraint")
TYPE_CONSTRAINT__LEFT__NAME = hydra.gen.core.Name("left")
TYPE_CONSTRAINT__RIGHT__NAME = hydra.gen.core.Name("right")
TYPE_CONSTRAINT__COMMENT__NAME = hydra.gen.core.Name("comment")

class TypeSubst(Node["FrozenDict[hydra.gen.core.Name, hydra.gen.core.Type]"]):
    """A substitution of type variables for types."""

TYPE_SUBST__NAME = hydra.gen.core.Name("hydra.typing.TypeSubst")

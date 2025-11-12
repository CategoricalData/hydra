# Note: this is an automatically generated file. Do not edit.

r"""Types supporting type inference and type reconstruction."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Node
from typing import Annotated
import hydra.core

@dataclass
class InferenceContext:
    r"""The context provided to type inference, including various typing enviroments."""
    
    schema_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A fixed typing environment which is derived from the schema of the graph."]
    primitive_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A fixed typing environment which is derived from the set of primitives in the graph."]
    data_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A mutable typing environment which is specific to the current graph being processed. This environment is (usually) smaller than the schema and primitive typing environments, and is subject to global substitutions."]
    debug: Annotated[bool, "Whether to enable debug output during type inference"]

INFERENCE_CONTEXT__NAME = hydra.core.Name("hydra.typing.InferenceContext")
INFERENCE_CONTEXT__SCHEMA_TYPES__NAME = hydra.core.Name("schemaTypes")
INFERENCE_CONTEXT__PRIMITIVE_TYPES__NAME = hydra.core.Name("primitiveTypes")
INFERENCE_CONTEXT__DATA_TYPES__NAME = hydra.core.Name("dataTypes")
INFERENCE_CONTEXT__DEBUG__NAME = hydra.core.Name("debug")

@dataclass
class InferenceResult:
    r"""The result of applying inference rules to a term."""
    
    term: Annotated[hydra.core.Term, "The term which was inferred"]
    type: Annotated[hydra.core.Type, "The inferred type of the term"]
    subst: Annotated[TypeSubst, "The type substitution resulting from unification"]

INFERENCE_RESULT__NAME = hydra.core.Name("hydra.typing.InferenceResult")
INFERENCE_RESULT__TERM__NAME = hydra.core.Name("term")
INFERENCE_RESULT__TYPE__NAME = hydra.core.Name("type")
INFERENCE_RESULT__SUBST__NAME = hydra.core.Name("subst")

class TermSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Term]"]):
    r"""A substitution of term variables for terms."""

TERM_SUBST__NAME = hydra.core.Name("hydra.typing.TermSubst")

@dataclass
class TypeConstraint:
    r"""An assertion that two types can be unified into a single type."""
    
    left: Annotated[hydra.core.Type, "The left-hand side of the constraint"]
    right: Annotated[hydra.core.Type, "The right-hand side of the constraint"]
    comment: Annotated[str, "A description of the type constraint which may be used for tracing or debugging"]

TYPE_CONSTRAINT__NAME = hydra.core.Name("hydra.typing.TypeConstraint")
TYPE_CONSTRAINT__LEFT__NAME = hydra.core.Name("left")
TYPE_CONSTRAINT__RIGHT__NAME = hydra.core.Name("right")
TYPE_CONSTRAINT__COMMENT__NAME = hydra.core.Name("comment")

@dataclass
class TypeContext:
    r"""A typing environment used for type reconstruction (typeOf) over System F terms."""
    
    types: Annotated[FrozenDict[hydra.core.Name, hydra.core.Type], "A mapping of lambda- and let-bound variables to their types"]
    variables: Annotated[frozenset[hydra.core.Name], "The set of type variables introduced by enclosing type lambdas"]
    inference_context: Annotated[InferenceContext, "The schema types, primitive types, and data types of the graph"]

TYPE_CONTEXT__NAME = hydra.core.Name("hydra.typing.TypeContext")
TYPE_CONTEXT__TYPES__NAME = hydra.core.Name("types")
TYPE_CONTEXT__VARIABLES__NAME = hydra.core.Name("variables")
TYPE_CONTEXT__INFERENCE_CONTEXT__NAME = hydra.core.Name("inferenceContext")

class TypeSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Type]"]):
    r"""A substitution of type variables for types."""

TYPE_SUBST__NAME = hydra.core.Name("hydra.typing.TypeSubst")

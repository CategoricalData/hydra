# Note: this is an automatically generated file. Do not edit.

r"""The extension to graphs of Hydra's core type system (hydra.core)."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.context
import hydra.core
import hydra.errors

A = TypeVar("A")

@dataclass(frozen=True)
class Graph:
    r"""A graph, or lexical environment which binds names to terms, types, primitives, and metadata."""

    bound_terms: Annotated[FrozenDict[hydra.core.Name, hydra.core.Term], "The terms bound by all term variables in scope"]
    bound_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "The type schemes of all term variables in scope"]
    class_constraints: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata], "A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered."]
    lambda_variables: Annotated[frozenset[hydra.core.Name], "The set of term variables introduced by specifically by lambdas"]
    metadata: Annotated[FrozenDict[hydra.core.Name, hydra.core.Term], "Any additional metadata bound to term variables in scope"]
    primitives: Annotated[FrozenDict[hydra.core.Name, Primitive], "All primitive functions and constants by name"]
    schema_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "All schema types (type schemes) in scope"]
    type_variables: Annotated[frozenset[hydra.core.Name], "The set of type variables introduced specifically by type lambdas"]

    TYPE_ = hydra.core.Name("hydra.graph.Graph")
    BOUND_TERMS = hydra.core.Name("boundTerms")
    BOUND_TYPES = hydra.core.Name("boundTypes")
    CLASS_CONSTRAINTS = hydra.core.Name("classConstraints")
    LAMBDA_VARIABLES = hydra.core.Name("lambdaVariables")
    METADATA = hydra.core.Name("metadata")
    PRIMITIVES = hydra.core.Name("primitives")
    SCHEMA_TYPES = hydra.core.Name("schemaTypes")
    TYPE_VARIABLES = hydra.core.Name("typeVariables")

@dataclass(frozen=True)
class Primitive:
    r"""A built-in function or constant."""

    name: Annotated[hydra.core.Name, "The unique name of the primitive function"]
    type: Annotated[hydra.core.TypeScheme, "The type signature of the primitive function"]
    implementation: Annotated[Callable[[hydra.context.Context, Graph, frozenlist[hydra.core.Term]], Either[hydra.errors.Error, hydra.core.Term]], "A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting."]

    TYPE_ = hydra.core.Name("hydra.graph.Primitive")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    IMPLEMENTATION = hydra.core.Name("implementation")

@dataclass(frozen=True)
class TermCoder(Generic[A]):
    r"""A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms."""

    type: Annotated[hydra.core.Type, "The Hydra type of encoded terms"]
    encode: Annotated[Callable[[hydra.context.Context, Graph, hydra.core.Term], Either[hydra.errors.Error, A]], "An encode function from terms to native values"]
    decode: Annotated[Callable[[hydra.context.Context, A], Either[hydra.errors.Error, hydra.core.Term]], "A decode function from native values to terms"]

    TYPE_ = hydra.core.Name("hydra.graph.TermCoder")
    TYPE = hydra.core.Name("type")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

# Note: this is an automatically generated file. Do not edit.

r"""Abstractions for paired transformations between languages."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Node
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.context
import hydra.core
import hydra.errors
import hydra.graph
import hydra.variants

T = TypeVar("T")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
V = TypeVar("V")
V1 = TypeVar("V1")
V2 = TypeVar("V2")

@dataclass(frozen=True)
class Adapter(Generic[T1, T2, V1, V2]):
    r"""A two-level bidirectional encoder which adapts types to types and terms to terms."""

    is_lossy: Annotated[bool, "Whether information may be lost in the course of this adaptation"]
    source: Annotated[T1, "The source type"]
    target: Annotated[T2, "The target type"]
    coder: Annotated[Coder[V1, V2], "The coder for transforming instances of the source type to instances of the target type"]

    TYPE_ = hydra.core.Name("hydra.coders.Adapter")
    IS_LOSSY = hydra.core.Name("isLossy")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")
    CODER = hydra.core.Name("coder")

@dataclass(frozen=True)
class AdapterContext:
    r"""An evaluation context together with a source language and a target language."""

    graph: Annotated[hydra.graph.Graph, "The underlying graph of elements and primitives"]
    language: Annotated[Language, "The language being encoded or decoded"]
    adapters: Annotated[FrozenDict[hydra.core.Name, Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], "A map of type names to adapters for those types"]

    TYPE_ = hydra.core.Name("hydra.coders.AdapterContext")
    GRAPH = hydra.core.Name("graph")
    LANGUAGE = hydra.core.Name("language")
    ADAPTERS = hydra.core.Name("adapters")

@dataclass(frozen=True)
class Bicoder(Generic[T1, T2, V1, V2]):
    r"""A two-level encoder and decoder, operating both at a type level and an instance (data) level."""

    encode: Annotated[Callable[[T1], Adapter[T1, T2, V1, V2]], "A function from source types to adapters"]
    decode: Annotated[Callable[[T2], Adapter[T2, T1, V2, V1]], "A function from target types to adapters"]

    TYPE_ = hydra.core.Name("hydra.coders.Bicoder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

@dataclass(frozen=True)
class Coder(Generic[V1, V2]):
    r"""An encoder and decoder; a bidirectional transformation between two types."""

    encode: Annotated[Callable[[hydra.context.Context, V1], Either[hydra.errors.Error, V2]], "A function which encodes source values as target values in a given context"]
    decode: Annotated[Callable[[hydra.context.Context, V2], Either[hydra.errors.Error, V1]], "A function which decodes target values as source values in a given context"]

    TYPE_ = hydra.core.Name("hydra.coders.Coder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

class CoderDirection(Enum):
    r"""Indicates either the 'out' or the 'in' direction of a coder."""

    ENCODE = hydra.core.Name("encode")

    DECODE = hydra.core.Name("decode")

CoderDirection.TYPE_ = hydra.core.Name("hydra.coders.CoderDirection")

@dataclass(frozen=True)
class Language:
    r"""A named language together with language-specific constraints."""

    name: Annotated[LanguageName, "The unique name of the language"]
    constraints: Annotated[LanguageConstraints, "The constraints which characterize the language"]

    TYPE_ = hydra.core.Name("hydra.coders.Language")
    NAME = hydra.core.Name("name")
    CONSTRAINTS = hydra.core.Name("constraints")

@dataclass(frozen=True)
class LanguageConstraints:
    r"""A set of constraints on valid type and term expressions, characterizing a language."""

    elimination_variants: Annotated[frozenset[hydra.variants.EliminationVariant], "All supported elimination variants"]
    literal_variants: Annotated[frozenset[hydra.variants.LiteralVariant], "All supported literal variants"]
    float_types: Annotated[frozenset[hydra.core.FloatType], "All supported float types"]
    function_variants: Annotated[frozenset[hydra.variants.FunctionVariant], "All supported function variants"]
    integer_types: Annotated[frozenset[hydra.core.IntegerType], "All supported integer types"]
    term_variants: Annotated[frozenset[hydra.variants.TermVariant], "All supported term variants"]
    type_variants: Annotated[frozenset[hydra.variants.TypeVariant], "All supported type variants"]
    types: Annotated[Callable[[hydra.core.Type], bool], "A logical set of types, as a predicate which tests a type for inclusion"]

    TYPE_ = hydra.core.Name("hydra.coders.LanguageConstraints")
    ELIMINATION_VARIANTS = hydra.core.Name("eliminationVariants")
    LITERAL_VARIANTS = hydra.core.Name("literalVariants")
    FLOAT_TYPES = hydra.core.Name("floatTypes")
    FUNCTION_VARIANTS = hydra.core.Name("functionVariants")
    INTEGER_TYPES = hydra.core.Name("integerTypes")
    TERM_VARIANTS = hydra.core.Name("termVariants")
    TYPE_VARIANTS = hydra.core.Name("typeVariants")
    TYPES = hydra.core.Name("types")

class LanguageName(Node[str]):
    r"""The unique name of a language."""

LanguageName.TYPE_ = hydra.core.Name("hydra.coders.LanguageName")

# A bidirectional encoder which maps between the same type and term languages on either side.
SymmetricAdapter: TypeAlias = "Adapter[T, T, V, V]"

class TraversalOrder(Enum):
    r"""Specifies either a pre-order or post-order traversal."""

    PRE = hydra.core.Name("pre")
    r"""Pre-order traversal"""

    POST = hydra.core.Name("post")
    r"""Post-order traversal"""

TraversalOrder.TYPE_ = hydra.core.Name("hydra.coders.TraversalOrder")

# A function which maps a Hydra type to a symmetric adapter between types and terms.
TypeAdapter: TypeAlias = "Callable[[AdapterContext, hydra.core.Type], Either[str, SymmetricAdapter[hydra.core.Type, hydra.core.Term]]]"

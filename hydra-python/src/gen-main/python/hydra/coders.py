# Note: this is an automatically generated file. Do not edit.

r"""Abstractions for paired transformations between languages."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Node
from typing import Annotated, TypeAlias, TypeVar, cast
import hydra.core
import hydra.graph
import hydra.util
import hydra.variants

T = TypeVar("T")
V = TypeVar("V")

@dataclass(frozen=True)
class AdapterContext:
    r"""An evaluation context together with a source language and a target language."""
    
    graph: Annotated[hydra.graph.Graph, "The underlying graph of elements and primitives"]
    language: Annotated[Language, "The language being encoded or decoded"]
    adapters: Annotated[FrozenDict[hydra.core.Name, hydra.util.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], "A map of type names to adapters for those types"]
    
    TYPE_ = hydra.core.Name("hydra.coders.AdapterContext")
    GRAPH = hydra.core.Name("graph")
    LANGUAGE = hydra.core.Name("language")
    ADAPTERS = hydra.core.Name("adapters")

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
SymmetricAdapter: TypeAlias = "hydra.util.Adapter[T, T, V, V]"

class TraversalOrder(Enum):
    r"""Specifies either a pre-order or post-order traversal."""
    
    PRE = hydra.core.Name("pre")
    r"""Pre-order traversal"""
    
    POST = hydra.core.Name("post")
    r"""Post-order traversal"""

TraversalOrder.TYPE_ = hydra.core.Name("hydra.coders.TraversalOrder")

# A function which maps a Hydra type to a symmetric adapter between types and terms.
TypeAdapter: TypeAlias = "Callable[[AdapterContext, hydra.core.Type], Either[str, SymmetricAdapter[hydra.core.Type, hydra.core.Term]]]"

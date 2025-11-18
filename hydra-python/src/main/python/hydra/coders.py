# Note: this is an automatically generated file. Do not edit.

r"""Abstractions for paired transformations between languages."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import FrozenDict, Node
from typing import Annotated, TypeVar
import hydra.compute
import hydra.core
import hydra.graph
import hydra.variants

S = TypeVar("S")
T = TypeVar("T")
V = TypeVar("V")

@dataclass
class AdapterContext:
    r"""An evaluation context together with a source language and a target language."""
    
    graph: Annotated[hydra.graph.Graph, "The underlying graph of elements and primitives"]
    language: Annotated[Language, "The language being encoded or decoded"]
    adapters: Annotated[FrozenDict[hydra.core.Name, hydra.compute.Adapter[AdapterContext, AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], "A map of type names to adapters for those types"]

ADAPTER_CONTEXT__NAME = hydra.core.Name("hydra.coders.AdapterContext")
ADAPTER_CONTEXT__GRAPH__NAME = hydra.core.Name("graph")
ADAPTER_CONTEXT__LANGUAGE__NAME = hydra.core.Name("language")
ADAPTER_CONTEXT__ADAPTERS__NAME = hydra.core.Name("adapters")

class CoderDirection(Enum):
    r"""Indicates either the 'out' or the 'in' direction of a coder."""
    
    ENCODE = "encode"
    
    DECODE = "decode"

CODER_DIRECTION__NAME = hydra.core.Name("hydra.coders.CoderDirection")
CODER_DIRECTION__ENCODE__NAME = hydra.core.Name("encode")
CODER_DIRECTION__DECODE__NAME = hydra.core.Name("decode")

@dataclass
class Language:
    r"""A named language together with language-specific constraints."""
    
    name: Annotated[LanguageName, "The unique name of the language"]
    constraints: Annotated[LanguageConstraints, "The constraints which characterize the language"]

LANGUAGE__NAME = hydra.core.Name("hydra.coders.Language")
LANGUAGE__NAME__NAME = hydra.core.Name("name")
LANGUAGE__CONSTRAINTS__NAME = hydra.core.Name("constraints")

@dataclass
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

LANGUAGE_CONSTRAINTS__NAME = hydra.core.Name("hydra.coders.LanguageConstraints")
LANGUAGE_CONSTRAINTS__ELIMINATION_VARIANTS__NAME = hydra.core.Name("eliminationVariants")
LANGUAGE_CONSTRAINTS__LITERAL_VARIANTS__NAME = hydra.core.Name("literalVariants")
LANGUAGE_CONSTRAINTS__FLOAT_TYPES__NAME = hydra.core.Name("floatTypes")
LANGUAGE_CONSTRAINTS__FUNCTION_VARIANTS__NAME = hydra.core.Name("functionVariants")
LANGUAGE_CONSTRAINTS__INTEGER_TYPES__NAME = hydra.core.Name("integerTypes")
LANGUAGE_CONSTRAINTS__TERM_VARIANTS__NAME = hydra.core.Name("termVariants")
LANGUAGE_CONSTRAINTS__TYPE_VARIANTS__NAME = hydra.core.Name("typeVariants")
LANGUAGE_CONSTRAINTS__TYPES__NAME = hydra.core.Name("types")

class LanguageName(Node[str]):
    r"""The unique name of a language."""

LANGUAGE_NAME__NAME = hydra.core.Name("hydra.coders.LanguageName")

# A bidirectional encoder which maps between the same type and term languages on either side.
type SymmetricAdapter[S, T, V] = hydra.compute.Adapter[S, S, T, T, V, V]

SYMMETRIC_ADAPTER__NAME = hydra.core.Name("hydra.coders.SymmetricAdapter")

class TraversalOrder(Enum):
    r"""Specifies either a pre-order or post-order traversal."""
    
    PRE = "pre"
    r"""Pre-order traversal."""
    
    POST = "post"
    r"""Post-order traversal."""

TRAVERSAL_ORDER__NAME = hydra.core.Name("hydra.coders.TraversalOrder")
TRAVERSAL_ORDER__PRE__NAME = hydra.core.Name("pre")
TRAVERSAL_ORDER__POST__NAME = hydra.core.Name("post")

# A function which maps a Hydra type to a symmetric adapter between types and terms.
type TypeAdapter = Callable[[hydra.core.Type], hydra.compute.Flow[AdapterContext, SymmetricAdapter[AdapterContext, hydra.core.Type, hydra.core.Term]]]

TYPE_ADAPTER__NAME = hydra.core.Name("hydra.coders.TypeAdapter")

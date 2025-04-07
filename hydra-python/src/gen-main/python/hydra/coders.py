"""Abstractions for paired transformations between languages."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import FrozenDict, Node
from typing import Annotated
import hydra.compute
import hydra.core
import hydra.graph
import hydra.mantle

@dataclass
class AdapterContext:
    """An evaluation context together with a source language and a target language."""
    
    graph: hydra.graph.Graph
    language: Language
    adapters: FrozenDict[hydra.core.Name, hydra.compute.Adapter[AdapterContext, AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]

ADAPTER_CONTEXT__NAME = hydra.core.Name("hydra.coders.AdapterContext")
ADAPTER_CONTEXT__GRAPH__NAME = hydra.core.Name("graph")
ADAPTER_CONTEXT__LANGUAGE__NAME = hydra.core.Name("language")
ADAPTER_CONTEXT__ADAPTERS__NAME = hydra.core.Name("adapters")

class CoderDirection(Enum):
    """Indicates either the 'out' or the 'in' direction of a coder."""
    
    ENCODE = "encode"
    
    DECODE = "decode"

CODER_DIRECTION__NAME = hydra.core.Name("hydra.coders.CoderDirection")
CODER_DIRECTION__ENCODE__NAME = hydra.core.Name("encode")
CODER_DIRECTION__DECODE__NAME = hydra.core.Name("decode")

@dataclass
class Language:
    """A named language together with language-specific constraints."""
    
    name: LanguageName
    constraints: LanguageConstraints

LANGUAGE__NAME = hydra.core.Name("hydra.coders.Language")
LANGUAGE__NAME__NAME = hydra.core.Name("name")
LANGUAGE__CONSTRAINTS__NAME = hydra.core.Name("constraints")

@dataclass
class LanguageConstraints:
    """A set of constraints on valid type and term expressions, characterizing a language."""
    
    elimination_variants: Annotated[frozenset[hydra.mantle.EliminationVariant], "All supported elimination variants"]
    literal_variants: Annotated[frozenset[hydra.mantle.LiteralVariant], "All supported literal variants"]
    float_types: Annotated[frozenset[hydra.core.FloatType], "All supported float types"]
    function_variants: Annotated[frozenset[hydra.mantle.FunctionVariant], "All supported function variants"]
    integer_types: Annotated[frozenset[hydra.core.IntegerType], "All supported integer types"]
    term_variants: Annotated[frozenset[hydra.mantle.TermVariant], "All supported term variants"]
    type_variants: Annotated[frozenset[hydra.mantle.TypeVariant], "All supported type variants"]
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
    """The unique name of a language."""

LANGUAGE_NAME__NAME = hydra.core.Name("hydra.coders.LanguageName")

class TraversalOrder(Enum):
    """Specifies either a pre-order or post-order traversal."""
    
    PRE = "pre"
    """Pre-order traversal."""
    
    POST = "post"
    """Post-order traversal."""

TRAVERSAL_ORDER__NAME = hydra.core.Name("hydra.coders.TraversalOrder")
TRAVERSAL_ORDER__PRE__NAME = hydra.core.Name("pre")
TRAVERSAL_ORDER__POST__NAME = hydra.core.Name("post")

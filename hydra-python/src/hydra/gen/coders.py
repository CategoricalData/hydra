"""Abstractions for paired transformations between languages."""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from typing import Annotated

import hydra.gen.compute
import hydra.gen.core
import hydra.gen.graph
import hydra.gen.mantle
from hydra.dsl.python import FrozenDict, Node


@dataclass
class AdapterContext:
    """An evaluation context together with a source language and a target language."""
    
    graph: hydra.gen.graph.Graph
    language: Language
    adapters: FrozenDict[hydra.gen.core.Name, hydra.gen.compute.Adapter[AdapterContext, AdapterContext, hydra.gen.core.Type, hydra.gen.core.Type, hydra.gen.core.Term, hydra.gen.core.Term]]

ADAPTER_CONTEXT__NAME = hydra.gen.core.Name("hydra.coders.AdapterContext")
ADAPTER_CONTEXT__GRAPH__NAME = hydra.gen.core.Name("graph")
ADAPTER_CONTEXT__LANGUAGE__NAME = hydra.gen.core.Name("language")
ADAPTER_CONTEXT__ADAPTERS__NAME = hydra.gen.core.Name("adapters")

class CoderDirection(Enum):
    """Indicates either the 'out' or the 'in' direction of a coder."""
    
    ENCODE = "encode"
    
    DECODE = "decode"

CODER_DIRECTION__NAME = hydra.gen.core.Name("hydra.coders.CoderDirection")
CODER_DIRECTION__ENCODE__NAME = hydra.gen.core.Name("encode")
CODER_DIRECTION__DECODE__NAME = hydra.gen.core.Name("decode")

@dataclass
class Language:
    """A named language together with language-specific constraints."""
    
    name: LanguageName
    constraints: LanguageConstraints

LANGUAGE__NAME = hydra.gen.core.Name("hydra.coders.Language")
LANGUAGE__NAME__NAME = hydra.gen.core.Name("name")
LANGUAGE__CONSTRAINTS__NAME = hydra.gen.core.Name("constraints")

@dataclass
class LanguageConstraints:
    """A set of constraints on valid type and term expressions, characterizing a language."""
    
    elimination_variants: Annotated[frozenset[hydra.gen.mantle.EliminationVariant], "All supported elimination variants"]
    literal_variants: Annotated[frozenset[hydra.gen.mantle.LiteralVariant], "All supported literal variants"]
    float_types: Annotated[frozenset[hydra.gen.core.FloatType], "All supported float types"]
    function_variants: Annotated[frozenset[hydra.gen.mantle.FunctionVariant], "All supported function variants"]
    integer_types: Annotated[frozenset[hydra.gen.core.IntegerType], "All supported integer types"]
    term_variants: Annotated[frozenset[hydra.gen.mantle.TermVariant], "All supported term variants"]
    type_variants: Annotated[frozenset[hydra.gen.mantle.TypeVariant], "All supported type variants"]
    types: Annotated[Callable[[hydra.gen.core.Type], bool], "A logical set of types, as a predicate which tests a type for inclusion"]

LANGUAGE_CONSTRAINTS__NAME = hydra.gen.core.Name("hydra.coders.LanguageConstraints")
LANGUAGE_CONSTRAINTS__ELIMINATION_VARIANTS__NAME = hydra.gen.core.Name("eliminationVariants")
LANGUAGE_CONSTRAINTS__LITERAL_VARIANTS__NAME = hydra.gen.core.Name("literalVariants")
LANGUAGE_CONSTRAINTS__FLOAT_TYPES__NAME = hydra.gen.core.Name("floatTypes")
LANGUAGE_CONSTRAINTS__FUNCTION_VARIANTS__NAME = hydra.gen.core.Name("functionVariants")
LANGUAGE_CONSTRAINTS__INTEGER_TYPES__NAME = hydra.gen.core.Name("integerTypes")
LANGUAGE_CONSTRAINTS__TERM_VARIANTS__NAME = hydra.gen.core.Name("termVariants")
LANGUAGE_CONSTRAINTS__TYPE_VARIANTS__NAME = hydra.gen.core.Name("typeVariants")
LANGUAGE_CONSTRAINTS__TYPES__NAME = hydra.gen.core.Name("types")

class LanguageName(Node[str]):
    """The unique name of a language."""

LANGUAGE_NAME__NAME = hydra.gen.core.Name("hydra.coders.LanguageName")

class TraversalOrder(Enum):
    """Specifies either a pre-order or post-order traversal."""
    
    PRE = "pre"
    """Pre-order traversal."""
    
    POST = "post"
    """Post-order traversal."""

TRAVERSAL_ORDER__NAME = hydra.gen.core.Name("hydra.coders.TraversalOrder")
TRAVERSAL_ORDER__PRE__NAME = hydra.gen.core.Name("pre")
TRAVERSAL_ORDER__POST__NAME = hydra.gen.core.Name("post")

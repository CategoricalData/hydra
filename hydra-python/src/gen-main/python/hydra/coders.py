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

class CoderDirection(Enum):
    """Indicates either the 'out' or the 'in' direction of a coder."""
    
    ENCODE = "encode"
    
    DECODE = "decode"

@dataclass
class Language:
    """A named language together with language-specific constraints."""
    
    name: LanguageName
    constraints: LanguageConstraints

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

class LanguageName(Node[str]):
    """The unique name of a language."""

class TraversalOrder(Enum):
    """Specifies either a pre-order or post-order traversal."""
    
    PRE = "pre"
    """Pre-order traversal."""
    
    POST = "post"
    """Post-order traversal."""

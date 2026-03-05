# Note: this is an automatically generated file. Do not edit.

r"""A common API for BNF-based grammars, specifying context-free languages."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

class Constant(Node[str]):
    r"""A constant pattern."""

Constant.TYPE_ = hydra.core.Name("hydra.grammar.Constant")

class Grammar(Node["frozenlist[Production]"]):
    r"""An enhanced Backus-Naur form (BNF) grammar."""

Grammar.TYPE_ = hydra.core.Name("hydra.grammar.Grammar")

class Label(Node[str]):
    r"""A name for a pattern."""

Label.TYPE_ = hydra.core.Name("hydra.grammar.Label")

@dataclass(frozen=True)
class LabeledPattern:
    r"""A pattern together with a name (label)."""
    
    label: Annotated[Label, "The label for the pattern"]
    pattern: Annotated[Pattern, "The pattern being labeled"]
    
    TYPE_ = hydra.core.Name("hydra.grammar.LabeledPattern")
    LABEL = hydra.core.Name("label")
    PATTERN = hydra.core.Name("pattern")

class PatternAlternatives(Node["frozenlist[Pattern]"]):
    r"""A choice between alternative patterns"""

class PatternConstant(Node["Constant"]):
    r"""A constant (terminal) pattern"""

class PatternIgnored(Node["Pattern"]):
    r"""A pattern to be ignored (not captured)"""

class PatternLabeled(Node["LabeledPattern"]):
    r"""A labeled pattern"""

class PatternNil:
    r"""An empty pattern"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PatternNil)
    def __hash__(self):
        return hash("PatternNil")

class PatternNonterminal(Node["Symbol"]):
    r"""A nonterminal symbol reference"""

class PatternOption(Node["Pattern"]):
    r"""An optional pattern (zero or one occurrence)"""

class PatternPlus(Node["Pattern"]):
    r"""One or more occurrences of a pattern"""

class PatternRegex(Node["Regex"]):
    r"""A regular expression pattern"""

class PatternSequence(Node["frozenlist[Pattern]"]):
    r"""A sequence of patterns"""

class PatternStar(Node["Pattern"]):
    r"""Zero or more occurrences of a pattern"""

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

# A pattern which matches valid expressions in the language.
class Pattern(metaclass=_PatternMeta):
    r"""PatternAlternatives | PatternConstant | PatternIgnored | PatternLabeled | PatternNil | PatternNonterminal | PatternOption | PatternPlus | PatternRegex | PatternSequence | PatternStar"""
    
    TYPE_ = hydra.core.Name("hydra.grammar.Pattern")
    ALTERNATIVES = hydra.core.Name("alternatives")
    CONSTANT = hydra.core.Name("constant")
    IGNORED = hydra.core.Name("ignored")
    LABELED = hydra.core.Name("labeled")
    NIL = hydra.core.Name("nil")
    NONTERMINAL = hydra.core.Name("nonterminal")
    OPTION = hydra.core.Name("option")
    PLUS = hydra.core.Name("plus")
    REGEX = hydra.core.Name("regex")
    SEQUENCE = hydra.core.Name("sequence")
    STAR = hydra.core.Name("star")

@dataclass(frozen=True)
class Production:
    r"""A BNF production."""
    
    symbol: Annotated[Symbol, "The nonterminal symbol being defined"]
    pattern: Annotated[Pattern, "The pattern which defines the symbol"]
    
    TYPE_ = hydra.core.Name("hydra.grammar.Production")
    SYMBOL = hydra.core.Name("symbol")
    PATTERN = hydra.core.Name("pattern")

class Regex(Node[str]):
    r"""A regular expression."""

Regex.TYPE_ = hydra.core.Name("hydra.grammar.Regex")

class Symbol(Node[str]):
    r"""A nonterminal symbol."""

Symbol.TYPE_ = hydra.core.Name("hydra.grammar.Symbol")

# Note: this is an automatically generated file. Do not edit.

r"""A common API for BNF-based grammars, specifying context-free languages."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias
import hydra.core

class Constant(Node[str]):
    r"""A constant pattern."""

CONSTANT__NAME = hydra.core.Name("hydra.grammar.Constant")

class Grammar(Node["frozenlist[Production]"]):
    r"""An enhanced Backus-Naur form (BNF) grammar."""

GRAMMAR__NAME = hydra.core.Name("hydra.grammar.Grammar")

class Label(Node[str]):
    r"""A name for a pattern."""

LABEL__NAME = hydra.core.Name("hydra.grammar.Label")

@dataclass(frozen=True)
class LabeledPattern:
    r"""A pattern together with a name (label)."""
    
    label: Annotated[Label, "The label for the pattern"]
    pattern: Annotated[Pattern, "The pattern being labeled"]

LABELED_PATTERN__NAME = hydra.core.Name("hydra.grammar.LabeledPattern")
LABELED_PATTERN__LABEL__NAME = hydra.core.Name("label")
LABELED_PATTERN__PATTERN__NAME = hydra.core.Name("pattern")

class PatternAlternatives(Node["frozenlist[Pattern]"]):
    r"""A choice between alternative patterns."""

class PatternConstant(Node["Constant"]):
    r"""A constant (terminal) pattern."""

class PatternIgnored(Node["Pattern"]):
    r"""A pattern to be ignored (not captured)."""

class PatternLabeled(Node["LabeledPattern"]):
    r"""A labeled pattern."""

class PatternNil:
    r"""An empty pattern."""

class PatternNonterminal(Node["Symbol"]):
    r"""A nonterminal symbol reference."""

class PatternOption(Node["Pattern"]):
    r"""An optional pattern (zero or one occurrence)."""

class PatternPlus(Node["Pattern"]):
    r"""One or more occurrences of a pattern."""

class PatternRegex(Node["Regex"]):
    r"""A regular expression pattern."""

class PatternSequence(Node["frozenlist[Pattern]"]):
    r"""A sequence of patterns."""

class PatternStar(Node["Pattern"]):
    r"""Zero or more occurrences of a pattern."""

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

# A pattern which matches valid expressions in the language.
class Pattern(metaclass=_PatternMeta):
    r"""PatternAlternatives | PatternConstant | PatternIgnored | PatternLabeled | PatternNil | PatternNonterminal | PatternOption | PatternPlus | PatternRegex | PatternSequence | PatternStar"""
    
    pass

PATTERN__NAME = hydra.core.Name("hydra.grammar.Pattern")
PATTERN__ALTERNATIVES__NAME = hydra.core.Name("alternatives")
PATTERN__CONSTANT__NAME = hydra.core.Name("constant")
PATTERN__IGNORED__NAME = hydra.core.Name("ignored")
PATTERN__LABELED__NAME = hydra.core.Name("labeled")
PATTERN__NIL__NAME = hydra.core.Name("nil")
PATTERN__NONTERMINAL__NAME = hydra.core.Name("nonterminal")
PATTERN__OPTION__NAME = hydra.core.Name("option")
PATTERN__PLUS__NAME = hydra.core.Name("plus")
PATTERN__REGEX__NAME = hydra.core.Name("regex")
PATTERN__SEQUENCE__NAME = hydra.core.Name("sequence")
PATTERN__STAR__NAME = hydra.core.Name("star")

@dataclass(frozen=True)
class Production:
    r"""A BNF production."""
    
    symbol: Annotated[Symbol, "The nonterminal symbol being defined"]
    pattern: Annotated[Pattern, "The pattern which defines the symbol"]

PRODUCTION__NAME = hydra.core.Name("hydra.grammar.Production")
PRODUCTION__SYMBOL__NAME = hydra.core.Name("symbol")
PRODUCTION__PATTERN__NAME = hydra.core.Name("pattern")

class Regex(Node[str]):
    r"""A regular expression."""

REGEX__NAME = hydra.core.Name("hydra.grammar.Regex")

class Symbol(Node[str]):
    r"""A nonterminal symbol."""

SYMBOL__NAME = hydra.core.Name("hydra.grammar.Symbol")

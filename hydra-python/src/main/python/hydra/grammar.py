# Note: this is an automatically generated file. Do not edit.

r"""A common API for BNF-based grammars, specifying context-free languages."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Node, frozenlist
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

@dataclass
class LabeledPattern:
    r"""A pattern together with a name (label)."""
    
    label: Label
    pattern: Pattern

LABELED_PATTERN__NAME = hydra.core.Name("hydra.grammar.LabeledPattern")
LABELED_PATTERN__LABEL__NAME = hydra.core.Name("label")
LABELED_PATTERN__PATTERN__NAME = hydra.core.Name("pattern")

class PatternAlternatives(Node["frozenlist[Pattern]"]): ...

class PatternConstant(Node["Constant"]): ...

class PatternIgnored(Node["Pattern"]): ...

class PatternLabeled(Node["LabeledPattern"]): ...

class PatternNil: ...

class PatternNonterminal(Node["Symbol"]): ...

class PatternOption(Node["Pattern"]): ...

class PatternPlus(Node["Pattern"]): ...

class PatternRegex(Node["Regex"]): ...

class PatternSequence(Node["frozenlist[Pattern]"]): ...

class PatternStar(Node["Pattern"]): ...

# A pattern which matches valid expressions in the language.
type Pattern = PatternAlternatives | PatternConstant | PatternIgnored | PatternLabeled | PatternNil | PatternNonterminal | PatternOption | PatternPlus | PatternRegex | PatternSequence | PatternStar

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

@dataclass
class Production:
    r"""A BNF production."""
    
    symbol: Symbol
    pattern: Pattern

PRODUCTION__NAME = hydra.core.Name("hydra.grammar.Production")
PRODUCTION__SYMBOL__NAME = hydra.core.Name("symbol")
PRODUCTION__PATTERN__NAME = hydra.core.Name("pattern")

class Regex(Node[str]):
    r"""A regular expression."""

REGEX__NAME = hydra.core.Name("hydra.grammar.Regex")

class Symbol(Node[str]):
    r"""A nonterminal symbol."""

SYMBOL__NAME = hydra.core.Name("hydra.grammar.Symbol")

"""A common API for BNF-based grammars, specifying context-free languages."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import frozenlist, Node
import hydra.core

class Constant(Node[str]):
    """A constant pattern."""

CONSTANT__NAME = hydra.core.Name("hydra.grammar.Constant")

# An enhanced Backus-Naur form (BNF) grammar.
type Grammar = frozenlist[Production]

GRAMMAR__NAME = hydra.core.Name("hydra.grammar.Grammar")

class Label(Node[str]):
    """A name for a pattern."""

LABEL__NAME = hydra.core.Name("hydra.grammar.Label")

@dataclass
class LabeledPattern:
    """A pattern together with a name (label)."""
    
    label: Label
    pattern: Pattern

LABELED_PATTERN__NAME = hydra.core.Name("hydra.grammar.LabeledPattern")
LABELED_PATTERN__LABEL__NAME = hydra.core.Name("label")
LABELED_PATTERN__PATTERN__NAME = hydra.core.Name("pattern")

class PatternNil(Node[None]): ...

class PatternIgnored(Node["Pattern"]): ...

class PatternLabeled(Node["LabeledPattern"]): ...

class PatternConstant(Node["Constant"]): ...

class PatternRegex(Node["Regex"]): ...

class PatternNonterminal(Node["Symbol"]): ...

class PatternSequence(Node["frozenlist[Pattern]"]): ...

class PatternAlternatives(Node["frozenlist[Pattern]"]): ...

class PatternOption(Node["Pattern"]): ...

class PatternStar(Node["Pattern"]): ...

class PatternPlus(Node["Pattern"]): ...

# A pattern which matches valid expressions in the language.
type Pattern = PatternNil | PatternIgnored | PatternLabeled | PatternConstant | PatternRegex | PatternNonterminal | PatternSequence | PatternAlternatives | PatternOption | PatternStar | PatternPlus

PATTERN__NAME = hydra.core.Name("hydra.grammar.Pattern")
PATTERN__NIL__NAME = hydra.core.Name("nil")
PATTERN__IGNORED__NAME = hydra.core.Name("ignored")
PATTERN__LABELED__NAME = hydra.core.Name("labeled")
PATTERN__CONSTANT__NAME = hydra.core.Name("constant")
PATTERN__REGEX__NAME = hydra.core.Name("regex")
PATTERN__NONTERMINAL__NAME = hydra.core.Name("nonterminal")
PATTERN__SEQUENCE__NAME = hydra.core.Name("sequence")
PATTERN__ALTERNATIVES__NAME = hydra.core.Name("alternatives")
PATTERN__OPTION__NAME = hydra.core.Name("option")
PATTERN__STAR__NAME = hydra.core.Name("star")
PATTERN__PLUS__NAME = hydra.core.Name("plus")

@dataclass
class Production:
    """A BNF production."""
    
    symbol: Symbol
    pattern: Pattern

PRODUCTION__NAME = hydra.core.Name("hydra.grammar.Production")
PRODUCTION__SYMBOL__NAME = hydra.core.Name("symbol")
PRODUCTION__PATTERN__NAME = hydra.core.Name("pattern")

class Regex(Node[str]):
    """A regular expression."""

REGEX__NAME = hydra.core.Name("hydra.grammar.Regex")

class Symbol(Node[str]):
    """A nonterminal symbol."""

SYMBOL__NAME = hydra.core.Name("hydra.grammar.Symbol")

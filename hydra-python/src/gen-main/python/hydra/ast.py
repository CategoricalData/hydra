# Note: this is an automatically generated file. Do not edit.

"""A model which provides a common syntax tree for Hydra serializers."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Node
import hydra.core

class Associativity(Enum):
    """Operator associativity."""
    
    NONE = "none"
    
    LEFT = "left"
    
    RIGHT = "right"
    
    BOTH = "both"

ASSOCIATIVITY__NAME = hydra.core.Name("hydra.ast.Associativity")
ASSOCIATIVITY__NONE__NAME = hydra.core.Name("none")
ASSOCIATIVITY__LEFT__NAME = hydra.core.Name("left")
ASSOCIATIVITY__RIGHT__NAME = hydra.core.Name("right")
ASSOCIATIVITY__BOTH__NAME = hydra.core.Name("both")

@dataclass
class BlockStyle:
    """Formatting option for code blocks."""
    
    indent: str | None
    newline_before_content: bool
    newline_after_content: bool

BLOCK_STYLE__NAME = hydra.core.Name("hydra.ast.BlockStyle")
BLOCK_STYLE__INDENT__NAME = hydra.core.Name("indent")
BLOCK_STYLE__NEWLINE_BEFORE_CONTENT__NAME = hydra.core.Name("newlineBeforeContent")
BLOCK_STYLE__NEWLINE_AFTER_CONTENT__NAME = hydra.core.Name("newlineAfterContent")

@dataclass
class BracketExpr:
    """An expression enclosed by brackets."""
    
    brackets: Brackets
    enclosed: Expr
    style: BlockStyle

BRACKET_EXPR__NAME = hydra.core.Name("hydra.ast.BracketExpr")
BRACKET_EXPR__BRACKETS__NAME = hydra.core.Name("brackets")
BRACKET_EXPR__ENCLOSED__NAME = hydra.core.Name("enclosed")
BRACKET_EXPR__STYLE__NAME = hydra.core.Name("style")

@dataclass
class Brackets:
    """Matching open and close bracket symbols."""
    
    open: Symbol
    close: Symbol

BRACKETS__NAME = hydra.core.Name("hydra.ast.Brackets")
BRACKETS__OPEN__NAME = hydra.core.Name("open")
BRACKETS__CLOSE__NAME = hydra.core.Name("close")

class ExprConst(Node["Symbol"]): ...

class ExprIndent(Node["IndentedExpression"]): ...

class ExprOp(Node["OpExpr"]): ...

class ExprBrackets(Node["BracketExpr"]): ...

# An abstract expression.
type Expr = ExprConst | ExprIndent | ExprOp | ExprBrackets

EXPR__NAME = hydra.core.Name("hydra.ast.Expr")
EXPR__CONST__NAME = hydra.core.Name("const")
EXPR__INDENT__NAME = hydra.core.Name("indent")
EXPR__OP__NAME = hydra.core.Name("op")
EXPR__BRACKETS__NAME = hydra.core.Name("brackets")

@dataclass
class IndentedExpression:
    """An expression indented in a certain style."""
    
    style: IndentStyle
    expr: Expr

INDENTED_EXPRESSION__NAME = hydra.core.Name("hydra.ast.IndentedExpression")
INDENTED_EXPRESSION__STYLE__NAME = hydra.core.Name("style")
INDENTED_EXPRESSION__EXPR__NAME = hydra.core.Name("expr")

class IndentStyleAllLines(Node[str]): ...

class IndentStyleSubsequentLines(Node[str]): ...

# Any of several indentation styles.
type IndentStyle = IndentStyleAllLines | IndentStyleSubsequentLines

INDENT_STYLE__NAME = hydra.core.Name("hydra.ast.IndentStyle")
INDENT_STYLE__ALL_LINES__NAME = hydra.core.Name("allLines")
INDENT_STYLE__SUBSEQUENT_LINES__NAME = hydra.core.Name("subsequentLines")

@dataclass
class Op:
    """An operator symbol."""
    
    symbol: Symbol
    padding: Padding
    precedence: Precedence
    associativity: Associativity

OP__NAME = hydra.core.Name("hydra.ast.Op")
OP__SYMBOL__NAME = hydra.core.Name("symbol")
OP__PADDING__NAME = hydra.core.Name("padding")
OP__PRECEDENCE__NAME = hydra.core.Name("precedence")
OP__ASSOCIATIVITY__NAME = hydra.core.Name("associativity")

@dataclass
class OpExpr:
    """An operator expression."""
    
    op: Op
    lhs: Expr
    rhs: Expr

OP_EXPR__NAME = hydra.core.Name("hydra.ast.OpExpr")
OP_EXPR__OP__NAME = hydra.core.Name("op")
OP_EXPR__LHS__NAME = hydra.core.Name("lhs")
OP_EXPR__RHS__NAME = hydra.core.Name("rhs")

@dataclass
class Padding:
    """Left and right padding for an operator."""
    
    left: Ws
    right: Ws

PADDING__NAME = hydra.core.Name("hydra.ast.Padding")
PADDING__LEFT__NAME = hydra.core.Name("left")
PADDING__RIGHT__NAME = hydra.core.Name("right")

class Precedence(Node[int]):
    """Operator precedence."""

PRECEDENCE__NAME = hydra.core.Name("hydra.ast.Precedence")

class Symbol(Node[str]):
    """Any symbol."""

SYMBOL__NAME = hydra.core.Name("hydra.ast.Symbol")

class WsNone(Node[None]): ...

class WsSpace(Node[None]): ...

class WsBreak(Node[None]): ...

class WsBreakAndIndent(Node[str]): ...

class WsDoubleBreak(Node[None]): ...

# One of several classes of whitespace.
type Ws = WsNone | WsSpace | WsBreak | WsBreakAndIndent | WsDoubleBreak

WS__NAME = hydra.core.Name("hydra.ast.Ws")
WS__NONE__NAME = hydra.core.Name("none")
WS__SPACE__NAME = hydra.core.Name("space")
WS__BREAK__NAME = hydra.core.Name("break")
WS__BREAK_AND_INDENT__NAME = hydra.core.Name("breakAndIndent")
WS__DOUBLE_BREAK__NAME = hydra.core.Name("doubleBreak")

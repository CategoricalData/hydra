# Note: this is an automatically generated file. Do not edit.

r"""A model which provides a common syntax tree for Hydra serializers."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Maybe, Node
from typing import Annotated, TypeAlias
import hydra.core

class Associativity(Enum):
    r"""Operator associativity."""
    
    NONE = "none"
    
    LEFT = "left"
    
    RIGHT = "right"
    
    BOTH = "both"

ASSOCIATIVITY__NAME = hydra.core.Name("hydra.ast.Associativity")
ASSOCIATIVITY__NONE__NAME = hydra.core.Name("none")
ASSOCIATIVITY__LEFT__NAME = hydra.core.Name("left")
ASSOCIATIVITY__RIGHT__NAME = hydra.core.Name("right")
ASSOCIATIVITY__BOTH__NAME = hydra.core.Name("both")

@dataclass(frozen=True)
class BlockStyle:
    r"""Formatting option for code blocks."""
    
    indent: Annotated[Maybe[str], "An optional indentation string"]
    newline_before_content: Annotated[bool, "Whether to place a newline before the content"]
    newline_after_content: Annotated[bool, "Whether to place a newline after the content"]

BLOCK_STYLE__NAME = hydra.core.Name("hydra.ast.BlockStyle")
BLOCK_STYLE__INDENT__NAME = hydra.core.Name("indent")
BLOCK_STYLE__NEWLINE_BEFORE_CONTENT__NAME = hydra.core.Name("newlineBeforeContent")
BLOCK_STYLE__NEWLINE_AFTER_CONTENT__NAME = hydra.core.Name("newlineAfterContent")

@dataclass(frozen=True)
class BracketExpr:
    r"""An expression enclosed by brackets."""
    
    brackets: Annotated[Brackets, "The bracket pair enclosing the expression"]
    enclosed: Annotated[Expr, "The expression within the brackets"]
    style: Annotated[BlockStyle, "The formatting style for the bracketed block"]

BRACKET_EXPR__NAME = hydra.core.Name("hydra.ast.BracketExpr")
BRACKET_EXPR__BRACKETS__NAME = hydra.core.Name("brackets")
BRACKET_EXPR__ENCLOSED__NAME = hydra.core.Name("enclosed")
BRACKET_EXPR__STYLE__NAME = hydra.core.Name("style")

@dataclass(frozen=True)
class Brackets:
    r"""Matching open and close bracket symbols."""
    
    open: Annotated[Symbol, "The opening bracket symbol"]
    close: Annotated[Symbol, "The closing bracket symbol"]

BRACKETS__NAME = hydra.core.Name("hydra.ast.Brackets")
BRACKETS__OPEN__NAME = hydra.core.Name("open")
BRACKETS__CLOSE__NAME = hydra.core.Name("close")

class ExprConst(Node["Symbol"]):
    r"""A constant symbol."""

class ExprIndent(Node["IndentedExpression"]):
    r"""An indented expression."""

class ExprOp(Node["OpExpr"]):
    r"""An operator expression."""

class ExprBrackets(Node["BracketExpr"]):
    r"""A bracketed expression."""

# An abstract expression.
Expr: TypeAlias = "ExprConst | ExprIndent | ExprOp | ExprBrackets"

EXPR__NAME = hydra.core.Name("hydra.ast.Expr")
EXPR__CONST__NAME = hydra.core.Name("const")
EXPR__INDENT__NAME = hydra.core.Name("indent")
EXPR__OP__NAME = hydra.core.Name("op")
EXPR__BRACKETS__NAME = hydra.core.Name("brackets")

@dataclass(frozen=True)
class IndentedExpression:
    r"""An expression indented in a certain style."""
    
    style: Annotated[IndentStyle, "The indentation style"]
    expr: Annotated[Expr, "The expression to be indented"]

INDENTED_EXPRESSION__NAME = hydra.core.Name("hydra.ast.IndentedExpression")
INDENTED_EXPRESSION__STYLE__NAME = hydra.core.Name("style")
INDENTED_EXPRESSION__EXPR__NAME = hydra.core.Name("expr")

class IndentStyleAllLines(Node[str]):
    r"""Indent all lines with the given string."""

class IndentStyleSubsequentLines(Node[str]):
    r"""Indent only lines after the first with the given string."""

# Any of several indentation styles.
IndentStyle: TypeAlias = "IndentStyleAllLines | IndentStyleSubsequentLines"

INDENT_STYLE__NAME = hydra.core.Name("hydra.ast.IndentStyle")
INDENT_STYLE__ALL_LINES__NAME = hydra.core.Name("allLines")
INDENT_STYLE__SUBSEQUENT_LINES__NAME = hydra.core.Name("subsequentLines")

@dataclass(frozen=True)
class Op:
    r"""An operator symbol."""
    
    symbol: Annotated[Symbol, "The operator symbol"]
    padding: Annotated[Padding, "The padding around the operator"]
    precedence: Annotated[Precedence, "The precedence of the operator"]
    associativity: Annotated[Associativity, "The associativity of the operator"]

OP__NAME = hydra.core.Name("hydra.ast.Op")
OP__SYMBOL__NAME = hydra.core.Name("symbol")
OP__PADDING__NAME = hydra.core.Name("padding")
OP__PRECEDENCE__NAME = hydra.core.Name("precedence")
OP__ASSOCIATIVITY__NAME = hydra.core.Name("associativity")

@dataclass(frozen=True)
class OpExpr:
    r"""An operator expression."""
    
    op: Annotated[Op, "The operator"]
    lhs: Annotated[Expr, "The left-hand side operand"]
    rhs: Annotated[Expr, "The right-hand side operand"]

OP_EXPR__NAME = hydra.core.Name("hydra.ast.OpExpr")
OP_EXPR__OP__NAME = hydra.core.Name("op")
OP_EXPR__LHS__NAME = hydra.core.Name("lhs")
OP_EXPR__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Padding:
    r"""Left and right padding for an operator."""
    
    left: Annotated[Ws, "Padding to the left of the operator"]
    right: Annotated[Ws, "Padding to the right of the operator"]

PADDING__NAME = hydra.core.Name("hydra.ast.Padding")
PADDING__LEFT__NAME = hydra.core.Name("left")
PADDING__RIGHT__NAME = hydra.core.Name("right")

class Precedence(Node[int]):
    r"""Operator precedence."""

PRECEDENCE__NAME = hydra.core.Name("hydra.ast.Precedence")

class Symbol(Node[str]):
    r"""Any symbol."""

SYMBOL__NAME = hydra.core.Name("hydra.ast.Symbol")

class WsNone:
    r"""No whitespace."""

class WsSpace:
    r"""A single space."""

class WsBreak:
    r"""A line break."""

class WsBreakAndIndent(Node[str]):
    r"""A line break followed by indentation."""

class WsDoubleBreak:
    r"""Two line breaks."""

# One of several classes of whitespace.
Ws: TypeAlias = "WsNone | WsSpace | WsBreak | WsBreakAndIndent | WsDoubleBreak"

WS__NAME = hydra.core.Name("hydra.ast.Ws")
WS__NONE__NAME = hydra.core.Name("none")
WS__SPACE__NAME = hydra.core.Name("space")
WS__BREAK__NAME = hydra.core.Name("break")
WS__BREAK_AND_INDENT__NAME = hydra.core.Name("breakAndIndent")
WS__DOUBLE_BREAK__NAME = hydra.core.Name("doubleBreak")

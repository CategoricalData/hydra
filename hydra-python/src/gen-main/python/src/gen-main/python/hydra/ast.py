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
    
    NONE = hydra.core.Name("none")
    
    LEFT = hydra.core.Name("left")
    
    RIGHT = hydra.core.Name("right")
    
    BOTH = hydra.core.Name("both")

Associativity.TYPE_ = hydra.core.Name("hydra.ast.Associativity")

@dataclass(frozen=True)
class BlockStyle:
    r"""Formatting option for code blocks."""
    
    indent: Annotated[Maybe[str], "An optional indentation string"]
    newline_before_content: Annotated[bool, "Whether to place a newline before the content"]
    newline_after_content: Annotated[bool, "Whether to place a newline after the content"]
    
    TYPE_ = hydra.core.Name("hydra.ast.BlockStyle")
    INDENT = hydra.core.Name("indent")
    NEWLINE_BEFORE_CONTENT = hydra.core.Name("newlineBeforeContent")
    NEWLINE_AFTER_CONTENT = hydra.core.Name("newlineAfterContent")

@dataclass(frozen=True)
class BracketExpr:
    r"""An expression enclosed by brackets."""
    
    brackets: Annotated[Brackets, "The bracket pair enclosing the expression"]
    enclosed: Annotated[Expr, "The expression within the brackets"]
    style: Annotated[BlockStyle, "The formatting style for the bracketed block"]
    
    TYPE_ = hydra.core.Name("hydra.ast.BracketExpr")
    BRACKETS = hydra.core.Name("brackets")
    ENCLOSED = hydra.core.Name("enclosed")
    STYLE = hydra.core.Name("style")

@dataclass(frozen=True)
class Brackets:
    r"""Matching open and close bracket symbols."""
    
    open: Annotated[Symbol, "The opening bracket symbol"]
    close: Annotated[Symbol, "The closing bracket symbol"]
    
    TYPE_ = hydra.core.Name("hydra.ast.Brackets")
    OPEN = hydra.core.Name("open")
    CLOSE = hydra.core.Name("close")

class ExprConst(Node["Symbol"]):
    r"""A constant symbol"""

class ExprIndent(Node["IndentedExpression"]):
    r"""An indented expression"""

class ExprOp(Node["OpExpr"]):
    r"""An operator expression"""

class ExprBrackets(Node["BracketExpr"]):
    r"""A bracketed expression"""

class _ExprMeta(type):
    def __getitem__(cls, item):
        return object

# An abstract expression.
class Expr(metaclass=_ExprMeta):
    r"""ExprConst | ExprIndent | ExprOp | ExprBrackets"""
    
    TYPE_ = hydra.core.Name("hydra.ast.Expr")
    CONST = hydra.core.Name("const")
    INDENT = hydra.core.Name("indent")
    OP = hydra.core.Name("op")
    BRACKETS = hydra.core.Name("brackets")

@dataclass(frozen=True)
class IndentedExpression:
    r"""An expression indented in a certain style."""
    
    style: Annotated[IndentStyle, "The indentation style"]
    expr: Annotated[Expr, "The expression to be indented"]
    
    TYPE_ = hydra.core.Name("hydra.ast.IndentedExpression")
    STYLE = hydra.core.Name("style")
    EXPR = hydra.core.Name("expr")

class IndentStyleAllLines(Node[str]):
    r"""Indent all lines with the given string"""

class IndentStyleSubsequentLines(Node[str]):
    r"""Indent only lines after the first with the given string"""

class _IndentStyleMeta(type):
    def __getitem__(cls, item):
        return object

# Any of several indentation styles.
class IndentStyle(metaclass=_IndentStyleMeta):
    r"""IndentStyleAllLines | IndentStyleSubsequentLines"""
    
    TYPE_ = hydra.core.Name("hydra.ast.IndentStyle")
    ALL_LINES = hydra.core.Name("allLines")
    SUBSEQUENT_LINES = hydra.core.Name("subsequentLines")

@dataclass(frozen=True)
class Op:
    r"""An operator symbol."""
    
    symbol: Annotated[Symbol, "The operator symbol"]
    padding: Annotated[Padding, "The padding around the operator"]
    precedence: Annotated[Precedence, "The precedence of the operator"]
    associativity: Annotated[Associativity, "The associativity of the operator"]
    
    TYPE_ = hydra.core.Name("hydra.ast.Op")
    SYMBOL = hydra.core.Name("symbol")
    PADDING = hydra.core.Name("padding")
    PRECEDENCE = hydra.core.Name("precedence")
    ASSOCIATIVITY = hydra.core.Name("associativity")

@dataclass(frozen=True)
class OpExpr:
    r"""An operator expression."""
    
    op: Annotated[Op, "The operator"]
    lhs: Annotated[Expr, "The left-hand side operand"]
    rhs: Annotated[Expr, "The right-hand side operand"]
    
    TYPE_ = hydra.core.Name("hydra.ast.OpExpr")
    OP = hydra.core.Name("op")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Padding:
    r"""Left and right padding for an operator."""
    
    left: Annotated[Ws, "Padding to the left of the operator"]
    right: Annotated[Ws, "Padding to the right of the operator"]
    
    TYPE_ = hydra.core.Name("hydra.ast.Padding")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class Precedence(Node[int]):
    r"""Operator precedence."""

Precedence.TYPE_ = hydra.core.Name("hydra.ast.Precedence")

class Symbol(Node[str]):
    r"""Any symbol."""

Symbol.TYPE_ = hydra.core.Name("hydra.ast.Symbol")

class WsNone:
    r"""No whitespace"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, WsNone)
    def __hash__(self):
        return hash("WsNone")

class WsSpace:
    r"""A single space"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, WsSpace)
    def __hash__(self):
        return hash("WsSpace")

class WsBreak:
    r"""A line break"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, WsBreak)
    def __hash__(self):
        return hash("WsBreak")

class WsBreakAndIndent(Node[str]):
    r"""A line break followed by indentation"""

class WsDoubleBreak:
    r"""Two line breaks"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, WsDoubleBreak)
    def __hash__(self):
        return hash("WsDoubleBreak")

class _WsMeta(type):
    def __getitem__(cls, item):
        return object

# One of several classes of whitespace.
class Ws(metaclass=_WsMeta):
    r"""WsNone | WsSpace | WsBreak | WsBreakAndIndent | WsDoubleBreak"""
    
    TYPE_ = hydra.core.Name("hydra.ast.Ws")
    NONE = hydra.core.Name("none")
    SPACE = hydra.core.Name("space")
    BREAK = hydra.core.Name("break")
    BREAK_AND_INDENT = hydra.core.Name("breakAndIndent")
    DOUBLE_BREAK = hydra.core.Name("doubleBreak")

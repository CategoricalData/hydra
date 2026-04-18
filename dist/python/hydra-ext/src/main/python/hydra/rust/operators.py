# Note: this is an automatically generated file. Do not edit.

r"""AST operators for Rust serialization."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.ast
import hydra.core
import hydra.serialization

@lru_cache(1)
def add_assign_op() -> hydra.ast.Op:
    r"""Add-assign operator (+=)."""

    return hydra.serialization.op("+=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def add_op() -> hydra.ast.Op:
    r"""Addition operator (+)."""

    return hydra.serialization.op("+", 10, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def and_op() -> hydra.ast.Op:
    r"""Logical AND operator (&&)."""

    return hydra.serialization.op("&&", 4, hydra.ast.Associativity.LEFT)

# Function application operator (whitespace).
app_op = hydra.ast.Op(hydra.ast.Symbol(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.ast.Precedence(0), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def arrow_op() -> hydra.ast.Op:
    r"""Return type arrow (->)."""

    return hydra.serialization.op("->", 0, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def as_op() -> hydra.ast.Op:
    r"""Type cast operator (as)."""

    return hydra.serialization.op("as", 12, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def assign_op() -> hydra.ast.Op:
    r"""Assignment operator (=)."""

    return hydra.serialization.op("=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def bit_and_assign_op() -> hydra.ast.Op:
    r"""Bitwise and-assign operator (&=)."""

    return hydra.serialization.op("&=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def bit_and_op() -> hydra.ast.Op:
    r"""Bitwise AND operator (&)."""

    return hydra.serialization.op("&", 8, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def bit_or_assign_op() -> hydra.ast.Op:
    r"""Bitwise or-assign operator (|=)."""

    return hydra.serialization.op("|=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def bit_or_op() -> hydra.ast.Op:
    r"""Bitwise OR operator (|)."""

    return hydra.serialization.op("|", 6, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def bit_xor_assign_op() -> hydra.ast.Op:
    r"""Bitwise xor-assign operator (^=)."""

    return hydra.serialization.op("^=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def bit_xor_op() -> hydra.ast.Op:
    r"""Bitwise XOR operator (^)."""

    return hydra.serialization.op("^", 7, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def colon_colon_op() -> hydra.ast.Op:
    r"""Type annotation (::) for let statements."""

    return hydra.serialization.op(":", 0, hydra.ast.Associativity.NONE)

@lru_cache(1)
def colon_op() -> hydra.ast.Op:
    r"""Type ascription operator (:)."""

    return hydra.serialization.op(":", 12, hydra.ast.Associativity.LEFT)

# Dereference operator (*).
deref_op = hydra.ast.Op(hydra.ast.Symbol("*"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(13), hydra.ast.Associativity.NONE)

@lru_cache(1)
def div_assign_op() -> hydra.ast.Op:
    r"""Div-assign operator (/=)."""

    return hydra.serialization.op("/=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def div_op() -> hydra.ast.Op:
    r"""Division operator (/)."""

    return hydra.serialization.op("/", 11, hydra.ast.Associativity.LEFT)

# Path separator (::).
double_colon_op = hydra.ast.Op(hydra.ast.Symbol("::"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(15), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def eq_op() -> hydra.ast.Op:
    r"""Equality operator (==)."""

    return hydra.serialization.op("==", 5, hydra.ast.Associativity.NONE)

@lru_cache(1)
def fat_arrow_op() -> hydra.ast.Op:
    r"""Match arm arrow (=>)."""

    return hydra.serialization.op("=>", 0, hydra.ast.Associativity.NONE)

# Field access operator (.).
field_op = hydra.ast.Op(hydra.ast.Symbol("."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(14), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def ge_op() -> hydra.ast.Op:
    r"""Greater-than-or-equal operator (>=)."""

    return hydra.serialization.op(">=", 5, hydra.ast.Associativity.NONE)

@lru_cache(1)
def gt_op() -> hydra.ast.Op:
    r"""Greater-than operator (>)."""

    return hydra.serialization.op(">", 5, hydra.ast.Associativity.NONE)

@lru_cache(1)
def le_op() -> hydra.ast.Op:
    r"""Less-than-or-equal operator (<=)."""

    return hydra.serialization.op("<=", 5, hydra.ast.Associativity.NONE)

@lru_cache(1)
def lt_op() -> hydra.ast.Op:
    r"""Less-than operator (<)."""

    return hydra.serialization.op("<", 5, hydra.ast.Associativity.NONE)

# Method call operator (.).
method_op = hydra.ast.Op(hydra.ast.Symbol("."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(14), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def mul_assign_op() -> hydra.ast.Op:
    r"""Mul-assign operator (*=)."""

    return hydra.serialization.op("*=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def mul_op() -> hydra.ast.Op:
    r"""Multiplication operator (*)."""

    return hydra.serialization.op("*", 11, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def ne_op() -> hydra.ast.Op:
    r"""Not-equal operator (!=)."""

    return hydra.serialization.op("!=", 5, hydra.ast.Associativity.NONE)

# Unary negation operator (-).
neg_op = hydra.ast.Op(hydra.ast.Symbol("-"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(13), hydra.ast.Associativity.NONE)

# Unary logical not operator (!).
not_op = hydra.ast.Op(hydra.ast.Symbol("!"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(13), hydra.ast.Associativity.NONE)

@lru_cache(1)
def or_op() -> hydra.ast.Op:
    r"""Logical OR operator (||)."""

    return hydra.serialization.op("||", 3, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def range_inclusive_op() -> hydra.ast.Op:
    r"""Inclusive range operator (..=)."""

    return hydra.serialization.op("..=", 2, hydra.ast.Associativity.NONE)

@lru_cache(1)
def range_op() -> hydra.ast.Op:
    r"""Range operator (..)."""

    return hydra.serialization.op("..", 2, hydra.ast.Associativity.NONE)

# Reference operator (&).
ref_op = hydra.ast.Op(hydra.ast.Symbol("&"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(13), hydra.ast.Associativity.NONE)

@lru_cache(1)
def rem_assign_op() -> hydra.ast.Op:
    r"""Rem-assign operator (%=)."""

    return hydra.serialization.op("%=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def rem_op() -> hydra.ast.Op:
    r"""Remainder operator (%)."""

    return hydra.serialization.op("%", 11, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def shl_assign_op() -> hydra.ast.Op:
    r"""Shift-left assign operator (<<=)."""

    return hydra.serialization.op("<<=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def shl_op() -> hydra.ast.Op:
    r"""Shift-left operator (<<)."""

    return hydra.serialization.op("<<", 9, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def shr_assign_op() -> hydra.ast.Op:
    r"""Shift-right assign operator (>>=)."""

    return hydra.serialization.op(">>=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def shr_op() -> hydra.ast.Op:
    r"""Shift-right operator (>>)."""

    return hydra.serialization.op(">>", 9, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def sub_assign_op() -> hydra.ast.Op:
    r"""Sub-assign operator (-=)."""

    return hydra.serialization.op("-=", 1, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def sub_op() -> hydra.ast.Op:
    r"""Subtraction operator (-)."""

    return hydra.serialization.op("-", 10, hydra.ast.Associativity.LEFT)

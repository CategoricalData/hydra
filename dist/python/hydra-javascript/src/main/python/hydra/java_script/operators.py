# Note: this is an automatically generated file. Do not edit.

r"""AST operators for JavaScript with precedence and associativity."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.ast
import hydra.core
import hydra.serialization

@lru_cache(1)
def add_op() -> hydra.ast.Op:
    r"""Addition operator (+)."""

    return hydra.serialization.op("+", 14, hydra.ast.Associativity.LEFT)

# Function application (whitespace).
app_op = hydra.ast.Op(hydra.ast.Symbol(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(20), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def arrow_op() -> hydra.ast.Op:
    r"""Arrow function operator (=>)."""

    return hydra.serialization.op("=>", 2, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def assign_op() -> hydra.ast.Op:
    r"""Assignment operator (=)."""

    return hydra.serialization.op("=", 2, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def bitwise_and_op() -> hydra.ast.Op:
    r"""Bitwise AND operator (&)."""

    return hydra.serialization.op("&", 10, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def bitwise_or_op() -> hydra.ast.Op:
    r"""Bitwise OR operator (|)."""

    return hydra.serialization.op("|", 8, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def bitwise_xor_op() -> hydra.ast.Op:
    r"""Bitwise XOR operator (^)."""

    return hydra.serialization.op("^", 9, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def colon_op() -> hydra.ast.Op:
    r"""Type annotation colon (:)."""

    return hydra.serialization.op(":", 0, hydra.ast.Associativity.NONE)

@lru_cache(1)
def comma_op() -> hydra.ast.Op:
    r"""Comma operator (,)."""

    return hydra.serialization.op(",", 1, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def define_op() -> hydra.ast.Op:
    r"""Definition operator (= in const x = ...)."""

    return hydra.serialization.op("=", 0, hydra.ast.Associativity.NONE)

@lru_cache(1)
def divide_op() -> hydra.ast.Op:
    r"""Division operator (/)."""

    return hydra.serialization.op("/", 15, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def equal_op() -> hydra.ast.Op:
    r"""Equality operator (==)."""

    return hydra.serialization.op("==", 11, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def exponentiate_op() -> hydra.ast.Op:
    r"""Exponentiation operator (**)."""

    return hydra.serialization.op("**", 16, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def greater_than_op() -> hydra.ast.Op:
    r"""Greater than operator (>)."""

    return hydra.serialization.op(">", 12, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def greater_than_or_equal_op() -> hydra.ast.Op:
    r"""Greater than or equal operator (>=)."""

    return hydra.serialization.op(">=", 12, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def in_op() -> hydra.ast.Op:
    r"""In operator (in)."""

    return hydra.serialization.op("in", 12, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def instance_of_op() -> hydra.ast.Op:
    r"""Instance of operator (instanceof)."""

    return hydra.serialization.op("instanceof", 12, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def left_shift_op() -> hydra.ast.Op:
    r"""Left shift operator (<<)."""

    return hydra.serialization.op("<<", 13, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def less_than_op() -> hydra.ast.Op:
    r"""Less than operator (<)."""

    return hydra.serialization.op("<", 12, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def less_than_or_equal_op() -> hydra.ast.Op:
    r"""Less than or equal operator (<=)."""

    return hydra.serialization.op("<=", 12, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def logical_and_op() -> hydra.ast.Op:
    r"""Logical AND operator (&&)."""

    return hydra.serialization.op("&&", 6, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def logical_or_op() -> hydra.ast.Op:
    r"""Logical OR operator (||)."""

    return hydra.serialization.op("||", 5, hydra.ast.Associativity.LEFT)

# Member access operator (.).
member_op = hydra.ast.Op(hydra.ast.Symbol("."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(20), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def modulo_op() -> hydra.ast.Op:
    r"""Modulo operator (%)."""

    return hydra.serialization.op("%", 15, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def multiply_op() -> hydra.ast.Op:
    r"""Multiplication operator (*)."""

    return hydra.serialization.op("*", 15, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def not_equal_op() -> hydra.ast.Op:
    r"""Inequality operator (!=)."""

    return hydra.serialization.op("!=", 11, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def nullish_coalescing_op() -> hydra.ast.Op:
    r"""Nullish coalescing operator (??)."""

    return hydra.serialization.op("??", 4, hydra.ast.Associativity.LEFT)

# Optional chaining operator (?.).
optional_chain_op = hydra.ast.Op(hydra.ast.Symbol("?."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(20), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def right_shift_op() -> hydra.ast.Op:
    r"""Right shift operator (>>)."""

    return hydra.serialization.op(">>", 13, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def strict_equal_op() -> hydra.ast.Op:
    r"""Strict equality operator (===)."""

    return hydra.serialization.op("===", 11, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def strict_not_equal_op() -> hydra.ast.Op:
    r"""Strict inequality operator (!==)."""

    return hydra.serialization.op("!==", 11, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def subtract_op() -> hydra.ast.Op:
    r"""Subtraction operator (-)."""

    return hydra.serialization.op("-", 14, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def ternary_op() -> hydra.ast.Op:
    r"""Ternary operator (?:) - represents the ? part."""

    return hydra.serialization.op("?", 3, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def unsigned_right_shift_op() -> hydra.ast.Op:
    r"""Unsigned right shift operator (>>>)."""

    return hydra.serialization.op(">>>", 13, hydra.ast.Associativity.LEFT)

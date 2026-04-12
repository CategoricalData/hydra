# Note: this is an automatically generated file. Do not edit.

r"""AST operators for Haskell."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.ast
import hydra.core
import hydra.lib.math
import hydra.serialization

@lru_cache(1)
def and_op() -> hydra.ast.Op:
    r"""Logical AND operator (&&)."""

    return hydra.serialization.op("&&", 3, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def ap_op() -> hydra.ast.Op:
    r"""Applicative apply operator (<*>)."""

    return hydra.serialization.op("<*>", 4, hydra.ast.Associativity.LEFT)

# Function application operator (whitespace).
app_op = hydra.ast.Op(hydra.ast.Symbol(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.ast.Precedence(0), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def apply_op() -> hydra.ast.Op:
    r"""Low-precedence function application ($)."""

    return hydra.serialization.op("$", 0, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def arrow_op() -> hydra.ast.Op:
    r"""Function type arrow (->)."""

    return hydra.serialization.op("->", hydra.lib.math.negate(1), hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def assert_op() -> hydra.ast.Op:
    r"""Type class constraint arrow (=>)."""

    return hydra.serialization.op("=>", 0, hydra.ast.Associativity.NONE)

@lru_cache(1)
def bind_op() -> hydra.ast.Op:
    r"""Monadic bind operator (>>=)."""

    return hydra.serialization.op(">>=", 1, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def case_op() -> hydra.ast.Op:
    r"""Case alternative arrow (->)."""

    return hydra.serialization.op("->", 0, hydra.ast.Associativity.NONE)

@lru_cache(1)
def compose_op() -> hydra.ast.Op:
    r"""Function composition (.)."""

    return hydra.serialization.op(".", 9, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def concat_op() -> hydra.ast.Op:
    r"""List concatenation (++)."""

    return hydra.serialization.op("++", 5, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def cons_op() -> hydra.ast.Op:
    r"""List cons (:)."""

    return hydra.serialization.op(":", 5, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def define_op() -> hydra.ast.Op:
    r"""Definition operator (=)."""

    return hydra.serialization.op("=", 0, hydra.ast.Associativity.NONE)

@lru_cache(1)
def diamond_op() -> hydra.ast.Op:
    r"""Semigroup append (<>)."""

    return hydra.serialization.op("<>", 6, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def div_op() -> hydra.ast.Op:
    r"""Integer division (`div`)."""

    return hydra.serialization.op("`div`", 7, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def divide_op() -> hydra.ast.Op:
    r"""Fractional division (/)."""

    return hydra.serialization.op("/", 7, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def elem_op() -> hydra.ast.Op:
    r"""List membership (`elem`)."""

    return hydra.serialization.op("`elem`", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def equal_op() -> hydra.ast.Op:
    r"""Equality comparison (==)."""

    return hydra.serialization.op("==", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def fmap_op() -> hydra.ast.Op:
    r"""Functor map (<$>)."""

    return hydra.serialization.op("<$>", 4, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def gt_op() -> hydra.ast.Op:
    r"""Greater than (>)."""

    return hydra.serialization.op(">", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def gte_op() -> hydra.ast.Op:
    r"""Greater than or equal (>=)."""

    return hydra.serialization.op(">=", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def index_op() -> hydra.ast.Op:
    r"""List indexing (!!)."""

    return hydra.serialization.op("!!", 9, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def lambda_op() -> hydra.ast.Op:
    r"""Lambda body arrow (->)."""

    return hydra.serialization.op("->", hydra.lib.math.negate(1), hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def lt_op() -> hydra.ast.Op:
    r"""Less than (<)."""

    return hydra.serialization.op("<", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def lte_op() -> hydra.ast.Op:
    r"""Less than or equal (<=)."""

    return hydra.serialization.op(">=", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def minus_op() -> hydra.ast.Op:
    r"""Subtraction (-). Originally: associativityLeft."""

    return hydra.serialization.op("-", 6, hydra.ast.Associativity.BOTH)

@lru_cache(1)
def mod_op() -> hydra.ast.Op:
    r"""Modulo (`mod`)."""

    return hydra.serialization.op("`mod`", 7, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def mult_op() -> hydra.ast.Op:
    r"""Multiplication (*). Originally: associativityLeft."""

    return hydra.serialization.op("*", 7, hydra.ast.Associativity.BOTH)

@lru_cache(1)
def neq_op() -> hydra.ast.Op:
    r"""Not equal (/=)."""

    return hydra.serialization.op("/=", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def not_elem_op() -> hydra.ast.Op:
    r"""List non-membership (`notElem`)."""

    return hydra.serialization.op("`notElem`", 4, hydra.ast.Associativity.NONE)

@lru_cache(1)
def or_op() -> hydra.ast.Op:
    r"""Logical OR (||)."""

    return hydra.serialization.op("||", 2, hydra.ast.Associativity.RIGHT)

@lru_cache(1)
def plus_op() -> hydra.ast.Op:
    r"""Addition (+). Originally: associativityLeft."""

    return hydra.serialization.op("+", 6, hydra.ast.Associativity.BOTH)

@lru_cache(1)
def quot_op() -> hydra.ast.Op:
    r"""Integer quotient (`quot`)."""

    return hydra.serialization.op("`quot`", 7, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def rem_op() -> hydra.ast.Op:
    r"""Integer remainder (`rem`)."""

    return hydra.serialization.op("`rem`", 7, hydra.ast.Associativity.LEFT)

@lru_cache(1)
def type_op() -> hydra.ast.Op:
    r"""Type annotation (::)."""

    return hydra.serialization.op("::", 0, hydra.ast.Associativity.NONE)

# Note: this is an automatically generated file. Do not edit.

r"""AST operators for Haskell."""

from __future__ import annotations
from typing import cast
import hydra.ast
import hydra.core
import hydra.lib.math
import hydra.serialization

# Logical AND operator (&&).
and_op = hydra.serialization.op("&&", 3, hydra.ast.Associativity.RIGHT)

# Applicative apply operator (<*>).
ap_op = hydra.serialization.op("<*>", 4, hydra.ast.Associativity.LEFT)

# Function application operator (whitespace).
app_op = hydra.ast.Op(hydra.ast.Symbol(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.ast.Precedence(0), hydra.ast.Associativity.LEFT)

# Low-precedence function application ($).
apply_op = hydra.serialization.op("$", 0, hydra.ast.Associativity.RIGHT)

# Function type arrow (->).
arrow_op = hydra.serialization.op("->", hydra.lib.math.negate(1), hydra.ast.Associativity.RIGHT)

# Type class constraint arrow (=>).
assert_op = hydra.serialization.op("=>", 0, hydra.ast.Associativity.NONE)

# Monadic bind operator (>>=).
bind_op = hydra.serialization.op(">>=", 1, hydra.ast.Associativity.LEFT)

# Case alternative arrow (->).
case_op = hydra.serialization.op("->", 0, hydra.ast.Associativity.NONE)

# Function composition (.).
compose_op = hydra.serialization.op(".", 9, hydra.ast.Associativity.LEFT)

# List concatenation (++).
concat_op = hydra.serialization.op("++", 5, hydra.ast.Associativity.RIGHT)

# List cons (:).
cons_op = hydra.serialization.op(":", 5, hydra.ast.Associativity.RIGHT)

# Definition operator (=).
define_op = hydra.serialization.op("=", 0, hydra.ast.Associativity.NONE)

# Semigroup append (<>).
diamond_op = hydra.serialization.op("<>", 6, hydra.ast.Associativity.RIGHT)

# Integer division (`div`).
div_op = hydra.serialization.op("`div`", 7, hydra.ast.Associativity.LEFT)

# Fractional division (/).
divide_op = hydra.serialization.op("/", 7, hydra.ast.Associativity.LEFT)

# List membership (`elem`).
elem_op = hydra.serialization.op("`elem`", 4, hydra.ast.Associativity.NONE)

# Equality comparison (==).
equal_op = hydra.serialization.op("==", 4, hydra.ast.Associativity.NONE)

# Functor map (<$>).
fmap_op = hydra.serialization.op("<$>", 4, hydra.ast.Associativity.LEFT)

# Greater than (>).
gt_op = hydra.serialization.op(">", 4, hydra.ast.Associativity.NONE)

# Greater than or equal (>=).
gte_op = hydra.serialization.op(">=", 4, hydra.ast.Associativity.NONE)

# List indexing (!!).
index_op = hydra.serialization.op("!!", 9, hydra.ast.Associativity.LEFT)

# Lambda body arrow (->).
lambda_op = hydra.serialization.op("->", hydra.lib.math.negate(1), hydra.ast.Associativity.RIGHT)

# Less than (<).
lt_op = hydra.serialization.op("<", 4, hydra.ast.Associativity.NONE)

# Less than or equal (<=).
lte_op = hydra.serialization.op(">=", 4, hydra.ast.Associativity.NONE)

# Subtraction (-). Originally: associativityLeft.
minus_op = hydra.serialization.op("-", 6, hydra.ast.Associativity.BOTH)

# Modulo (`mod`).
mod_op = hydra.serialization.op("`mod`", 7, hydra.ast.Associativity.LEFT)

# Multiplication (*). Originally: associativityLeft.
mult_op = hydra.serialization.op("*", 7, hydra.ast.Associativity.BOTH)

# Not equal (/=).
neq_op = hydra.serialization.op("/=", 4, hydra.ast.Associativity.NONE)

# List non-membership (`notElem`).
not_elem_op = hydra.serialization.op("`notElem`", 4, hydra.ast.Associativity.NONE)

# Logical OR (||).
or_op = hydra.serialization.op("||", 2, hydra.ast.Associativity.RIGHT)

# Addition (+). Originally: associativityLeft.
plus_op = hydra.serialization.op("+", 6, hydra.ast.Associativity.BOTH)

# Integer quotient (`quot`).
quot_op = hydra.serialization.op("`quot`", 7, hydra.ast.Associativity.LEFT)

# Integer remainder (`rem`).
rem_op = hydra.serialization.op("`rem`", 7, hydra.ast.Associativity.LEFT)

# Type annotation (::).
type_op = hydra.serialization.op("::", 0, hydra.ast.Associativity.NONE)

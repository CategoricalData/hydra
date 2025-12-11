# Note: this is an automatically generated file. Do not edit.

r"""AST operators for Haskell."""

from __future__ import annotations
from typing import cast
import hydra.ast
import hydra.core
import hydra.lib.math
import hydra.serialization

and_op = hydra.serialization.op("&&", 3, hydra.ast.Associativity.RIGHT)

ap_op = hydra.serialization.op("<*>", 4, hydra.ast.Associativity.LEFT)

# No source.
app_op = hydra.ast.Op(hydra.ast.Symbol(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.ast.Precedence(0), hydra.ast.Associativity.LEFT)

apply_op = hydra.serialization.op("$", 0, hydra.ast.Associativity.RIGHT)

arrow_op = hydra.serialization.op("->", hydra.lib.math.negate(1), hydra.ast.Associativity.RIGHT)

# No source.
assert_op = hydra.serialization.op("=>", 0, hydra.ast.Associativity.NONE)

bind_op = hydra.serialization.op(">>=", 1, hydra.ast.Associativity.LEFT)

# No source.
case_op = hydra.serialization.op("->", 0, hydra.ast.Associativity.NONE)

compose_op = hydra.serialization.op(".", 9, hydra.ast.Associativity.LEFT)

concat_op = hydra.serialization.op("++", 5, hydra.ast.Associativity.RIGHT)

cons_op = hydra.serialization.op(":", 5, hydra.ast.Associativity.RIGHT)

# No source.
define_op = hydra.serialization.op("=", 0, hydra.ast.Associativity.NONE)

diamond_op = hydra.serialization.op("<>", 6, hydra.ast.Associativity.RIGHT)

div_op = hydra.serialization.op("`div`", 7, hydra.ast.Associativity.LEFT)

divide_op = hydra.serialization.op("/", 7, hydra.ast.Associativity.LEFT)

elem_op = hydra.serialization.op("`elem`", 4, hydra.ast.Associativity.NONE)

equal_op = hydra.serialization.op("==", 4, hydra.ast.Associativity.NONE)

fmap_op = hydra.serialization.op("<$>", 4, hydra.ast.Associativity.LEFT)

gt_op = hydra.serialization.op(">", 4, hydra.ast.Associativity.NONE)

gte_op = hydra.serialization.op(">=", 4, hydra.ast.Associativity.NONE)

index_op = hydra.serialization.op("!!", 9, hydra.ast.Associativity.LEFT)

# No source.
lambda_op = hydra.serialization.op("->", hydra.lib.math.negate(1), hydra.ast.Associativity.RIGHT)

lt_op = hydra.serialization.op("<", 4, hydra.ast.Associativity.NONE)

lte_op = hydra.serialization.op(">=", 4, hydra.ast.Associativity.NONE)

# Originally: associativityLeft.
minus_op = hydra.serialization.op("-", 6, hydra.ast.Associativity.BOTH)

mod_op = hydra.serialization.op("`mod`", 7, hydra.ast.Associativity.LEFT)

# Originally: associativityLeft.
mult_op = hydra.serialization.op("*", 7, hydra.ast.Associativity.BOTH)

neq_op = hydra.serialization.op("/=", 4, hydra.ast.Associativity.NONE)

not_elem_op = hydra.serialization.op("`notElem`", 4, hydra.ast.Associativity.NONE)

or_op = hydra.serialization.op("||", 2, hydra.ast.Associativity.RIGHT)

# Originally: associativityLeft.
plus_op = hydra.serialization.op("+", 6, hydra.ast.Associativity.BOTH)

quot_op = hydra.serialization.op("`quot`", 7, hydra.ast.Associativity.LEFT)

rem_op = hydra.serialization.op("`rem`", 7, hydra.ast.Associativity.LEFT)

# No source.
type_op = hydra.serialization.op("::", 0, hydra.ast.Associativity.NONE)

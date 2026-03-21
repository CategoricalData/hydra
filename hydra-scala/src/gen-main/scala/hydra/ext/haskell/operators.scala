package hydra.ext.haskell.operators

import hydra.ast.*

import hydra.lib.math

val andOp: hydra.ast.Op = hydra.serialization.op("&&")(3)(hydra.ast.Associativity.right)

val apOp: hydra.ast.Op = hydra.serialization.op("<*>")(4)(hydra.ast.Associativity.left)

val appOp: hydra.ast.Op = hydra.ast.Op("", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.space), 0, hydra.ast.Associativity.left)

val applyOp: hydra.ast.Op = hydra.serialization.op("$")(0)(hydra.ast.Associativity.right)

val arrowOp: hydra.ast.Op = hydra.serialization.op("->")(math.negate(1))(hydra.ast.Associativity.right)

val assertOp: hydra.ast.Op = hydra.serialization.op("=>")(0)(hydra.ast.Associativity.none)

val bindOp: hydra.ast.Op = hydra.serialization.op(">>=")(1)(hydra.ast.Associativity.left)

val caseOp: hydra.ast.Op = hydra.serialization.op("->")(0)(hydra.ast.Associativity.none)

val composeOp: hydra.ast.Op = hydra.serialization.op(".")(9)(hydra.ast.Associativity.left)

val concatOp: hydra.ast.Op = hydra.serialization.op("++")(5)(hydra.ast.Associativity.right)

val consOp: hydra.ast.Op = hydra.serialization.op(":")(5)(hydra.ast.Associativity.right)

val defineOp: hydra.ast.Op = hydra.serialization.op("=")(0)(hydra.ast.Associativity.none)

val diamondOp: hydra.ast.Op = hydra.serialization.op("<>")(6)(hydra.ast.Associativity.right)

val divOp: hydra.ast.Op = hydra.serialization.op("`div`")(7)(hydra.ast.Associativity.left)

val divideOp: hydra.ast.Op = hydra.serialization.op("/")(7)(hydra.ast.Associativity.left)

val elemOp: hydra.ast.Op = hydra.serialization.op("`elem`")(4)(hydra.ast.Associativity.none)

val equalOp: hydra.ast.Op = hydra.serialization.op("==")(4)(hydra.ast.Associativity.none)

val fmapOp: hydra.ast.Op = hydra.serialization.op("<$>")(4)(hydra.ast.Associativity.left)

val gtOp: hydra.ast.Op = hydra.serialization.op(">")(4)(hydra.ast.Associativity.none)

val gteOp: hydra.ast.Op = hydra.serialization.op(">=")(4)(hydra.ast.Associativity.none)

val indexOp: hydra.ast.Op = hydra.serialization.op("!!")(9)(hydra.ast.Associativity.left)

val lambdaOp: hydra.ast.Op = hydra.serialization.op("->")(math.negate(1))(hydra.ast.Associativity.right)

val ltOp: hydra.ast.Op = hydra.serialization.op("<")(4)(hydra.ast.Associativity.none)

val lteOp: hydra.ast.Op = hydra.serialization.op(">=")(4)(hydra.ast.Associativity.none)

val minusOp: hydra.ast.Op = hydra.serialization.op("-")(6)(hydra.ast.Associativity.both)

val modOp: hydra.ast.Op = hydra.serialization.op("`mod`")(7)(hydra.ast.Associativity.left)

val multOp: hydra.ast.Op = hydra.serialization.op("*")(7)(hydra.ast.Associativity.both)

val neqOp: hydra.ast.Op = hydra.serialization.op("/=")(4)(hydra.ast.Associativity.none)

val notElemOp: hydra.ast.Op = hydra.serialization.op("`notElem`")(4)(hydra.ast.Associativity.none)

val orOp: hydra.ast.Op = hydra.serialization.op("||")(2)(hydra.ast.Associativity.right)

val plusOp: hydra.ast.Op = hydra.serialization.op("+")(6)(hydra.ast.Associativity.both)

val quotOp: hydra.ast.Op = hydra.serialization.op("`quot`")(7)(hydra.ast.Associativity.left)

val remOp: hydra.ast.Op = hydra.serialization.op("`rem`")(7)(hydra.ast.Associativity.left)

val typeOp: hydra.ast.Op = hydra.serialization.op("::")(0)(hydra.ast.Associativity.none)

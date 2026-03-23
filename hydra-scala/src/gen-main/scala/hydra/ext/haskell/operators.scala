package hydra.ext.haskell.operators

import hydra.ast.*

import hydra.lib.math

lazy val andOp: hydra.ast.Op = hydra.serialization.op("&&")(3)(hydra.ast.Associativity.right)

lazy val apOp: hydra.ast.Op = hydra.serialization.op("<*>")(4)(hydra.ast.Associativity.left)

lazy val appOp: hydra.ast.Op = hydra.ast.Op("", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.space), 0, hydra.ast.Associativity.left)

lazy val applyOp: hydra.ast.Op = hydra.serialization.op("$")(0)(hydra.ast.Associativity.right)

lazy val arrowOp: hydra.ast.Op = hydra.serialization.op("->")(hydra.lib.math.negate(1))(hydra.ast.Associativity.right)

lazy val assertOp: hydra.ast.Op = hydra.serialization.op("=>")(0)(hydra.ast.Associativity.none)

lazy val bindOp: hydra.ast.Op = hydra.serialization.op(">>=")(1)(hydra.ast.Associativity.left)

lazy val caseOp: hydra.ast.Op = hydra.serialization.op("->")(0)(hydra.ast.Associativity.none)

lazy val composeOp: hydra.ast.Op = hydra.serialization.op(".")(9)(hydra.ast.Associativity.left)

lazy val concatOp: hydra.ast.Op = hydra.serialization.op("++")(5)(hydra.ast.Associativity.right)

lazy val consOp: hydra.ast.Op = hydra.serialization.op(":")(5)(hydra.ast.Associativity.right)

lazy val defineOp: hydra.ast.Op = hydra.serialization.op("=")(0)(hydra.ast.Associativity.none)

lazy val diamondOp: hydra.ast.Op = hydra.serialization.op("<>")(6)(hydra.ast.Associativity.right)

lazy val divOp: hydra.ast.Op = hydra.serialization.op("`div`")(7)(hydra.ast.Associativity.left)

lazy val divideOp: hydra.ast.Op = hydra.serialization.op("/")(7)(hydra.ast.Associativity.left)

lazy val elemOp: hydra.ast.Op = hydra.serialization.op("`elem`")(4)(hydra.ast.Associativity.none)

lazy val equalOp: hydra.ast.Op = hydra.serialization.op("==")(4)(hydra.ast.Associativity.none)

lazy val fmapOp: hydra.ast.Op = hydra.serialization.op("<$>")(4)(hydra.ast.Associativity.left)

lazy val gtOp: hydra.ast.Op = hydra.serialization.op(">")(4)(hydra.ast.Associativity.none)

lazy val gteOp: hydra.ast.Op = hydra.serialization.op(">=")(4)(hydra.ast.Associativity.none)

lazy val indexOp: hydra.ast.Op = hydra.serialization.op("!!")(9)(hydra.ast.Associativity.left)

lazy val lambdaOp: hydra.ast.Op = hydra.serialization.op("->")(hydra.lib.math.negate(1))(hydra.ast.Associativity.right)

lazy val ltOp: hydra.ast.Op = hydra.serialization.op("<")(4)(hydra.ast.Associativity.none)

lazy val lteOp: hydra.ast.Op = hydra.serialization.op(">=")(4)(hydra.ast.Associativity.none)

lazy val minusOp: hydra.ast.Op = hydra.serialization.op("-")(6)(hydra.ast.Associativity.both)

lazy val modOp: hydra.ast.Op = hydra.serialization.op("`mod`")(7)(hydra.ast.Associativity.left)

lazy val multOp: hydra.ast.Op = hydra.serialization.op("*")(7)(hydra.ast.Associativity.both)

lazy val neqOp: hydra.ast.Op = hydra.serialization.op("/=")(4)(hydra.ast.Associativity.none)

lazy val notElemOp: hydra.ast.Op = hydra.serialization.op("`notElem`")(4)(hydra.ast.Associativity.none)

lazy val orOp: hydra.ast.Op = hydra.serialization.op("||")(2)(hydra.ast.Associativity.right)

lazy val plusOp: hydra.ast.Op = hydra.serialization.op("+")(6)(hydra.ast.Associativity.both)

lazy val quotOp: hydra.ast.Op = hydra.serialization.op("`quot`")(7)(hydra.ast.Associativity.left)

lazy val remOp: hydra.ast.Op = hydra.serialization.op("`rem`")(7)(hydra.ast.Associativity.left)

lazy val typeOp: hydra.ast.Op = hydra.serialization.op("::")(0)(hydra.ast.Associativity.none)

// Note: this is an automatically generated file. Do not edit.

/**
 * AST operators for Haskell
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibMath from "../lib/math.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Serialization from "../serialization.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const andOp: Ast.Op = Serialization.op("&&")(3)(({ tag: "right" }));

export const apOp: Ast.Op = Serialization.op("<*>")(4)(({ tag: "left" }));

export const appOp: Ast.Op = ({
    symbol: "",
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "space" })
  }),
    precedence: 0,
    associativity: ({ tag: "left" })
  });

export const applyOp: Ast.Op = Serialization.op("$")(0)(({ tag: "right" }));

export const arrowOp: Ast.Op = Serialization.op("->")(LibMath.negate(1))(({ tag: "right" }));

export const assertOp: Ast.Op = Serialization.op("=>")(0)(({ tag: "none" }));

export const bindOp: Ast.Op = Serialization.op(">>=")(1)(({ tag: "left" }));

export const caseOp: Ast.Op = Serialization.op("->")(0)(({ tag: "none" }));

export const composeOp: Ast.Op = Serialization.op(".")(9)(({ tag: "left" }));

export const concatOp: Ast.Op = Serialization.op("++")(5)(({ tag: "right" }));

export const consOp: Ast.Op = Serialization.op(":")(5)(({ tag: "right" }));

export const defineOp: Ast.Op = Serialization.op("=")(0)(({ tag: "none" }));

export const diamondOp: Ast.Op = Serialization.op("<>")(6)(({ tag: "right" }));

export const divOp: Ast.Op = Serialization.op("`div`")(7)(({ tag: "left" }));

export const divideOp: Ast.Op = Serialization.op("/")(7)(({ tag: "left" }));

export const elemOp: Ast.Op = Serialization.op("`elem`")(4)(({ tag: "none" }));

export const equalOp: Ast.Op = Serialization.op("==")(4)(({ tag: "none" }));

export const fmapOp: Ast.Op = Serialization.op("<$>")(4)(({ tag: "left" }));

export const gtOp: Ast.Op = Serialization.op(">")(4)(({ tag: "none" }));

export const gteOp: Ast.Op = Serialization.op(">=")(4)(({ tag: "none" }));

export const indexOp: Ast.Op = Serialization.op("!!")(9)(({ tag: "left" }));

export const lambdaOp: Ast.Op = Serialization.op("->")(LibMath.negate(1))(({ tag: "right" }));

export const ltOp: Ast.Op = Serialization.op("<")(4)(({ tag: "none" }));

export const lteOp: Ast.Op = Serialization.op(">=")(4)(({ tag: "none" }));

export const minusOp: Ast.Op = Serialization.op("-")(6)(({ tag: "both" }));

export const modOp: Ast.Op = Serialization.op("`mod`")(7)(({ tag: "left" }));

export const multOp: Ast.Op = Serialization.op("*")(7)(({ tag: "both" }));

export const neqOp: Ast.Op = Serialization.op("/=")(4)(({ tag: "none" }));

export const notElemOp: Ast.Op = Serialization.op("`notElem`")(4)(({ tag: "none" }));

export const orOp: Ast.Op = Serialization.op("||")(2)(({ tag: "right" }));

export const plusOp: Ast.Op = Serialization.op("+")(6)(({ tag: "both" }));

export const quotOp: Ast.Op = Serialization.op("`quot`")(7)(({ tag: "left" }));

export const remOp: Ast.Op = Serialization.op("`rem`")(7)(({ tag: "left" }));

export const typeOp: Ast.Op = Serialization.op("::")(0)(({ tag: "none" }));

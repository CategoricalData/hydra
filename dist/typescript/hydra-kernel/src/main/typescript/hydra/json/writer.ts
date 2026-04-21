// Note: this is an automatically generated file. Do not edit.

/**
 * JSON serialization functions using the Hydra AST
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
import * as JsonModel from "./model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMath from "../lib/math.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
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

export const colonOp: Ast.Op = ({
    symbol: ":",
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "space" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });

export function jsonString(s: string): string {
  return (() => {
  const hexEscape = ((c: number) => (() => {
  const hi = LibStrings.fromList(LibLists.pure(LibStrings.charAt(LibMath.div(c)(16))("0123456789abcdef")));
  return (() => {
  const lo = LibStrings.fromList(LibLists.pure(LibStrings.charAt(LibMath.mod(c)(16))("0123456789abcdef")));
  return LibStrings.cat2(LibStrings.cat2("\\u00")(hi))(lo);
})();
})());
  return (() => {
  const escape = ((c: number) => LibLogic.ifElse(LibEquality.equal(c)(34))("\\\"")(LibLogic.ifElse(LibEquality.equal(c)(92))("\\\\")(LibLogic.ifElse(LibEquality.equal(c)(8))("\\b")(LibLogic.ifElse(LibEquality.equal(c)(12))("\\f")(LibLogic.ifElse(LibEquality.equal(c)(10))("\\n")(LibLogic.ifElse(LibEquality.equal(c)(13))("\\r")(LibLogic.ifElse(LibEquality.equal(c)(9))("\\t")(LibLogic.ifElse(LibEquality.lt(c)(32))(hexEscape(c))(LibStrings.fromList(LibLists.pure(c)))))))))));
  return (() => {
  const escaped = LibStrings.cat(LibLists.map(escape)(LibStrings.toList(s)));
  return LibStrings.cat2(LibStrings.cat2("\"")(escaped))("\"");
})();
})();
})();
}

export function keyValueToExpr(pair: readonly [string, JsonModel.Value]): Ast.Expr {
  return (() => {
  const key = LibPairs.first(pair);
  return (() => {
  const value = LibPairs.second(pair);
  return Serialization.ifx(colonOp)(Serialization.cst(jsonString(key)))(valueToExpr(value));
})();
})();
}

export function printJson(value: JsonModel.Value): string {
  return Serialization.printExpr(valueToExpr(value));
}

export function valueToExpr(value: JsonModel.Value): Ast.Expr {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "array": return ((arr: ReadonlyArray<JsonModel.Value>) => Serialization.bracketListAdaptive(LibLists.map(valueToExpr)(arr)))((_m as any).value);
    case "boolean": return ((b: boolean) => Serialization.cst(LibLogic.ifElse(b)("true")("false")))((_m as any).value);
    case "null": return ((_: void) => Serialization.cst("null"))((_m as any).value);
    case "number": return ((n: number) => (() => {
  const rounded = LibLiterals.bigfloatToBigint(n);
  return (() => {
  const shown = LibLiterals.showBigfloat(n);
  return Serialization.cst(LibLogic.ifElse(LibLogic.and(LibEquality.equal(n)(LibLiterals.bigintToBigfloat(rounded)))(LibLogic.not(LibEquality.equal(shown)("-0.0"))))(LibLiterals.showBigint(rounded))(shown));
})();
})())((_m as any).value);
    case "object": return ((obj: ReadonlyMap<string, JsonModel.Value>) => Serialization.bracesListAdaptive(LibLists.map(keyValueToExpr)(LibMaps.toList(obj))))((_m as any).value);
    case "string": return ((s: string) => Serialization.cst(jsonString(s)))((_m as any).value);
  }
})();
}

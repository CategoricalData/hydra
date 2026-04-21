// Note: this is an automatically generated file. Do not edit.

/**
 * Conversion functions for literal values.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibLiterals from "./lib/literals.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function bigfloatToFloatValue(ft: Core.FloatType): ((x: number) => Core.FloatValue) {
  return ((bf: number) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => ({ tag: "bigfloat", value: bf }))((_m as any).value);
    case "float32": return ((_: void) => ({ tag: "float32", value: LibLiterals.bigfloatToFloat32(bf) }))((_m as any).value);
    case "float64": return ((_: void) => ({ tag: "float64", value: LibLiterals.bigfloatToFloat64(bf) }))((_m as any).value);
  }
})());
}

export function bigintToIntegerValue(it: Core.IntegerType): ((x: bigint) => Core.IntegerValue) {
  return ((bi: bigint) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => ({ tag: "bigint", value: bi }))((_m as any).value);
    case "int8": return ((_: void) => ({ tag: "int8", value: LibLiterals.bigintToInt8(bi) }))((_m as any).value);
    case "int16": return ((_: void) => ({ tag: "int16", value: LibLiterals.bigintToInt16(bi) }))((_m as any).value);
    case "int32": return ((_: void) => ({ tag: "int32", value: LibLiterals.bigintToInt32(bi) }))((_m as any).value);
    case "int64": return ((_: void) => ({ tag: "int64", value: LibLiterals.bigintToInt64(bi) }))((_m as any).value);
    case "uint8": return ((_: void) => ({ tag: "uint8", value: LibLiterals.bigintToUint8(bi) }))((_m as any).value);
    case "uint16": return ((_: void) => ({ tag: "uint16", value: LibLiterals.bigintToUint16(bi) }))((_m as any).value);
    case "uint32": return ((_: void) => ({ tag: "uint32", value: LibLiterals.bigintToUint32(bi) }))((_m as any).value);
    case "uint64": return ((_: void) => ({ tag: "uint64", value: LibLiterals.bigintToUint64(bi) }))((_m as any).value);
  }
})());
}

export function floatValueToBigfloat(v1: Core.FloatValue): number {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigfloat": return ((bf: number) => bf)((_m as any).value);
    case "float32": return ((f32: number) => LibLiterals.float32ToBigfloat(f32))((_m as any).value);
    case "float64": return ((f64: number) => LibLiterals.float64ToBigfloat(f64))((_m as any).value);
  }
})();
}

export function integerValueToBigint(v1: Core.IntegerValue): bigint {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigint": return ((bi: bigint) => bi)((_m as any).value);
    case "int8": return ((i8: number) => LibLiterals.int8ToBigint(i8))((_m as any).value);
    case "int16": return ((i16: bigint) => LibLiterals.int16ToBigint(i16))((_m as any).value);
    case "int32": return ((i32: number) => LibLiterals.int32ToBigint(i32))((_m as any).value);
    case "int64": return ((i64: bigint) => LibLiterals.int64ToBigint(i64))((_m as any).value);
    case "uint8": return ((ui8: bigint) => LibLiterals.uint8ToBigint(ui8))((_m as any).value);
    case "uint16": return ((ui16: number) => LibLiterals.uint16ToBigint(ui16))((_m as any).value);
    case "uint32": return ((ui32: bigint) => LibLiterals.uint32ToBigint(ui32))((_m as any).value);
    case "uint64": return ((ui64: bigint) => LibLiterals.uint64ToBigint(ui64))((_m as any).value);
  }
})();
}

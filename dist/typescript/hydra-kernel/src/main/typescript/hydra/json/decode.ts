// Note: this is an automatically generated file. Do not edit.

/**
 * JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling.
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
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as JsonModel from "./model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Literals from "../literals.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function decodeFloat(ft: Core.FloatType): ((x: JsonModel.Value) => string | Core.Term) {
  return ((value: JsonModel.Value) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => (() => {
  const _m = value;
  switch (_m.tag) {
    case "number": return ((n: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: n }) }) }) }))((_m as any).value);
    default: return ({ tag: "left", value: "expected number for bigfloat" })(_m);
  }
})())((_m as any).value);
    case "float32": return ((_: void) => (() => {
  const strResult = expectString(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((s: string) => (() => {
  const parsed = LibLiterals.readFloat32(s);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["invalid float32: ", s]) }))(((v: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: v }) }) }) })))(parsed);
})()))(strResult);
})())((_m as any).value);
    case "float64": return ((_: void) => (() => {
  const _m = value;
  switch (_m.tag) {
    case "number": return ((n: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: LibLiterals.bigfloatToFloat64(n) }) }) }) }))((_m as any).value);
    case "string": return ((s: string) => LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["invalid float64 sentinel: ", s]) }))(((v: number) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: v }) }) }) })))(parseSpecialFloat(s)))((_m as any).value);
    default: return ({ tag: "left", value: "expected number or special float string for float64" })(_m);
  }
})())((_m as any).value);
  }
})());
}

export function decodeInteger(it: Core.IntegerType): ((x: JsonModel.Value) => string | Core.Term) {
  return ((value: JsonModel.Value) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => (() => {
  const strResult = expectString(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((s: string) => (() => {
  const parsed = LibLiterals.readBigint(s);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["invalid bigint: ", s]) }))(((v: bigint) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: v }) }) }) })))(parsed);
})()))(strResult);
})())((_m as any).value);
    case "int64": return ((_: void) => (() => {
  const strResult = expectString(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((s: string) => (() => {
  const parsed = LibLiterals.readInt64(s);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["invalid int64: ", s]) }))(((v: bigint) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: v }) }) }) })))(parsed);
})()))(strResult);
})())((_m as any).value);
    case "uint32": return ((_: void) => (() => {
  const strResult = expectString(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((s: string) => (() => {
  const parsed = LibLiterals.readUint32(s);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["invalid uint32: ", s]) }))(((v: bigint) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: v }) }) }) })))(parsed);
})()))(strResult);
})())((_m as any).value);
    case "uint64": return ((_: void) => (() => {
  const strResult = expectString(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((s: string) => (() => {
  const parsed = LibLiterals.readUint64(s);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["invalid uint64: ", s]) }))(((v: bigint) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: v }) }) }) })))(parsed);
})()))(strResult);
})())((_m as any).value);
    case "int8": return ((_: void) => (() => {
  const numResult = expectNumber(value);
  return LibEithers.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: LibLiterals.bigintToInt8(LibLiterals.bigfloatToBigint(n)) }) }) })))(numResult);
})())((_m as any).value);
    case "int16": return ((_: void) => (() => {
  const numResult = expectNumber(value);
  return LibEithers.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: LibLiterals.bigintToInt16(LibLiterals.bigfloatToBigint(n)) }) }) })))(numResult);
})())((_m as any).value);
    case "int32": return ((_: void) => (() => {
  const numResult = expectNumber(value);
  return LibEithers.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: LibLiterals.bigintToInt32(LibLiterals.bigfloatToBigint(n)) }) }) })))(numResult);
})())((_m as any).value);
    case "uint8": return ((_: void) => (() => {
  const numResult = expectNumber(value);
  return LibEithers.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: LibLiterals.bigintToUint8(LibLiterals.bigfloatToBigint(n)) }) }) })))(numResult);
})())((_m as any).value);
    case "uint16": return ((_: void) => (() => {
  const numResult = expectNumber(value);
  return LibEithers.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: LibLiterals.bigintToUint16(LibLiterals.bigfloatToBigint(n)) }) }) })))(numResult);
})())((_m as any).value);
  }
})());
}

export function decodeLiteral(lt: Core.LiteralType): ((x: JsonModel.Value) => string | Core.Term) {
  return ((value: JsonModel.Value) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => (() => {
  const strResult = expectString(value);
  return LibEithers.map(((s: string) => ({ tag: "literal", value: ({ tag: "binary", value: LibLiterals.stringToBinary(s) }) })))(strResult);
})())((_m as any).value);
    case "boolean": return ((_: void) => (() => {
  const _m = value;
  switch (_m.tag) {
    case "boolean": return ((b: boolean) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "boolean", value: b }) }) }))((_m as any).value);
    default: return ({ tag: "left", value: "expected boolean" })(_m);
  }
})())((_m as any).value);
    case "float": return ((ft: Core.FloatType) => decodeFloat(ft)(value))((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => decodeInteger(it)(value))((_m as any).value);
    case "string": return ((_: void) => (() => {
  const strResult = expectString(value);
  return LibEithers.map(((s: string) => ({ tag: "literal", value: ({ tag: "string", value: s }) })))(strResult);
})())((_m as any).value);
  }
})());
}

export function expectArray(value: JsonModel.Value): string | ReadonlyArray<JsonModel.Value> {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "array": return ((arr: ReadonlyArray<JsonModel.Value>) => ({ tag: "right", value: arr }))((_m as any).value);
    default: return ({ tag: "left", value: "expected array" })(_m);
  }
})();
}

export function expectNumber(value: JsonModel.Value): string | number {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "number": return ((n: number) => ({ tag: "right", value: n }))((_m as any).value);
    default: return ({ tag: "left", value: "expected number" })(_m);
  }
})();
}

export function expectObject(value: JsonModel.Value): string | ReadonlyMap<string, JsonModel.Value> {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "object": return ((obj: ReadonlyMap<string, JsonModel.Value>) => ({ tag: "right", value: obj }))((_m as any).value);
    default: return ({ tag: "left", value: "expected object" })(_m);
  }
})();
}

export function expectString(value: JsonModel.Value): string | string {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected string" })(_m);
  }
})();
}

export function fromJson(types: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Name) => ((x: Core.Type) => ((x: JsonModel.Value) => string | Core.Term))) {
  return ((tname: Core.Name) => ((typ: Core.Type) => ((value: JsonModel.Value) => (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "literal": return ((lt: Core.LiteralType) => decodeLiteral(lt)(value))((_m as any).value);
    case "list": return ((elemType: Core.Type) => (() => {
  const decodeElem = ((v: JsonModel.Value) => fromJson(types)(tname)(elemType)(v));
  return (() => {
  const arrResult = expectArray(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((arr: ReadonlyArray<JsonModel.Value>) => (() => {
  const decoded = LibEithers.mapList(decodeElem)(arr);
  return LibEithers.map(((ts: ReadonlyArray<Core.Term>) => ({ tag: "list", value: ts })))(decoded);
})()))(arrResult);
})();
})())((_m as any).value);
    case "set": return ((elemType: Core.Type) => (() => {
  const decodeElem = ((v: JsonModel.Value) => fromJson(types)(tname)(elemType)(v));
  return (() => {
  const arrResult = expectArray(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((arr: ReadonlyArray<JsonModel.Value>) => (() => {
  const decoded = LibEithers.mapList(decodeElem)(arr);
  return LibEithers.map(((elems: ReadonlyArray<Core.Term>) => ({ tag: "set", value: LibSets.fromList(elems) })))(decoded);
})()))(arrResult);
})();
})())((_m as any).value);
    case "maybe": return ((innerType: Core.Type) => (() => {
  const innerStripped = Strip.deannotateType(innerType);
  return (() => {
  const isNestedMaybe = (() => {
  const _m = innerStripped;
  switch (_m.tag) {
    case "maybe": return ((_: Core.Type) => true)((_m as any).value);
    default: return false(_m);
  }
})();
  return LibLogic.ifElse(isNestedMaybe)((() => {
  const decodeJust = ((arr: ReadonlyArray<JsonModel.Value>) => LibEithers.map(((v: Core.Term) => ({ tag: "maybe", value: v })))(fromJson(types)(tname)(innerType)(LibLists.head(arr))));
  return (() => {
  const decodeMaybeArray = ((arr: ReadonlyArray<JsonModel.Value>) => (() => {
  const len = LibLists.length(arr);
  return LibLogic.ifElse(LibEquality.equal(len)(0))(({ tag: "right", value: ({ tag: "maybe", value: null }) }))(LibLogic.ifElse(LibEquality.equal(len)(1))(decodeJust(arr))(({ tag: "left", value: "expected single-element array for Just" })));
})());
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "null": return ((_: void) => ({ tag: "right", value: ({ tag: "maybe", value: null }) }))((_m as any).value);
    case "array": return ((arr: ReadonlyArray<JsonModel.Value>) => decodeMaybeArray(arr))((_m as any).value);
    default: return ({ tag: "left", value: "expected null or single-element array for nested Maybe" })(_m);
  }
})();
})();
})())((() => {
  const _m = value;
  switch (_m.tag) {
    case "null": return ((_: void) => ({ tag: "right", value: ({ tag: "maybe", value: null }) }))((_m as any).value);
    default: return LibEithers.map(((v: Core.Term) => ({ tag: "maybe", value: v })))(fromJson(types)(tname)(innerType)(value))(_m);
  }
})());
})();
})())((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const objResult = expectObject(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((obj: ReadonlyMap<string, JsonModel.Value>) => (() => {
  const decodeField = ((ft: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(ft);
  return (() => {
  const ftype = ((_x) => _x.type)(ft);
  return (() => {
  const mval = LibMaps.lookup(((_x) => _x)(fname))(obj);
  return (() => {
  const defaultVal = ({ tag: "null" });
  return (() => {
  const jsonVal = LibMaybes.fromMaybe(defaultVal)(mval);
  return (() => {
  const decoded = fromJson(types)(tname)(ftype)(jsonVal);
  return LibEithers.map(((v: Core.Term) => ({
    name: fname,
    term: v
  })))(decoded);
})();
})();
})();
})();
})();
})());
  return (() => {
  const decodedFields = LibEithers.mapList(decodeField)(rt);
  return LibEithers.map(((fs: ReadonlyArray<Core.Field>) => ({ tag: "record", value: ({
    typeName: tname,
    fields: fs
  }) })))(decodedFields);
})();
})()))(objResult);
})())((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const decodeVariant = ((key: string) => ((val: JsonModel.Value | null) => ((ftype: Core.Type) => (() => {
  const jsonVal = LibMaybes.fromMaybe(({ tag: "null" }))(val);
  return (() => {
  const decoded = fromJson(types)(tname)(ftype)(jsonVal);
  return LibEithers.map(((v: Core.Term) => ({ tag: "inject", value: ({
    typeName: tname,
    field: ({
    name: key,
    term: v
  })
  }) })))(decoded);
})();
})())));
  return (() => {
  const tryField = ((key: string) => ((val: JsonModel.Value | null) => ((ft: Core.FieldType) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.name)(ft)))(key))(decodeVariant(key)(val)(((_x) => _x.type)(ft)))(null))));
  return (() => {
  const findAndDecode = ((key: string) => ((val: JsonModel.Value | null) => ((fts: ReadonlyArray<Core.FieldType>) => LibLogic.ifElse(LibLists.null_(fts))(({ tag: "left", value: LibStrings.cat(["unknown variant: ", key]) }))(LibMaybes.maybe(findAndDecode(key)(val)(LibLists.tail(fts)))(((r: string | Core.Term) => r))(tryField(key)(val)(LibLists.head(fts)))))));
  return (() => {
  const decodeSingleKey = ((obj: ReadonlyMap<string, JsonModel.Value>) => findAndDecode(LibLists.head(LibMaps.keys(obj)))(LibMaps.lookup(LibLists.head(LibMaps.keys(obj)))(obj))(rt));
  return (() => {
  const processUnion = ((obj: ReadonlyMap<string, JsonModel.Value>) => LibLogic.ifElse(LibEquality.equal(LibLists.length(LibMaps.keys(obj)))(1))(decodeSingleKey(obj))(({ tag: "left", value: "expected single-key object for union" })));
  return (() => {
  const objResult = expectObject(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((obj: ReadonlyMap<string, JsonModel.Value>) => processUnion(obj)))(objResult);
})();
})();
})();
})();
})();
})())((_m as any).value);
    case "unit": return ((_: void) => (() => {
  const objResult = expectObject(value);
  return LibEithers.map(((_2: ReadonlyMap<string, JsonModel.Value>) => ({ tag: "unit" })))(objResult);
})())((_m as any).value);
    case "wrap": return ((wn: Core.Type) => (() => {
  const decoded = fromJson(types)(tname)(wn)(value);
  return LibEithers.map(((v: Core.Term) => ({ tag: "wrap", value: ({
    typeName: tname,
    body: v
  }) })))(decoded);
})())((_m as any).value);
    case "map": return ((mt: Core.MapType) => (() => {
  const keyType = ((_x) => _x.keys)(mt);
  return (() => {
  const valType = ((_x) => _x.values)(mt);
  return (() => {
  const arrResult = expectArray(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((arr: ReadonlyArray<JsonModel.Value>) => (() => {
  const decodeEntry = ((entryJson: JsonModel.Value) => (() => {
  const objResult = expectObject(entryJson);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((entryObj: ReadonlyMap<string, JsonModel.Value>) => (() => {
  const keyJson = LibMaps.lookup("@key")(entryObj);
  return (() => {
  const valJson = LibMaps.lookup("@value")(entryObj);
  return LibMaybes.maybe(({ tag: "left", value: "missing @key in map entry" }))(((kj: JsonModel.Value) => LibMaybes.maybe(({ tag: "left", value: "missing @value in map entry" }))(((vj: JsonModel.Value) => (() => {
  const decodedKey = fromJson(types)(tname)(keyType)(kj);
  return (() => {
  const decodedVal = fromJson(types)(tname)(valType)(vj);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((k: Core.Term) => LibEithers.map(((v: Core.Term) => [k, v]))(decodedVal)))(decodedKey);
})();
})()))(valJson)))(keyJson);
})();
})()))(objResult);
})());
  return (() => {
  const entries = LibEithers.mapList(decodeEntry)(arr);
  return LibEithers.map(((es: ReadonlyArray<readonly [Core.Term, Core.Term]>) => ({ tag: "map", value: LibMaps.fromList(es) })))(entries);
})();
})()))(arrResult);
})();
})();
})())((_m as any).value);
    case "pair": return ((pt: Core.PairType) => (() => {
  const firstType = ((_x) => _x.first)(pt);
  return (() => {
  const secondType = ((_x) => _x.second)(pt);
  return (() => {
  const objResult = expectObject(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((obj: ReadonlyMap<string, JsonModel.Value>) => (() => {
  const firstJson = LibMaps.lookup("@first")(obj);
  return (() => {
  const secondJson = LibMaps.lookup("@second")(obj);
  return LibMaybes.maybe(({ tag: "left", value: "missing @first in pair" }))(((fj: JsonModel.Value) => LibMaybes.maybe(({ tag: "left", value: "missing @second in pair" }))(((sj: JsonModel.Value) => (() => {
  const decodedFirst = fromJson(types)(tname)(firstType)(fj);
  return (() => {
  const decodedSecond = fromJson(types)(tname)(secondType)(sj);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((f: Core.Term) => LibEithers.map(((s: Core.Term) => ({ tag: "pair", value: [f, s] })))(decodedSecond)))(decodedFirst);
})();
})()))(secondJson)))(firstJson);
})();
})()))(objResult);
})();
})();
})())((_m as any).value);
    case "either": return ((et: Core.EitherType) => (() => {
  const leftType = ((_x) => _x.left)(et);
  return (() => {
  const rightType = ((_x) => _x.right)(et);
  return (() => {
  const objResult = expectObject(value);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((obj: ReadonlyMap<string, JsonModel.Value>) => (() => {
  const leftJson = LibMaps.lookup("@left")(obj);
  return (() => {
  const rightJson = LibMaps.lookup("@right")(obj);
  return LibMaybes.maybe(LibMaybes.maybe(({ tag: "left", value: "expected @left or @right in Either" }))(((rj: JsonModel.Value) => (() => {
  const decoded = fromJson(types)(tname)(rightType)(rj);
  return LibEithers.map(((v: Core.Term) => ({ tag: "either", value: ({ tag: "right", value: v }) })))(decoded);
})()))(rightJson))(((lj: JsonModel.Value) => (() => {
  const decoded = fromJson(types)(tname)(leftType)(lj);
  return LibEithers.map(((v: Core.Term) => ({ tag: "either", value: ({ tag: "left", value: v }) })))(decoded);
})()))(leftJson);
})();
})()))(objResult);
})();
})();
})())((_m as any).value);
    case "variable": return ((name: Core.Name) => (() => {
  const lookedUp = LibMaps.lookup(name)(types);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["unknown type variable: ", ((_x) => _x)(name)]) }))(((resolvedType: Core.Type) => fromJson(types)(name)(resolvedType)(value)))(lookedUp);
})())((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat(["unsupported type for JSON decoding: ", ShowCore.type(typ)]) })(_m);
  }
})();
})())));
}

export function parseSpecialFloat(s: string): number | null {
  return LibLogic.ifElse(LibLogic.or(LibEquality.equal(s)("NaN"))(LibLogic.or(LibEquality.equal(s)("Infinity"))(LibLogic.or(LibEquality.equal(s)("-Infinity"))(LibEquality.equal(s)("-0.0")))))(LibLiterals.readFloat64(s))(null);
}

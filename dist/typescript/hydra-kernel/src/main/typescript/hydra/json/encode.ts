// Note: this is an automatically generated file. Do not edit.

/**
 * JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling.
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
import * as LibPairs from "../lib/pairs.js";
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

export function encodeFloat<t0>(fv: Core.FloatValue): t0 | JsonModel.Value {
  return (() => {
  const _m = fv;
  switch (_m.tag) {
    case "bigfloat": return ((bf: number) => (() => {
  const s = LibLiterals.showBigfloat(bf);
  return LibLogic.ifElse(isSpecialFloatString(s))(({ tag: "right", value: ({ tag: "string", value: s }) }))(({ tag: "right", value: ({ tag: "number", value: bf }) }));
})())((_m as any).value);
    case "float32": return ((f: number) => ({ tag: "right", value: ({ tag: "string", value: LibLiterals.showFloat32(f) }) }))((_m as any).value);
    case "float64": return ((f: number) => (() => {
  const s = LibLiterals.showFloat64(f);
  return LibLogic.ifElse(isSpecialFloatString(s))(({ tag: "right", value: ({ tag: "string", value: s }) }))(({ tag: "right", value: ({ tag: "number", value: LibLiterals.float64ToBigfloat(f) }) }));
})())((_m as any).value);
  }
})();
}

export function encodeInteger<t0>(iv: Core.IntegerValue): t0 | JsonModel.Value {
  return (() => {
  const _m = iv;
  switch (_m.tag) {
    case "bigint": return ((bi: bigint) => ({ tag: "right", value: ({ tag: "string", value: LibLiterals.showBigint(bi) }) }))((_m as any).value);
    case "int64": return ((i: bigint) => ({ tag: "right", value: ({ tag: "string", value: LibLiterals.showInt64(i) }) }))((_m as any).value);
    case "uint32": return ((i: bigint) => ({ tag: "right", value: ({ tag: "string", value: LibLiterals.showUint32(i) }) }))((_m as any).value);
    case "uint64": return ((i: bigint) => ({ tag: "right", value: ({ tag: "string", value: LibLiterals.showUint64(i) }) }))((_m as any).value);
    case "int8": return ((i: number) => ({ tag: "right", value: ({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.int8ToBigint(i)) }) }))((_m as any).value);
    case "int16": return ((i: bigint) => ({ tag: "right", value: ({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.int16ToBigint(i)) }) }))((_m as any).value);
    case "int32": return ((i: number) => ({ tag: "right", value: ({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.int32ToBigint(i)) }) }))((_m as any).value);
    case "uint8": return ((i: bigint) => ({ tag: "right", value: ({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.uint8ToBigint(i)) }) }))((_m as any).value);
    case "uint16": return ((i: number) => ({ tag: "right", value: ({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.uint16ToBigint(i)) }) }))((_m as any).value);
  }
})();
}

export function encodeLiteral<t0>(lit: Core.Literal): t0 | JsonModel.Value {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "binary": return ((b: Uint8Array) => ({ tag: "right", value: ({ tag: "string", value: LibLiterals.binaryToString(b) }) }))((_m as any).value);
    case "boolean": return ((b: boolean) => ({ tag: "right", value: ({ tag: "boolean", value: b }) }))((_m as any).value);
    case "float": return ((f: Core.FloatValue) => encodeFloat(f))((_m as any).value);
    case "integer": return ((i: Core.IntegerValue) => encodeInteger(i))((_m as any).value);
    case "string": return ((s: string) => ({ tag: "right", value: ({ tag: "string", value: s }) }))((_m as any).value);
  }
})();
}

export function isSpecialFloatString(s: string): boolean {
  return LibLogic.or(LibEquality.equal(s)("NaN"))(LibLogic.or(LibEquality.equal(s)("Infinity"))(LibLogic.or(LibEquality.equal(s)("-Infinity"))(LibEquality.equal(s)("-0.0"))));
}

export function toJson(types: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Name) => ((x: Core.Type) => ((x: Core.Term) => string | JsonModel.Value))) {
  return ((tname: Core.Name) => ((typ: Core.Type) => ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const strippedTerm = Strip.deannotateTerm(term);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "literal": return ((_: Core.LiteralType) => (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => encodeLiteral(lit))((_m as any).value);
    default: return ({ tag: "left", value: "expected literal term" })(_m);
  }
})())((_m as any).value);
    case "list": return ((elemType: Core.Type) => (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "list": return ((terms: ReadonlyArray<Core.Term>) => (() => {
  const results = LibEithers.mapList(((t: Core.Term) => toJson(types)(tname)(elemType)(t)))(terms);
  return LibEithers.map(((vs: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: vs })))(results);
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected list term" })(_m);
  }
})())((_m as any).value);
    case "set": return ((elemType: Core.Type) => (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "set": return ((vals: ReadonlySet<Core.Term>) => (() => {
  const terms = LibSets.toList(vals);
  return (() => {
  const results = LibEithers.mapList(((t: Core.Term) => toJson(types)(tname)(elemType)(t)))(terms);
  return LibEithers.map(((vs: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: vs })))(results);
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected set term" })(_m);
  }
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
  return (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "maybe": return ((opt: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: ({ tag: "null" }) }))(((v: Core.Term) => (() => {
  const encoded = toJson(types)(tname)(innerType)(v);
  return LibLogic.ifElse(isNestedMaybe)(LibEithers.map(((ev: JsonModel.Value) => ({ tag: "array", value: [ev] })))(encoded))(encoded);
})()))(opt))((_m as any).value);
    default: return ({ tag: "left", value: "expected maybe term" })(_m);
  }
})();
})();
})())((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "record": return ((r: Core.Record) => (() => {
  const isSimpleMaybe = ((ftype: Core.Type) => (() => {
  const _m = Strip.deannotateType(ftype);
  switch (_m.tag) {
    case "maybe": return ((innerT: Core.Type) => (() => {
  const _m = Strip.deannotateType(innerT);
  switch (_m.tag) {
    case "maybe": return ((_: Core.Type) => false)((_m as any).value);
    default: return true(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})());
  return (() => {
  const encodeFieldWithType = ((ft: Core.FieldType) => ((f: Core.Field) => (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(f));
  return (() => {
  const fterm = ((_x) => _x.term)(f);
  return (() => {
  const ftype = ((_x) => _x.type)(ft);
  return LibLogic.ifElse(isSimpleMaybe(ftype))((() => {
  const _m = Strip.deannotateTerm(fterm);
  switch (_m.tag) {
    case "maybe": return ((opt: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: null }))(((v: Core.Term) => (() => {
  const innerType = (() => {
  const _m = Strip.deannotateType(ftype);
  switch (_m.tag) {
    case "maybe": return ((it: Core.Type) => it)((_m as any).value);
    default: return ftype(_m);
  }
})();
  return (() => {
  const encoded = toJson(types)(tname)(innerType)(v);
  return LibEithers.map(((ev: JsonModel.Value) => [fname, ev]))(encoded);
})();
})()))(opt))((_m as any).value);
    default: return ({ tag: "left", value: "expected maybe term for optional field" })(_m);
  }
})())((() => {
  const encoded = toJson(types)(tname)(ftype)(fterm);
  return LibEithers.map(((ev: JsonModel.Value) => [fname, ev]))(encoded);
})());
})();
})();
})()));
  return (() => {
  const fieldTypes = rt;
  return (() => {
  const fields = ((_x) => _x.fields)(r);
  return (() => {
  const encodedPairs = LibEithers.mapList(((ftf: readonly [Core.FieldType, Core.Field]) => encodeFieldWithType(LibPairs.first(ftf))(LibPairs.second(ftf))))(LibLists.zip(fieldTypes)(fields));
  return LibEithers.map(((pairs: ReadonlyArray<readonly [string, JsonModel.Value] | null>) => ({ tag: "object", value: LibMaps.fromList(LibMaybes.cat(pairs)) })))(encodedPairs);
})();
})();
})();
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record term" })(_m);
  }
})())((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  return (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(field));
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return (() => {
  const findFieldType = ((fts: ReadonlyArray<Core.FieldType>) => LibLogic.ifElse(LibLists.null_(fts))(({ tag: "left", value: LibStrings.cat(["unknown variant: ", fname]) }))(LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.name)(LibLists.head(fts))))(fname))(({ tag: "right", value: ((_x) => _x.type)(LibLists.head(fts)) }))(findFieldType(LibLists.tail(fts)))));
  return (() => {
  const ftypeResult = findFieldType(rt);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((ftype: Core.Type) => (() => {
  const encodedUnion = toJson(types)(tname)(ftype)(fterm);
  return LibEithers.map(((v: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([[fname, v]]) })))(encodedUnion);
})()))(ftypeResult);
})();
})();
})();
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union term" })(_m);
  }
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "object", value: LibMaps.empty }) }))((_m as any).value);
    case "wrap": return ((wn: Core.Type) => (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "wrap": return ((wt: Core.WrappedTerm) => toJson(types)(tname)(wn)(((_x) => _x.body)(wt)))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped term" })(_m);
  }
})())((_m as any).value);
    case "map": return ((mt: Core.MapType) => (() => {
  const keyType = ((_x) => _x.keys)(mt);
  return (() => {
  const valType = ((_x) => _x.values)(mt);
  return (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const encodeEntry = ((kv: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(kv);
  return (() => {
  const v = LibPairs.second(kv);
  return (() => {
  const encodedK = toJson(types)(tname)(keyType)(k);
  return (() => {
  const encodedV = toJson(types)(tname)(valType)(v);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((ek: JsonModel.Value) => LibEithers.map(((ev: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@key", ek], ["@value", ev]]) })))(encodedV)))(encodedK);
})();
})();
})();
})());
  return (() => {
  const entries = LibEithers.mapList(encodeEntry)(LibMaps.toList(m));
  return LibEithers.map(((es: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: es })))(entries);
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected map term" })(_m);
  }
})();
})();
})())((_m as any).value);
    case "pair": return ((pt: Core.PairType) => (() => {
  const firstType = ((_x) => _x.first)(pt);
  return (() => {
  const secondType = ((_x) => _x.second)(pt);
  return (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const first = LibPairs.first(p);
  return (() => {
  const second = LibPairs.second(p);
  return (() => {
  const encodedFirst = toJson(types)(tname)(firstType)(first);
  return (() => {
  const encodedSecond = toJson(types)(tname)(secondType)(second);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((ef: JsonModel.Value) => LibEithers.map(((es: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@first", ef], ["@second", es]]) })))(encodedSecond)))(encodedFirst);
})();
})();
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected pair term" })(_m);
  }
})();
})();
})())((_m as any).value);
    case "either": return ((et: Core.EitherType) => (() => {
  const leftType = ((_x) => _x.left)(et);
  return (() => {
  const rightType = ((_x) => _x.right)(et);
  return (() => {
  const _m = strippedTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => (() => {
  const encodedL = toJson(types)(tname)(leftType)(l);
  return LibEithers.map(((v: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@left", v]]) })))(encodedL);
})()))(((r: Core.Term) => (() => {
  const encodedR = toJson(types)(tname)(rightType)(r);
  return LibEithers.map(((v: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@right", v]]) })))(encodedR);
})()))(e))((_m as any).value);
    default: return ({ tag: "left", value: "expected either term" })(_m);
  }
})();
})();
})())((_m as any).value);
    case "variable": return ((name: Core.Name) => (() => {
  const lookedUp = LibMaps.lookup(name)(types);
  return LibMaybes.maybe(toJsonUntyped(term))(((resolvedType: Core.Type) => toJson(types)(name)(resolvedType)(term)))(lookedUp);
})())((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat(["unsupported type for JSON encoding: ", ShowCore.type(typ)]) })(_m);
  }
})();
})();
})())));
}

export function toJsonUntyped(term: Core.Term): string | JsonModel.Value {
  return (() => {
  const stripped = Strip.deannotateTerm(term);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => encodeLiteral(lit))((_m as any).value);
    case "list": return ((terms: ReadonlyArray<Core.Term>) => (() => {
  const results = LibEithers.mapList(((t: Core.Term) => toJsonUntyped(t)))(terms);
  return LibEithers.map(((vs: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: vs })))(results);
})())((_m as any).value);
    case "set": return ((vals: ReadonlySet<Core.Term>) => (() => {
  const terms = LibSets.toList(vals);
  return (() => {
  const results = LibEithers.mapList(((t: Core.Term) => toJsonUntyped(t)))(terms);
  return LibEithers.map(((vs: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: vs })))(results);
})();
})())((_m as any).value);
    case "maybe": return ((opt: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: ({ tag: "null" }) }))(((v: Core.Term) => (() => {
  const encodedMaybe = toJsonUntyped(v);
  return LibEithers.map(((encoded: JsonModel.Value) => ({ tag: "array", value: [encoded] })))(encodedMaybe);
})()))(opt))((_m as any).value);
    case "record": return ((r: Core.Record) => (() => {
  const encodeField = ((f: Core.Field) => (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(f));
  return (() => {
  const fterm = ((_x) => _x.term)(f);
  return (() => {
  const encodedField = toJsonUntyped(fterm);
  return LibEithers.map(((v: JsonModel.Value) => [fname, v]))(encodedField);
})();
})();
})());
  return (() => {
  const fields = ((_x) => _x.fields)(r);
  return (() => {
  const encodedFields = LibEithers.mapList(encodeField)(fields);
  return LibEithers.map(((fs: ReadonlyArray<readonly [string, JsonModel.Value]>) => ({ tag: "object", value: LibMaps.fromList(fs) })))(encodedFields);
})();
})();
})())((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  return (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(field));
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return (() => {
  const encodedUnion = toJsonUntyped(fterm);
  return LibEithers.map(((v: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([[fname, v]]) })))(encodedUnion);
})();
})();
})();
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "object", value: LibMaps.empty }) }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => toJsonUntyped(((_x) => _x.body)(wt)))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const encodeEntry = ((kv: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(kv);
  return (() => {
  const v = LibPairs.second(kv);
  return (() => {
  const encodedK = toJsonUntyped(k);
  return (() => {
  const encodedV = toJsonUntyped(v);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((ek: JsonModel.Value) => LibEithers.map(((ev: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@key", ek], ["@value", ev]]) })))(encodedV)))(encodedK);
})();
})();
})();
})());
  return (() => {
  const entries = LibEithers.mapList(encodeEntry)(LibMaps.toList(m));
  return LibEithers.map(((es: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: es })))(entries);
})();
})())((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const first = LibPairs.first(p);
  return (() => {
  const second = LibPairs.second(p);
  return (() => {
  const encodedFirst = toJsonUntyped(first);
  return (() => {
  const encodedSecond = toJsonUntyped(second);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((ef: JsonModel.Value) => LibEithers.map(((es: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@first", ef], ["@second", es]]) })))(encodedSecond)))(encodedFirst);
})();
})();
})();
})())((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => (() => {
  const encodedL = toJsonUntyped(l);
  return LibEithers.map(((v: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@left", v]]) })))(encodedL);
})()))(((r: Core.Term) => (() => {
  const encodedR = toJsonUntyped(r);
  return LibEithers.map(((v: JsonModel.Value) => ({ tag: "object", value: LibMaps.fromList([["@right", v]]) })))(encodedR);
})()))(e))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat(["unsupported term variant for JSON encoding: ", ShowCore.term(term)]) })(_m);
  }
})();
})();
}

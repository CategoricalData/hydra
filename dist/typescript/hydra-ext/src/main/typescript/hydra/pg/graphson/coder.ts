// Note: this is an automatically generated file. Do not edit.

/**
 * Encoding functions for converting GraphSON syntax to JSON.
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as LibLists from "../../lib/lists.js";
import * as LibLiterals from "../../lib/literals.js";
import * as LibLogic from "../../lib/logic.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibPairs from "../../lib/pairs.js";
import * as LibStrings from "../../lib/strings.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as PgGraphsonSyntax from "./syntax.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function adjacentEdgeToJson(out: boolean): ((x: PgGraphsonSyntax.AdjacentEdge) => JsonModel.Value) {
  return ((ae: PgGraphsonSyntax.AdjacentEdge) => toJsonObject([["id", valueToJson(((_x) => _x.id)(ae))], ["inV", LibLogic.ifElse(out)(valueToJson(((_x) => _x.vertexId)(ae)))(null)], ["outV", LibLogic.ifElse(out)(null)(valueToJson(((_x) => _x.vertexId)(ae)))], ["properties", edgePropertyMapToJson(((_x) => _x.properties)(ae))]]));
}

export function doubleValueToJson(v1: PgGraphsonSyntax.DoubleValue): JsonModel.Value {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "finite": return ((d: number) => ({ tag: "number", value: LibLiterals.float64ToBigfloat(d) }))((_m as any).value);
    case "infinity": return ((_: void) => ({ tag: "string", value: "Infinity" }))((_m as any).value);
    case "negativeInfinity": return ((_: void) => ({ tag: "string", value: "-Infinity" }))((_m as any).value);
    case "notANumber": return ((_: void) => ({ tag: "string", value: "NaN" }))((_m as any).value);
  }
})();
}

export function edgeMapToJson(out: boolean): ((x: ReadonlyMap<PgGraphsonSyntax.EdgeLabel, ReadonlyArray<PgGraphsonSyntax.AdjacentEdge>>) => JsonModel.Value | null) {
  return ((m: ReadonlyMap<PgGraphsonSyntax.EdgeLabel, ReadonlyArray<PgGraphsonSyntax.AdjacentEdge>>) => LibLogic.ifElse(LibMaps.null_(m))(null)(({ tag: "object", value: LibMaps.fromList(LibLists.map(((p: readonly [PgGraphsonSyntax.EdgeLabel, ReadonlyArray<PgGraphsonSyntax.AdjacentEdge>]) => [((_x) => _x)(LibPairs.first(p)), ({ tag: "array", value: LibLists.map(((v1: PgGraphsonSyntax.AdjacentEdge) => adjacentEdgeToJson(out)(v1)))(LibPairs.second(p)) })]))(LibMaps.toList(m))) })));
}

export function edgePropertyMapToJson(m: ReadonlyMap<PgGraphsonSyntax.PropertyKey, PgGraphsonSyntax.Value>): JsonModel.Value | null {
  return LibLogic.ifElse(LibMaps.null_(m))(null)(({ tag: "object", value: LibMaps.fromList(LibLists.map(((p: readonly [PgGraphsonSyntax.PropertyKey, PgGraphsonSyntax.Value]) => [((_x) => _x)(LibPairs.first(p)), valueToJson(LibPairs.second(p))]))(LibMaps.toList(m))) }));
}

export function floatValueToJson(v1: PgGraphsonSyntax.FloatValue): JsonModel.Value {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "finite": return ((f: number) => ({ tag: "number", value: LibLiterals.float32ToBigfloat(f) }))((_m as any).value);
    case "infinity": return ((_: void) => ({ tag: "string", value: "Infinity" }))((_m as any).value);
    case "negativeInfinity": return ((_: void) => ({ tag: "string", value: "-Infinity" }))((_m as any).value);
    case "notANumber": return ((_: void) => ({ tag: "string", value: "NaN" }))((_m as any).value);
  }
})();
}

export function mapToJson(m: PgGraphsonSyntax.Map): JsonModel.Value {
  return ({ tag: "array", value: LibLists.concat(LibLists.map(((vp: PgGraphsonSyntax.ValuePair) => [valueToJson(((_x) => _x.first)(vp)), valueToJson(((_x) => _x.second)(vp))]))(((_x) => _x)(m))) });
}

export function toJsonObject(pairs: ReadonlyArray<readonly [string, JsonModel.Value | null]>): JsonModel.Value {
  return ({ tag: "object", value: LibMaps.fromList(LibMaybes.cat(LibLists.map(((p: readonly [string, JsonModel.Value | null]) => LibMaybes.map(((v: JsonModel.Value) => [LibPairs.first(p), v]))(LibPairs.second(p))))(pairs))) });
}

export function typedValueToJson(typeName: string): ((x: JsonModel.Value) => JsonModel.Value) {
  return ((valueJson: JsonModel.Value) => toJsonObject([["@type", ({ tag: "string", value: typeName })], ["@value", valueJson]]));
}

export function valueToJson(v1: PgGraphsonSyntax.Value): JsonModel.Value {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigDecimal": return ((bd: PgGraphsonSyntax.BigDecimalValue) => typedValueToJson("g:BigDecimal")(({ tag: "string", value: ((_x) => _x)(bd) })))((_m as any).value);
    case "bigInteger": return ((i: bigint) => typedValueToJson("g:BigInteger")(({ tag: "number", value: LibLiterals.bigintToBigfloat(i) })))((_m as any).value);
    case "binary": return ((b: string) => typedValueToJson("g:Binary")(({ tag: "string", value: b })))((_m as any).value);
    case "boolean": return ((b: boolean) => ({ tag: "boolean", value: b }))((_m as any).value);
    case "byte": return ((b: bigint) => typedValueToJson("g:Byte")(({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.uint8ToBigint(b)) })))((_m as any).value);
    case "char": return ((c: bigint) => typedValueToJson("g:Char")(({ tag: "string", value: LibStrings.fromList(LibLists.pure(LibLiterals.bigintToInt32(LibLiterals.uint32ToBigint(c)))) })))((_m as any).value);
    case "composite": return ((ctv: PgGraphsonSyntax.CompositeTypedValue) => typedValueToJson(((_x) => _x)(((_x) => _x.type)(ctv)))(mapToJson(((_x) => _x.fields)(ctv))))((_m as any).value);
    case "dateTime": return ((dt: PgGraphsonSyntax.DateTime) => typedValueToJson("g:DateTime")(({ tag: "string", value: ((_x) => _x)(dt) })))((_m as any).value);
    case "double": return ((dv: PgGraphsonSyntax.DoubleValue) => typedValueToJson("g:Double")(doubleValueToJson(dv)))((_m as any).value);
    case "duration": return ((dur: PgGraphsonSyntax.Duration) => typedValueToJson("g:Duration")(({ tag: "string", value: ((_x) => _x)(dur) })))((_m as any).value);
    case "float": return ((fv: PgGraphsonSyntax.FloatValue) => typedValueToJson("g:Float")(floatValueToJson(fv)))((_m as any).value);
    case "integer": return ((i: number) => typedValueToJson("g:Int32")(({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.int32ToBigint(i)) })))((_m as any).value);
    case "list": return ((vals: ReadonlyArray<PgGraphsonSyntax.Value>) => typedValueToJson("g:List")(({ tag: "array", value: LibLists.map(valueToJson)(vals) })))((_m as any).value);
    case "long": return ((l: bigint) => typedValueToJson("g:Long")(({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.int64ToBigint(l)) })))((_m as any).value);
    case "map": return ((m: PgGraphsonSyntax.Map) => typedValueToJson("g:Map")(mapToJson(m)))((_m as any).value);
    case "null": return ((_: void) => ({ tag: "null" }))((_m as any).value);
    case "primitive": return ((ptv: PgGraphsonSyntax.PrimitiveTypedValue) => typedValueToJson("g:PrimitivePdt")(({ tag: "string", value: ((_x) => _x.value)(ptv) })))((_m as any).value);
    case "set": return ((vals: ReadonlyArray<PgGraphsonSyntax.Value>) => typedValueToJson("g:Set")(({ tag: "array", value: LibLists.map(valueToJson)(vals) })))((_m as any).value);
    case "short": return ((i: bigint) => typedValueToJson("g:Int16")(({ tag: "number", value: LibLiterals.bigintToBigfloat(LibLiterals.int16ToBigint(i)) })))((_m as any).value);
    case "string": return ((s: string) => ({ tag: "string", value: s }))((_m as any).value);
    case "uuid": return ((u: PgGraphsonSyntax.Uuid) => typedValueToJson("g:UUID")(({ tag: "string", value: ((_x) => _x)(u) })))((_m as any).value);
  }
})();
}

export function vertexPropertyMapToJson(m: ReadonlyMap<PgGraphsonSyntax.PropertyKey, ReadonlyArray<PgGraphsonSyntax.VertexPropertyValue>>): JsonModel.Value | null {
  return LibLogic.ifElse(LibMaps.null_(m))(null)(({ tag: "object", value: LibMaps.fromList(LibLists.map(((p: readonly [PgGraphsonSyntax.PropertyKey, ReadonlyArray<PgGraphsonSyntax.VertexPropertyValue>]) => [((_x) => _x)(LibPairs.first(p)), ({ tag: "array", value: LibLists.map(vertexPropertyValueToJson)(LibPairs.second(p)) })]))(LibMaps.toList(m))) }));
}

export function vertexPropertyValueToJson(vpv: PgGraphsonSyntax.VertexPropertyValue): JsonModel.Value {
  return toJsonObject([["id", LibMaybes.map(valueToJson)(((_x) => _x.id)(vpv))], ["value", valueToJson(((_x) => _x.value)(vpv))]]);
}

export function vertexToJson(v: PgGraphsonSyntax.Vertex): JsonModel.Value {
  return toJsonObject([["id", valueToJson(((_x) => _x.id)(v))], ["label", LibMaybes.map(((lbl: PgGraphsonSyntax.VertexLabel) => ({ tag: "string", value: ((_x) => _x)(lbl) })))(((_x) => _x.label)(v))], ["inE", edgeMapToJson(false)(((_x) => _x.inEdges)(v))], ["outE", edgeMapToJson(true)(((_x) => _x.outEdges)(v))], ["properties", vertexPropertyMapToJson(((_x) => _x.properties)(v))]]);
}

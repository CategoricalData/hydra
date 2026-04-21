// Note: this is an automatically generated file. Do not edit.

/**
 * Decoding functions for JSON data
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
import * as LibEithers from "../lib/eithers.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function decodeArray<t0>(decodeElem: ((x: JsonModel.Value) => string | t0)): ((x: JsonModel.Value) => string | ReadonlyArray<t0>) {
  return ((v1: JsonModel.Value) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "array": return ((a: ReadonlyArray<JsonModel.Value>) => LibEithers.mapList(decodeElem)(a))((_m as any).value);
    default: return ({ tag: "left", value: "expected an array" })(_m);
  }
})());
}

export function decodeBoolean(v1: JsonModel.Value): string | boolean {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "boolean": return ((b: boolean) => ({ tag: "right", value: b }))((_m as any).value);
    default: return ({ tag: "left", value: "expected a boolean" })(_m);
  }
})();
}

export function decodeField<t0, t1>(decodeValue: ((x: t0) => string | t1)): ((x: string) => ((x: ReadonlyMap<string, t0>) => string | t1)) {
  return ((name: string) => ((m: ReadonlyMap<string, t0>) => LibEithers.bind(decodeOptionalField(decodeValue)(name)(m))(((v1) => LibMaybes.maybe(({ tag: "left", value: LibStrings.cat2("missing field: ")(name) }))(((f: t1) => ({ tag: "right", value: f })))(v1)))));
}

export function decodeObject(v1: JsonModel.Value): string | ReadonlyMap<string, JsonModel.Value> {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "object": return ((o: ReadonlyMap<string, JsonModel.Value>) => ({ tag: "right", value: o }))((_m as any).value);
    default: return ({ tag: "left", value: "expected an object" })(_m);
  }
})();
}

export function decodeOptionalField<t0, t1, t2, t3>(decodeValue: ((x: t0) => t1 | t2)): ((x: t3) => ((x: ReadonlyMap<t3, t0>) => t1 | t2 | null)) {
  return ((name: t3) => ((m: ReadonlyMap<t3, t0>) => LibMaybes.maybe(({ tag: "right", value: null }))(((v: t0) => LibEithers.map(((x: t2) => x))(decodeValue(v))))(LibMaps.lookup(name)(m))));
}

export function decodeString(v1: JsonModel.Value): string | string {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected a string" })(_m);
  }
})();
}

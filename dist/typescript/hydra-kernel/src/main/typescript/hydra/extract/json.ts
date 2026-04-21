// Note: this is an automatically generated file. Do not edit.

/**
 * Utilities for extracting values from JSON objects
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

export function expectArray(value: JsonModel.Value): string | ReadonlyArray<JsonModel.Value> {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "array": return ((els: ReadonlyArray<JsonModel.Value>) => ({ tag: "right", value: els }))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat2(LibStrings.cat2("expected ")("JSON array"))(LibStrings.cat2(" but found ")(showValue(value))) })(_m);
  }
})();
}

export function expectNumber(value: JsonModel.Value): string | number {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "number": return ((d: number) => ({ tag: "right", value: d }))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat2(LibStrings.cat2("expected ")("JSON number"))(LibStrings.cat2(" but found ")(showValue(value))) })(_m);
  }
})();
}

export function expectObject(value: JsonModel.Value): string | ReadonlyMap<string, JsonModel.Value> {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "object": return ((m: ReadonlyMap<string, JsonModel.Value>) => ({ tag: "right", value: m }))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat2(LibStrings.cat2("expected ")("JSON object"))(LibStrings.cat2(" but found ")(showValue(value))) })(_m);
  }
})();
}

export function expectString(value: JsonModel.Value): string | string {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: LibStrings.cat2(LibStrings.cat2("expected ")("JSON string"))(LibStrings.cat2(" but found ")(showValue(value))) })(_m);
  }
})();
}

export function opt<t0, t1>(fname: t0): ((x: ReadonlyMap<t0, t1>) => t1 | null) {
  return ((m: ReadonlyMap<t0, t1>) => LibMaps.lookup(fname)(m));
}

export function optArray<t0>(fname: t0): ((x: ReadonlyMap<t0, JsonModel.Value>) => string | ReadonlyArray<JsonModel.Value> | null) {
  return ((m: ReadonlyMap<t0, JsonModel.Value>) => LibMaybes.maybe(({ tag: "right", value: null }))(((a: JsonModel.Value) => LibEithers.map(((x: ReadonlyArray<JsonModel.Value>) => x))(expectArray(a))))(opt(fname)(m)));
}

export function optString<t0>(fname: t0): ((x: ReadonlyMap<t0, JsonModel.Value>) => string | string | null) {
  return ((m: ReadonlyMap<t0, JsonModel.Value>) => LibMaybes.maybe(({ tag: "right", value: null }))(((s: JsonModel.Value) => LibEithers.map(((x: string) => x))(expectString(s))))(opt(fname)(m)));
}

export function require<t0, t1>(fname: t0): ((x: ReadonlyMap<t0, t1>) => string | t1) {
  return ((m: ReadonlyMap<t0, t1>) => LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["required attribute ", showValue(fname), " not found"]) }))(((value: t1) => ({ tag: "right", value: value })))(LibMaps.lookup(fname)(m)));
}

export function requireArray<t0>(fname: t0): ((x: ReadonlyMap<t0, JsonModel.Value>) => string | ReadonlyArray<JsonModel.Value>) {
  return ((m: ReadonlyMap<t0, JsonModel.Value>) => LibEithers.bind(require(fname)(m))(expectArray));
}

export function requireNumber<t0>(fname: t0): ((x: ReadonlyMap<t0, JsonModel.Value>) => string | number) {
  return ((m: ReadonlyMap<t0, JsonModel.Value>) => LibEithers.bind(require(fname)(m))(expectNumber));
}

export function requireString<t0>(fname: t0): ((x: ReadonlyMap<t0, JsonModel.Value>) => string | string) {
  return ((m: ReadonlyMap<t0, JsonModel.Value>) => LibEithers.bind(require(fname)(m))(expectString));
}

export function showValue<t0>(value: t0): string {
  return "TODO: implement showValue";
}

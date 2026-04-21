// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.json.model
 */



import * as Core from "../../core.js";
import * as DecodeCore from "../core.js";
import * as Errors from "../../errors.js";
import * as ExtractCore from "../../extract/core.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as Lexical from "../../lexical.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibStrings from "../../lib/strings.js";
import * as Rewriting from "../../rewriting.js";
import * as Util from "../../util.js";

export function value(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | JsonModel.Value) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["array", ((input: Core.Term) => LibEithers.map(((t: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: t })))(ExtractCore.decodeList(value)(cx)(input)))], ["boolean", ((input: Core.Term) => LibEithers.map(((t: boolean) => ({ tag: "boolean", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "boolean": return ((b: boolean) => ({ tag: "right", value: b }))((_m as any).value);
    default: return ({ tag: "left", value: "expected boolean literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["null", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "null", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["number", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "number", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "float": return ((v1: Core.FloatValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigfloat": return ((f: number) => ({ tag: "right", value: f }))((_m as any).value);
    default: return ({ tag: "left", value: "expected bigfloat value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected bigfloat literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["object", ((input: Core.Term) => LibEithers.map(((t: ReadonlyMap<string, JsonModel.Value>) => ({ tag: "object", value: t })))(ExtractCore.decodeMap(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected string literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(value)(cx)(input)))], ["string", ((input: Core.Term) => LibEithers.map(((t: string) => ({ tag: "string", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected string literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | JsonModel.Value)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

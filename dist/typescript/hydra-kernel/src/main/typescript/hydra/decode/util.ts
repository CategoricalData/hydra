// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.util
 */



import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function caseConvention(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Util.CaseConvention) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["camel", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "camel", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["pascal", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "pascal", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["lowerSnake", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "lowerSnake", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["upperSnake", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "upperSnake", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Util.CaseConvention)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function comparison(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Util.Comparison) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["lessThan", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "lessThan", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["equalTo", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "equalTo", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["greaterThan", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "greaterThan", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Util.Comparison)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function precision(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Util.Precision) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["arbitrary", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "arbitrary", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["bits", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "bits", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "int32": return ((i: number) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected int32 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected int32 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Util.Precision)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.variants
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
import * as Variants from "../variants.js";

export function eliminationVariant(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Variants.EliminationVariant) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["record", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "record", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["union", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "union", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["wrap", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "wrap", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Variants.EliminationVariant)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function functionVariant(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Variants.FunctionVariant) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["elimination", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "elimination", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["lambda", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "lambda", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Variants.FunctionVariant)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function literalVariant(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Variants.LiteralVariant) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["binary", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "binary", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["boolean", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "boolean", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["float", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "float", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["integer", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "integer", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["string", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "string", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Variants.LiteralVariant)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function termVariant(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Variants.TermVariant) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["annotated", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "annotated", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["application", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "application", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["cases", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "cases", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["either", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "either", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["inject", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "inject", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["lambda", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "lambda", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["let", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "let", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["list", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "list", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["literal", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "literal", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["map", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "map", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["maybe", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "maybe", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["pair", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "pair", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["project", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "project", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["record", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "record", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["set", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "set", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["typeApplication", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "typeApplication", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["typeLambda", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "typeLambda", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["unit", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "unit", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["unwrap", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "unwrap", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["variable", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "variable", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["wrap", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "wrap", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Variants.TermVariant)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeVariant(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Variants.TypeVariant) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["annotated", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "annotated", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["application", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "application", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["either", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "either", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["forall", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "forall", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["function", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "function", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["list", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "list", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["literal", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "literal", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["map", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "map", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["maybe", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "maybe", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["pair", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "pair", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["record", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "record", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["set", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "set", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["union", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "union", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["unit", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "unit", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["variable", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "variable", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["void", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "void", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["wrap", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "wrap", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Variants.TypeVariant)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

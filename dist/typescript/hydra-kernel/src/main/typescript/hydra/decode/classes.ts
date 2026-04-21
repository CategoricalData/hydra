// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.classes
 */



import * as Classes from "../classes.js";
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

export function typeClass(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Classes.TypeClass) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["equality", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "equality", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["ordering", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "ordering", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Classes.TypeClass)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

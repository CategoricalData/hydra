// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.phantoms
 */



import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as Phantoms from "../phantoms.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function tBinding<t0, t1>(a: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Phantoms.TBinding<t1>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("term")(((v1: Graph.Graph) => ((v2: Core.Term) => tTerm(a)(v1)(v2))))(fieldMap)(cx))(((field_term: Phantoms.TTerm<t1>) => ({ tag: "right", value: ({
    name: field_name,
    term: field_term
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function tTerm<t0, t1>(a: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Phantoms.TTerm<t1>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: Core.Term) => b))(DecodeCore.term(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function tTermDefinition<t0, t1>(a: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Phantoms.TTermDefinition<t1>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("term")(((v1: Graph.Graph) => ((v2: Core.Term) => tTerm(a)(v1)(v2))))(fieldMap)(cx))(((field_term: Phantoms.TTerm<t1>) => ({ tag: "right", value: ({
    name: field_name,
    term: field_term
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

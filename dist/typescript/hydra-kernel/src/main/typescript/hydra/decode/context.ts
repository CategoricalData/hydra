// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.context
 */



import * as Context from "../context.js";
import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function context(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Context.Context) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("trace")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(v1)(v2))))(fieldMap)(cx))(((field_trace: ReadonlyArray<string>) => LibEithers.bind(ExtractCore.requireField("messages")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(v1)(v2))))(fieldMap)(cx))(((field_messages: ReadonlyArray<string>) => LibEithers.bind(ExtractCore.requireField("other")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(DecodeCore.name)(DecodeCore.term)(v1)(v2))))(fieldMap)(cx))(((field_other: ReadonlyMap<Core.Name, Core.Term>) => ({ tag: "right", value: ({
    trace: field_trace,
    messages: field_messages,
    other: field_other
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function inContext<t0>(e: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Context.InContext<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("object")(e)(fieldMap)(cx))(((field_object: t0) => LibEithers.bind(ExtractCore.requireField("context")(context)(fieldMap)(cx))(((field_context: Context.Context) => ({ tag: "right", value: ({
    object: field_object,
    context: field_context
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

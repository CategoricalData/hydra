// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.topology
 */



import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as Rewriting from "../rewriting.js";
import * as Topology from "../topology.js";
import * as Util from "../util.js";

export function graph(v1: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ReadonlyMap<number, ReadonlyArray<number>>) {
  return ((v2: Core.Term) => ExtractCore.decodeMap(vertex)(((v12: Graph.Graph) => ((v22: Core.Term) => ExtractCore.decodeList(vertex)(v12)(v22))))(v1)(v2));
}

export function tarjanState(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Topology.TarjanState) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("counter")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_counter: number) => LibEithers.bind(ExtractCore.requireField("indices")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(vertex)(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v12: Core.IntegerValue) => (() => {
  const _m = v12;
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(v1)(v2))))(fieldMap)(cx))(((field_indices: ReadonlyMap<number, number>) => LibEithers.bind(ExtractCore.requireField("lowLinks")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(vertex)(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v12: Core.IntegerValue) => (() => {
  const _m = v12;
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(v1)(v2))))(fieldMap)(cx))(((field_lowLinks: ReadonlyMap<number, number>) => LibEithers.bind(ExtractCore.requireField("stack")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(vertex)(v1)(v2))))(fieldMap)(cx))(((field_stack: ReadonlyArray<number>) => LibEithers.bind(ExtractCore.requireField("onStack")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeSet(vertex)(v1)(v2))))(fieldMap)(cx))(((field_onStack: ReadonlySet<number>) => LibEithers.bind(ExtractCore.requireField("sccs")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => ExtractCore.decodeList(vertex)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_sccs: ReadonlyArray<ReadonlyArray<number>>) => ({ tag: "right", value: ({
    counter: field_counter,
    indices: field_indices,
    lowLinks: field_lowLinks,
    stack: field_stack,
    onStack: field_onStack,
    sccs: field_sccs
  }) })))))))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function vertex(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | number) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
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
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

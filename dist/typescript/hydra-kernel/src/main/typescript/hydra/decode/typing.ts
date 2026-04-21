// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.typing
 */



import * as Core from "../core.js";
import * as DecodeContext from "./context.js";
import * as DecodeCore from "./core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as Rewriting from "../rewriting.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";

export function functionStructure<t0>(env: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Typing.FunctionStructure<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("typeParams")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(DecodeCore.name)(v1)(v2))))(fieldMap)(cx))(((field_typeParams: ReadonlyArray<Core.Name>) => LibEithers.bind(ExtractCore.requireField("params")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(DecodeCore.name)(v1)(v2))))(fieldMap)(cx))(((field_params: ReadonlyArray<Core.Name>) => LibEithers.bind(ExtractCore.requireField("bindings")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(DecodeCore.binding)(v1)(v2))))(fieldMap)(cx))(((field_bindings: ReadonlyArray<Core.Binding>) => LibEithers.bind(ExtractCore.requireField("body")(DecodeCore.term)(fieldMap)(cx))(((field_body: Core.Term) => LibEithers.bind(ExtractCore.requireField("domains")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(DecodeCore.type)(v1)(v2))))(fieldMap)(cx))(((field_domains: ReadonlyArray<Core.Type>) => LibEithers.bind(ExtractCore.requireField("codomain")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(DecodeCore.type)(v1)(v2))))(fieldMap)(cx))(((field_codomain: Core.Type | null) => LibEithers.bind(ExtractCore.requireField("environment")(env)(fieldMap)(cx))(((field_environment: t0) => ({ tag: "right", value: ({
    typeParams: field_typeParams,
    params: field_params,
    bindings: field_bindings,
    body: field_body,
    domains: field_domains,
    codomain: field_codomain,
    environment: field_environment
  }) })))))))))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function inferenceResult(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Typing.InferenceResult) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("term")(DecodeCore.term)(fieldMap)(cx))(((field_term: Core.Term) => LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.type)(fieldMap)(cx))(((field_type: Core.Type) => LibEithers.bind(ExtractCore.requireField("subst")(typeSubst)(fieldMap)(cx))(((field_subst: Typing.TypeSubst) => LibEithers.bind(ExtractCore.requireField("classConstraints")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(DecodeCore.name)(DecodeCore.typeVariableMetadata)(v1)(v2))))(fieldMap)(cx))(((field_classConstraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => LibEithers.bind(ExtractCore.requireField("context")(DecodeContext.context)(fieldMap)(cx))(((field_context: hydra.context.Context) => ({ tag: "right", value: ({
    term: field_term,
    type: field_type,
    subst: field_subst,
    classConstraints: field_classConstraints,
    context: field_context
  }) })))))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function termSubst(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Typing.TermSubst) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyMap<Core.Name, Core.Term>) => b))(ExtractCore.decodeMap(DecodeCore.name)(DecodeCore.term)(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeConstraint(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Typing.TypeConstraint) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("left")(DecodeCore.type)(fieldMap)(cx))(((field_left: Core.Type) => LibEithers.bind(ExtractCore.requireField("right")(DecodeCore.type)(fieldMap)(cx))(((field_right: Core.Type) => LibEithers.bind(ExtractCore.requireField("comment")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_comment: string) => ({ tag: "right", value: ({
    left: field_left,
    right: field_right,
    comment: field_comment
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeSubst(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Typing.TypeSubst) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyMap<Core.Name, Core.Type>) => b))(ExtractCore.decodeMap(DecodeCore.name)(DecodeCore.type)(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.paths
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
import * as Paths from "../paths.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function subtermEdge(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtermEdge) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("source")(subtermNode)(fieldMap)(cx))(((field_source: Paths.SubtermNode) => LibEithers.bind(ExtractCore.requireField("path")(subtermPath)(fieldMap)(cx))(((field_path: Paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("target")(subtermNode)(fieldMap)(cx))(((field_target: Paths.SubtermNode) => ({ tag: "right", value: ({
    source: field_source,
    path: field_path,
    target: field_target
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtermGraph(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtermGraph) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("nodes")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(subtermNode)(v1)(v2))))(fieldMap)(cx))(((field_nodes: ReadonlyArray<Paths.SubtermNode>) => LibEithers.bind(ExtractCore.requireField("edges")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(subtermEdge)(v1)(v2))))(fieldMap)(cx))(((field_edges: ReadonlyArray<Paths.SubtermEdge>) => ({ tag: "right", value: ({
    nodes: field_nodes,
    edges: field_edges
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtermNode(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtermNode) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("label")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_label: string) => LibEithers.bind(ExtractCore.requireField("id")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_id: string) => ({ tag: "right", value: ({
    name: field_name,
    label: field_label,
    id: field_id
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtermPath(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtermPath) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyArray<Paths.SubtermStep>) => b))(ExtractCore.decodeList(subtermStep)(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtermStep(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtermStep) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["annotatedBody", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "annotatedBody", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["applicationFunction", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "applicationFunction", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["applicationArgument", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "applicationArgument", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["lambdaBody", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "lambdaBody", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["unionCasesDefault", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "unionCasesDefault", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["unionCasesBranch", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "unionCasesBranch", value: t })))(DecodeCore.name(cx)(input)))], ["letBody", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "letBody", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["letBinding", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "letBinding", value: t })))(DecodeCore.name(cx)(input)))], ["listElement", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "listElement", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["mapKey", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "mapKey", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["mapValue", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "mapValue", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["maybeTerm", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "maybeTerm", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["productTerm", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "productTerm", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["recordField", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "recordField", value: t })))(DecodeCore.name(cx)(input)))], ["setElement", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "setElement", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["sumTerm", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "sumTerm", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["typeLambdaBody", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "typeLambdaBody", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["typeApplicationTerm", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "typeApplicationTerm", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["injectionTerm", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "injectionTerm", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["wrappedTerm", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "wrappedTerm", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Paths.SubtermStep)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtypeEdge(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtypeEdge) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("source")(subtypeNode)(fieldMap)(cx))(((field_source: Paths.SubtypeNode) => LibEithers.bind(ExtractCore.requireField("path")(subtypePath)(fieldMap)(cx))(((field_path: Paths.SubtypePath) => LibEithers.bind(ExtractCore.requireField("target")(subtypeNode)(fieldMap)(cx))(((field_target: Paths.SubtypeNode) => ({ tag: "right", value: ({
    source: field_source,
    path: field_path,
    target: field_target
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtypeGraph(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtypeGraph) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("nodes")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(subtypeNode)(v1)(v2))))(fieldMap)(cx))(((field_nodes: ReadonlyArray<Paths.SubtypeNode>) => LibEithers.bind(ExtractCore.requireField("edges")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(subtypeEdge)(v1)(v2))))(fieldMap)(cx))(((field_edges: ReadonlyArray<Paths.SubtypeEdge>) => ({ tag: "right", value: ({
    nodes: field_nodes,
    edges: field_edges
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtypeNode(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtypeNode) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("label")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_label: string) => LibEithers.bind(ExtractCore.requireField("id")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_id: string) => ({ tag: "right", value: ({
    name: field_name,
    label: field_label,
    id: field_id
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtypePath(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtypePath) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyArray<Paths.SubtypeStep>) => b))(ExtractCore.decodeList(subtypeStep)(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function subtypeStep(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Paths.SubtypeStep) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["annotatedBody", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "annotatedBody", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["applicationFunction", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "applicationFunction", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["applicationArgument", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "applicationArgument", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["eitherLeft", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "eitherLeft", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["eitherRight", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "eitherRight", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["forallBody", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "forallBody", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["functionDomain", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "functionDomain", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["functionCodomain", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "functionCodomain", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["listElement", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "listElement", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["mapKeys", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "mapKeys", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["mapValues", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "mapValues", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["maybeElement", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "maybeElement", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["pairFirst", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "pairFirst", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["pairSecond", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "pairSecond", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["recordField", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "recordField", value: t })))(DecodeCore.name(cx)(input)))], ["setElement", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "setElement", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["unionField", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "unionField", value: t })))(DecodeCore.name(cx)(input)))], ["wrappedType", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "wrappedType", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Paths.SubtypeStep)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

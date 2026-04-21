// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.packaging
 */



import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as DecodeGraph from "./graph.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function definition(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.Definition) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["term", ((input: Core.Term) => LibEithers.map(((t: Packaging.TermDefinition) => ({ tag: "term", value: t })))(termDefinition(cx)(input)))], ["type", ((input: Core.Term) => LibEithers.map(((t: Packaging.TypeDefinition) => ({ tag: "type", value: t })))(typeDefinition(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Packaging.Definition)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function fileExtension(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.FileExtension) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: string) => b))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(((_x) => _x.body)(wrappedTerm)))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function module(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.Module) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("namespace")(namespace)(fieldMap)(cx))(((field_namespace: Packaging.Namespace) => LibEithers.bind(ExtractCore.requireField("definitions")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(definition)(v1)(v2))))(fieldMap)(cx))(((field_definitions: ReadonlyArray<Packaging.Definition>) => LibEithers.bind(ExtractCore.requireField("termDependencies")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(namespace)(v1)(v2))))(fieldMap)(cx))(((field_termDependencies: ReadonlyArray<Packaging.Namespace>) => LibEithers.bind(ExtractCore.requireField("typeDependencies")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(namespace)(v1)(v2))))(fieldMap)(cx))(((field_typeDependencies: ReadonlyArray<Packaging.Namespace>) => LibEithers.bind(ExtractCore.requireField("description")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(v1)(v2))))(fieldMap)(cx))(((field_description: string | null) => ({ tag: "right", value: ({
    namespace: field_namespace,
    definitions: field_definitions,
    termDependencies: field_termDependencies,
    typeDependencies: field_typeDependencies,
    description: field_description
  }) })))))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function namespace(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.Namespace) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: string) => b))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(((_x) => _x.body)(wrappedTerm)))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function namespaces<t0>(n: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Packaging.Namespaces<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("focus")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodePair(namespace)(n)(v1)(v2))))(fieldMap)(cx))(((field_focus: readonly [Packaging.Namespace, t0]) => LibEithers.bind(ExtractCore.requireField("mapping")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(namespace)(n)(v1)(v2))))(fieldMap)(cx))(((field_mapping: ReadonlyMap<Packaging.Namespace, t0>) => ({ tag: "right", value: ({
    focus: field_focus,
    mapping: field_mapping
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function package_(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.Package) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(packageName)(fieldMap)(cx))(((field_name: Packaging.PackageName) => LibEithers.bind(ExtractCore.requireField("modules")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(module)(v1)(v2))))(fieldMap)(cx))(((field_modules: ReadonlyArray<Packaging.Module>) => LibEithers.bind(ExtractCore.requireField("dependencies")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(packageName)(v1)(v2))))(fieldMap)(cx))(((field_dependencies: ReadonlyArray<Packaging.PackageName>) => LibEithers.bind(ExtractCore.requireField("description")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(v1)(v2))))(fieldMap)(cx))(((field_description: string | null) => ({ tag: "right", value: ({
    name: field_name,
    modules: field_modules,
    dependencies: field_dependencies,
    description: field_description
  }) })))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function packageName(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.PackageName) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: string) => b))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(((_x) => _x.body)(wrappedTerm)))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function qualifiedName(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.QualifiedName) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("namespace")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(namespace)(v1)(v2))))(fieldMap)(cx))(((field_namespace: Packaging.Namespace | null) => LibEithers.bind(ExtractCore.requireField("local")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_local: string) => ({ tag: "right", value: ({
    namespace: field_namespace,
    local: field_local
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function termDefinition(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.TermDefinition) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("term")(DecodeCore.term)(fieldMap)(cx))(((field_term: Core.Term) => LibEithers.bind(ExtractCore.requireField("type")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(DecodeCore.typeScheme)(v1)(v2))))(fieldMap)(cx))(((field_type: Core.TypeScheme | null) => ({ tag: "right", value: ({
    name: field_name,
    term: field_term,
    type: field_type
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeDefinition(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Packaging.TypeDefinition) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.typeScheme)(fieldMap)(cx))(((field_type: Core.TypeScheme) => ({ tag: "right", value: ({
    name: field_name,
    type: field_type
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.pg.model
 */



import * as Core from "../../core.js";
import * as DecodeCore from "../core.js";
import * as Errors from "../../errors.js";
import * as ExtractCore from "../../extract/core.js";
import * as Graph from "../../graph.js";
import * as Lexical from "../../lexical.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibStrings from "../../lib/strings.js";
import * as PgModel from "../../pg/model.js";
import * as Rewriting from "../../rewriting.js";
import * as Util from "../../util.js";

export function adjacentEdge<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.AdjacentEdge<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("label")(edgeLabel)(fieldMap)(cx))(((field_label: PgModel.EdgeLabel) => LibEithers.bind(ExtractCore.requireField("id")(v)(fieldMap)(cx))(((field_id: t0) => LibEithers.bind(ExtractCore.requireField("vertex")(v)(fieldMap)(cx))(((field_vertex: t0) => LibEithers.bind(ExtractCore.requireField("properties")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(propertyKey)(v)(v1)(v2))))(fieldMap)(cx))(((field_properties: ReadonlyMap<PgModel.PropertyKey, t0>) => ({ tag: "right", value: ({
    label: field_label,
    id: field_id,
    vertex: field_vertex,
    properties: field_properties
  }) })))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function direction(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | PgModel.Direction) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["out", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "out", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["in", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "in", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["both", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "both", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["undirected", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "undirected", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | PgModel.Direction)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function edge<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.Edge<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("label")(edgeLabel)(fieldMap)(cx))(((field_label: PgModel.EdgeLabel) => LibEithers.bind(ExtractCore.requireField("id")(v)(fieldMap)(cx))(((field_id: t0) => LibEithers.bind(ExtractCore.requireField("out")(v)(fieldMap)(cx))(((field_out: t0) => LibEithers.bind(ExtractCore.requireField("in")(v)(fieldMap)(cx))(((field_in: t0) => LibEithers.bind(ExtractCore.requireField("properties")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(propertyKey)(v)(v1)(v2))))(fieldMap)(cx))(((field_properties: ReadonlyMap<PgModel.PropertyKey, t0>) => ({ tag: "right", value: ({
    label: field_label,
    id: field_id,
    out: field_out,
    in: field_in,
    properties: field_properties
  }) })))))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function edgeLabel(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | PgModel.EdgeLabel) {
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

export function edgeType<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.EdgeType<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("label")(edgeLabel)(fieldMap)(cx))(((field_label: PgModel.EdgeLabel) => LibEithers.bind(ExtractCore.requireField("id")(t)(fieldMap)(cx))(((field_id: t0) => LibEithers.bind(ExtractCore.requireField("out")(vertexLabel)(fieldMap)(cx))(((field_out: PgModel.VertexLabel) => LibEithers.bind(ExtractCore.requireField("in")(vertexLabel)(fieldMap)(cx))(((field_in: PgModel.VertexLabel) => LibEithers.bind(ExtractCore.requireField("properties")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => propertyType(t)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_properties: ReadonlyArray<PgModel.PropertyType<t0>>) => ({ tag: "right", value: ({
    label: field_label,
    id: field_id,
    out: field_out,
    in: field_in,
    properties: field_properties
  }) })))))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function element<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.Element<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["vertex", ((input: Core.Term) => LibEithers.map(((t: PgModel.Vertex<t0>) => ({ tag: "vertex", value: t })))(vertex(v)(cx)(input)))], ["edge", ((input: Core.Term) => LibEithers.map(((t: PgModel.Edge<t0>) => ({ tag: "edge", value: t })))(edge(v)(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | PgModel.Element<t0>)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function elementKind(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | PgModel.ElementKind) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["vertex", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "vertex", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["edge", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "edge", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | PgModel.ElementKind)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function elementTree<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.ElementTree<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("self")(((v1: Graph.Graph) => ((v2: Core.Term) => element(v)(v1)(v2))))(fieldMap)(cx))(((field_self: PgModel.Element<t0>) => LibEithers.bind(ExtractCore.requireField("dependencies")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => elementTree(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_dependencies: ReadonlyArray<PgModel.ElementTree<t0>>) => ({ tag: "right", value: ({
    self: field_self,
    dependencies: field_dependencies
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function elementType<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.ElementType<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["vertex", ((input: Core.Term) => LibEithers.map(((t2: PgModel.VertexType<t0>) => ({ tag: "vertex", value: t2 })))(vertexType(t)(cx)(input)))], ["edge", ((input: Core.Term) => LibEithers.map(((t2: PgModel.EdgeType<t0>) => ({ tag: "edge", value: t2 })))(edgeType(t)(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | PgModel.ElementType<t0>)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function elementTypeTree<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.ElementTypeTree<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("self")(((v1: Graph.Graph) => ((v2: Core.Term) => elementType(t)(v1)(v2))))(fieldMap)(cx))(((field_self: PgModel.ElementType<t0>) => LibEithers.bind(ExtractCore.requireField("dependencies")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => elementTypeTree(t)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_dependencies: ReadonlyArray<PgModel.ElementTypeTree<t0>>) => ({ tag: "right", value: ({
    self: field_self,
    dependencies: field_dependencies
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function graph<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.Graph<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("vertices")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(v)(((v12: Graph.Graph) => ((v22: Core.Term) => vertex(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_vertices: ReadonlyMap<t0, PgModel.Vertex<t0>>) => LibEithers.bind(ExtractCore.requireField("edges")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(v)(((v12: Graph.Graph) => ((v22: Core.Term) => edge(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_edges: ReadonlyMap<t0, PgModel.Edge<t0>>) => ({ tag: "right", value: ({
    vertices: field_vertices,
    edges: field_edges
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function graphSchema<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.GraphSchema<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("vertices")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(vertexLabel)(((v12: Graph.Graph) => ((v22: Core.Term) => vertexType(t)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_vertices: ReadonlyMap<PgModel.VertexLabel, PgModel.VertexType<t0>>) => LibEithers.bind(ExtractCore.requireField("edges")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(edgeLabel)(((v12: Graph.Graph) => ((v22: Core.Term) => edgeType(t)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_edges: ReadonlyMap<PgModel.EdgeLabel, PgModel.EdgeType<t0>>) => ({ tag: "right", value: ({
    vertices: field_vertices,
    edges: field_edges
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function label(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | PgModel.Label) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["vertex", ((input: Core.Term) => LibEithers.map(((t: PgModel.VertexLabel) => ({ tag: "vertex", value: t })))(vertexLabel(cx)(input)))], ["edge", ((input: Core.Term) => LibEithers.map(((t: PgModel.EdgeLabel) => ({ tag: "edge", value: t })))(edgeLabel(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | PgModel.Label)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function lazyGraph<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.LazyGraph<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("vertices")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => vertex(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_vertices: ReadonlyArray<PgModel.Vertex<t0>>) => LibEithers.bind(ExtractCore.requireField("edges")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => edge(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_edges: ReadonlyArray<PgModel.Edge<t0>>) => ({ tag: "right", value: ({
    vertices: field_vertices,
    edges: field_edges
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function property<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.Property<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("key")(propertyKey)(fieldMap)(cx))(((field_key: PgModel.PropertyKey) => LibEithers.bind(ExtractCore.requireField("value")(v)(fieldMap)(cx))(((field_value: t0) => ({ tag: "right", value: ({
    key: field_key,
    value: field_value
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function propertyKey(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | PgModel.PropertyKey) {
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

export function propertyType<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.PropertyType<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("key")(propertyKey)(fieldMap)(cx))(((field_key: PgModel.PropertyKey) => LibEithers.bind(ExtractCore.requireField("value")(t)(fieldMap)(cx))(((field_value: t0) => LibEithers.bind(ExtractCore.requireField("required")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_required: boolean) => ({ tag: "right", value: ({
    key: field_key,
    value: field_value,
    required: field_required
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function vertex<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.Vertex<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("label")(vertexLabel)(fieldMap)(cx))(((field_label: PgModel.VertexLabel) => LibEithers.bind(ExtractCore.requireField("id")(v)(fieldMap)(cx))(((field_id: t0) => LibEithers.bind(ExtractCore.requireField("properties")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(propertyKey)(v)(v1)(v2))))(fieldMap)(cx))(((field_properties: ReadonlyMap<PgModel.PropertyKey, t0>) => ({ tag: "right", value: ({
    label: field_label,
    id: field_id,
    properties: field_properties
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function vertexLabel(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | PgModel.VertexLabel) {
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

export function vertexType<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.VertexType<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("label")(vertexLabel)(fieldMap)(cx))(((field_label: PgModel.VertexLabel) => LibEithers.bind(ExtractCore.requireField("id")(t)(fieldMap)(cx))(((field_id: t0) => LibEithers.bind(ExtractCore.requireField("properties")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => propertyType(t)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_properties: ReadonlyArray<PgModel.PropertyType<t0>>) => ({ tag: "right", value: ({
    label: field_label,
    id: field_id,
    properties: field_properties
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function vertexWithAdjacentEdges<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | PgModel.VertexWithAdjacentEdges<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("vertex")(((v1: Graph.Graph) => ((v2: Core.Term) => vertex(v)(v1)(v2))))(fieldMap)(cx))(((field_vertex: PgModel.Vertex<t0>) => LibEithers.bind(ExtractCore.requireField("ins")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => adjacentEdge(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_ins: ReadonlyArray<PgModel.AdjacentEdge<t0>>) => LibEithers.bind(ExtractCore.requireField("outs")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => adjacentEdge(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_outs: ReadonlyArray<PgModel.AdjacentEdge<t0>>) => ({ tag: "right", value: ({
    vertex: field_vertex,
    ins: field_ins,
    outs: field_outs
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

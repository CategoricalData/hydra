// Note: this is an automatically generated file. Do not edit.

/**
 * Utility functions for property graph operations
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as PgCoder from "./coder.js";
import * as PgMapping from "./mapping.js";
import * as PgModel from "./model.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const defaultTinkerpopAnnotations: PgMapping.AnnotationSchema = ({
    vertexLabel: "vertexLabel",
    edgeLabel: "edgeLabel",
    vertexId: "vertexId",
    edgeId: "edgeId",
    propertyKey: "key",
    propertyValue: "value",
    outVertex: "outVertex",
    outVertexLabel: "outVertexLabel",
    inVertex: "inVertex",
    inVertexLabel: "inVertexLabel",
    outEdge: "outEdge",
    outEdgeLabel: "outEdgeLabel",
    inEdge: "inEdge",
    inEdgeLabel: "inEdgeLabel",
    ignore: "ignore"
  });

export const examplePgSchema: PgMapping.Schema<t0, void, string> = ({
    vertexIdTypes: ({
    encode: ((_: Context.Context) => ((_2: Core.Type) => ({ tag: "right", value: undefined }))),
    decode: ((_: Context.Context) => ((_2: void) => ({ tag: "right", value: ({ tag: "unit" }) })))
  }),
    vertexIds: ({
    encode: ((cx: Context.Context) => ((t: Core.Term) => expString(cx)(t))),
    decode: ((_cx: Context.Context) => ((s: string) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: s }) }) })))
  }),
    edgeIdTypes: ({
    encode: ((_: Context.Context) => ((_2: Core.Type) => ({ tag: "right", value: undefined }))),
    decode: ((_: Context.Context) => ((_2: void) => ({ tag: "right", value: ({ tag: "unit" }) })))
  }),
    edgeIds: ({
    encode: ((cx: Context.Context) => ((t: Core.Term) => expString(cx)(t))),
    decode: ((_cx: Context.Context) => ((s: string) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: s }) }) })))
  }),
    propertyTypes: ({
    encode: ((_: Context.Context) => ((_2: Core.Type) => ({ tag: "right", value: undefined }))),
    decode: ((_: Context.Context) => ((_2: void) => ({ tag: "right", value: ({ tag: "unit" }) })))
  }),
    propertyValues: ({
    encode: ((cx: Context.Context) => ((t: Core.Term) => expString(cx)(t))),
    decode: ((_cx: Context.Context) => ((s: string) => ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: s }) }) })))
  }),
    annotations: defaultTinkerpopAnnotations,
    defaultVertexId: "defaultVertexId",
    defaultEdgeId: "defaultEdgeId"
  });

export function expString<t0>(cx: t0): ((x: Core.Term) => Errors.Error | string) {
  return ((term: Core.Term) => ExtractCore.string(({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  }))(term));
}

export function lazyGraphToElements<t0>(lg: PgModel.LazyGraph<t0>): ReadonlyArray<PgModel.Element<t0>> {
  return LibLists.concat2(LibLists.map(((x: PgModel.Vertex<t0>) => ({ tag: "vertex", value: x })))(((_x) => _x.vertices)(lg)))(LibLists.map(((x: PgModel.Edge<t0>) => ({ tag: "edge", value: x })))(((_x) => _x.edges)(lg)));
}

export function pgElementToJson<t0, t1, t2>(schema: PgMapping.Schema<t0, t1, t2>): ((x: PgModel.Element<t2>) => ((x: Context.Context) => Errors.Error | JsonModel.Value)) {
  return ((el: PgModel.Element<t2>) => ((cx: Context.Context) => ((v1: PgModel.Element) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((vertex: PgModel.Vertex<t2>) => LibEithers.bind(((_x) => _x.decode)(((_x) => _x.vertexIds)(schema))(cx)(((_x) => _x.id)(vertex)))(((term: Core.Term) => (() => {
  const labelJson = ({ tag: "string", value: ((_x) => _x)(((_x) => _x.label)(vertex)) });
  return LibEithers.map(((propsJson: readonly [string, JsonModel.Value] | null) => ({ tag: "object", value: LibMaps.fromList(LibMaybes.cat([["label", labelJson], ["id", ({ tag: "string", value: ShowCore.term(term) })], propsJson])) })))(LibLogic.ifElse(LibMaps.null_(((_x) => _x.properties)(vertex)))(({ tag: "right", value: null }))(LibEithers.map(((p: ReadonlyArray<readonly [string, JsonModel.Value]>) => ["properties", ({ tag: "object", value: LibMaps.fromList(p) })]))(LibEithers.mapList(((pair: readonly [PgModel.PropertyKey, t2]) => (() => {
  const key = LibPairs.first(pair);
  const v = LibPairs.second(pair);
  return LibEithers.bind(((_x) => _x.decode)(((_x) => _x.propertyValues)(schema))(cx)(v))(((term2: Core.Term) => ({ tag: "right", value: [((_x) => _x)(key), ({ tag: "string", value: ShowCore.term(term2) })] })));
})()))(LibMaps.toList(((_x) => _x.properties)(vertex))))));
})())))((_m as any).value);
    case "edge": return ((edge: PgModel.Edge<t2>) => LibEithers.bind(((_x) => _x.decode)(((_x) => _x.edgeIds)(schema))(cx)(((_x) => _x.id)(edge)))(((term: Core.Term) => LibEithers.bind(((_x) => _x.decode)(((_x) => _x.vertexIds)(schema))(cx)(((_x) => _x.out)(edge)))(((termOut: Core.Term) => LibEithers.bind(((_x) => _x.decode)(((_x) => _x.vertexIds)(schema))(cx)(((_x) => _x.in)(edge)))(((termIn: Core.Term) => (() => {
  const labelJson = ({ tag: "string", value: ((_x) => _x)(((_x) => _x.label)(edge)) });
  return LibEithers.map(((propsJson: readonly [string, JsonModel.Value] | null) => ({ tag: "object", value: LibMaps.fromList(LibMaybes.cat([["label", labelJson], ["id", ({ tag: "string", value: ShowCore.term(term) })], ["out", ({ tag: "string", value: ShowCore.term(termOut) })], ["in", ({ tag: "string", value: ShowCore.term(termIn) })], propsJson])) })))(LibLogic.ifElse(LibMaps.null_(((_x) => _x.properties)(edge)))(({ tag: "right", value: null }))(LibEithers.map(((p: ReadonlyArray<readonly [string, JsonModel.Value]>) => ["properties", ({ tag: "object", value: LibMaps.fromList(p) })]))(LibEithers.mapList(((pair: readonly [PgModel.PropertyKey, t2]) => (() => {
  const key = LibPairs.first(pair);
  const v = LibPairs.second(pair);
  return LibEithers.bind(((_x) => _x.decode)(((_x) => _x.propertyValues)(schema))(cx)(v))(((term2: Core.Term) => ({ tag: "right", value: [((_x) => _x)(key), ({ tag: "string", value: ShowCore.term(term2) })] })));
})()))(LibMaps.toList(((_x) => _x.properties)(edge))))));
})())))))))((_m as any).value);
  }
})())(el)));
}

export function pgElementsToJson<t0, t1, t2>(schema: PgMapping.Schema<t0, t1, t2>): ((x: ReadonlyArray<PgModel.Element<t2>>) => ((x: Context.Context) => Errors.Error | JsonModel.Value)) {
  return ((els: ReadonlyArray<PgModel.Element<t2>>) => ((cx: Context.Context) => LibEithers.map(((els_: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: els_ })))(LibEithers.mapList(((el: PgModel.Element<t2>) => pgElementToJson(schema)(el)(cx)))(els))));
}

export function propertyGraphElements<t0>(g: PgModel.Graph<t0>): ReadonlyArray<PgModel.Element<t0>> {
  return LibLists.concat2(LibLists.map(((x: PgModel.Vertex<t0>) => ({ tag: "vertex", value: x })))(LibMaps.elems(((_x) => _x.vertices)(g))))(LibLists.map(((x: PgModel.Edge<t0>) => ({ tag: "edge", value: x })))(LibMaps.elems(((_x) => _x.edges)(g))));
}

export function typeApplicationTermToPropertyGraph<t0, t1, t2>(schema: PgMapping.Schema<t0, t1, t2>): ((x: Core.Type) => ((x: t1) => ((x: t1) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ((x: Core.Term) => ((x: Context.Context) => Errors.Error | ReadonlyArray<PgModel.Element<t2>>))))))) {
  return ((typ: Core.Type) => ((vidType: t1) => ((eidType: t1) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(PgCoder.elementCoder(null)(schema)(typ)(vidType)(eidType)(cx)(g))(((adapter: Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>) => ({ tag: "right", value: ((term: Core.Term) => ((cx_: Context.Context) => LibEithers.map(((tree: PgModel.ElementTree<t2>) => (() => {
  const flattenTree = ((t: PgModel.ElementTree<t3>) => LibLists.cons(((_x) => _x.self)(t))(LibLists.concat(LibLists.map(flattenTree)(((_x) => _x.dependencies)(t)))));
  return flattenTree(tree);
})()))(((_x) => _x.encode)(((_x) => _x.coder)(adapter))(cx_)(term)))) }))))))));
}

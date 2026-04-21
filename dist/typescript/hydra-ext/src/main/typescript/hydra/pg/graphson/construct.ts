// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for constructing GraphSON vertices from property graph vertices.
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibLists from "../../lib/lists.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibPairs from "../../lib/pairs.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as PgGraphsonCoder from "./coder.js";
import * as PgGraphsonSyntax from "./syntax.js";
import * as PgModel from "../model.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function adjacentEdgeToGraphson<t0, t1>(encodeValue: ((x: t0) => t1 | PgGraphsonSyntax.Value)): ((x: PgModel.AdjacentEdge<t0>) => t1 | readonly [PgGraphsonSyntax.EdgeLabel, PgGraphsonSyntax.AdjacentEdge]) {
  return ((edge: PgModel.AdjacentEdge<t0>) => (() => {
  const label = ((_x) => _x.label)(edge);
  return (() => {
  const edgeId = ((_x) => _x.id)(edge);
  return (() => {
  const vertexId = ((_x) => _x.vertex)(edge);
  return (() => {
  const props = ((_x) => _x.properties)(edge);
  return LibEithers.bind(encodeValue(edgeId))(((gid: PgGraphsonSyntax.Value) => LibEithers.bind(encodeValue(vertexId))(((gv: PgGraphsonSyntax.Value) => LibEithers.bind(LibEithers.mapList(((v1: readonly [PgModel.PropertyKey, t0]) => edgePropertyToGraphson(encodeValue)(v1)))(LibMaps.toList(props)))(((propPairs: ReadonlyArray<readonly [PgGraphsonSyntax.PropertyKey, PgGraphsonSyntax.Value]>) => ({ tag: "right", value: [((_x) => _x)(label), ({
    id: gid,
    vertexId: gv,
    properties: LibMaps.fromList(propPairs)
  })] })))))));
})();
})();
})();
})());
}

export function aggregateMap<t0, t1>(pairs: ReadonlyArray<readonly [t0, t1]>): ReadonlyMap<t0, ReadonlyArray<t1>> {
  return LibLists.foldl(((m: ReadonlyMap<t0, ReadonlyArray<t1>>) => ((p: readonly [t0, t1]) => (() => {
  const k = LibPairs.first(p);
  return (() => {
  const v = LibPairs.second(p);
  return (() => {
  const existing = LibMaps.lookup(k)(m);
  return LibMaps.insert(k)(LibMaybes.maybe(LibLists.pure(v))(((vs: ReadonlyArray<t1>) => LibLists.cons(v)(vs)))(existing))(m);
})();
})();
})())))(LibMaps.empty)(pairs);
}

export function edgePropertyToGraphson<t0, t1, t2>(encodeValue: ((x: t0) => t1 | t2)): ((x: readonly [PgModel.PropertyKey, t0]) => t1 | readonly [PgGraphsonSyntax.PropertyKey, t2]) {
  return ((prop: readonly [PgModel.PropertyKey, t0]) => LibEithers.map(((gv: t2) => [((_x) => _x)(LibPairs.first(prop)), gv]))(encodeValue(LibPairs.second(prop))));
}

export const graphsonVertexToJsonCoder: Coders.Coder<PgGraphsonSyntax.Vertex, JsonModel.Value> = ({
    encode: ((_cx: Context.Context) => ((v: PgGraphsonSyntax.Vertex) => ({ tag: "right", value: PgGraphsonCoder.vertexToJson(v) }))),
    decode: ((_cx: Context.Context) => ((_: JsonModel.Value) => ({ tag: "left", value: ({ tag: "other", value: "decoding GraphSON JSON is currently unsupported" }) })))
  });

export function pgVertexWithAdjacentEdgesToGraphsonVertex<t0, t1>(encodeValue: ((x: t0) => t1 | PgGraphsonSyntax.Value)): ((x: PgModel.VertexWithAdjacentEdges<t0>) => t1 | PgGraphsonSyntax.Vertex) {
  return ((vae: PgModel.VertexWithAdjacentEdges<t0>) => (() => {
  const vertex = ((_x) => _x.vertex)(vae);
  return (() => {
  const ins = ((_x) => _x.ins)(vae);
  return (() => {
  const outs = ((_x) => _x.outs)(vae);
  return (() => {
  const label = ((_x) => _x.label)(vertex);
  return (() => {
  const vertexId = ((_x) => _x.id)(vertex);
  return (() => {
  const props = ((_x) => _x.properties)(vertex);
  return LibEithers.bind(encodeValue(vertexId))(((gid: PgGraphsonSyntax.Value) => LibEithers.bind(LibEithers.mapList(((v1: readonly [PgModel.PropertyKey, t0]) => vertexPropertyToGraphson(encodeValue)(v1)))(LibMaps.toList(props)))(((propPairs: ReadonlyArray<readonly [PgGraphsonSyntax.PropertyKey, PgGraphsonSyntax.VertexPropertyValue]>) => LibEithers.bind(LibEithers.mapList(((v1: PgModel.AdjacentEdge<t0>) => adjacentEdgeToGraphson(encodeValue)(v1)))(ins))(((inPairs: ReadonlyArray<readonly [PgGraphsonSyntax.EdgeLabel, PgGraphsonSyntax.AdjacentEdge]>) => LibEithers.bind(LibEithers.mapList(((v1: PgModel.AdjacentEdge<t0>) => adjacentEdgeToGraphson(encodeValue)(v1)))(outs))(((outPairs: ReadonlyArray<readonly [PgGraphsonSyntax.EdgeLabel, PgGraphsonSyntax.AdjacentEdge]>) => ({ tag: "right", value: ({
    id: gid,
    label: ((_x) => _x)(label),
    inEdges: aggregateMap(inPairs),
    outEdges: aggregateMap(outPairs),
    properties: aggregateMap(propPairs)
  }) })))))))));
})();
})();
})();
})();
})();
})());
}

export function pgVertexWithAdjacentEdgesToJson<t0, t1>(encodeValue: ((x: t0) => t1 | PgGraphsonSyntax.Value)): ((x: PgModel.VertexWithAdjacentEdges<t0>) => t1 | JsonModel.Value) {
  return ((vertex: PgModel.VertexWithAdjacentEdges<t0>) => LibEithers.bind(pgVertexWithAdjacentEdgesToGraphsonVertex(encodeValue)(vertex))(((gVertex: PgGraphsonSyntax.Vertex) => ({ tag: "right", value: PgGraphsonCoder.vertexToJson(gVertex) }))));
}

export function vertexPropertyToGraphson<t0, t1>(encodeValue: ((x: t0) => t1 | PgGraphsonSyntax.Value)): ((x: readonly [PgModel.PropertyKey, t0]) => t1 | readonly [PgGraphsonSyntax.PropertyKey, PgGraphsonSyntax.VertexPropertyValue]) {
  return ((prop: readonly [PgModel.PropertyKey, t0]) => LibEithers.map(((gv: PgGraphsonSyntax.Value) => [((_x) => _x)(LibPairs.first(prop)), ({
    id: null,
    value: gv
  })]))(encodeValue(LibPairs.second(prop))));
}

// Note: this is an automatically generated file. Do not edit.

/**
 * Utility functions for GraphSON encoding and property graph conversion.
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
import * as LibLiterals from "../../lib/literals.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibPairs from "../../lib/pairs.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as PgGraphsonConstruct from "./construct.js";
import * as PgGraphsonSyntax from "./syntax.js";
import * as PgModel from "../model.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Strip from "../../strip.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function elementsToVerticesWithAdjacentEdges<t0>(els: ReadonlyArray<PgModel.Element<t0>>): ReadonlyArray<PgModel.VertexWithAdjacentEdges<t0>> {
  return (() => {
  const partitioned = LibLists.foldl(((acc: readonly [ReadonlyArray<PgModel.Vertex<t0>>, ReadonlyArray<PgModel.Edge<t0>>]) => ((el: PgModel.Element<t0>) => ((v1: PgModel.Element) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((v: PgModel.Vertex<t0>) => [LibLists.cons(v)(LibPairs.first(acc)), LibPairs.second(acc)])((_m as any).value);
    case "edge": return ((e: PgModel.Edge<t0>) => [LibPairs.first(acc), LibLists.cons(e)(LibPairs.second(acc))])((_m as any).value);
  }
})())(el))))([[], []])(els);
  return (() => {
  const vertices = LibLists.reverse(LibPairs.first(partitioned));
  return (() => {
  const edges = LibLists.reverse(LibPairs.second(partitioned));
  return (() => {
  const vertexMap0 = LibMaps.fromList(LibLists.map(((v: PgModel.Vertex<t0>) => [((_x) => _x.id)(v), ({
    vertex: v,
    ins: [],
    outs: []
  })]))(vertices));
  return (() => {
  const vertexMap1 = LibLists.foldl(((vmap: ReadonlyMap<t0, PgModel.VertexWithAdjacentEdges<t0>>) => ((edge: PgModel.Edge<t0>) => (() => {
  const label = ((_x) => _x.label)(edge);
  return (() => {
  const edgeId = ((_x) => _x.id)(edge);
  return (() => {
  const outV = ((_x) => _x.out)(edge);
  return (() => {
  const inV = ((_x) => _x.in)(edge);
  return (() => {
  const props = ((_x) => _x.properties)(edge);
  return (() => {
  const adjEdgeOut = ({
    label: label,
    id: edgeId,
    vertex: inV,
    properties: props
  });
  return (() => {
  const adjEdgeIn = ({
    label: label,
    id: edgeId,
    vertex: outV,
    properties: props
  });
  return (() => {
  const vmap1 = LibMaybes.maybe(vmap)(((vae: PgModel.VertexWithAdjacentEdges<t0>) => LibMaps.insert(outV)(({
    vertex: ((_x) => _x.vertex)(vae),
    ins: ((_x) => _x.ins)(vae),
    outs: LibLists.cons(adjEdgeOut)(((_x) => _x.outs)(vae))
  }))(vmap)))(LibMaps.lookup(outV)(vmap));
  return LibMaybes.maybe(vmap1)(((vae: PgModel.VertexWithAdjacentEdges<t0>) => LibMaps.insert(inV)(({
    vertex: ((_x) => _x.vertex)(vae),
    ins: LibLists.cons(adjEdgeIn)(((_x) => _x.ins)(vae)),
    outs: ((_x) => _x.outs)(vae)
  }))(vmap1)))(LibMaps.lookup(inV)(vmap1));
})();
})();
})();
})();
})();
})();
})();
})())))(vertexMap0)(edges);
  return LibMaps.elems(vertexMap1);
})();
})();
})();
})();
})();
}

export function encodeStringValue<t0>(s: string): t0 | PgGraphsonSyntax.Value {
  return ({ tag: "right", value: ({ tag: "string", value: s }) });
}

export function encodeTermValue(term: Core.Term): Errors.Error | PgGraphsonSyntax.Value {
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "binary": return ((b: Uint8Array) => ({ tag: "right", value: ({ tag: "binary", value: LibLiterals.binaryToString(b) }) }))((_m as any).value);
    case "boolean": return ((b: boolean) => ({ tag: "right", value: ({ tag: "boolean", value: b }) }))((_m as any).value);
    case "float": return ((fv: Core.FloatValue) => (() => {
  const _m = fv;
  switch (_m.tag) {
    case "bigfloat": return ((f: number) => ({ tag: "right", value: ({ tag: "bigDecimal", value: LibLiterals.showBigfloat(f) }) }))((_m as any).value);
    case "float32": return ((f: number) => ({ tag: "right", value: ({ tag: "float", value: ({ tag: "finite", value: f }) }) }))((_m as any).value);
    case "float64": return ((f: number) => ({ tag: "right", value: ({ tag: "double", value: ({ tag: "finite", value: f }) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported float type" }) })(_m);
  }
})())((_m as any).value);
    case "integer": return ((iv: Core.IntegerValue) => (() => {
  const _m = iv;
  switch (_m.tag) {
    case "bigint": return ((i: bigint) => ({ tag: "right", value: ({ tag: "bigInteger", value: i }) }))((_m as any).value);
    case "int32": return ((i: number) => ({ tag: "right", value: ({ tag: "integer", value: i }) }))((_m as any).value);
    case "int64": return ((i: bigint) => ({ tag: "right", value: ({ tag: "long", value: i }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported integer type" }) })(_m);
  }
})())((_m as any).value);
    case "string": return ((s: string) => ({ tag: "right", value: ({ tag: "string", value: s }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported literal type for GraphSON encoding" }) })(_m);
  }
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "null" }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported term variant for GraphSON encoding" }) })(_m);
  }
})();
}

export function pgElementsToGraphson<t0, t1>(encodeValue: ((x: t0) => t1 | PgGraphsonSyntax.Value)): ((x: ReadonlyArray<PgModel.Element<t0>>) => t1 | ReadonlyArray<JsonModel.Value>) {
  return ((els: ReadonlyArray<PgModel.Element<t0>>) => LibEithers.mapList(((v1: PgModel.VertexWithAdjacentEdges<t0>) => PgGraphsonConstruct.pgVertexWithAdjacentEdgesToJson(encodeValue)(v1)))(elementsToVerticesWithAdjacentEdges(els)));
}

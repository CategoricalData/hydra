// Note: this is an automatically generated file. Do not edit.

/**
 * Printing functions for property graph elements
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
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as PgModel from "./model.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function printEdge<t0>(printValue: ((x: t0) => string)): ((x: PgModel.Edge<t0>) => string) {
  return ((edge: PgModel.Edge<t0>) => (() => {
  const label = ((_x) => _x)(((_x) => _x.label)(edge));
  const id = printValue(((_x) => _x.id)(edge));
  const outId = printValue(((_x) => _x.out)(edge));
  const inId = printValue(((_x) => _x.in)(edge));
  const props = LibStrings.intercalate(", ")(LibLists.map(((p: readonly [PgModel.PropertyKey, t0]) => printProperty(printValue)(LibPairs.first(p))(LibPairs.second(p))))(LibMaps.toList(((_x) => _x.properties)(edge))));
  return LibStrings.cat([id, ": ", "(", outId, ")-[:", label, " {", props, "}]->(", inId, ")"]);
})());
}

export function printGraph<t0>(printValue: ((x: t0) => string)): ((x: PgModel.Graph<t0>) => string) {
  return ((graph: PgModel.Graph<t0>) => printLazyGraph(printValue)(({
    vertices: LibMaps.elems(((_x) => _x.vertices)(graph)),
    edges: LibMaps.elems(((_x) => _x.edges)(graph))
  })));
}

export function printLazyGraph<t0>(printValue: ((x: t0) => string)): ((x: PgModel.LazyGraph<t0>) => string) {
  return ((lg: PgModel.LazyGraph<t0>) => (() => {
  const vertices = ((_x) => _x.vertices)(lg);
  const edges = ((_x) => _x.edges)(lg);
  return LibStrings.cat(["vertices:", LibStrings.cat(LibLists.map(((v: PgModel.Vertex<t0>) => LibStrings.cat(["\n\t", printVertex(printValue)(v)])))(vertices)), "\nedges:", LibStrings.cat(LibLists.map(((e: PgModel.Edge<t0>) => LibStrings.cat(["\n\t", printEdge(printValue)(e)])))(edges))]);
})());
}

export function printProperty<t0>(printValue: ((x: t0) => string)): ((x: PgModel.PropertyKey) => ((x: t0) => string)) {
  return ((key: PgModel.PropertyKey) => ((value: t0) => LibStrings.cat([((_x) => _x)(key), ": ", printValue(value)])));
}

export function printVertex<t0>(printValue: ((x: t0) => string)): ((x: PgModel.Vertex<t0>) => string) {
  return ((vertex: PgModel.Vertex<t0>) => (() => {
  const label = ((_x) => _x)(((_x) => _x.label)(vertex));
  const id = printValue(((_x) => _x.id)(vertex));
  const props = LibStrings.intercalate(", ")(LibLists.map(((p: readonly [PgModel.PropertyKey, t0]) => printProperty(printValue)(LibPairs.first(p))(LibPairs.second(p))))(LibMaps.toList(((_x) => _x.properties)(vertex))));
  return LibStrings.cat([id, ": (", label, ": {", props, "})"]);
})());
}

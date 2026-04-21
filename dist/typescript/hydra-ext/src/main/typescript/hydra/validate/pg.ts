// Note: this is an automatically generated file. Do not edit.

/**
 * Validation functions for property graphs
 */



import * as ErrorPg from "../error/pg.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as PgModel from "../pg/model.js";

export function checkAll<t0>(checks: ReadonlyArray<t0 | null>): t0 | null {
  return (() => {
  const errors = LibMaybes.cat(checks);
  return LibLists.safeHead(errors);
})();
}

export function validateEdge<t0, t1>(checkValue: ((x: t0) => ((x: t1) => ErrorPg.InvalidValueError | null))): ((x: ((x: t1) => PgModel.VertexLabel | null) | null) => ((x: PgModel.EdgeType<t0>) => ((x: PgModel.Edge<t1>) => ErrorPg.InvalidEdgeError | null))) {
  return ((labelForVertexId: ((x: t1) => PgModel.VertexLabel | null) | null) => ((typ: PgModel.EdgeType<t0>) => ((el: PgModel.Edge<t1>) => (() => {
  const checkLabel = (() => {
  const expected = ((_x) => _x.label)(typ);
  const actual = ((_x) => _x.label)(el);
  return LibLogic.ifElse(LibEquality.equal(((_x) => _x)(actual))(((_x) => _x)(expected)))(null)(({ tag: "label", value: ({
    label: actual
  }) }));
})();
  const checkId = LibMaybes.map(((err: ErrorPg.InvalidValueError) => ({ tag: "id", value: err })))(checkValue(((_x) => _x.id)(typ))(((_x) => _x.id)(el)));
  const checkProperties = LibMaybes.map(((err: ErrorPg.InvalidElementPropertyError) => ({ tag: "property", value: err })))(validateProperties(checkValue)(((_x) => _x.properties)(typ))(((_x) => _x.properties)(el)));
  const checkOut = LibMaybes.maybe(null)(((f: ((x: t1) => PgModel.VertexLabel | null)) => LibMaybes.maybe(({ tag: "outVertexNotFound" }))(((label: PgModel.VertexLabel) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(label))(((_x) => _x)(((_x) => _x.out)(typ))))(null)(({ tag: "outVertexLabel", value: ({
    expected: ((_x) => _x.out)(typ),
    actual: label
  }) }))))(f(((_x) => _x.out)(el)))))(labelForVertexId);
  const checkIn = LibMaybes.maybe(null)(((f: ((x: t1) => PgModel.VertexLabel | null)) => LibMaybes.maybe(({ tag: "inVertexNotFound" }))(((label: PgModel.VertexLabel) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(label))(((_x) => _x)(((_x) => _x.in)(typ))))(null)(({ tag: "inVertexLabel", value: ({
    expected: ((_x) => _x.in)(typ),
    actual: label
  }) }))))(f(((_x) => _x.in)(el)))))(labelForVertexId);
  return checkAll([checkLabel, checkId, checkProperties, checkOut, checkIn]);
})())));
}

export function validateGraph<t0, t1>(checkValue: ((x: t0) => ((x: t1) => ErrorPg.InvalidValueError | null))): ((x: PgModel.GraphSchema<t0>) => ((x: PgModel.Graph<t1>) => ErrorPg.InvalidGraphError<t1> | null)) {
  return ((schema: PgModel.GraphSchema<t0>) => ((graph: PgModel.Graph<t1>) => (() => {
  const checkVertices = (() => {
  const checkVertex = ((el: PgModel.Vertex<t1>) => LibMaybes.maybe(({ tag: "vertex", value: ({
    id: ((_x) => _x.id)(el),
    error: ({ tag: "label", value: ({
    label: ((_x) => _x.label)(el)
  }) })
  }) }))(((t: PgModel.VertexType<t0>) => LibMaybes.map(((err: ErrorPg.InvalidVertexError) => ({ tag: "vertex", value: ({
    id: ((_x) => _x.id)(el),
    error: err
  }) })))(validateVertex(checkValue)(t)(el))))(LibMaps.lookup(((_x) => _x.label)(el))(((_x) => _x.vertices)(schema))));
  return checkAll(LibLists.map(checkVertex)(LibMaps.elems(((_x) => _x.vertices)(graph))));
})();
  const checkEdges = (() => {
  const checkEdge = ((el: PgModel.Edge<t1>) => LibMaybes.maybe(({ tag: "edge", value: ({
    id: ((_x) => _x.id)(el),
    error: ({ tag: "label", value: ({
    label: ((_x) => _x.label)(el)
  }) })
  }) }))(((t: PgModel.EdgeType<t0>) => LibMaybes.map(((err: ErrorPg.InvalidEdgeError) => ({ tag: "edge", value: ({
    id: ((_x) => _x.id)(el),
    error: err
  }) })))(validateEdge(checkValue)(labelForVertexId)(t)(el))))(LibMaps.lookup(((_x) => _x.label)(el))(((_x) => _x.edges)(schema))));
  const labelForVertexId = ((i: t1) => LibMaybes.map(((_x) => _x.label))(LibMaps.lookup(i)(((_x) => _x.vertices)(graph))));
  return checkAll(LibLists.map(checkEdge)(LibMaps.elems(((_x) => _x.edges)(graph))));
})();
  return checkAll([checkVertices, checkEdges]);
})()));
}

export function validateProperties<t0, t1>(checkValue: ((x: t0) => ((x: t1) => ErrorPg.InvalidValueError | null))): ((x: ReadonlyArray<PgModel.PropertyType<t0>>) => ((x: ReadonlyMap<PgModel.PropertyKey, t1>) => ErrorPg.InvalidElementPropertyError | null)) {
  return ((types: ReadonlyArray<PgModel.PropertyType<t0>>) => ((props: ReadonlyMap<PgModel.PropertyKey, t1>) => (() => {
  const checkTypes = checkAll(LibLists.map(checkType)(types));
  const checkType = ((t: PgModel.PropertyType<t2>) => LibLogic.ifElse(((_x) => _x.required)(t))(LibMaybes.maybe(({
    key: ((_x) => _x.key)(t),
    error: ({ tag: "missingRequired", value: ((_x) => _x.key)(t) })
  }))(((_: t1) => null))(LibMaps.lookup(((_x) => _x.key)(t))(props)))(null));
  const checkValues = (() => {
  const m = LibMaps.fromList(LibLists.map(((p: PgModel.PropertyType<t0>) => [((_x) => _x.key)(p), ((_x) => _x.value)(p)]))(types));
  const checkPair = ((pair: readonly [PgModel.PropertyKey, t1]) => (() => {
  const key = LibPairs.first(pair);
  const val = LibPairs.second(pair);
  return LibMaybes.maybe(({
    key: key,
    error: ({ tag: "unexpectedKey", value: key })
  }))(((typ: t0) => LibMaybes.map(((err: ErrorPg.InvalidValueError) => ({
    key: key,
    error: ({ tag: "invalidValue", value: err })
  })))(checkValue(typ)(val))))(LibMaps.lookup(key)(m));
})());
  return checkAll(LibLists.map(checkPair)(LibMaps.toList(props)));
})();
  return checkAll([checkTypes, checkValues]);
})()));
}

export function validateVertex<t0, t1>(checkValue: ((x: t0) => ((x: t1) => ErrorPg.InvalidValueError | null))): ((x: PgModel.VertexType<t0>) => ((x: PgModel.Vertex<t1>) => ErrorPg.InvalidVertexError | null)) {
  return ((typ: PgModel.VertexType<t0>) => ((el: PgModel.Vertex<t1>) => (() => {
  const checkLabel = (() => {
  const expected = ((_x) => _x.label)(typ);
  const actual = ((_x) => _x.label)(el);
  return LibLogic.ifElse(LibEquality.equal(((_x) => _x)(actual))(((_x) => _x)(expected)))(null)(({ tag: "label", value: ({
    label: actual
  }) }));
})();
  const checkId = LibMaybes.map(((err: ErrorPg.InvalidValueError) => ({ tag: "id", value: err })))(checkValue(((_x) => _x.id)(typ))(((_x) => _x.id)(el)));
  const checkProperties = LibMaybes.map(((err: ErrorPg.InvalidElementPropertyError) => ({ tag: "property", value: err })))(validateProperties(checkValue)(((_x) => _x.properties)(typ))(((_x) => _x.properties)(el)));
  return checkAll([checkLabel, checkId, checkProperties]);
})()));
}

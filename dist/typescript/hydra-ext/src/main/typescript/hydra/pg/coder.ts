// Note: this is an automatically generated file. Do not edit.

/**
 * Property graph element coders for mapping Hydra terms to property graph elements
 */



import * as Annotations from "../annotations.js";
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
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as PgMapping from "./mapping.js";
import * as PgModel from "./model.js";
import * as PgTermsToElements from "./termsToElements.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Resolution from "../resolution.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function check<t0, t1>(_cx: t0): ((x: boolean) => ((x: t1 | void) => t1 | void)) {
  return ((b: boolean) => ((e: t1 | void) => LibLogic.ifElse(b)(({ tag: "right", value: undefined }))(e)));
}

export function checkRecordName<t0>(cx: t0): ((x: Core.Name) => ((x: Core.Name) => Errors.Error | void)) {
  return ((expected: Core.Name) => ((actual: Core.Name) => check(cx)(LibLogic.or(LibEquality.equal(((_x) => _x)(expected))("placeholder"))(LibEquality.equal(((_x) => _x)(actual))(((_x) => _x)(expected))))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("Expected record of type ")(((_x) => _x)(expected)))(", found record of type "))(((_x) => _x)(actual)) }) }))));
}

export function constructEdgeCoder<t0, t1, t2>(cx: Context.Context): ((x: Graph.Graph) => ((x: PgModel.VertexLabel) => ((x: PgMapping.Schema<t0, t1, t2>) => ((x: Core.Type) => ((x: t1) => ((x: t1) => ((x: PgModel.Direction) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t1>, Core.Field, PgModel.Property<t2>>>) => ((x: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => ((x: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => Errors.Error | Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>)))))))))))) {
  return ((g: Graph.Graph) => ((parentLabel: PgModel.VertexLabel) => ((schema: PgMapping.Schema<t0, t1, t2>) => ((source: Core.Type) => ((vidType: t1) => ((eidType: t1) => ((dir: PgModel.Direction) => ((name: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => ((propAdapters: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t1>, Core.Field, PgModel.Property<t2>>>) => ((mOutSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => ((mInSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => LibEithers.bind(findLabelString(cx)(g)(source)(name)(((_x) => _x.edgeLabel)(((_x) => _x.annotations)(schema))))(((labelStr: string) => (() => {
  const label = labelStr;
  const vertexIdsSchema = ((_x) => _x.vertexIds)(schema);
  return LibEithers.bind(edgeIdAdapter(cx)(g)(schema)(eidType)(name)(((_x) => _x.edgeId)(((_x) => _x.annotations)(schema)))(fields))(((idAdapter: readonly [Core.Name, Coders.Adapter<Core.Type, t1, Core.Term, t2>] | null) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((s: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => LibEithers.map(((x: readonly [Core.Name, Coders.Adapter<Core.Type, t1, Core.Term, t2>]) => x))(projectionAdapter(cx)(g)(vidType)(vertexIdsSchema)(s)("out"))))(mOutSpec))(((outIdAdapter: readonly [Core.Name, Coders.Adapter<Core.Type, t1, Core.Term, t2>] | null) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((s: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => LibEithers.map(((x: readonly [Core.Name, Coders.Adapter<Core.Type, t1, Core.Term, t2>]) => x))(projectionAdapter(cx)(g)(vidType)(vertexIdsSchema)(s)("in"))))(mInSpec))(((inIdAdapter: readonly [Core.Name, Coders.Adapter<Core.Type, t1, Core.Term, t2>] | null) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((s: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => LibEithers.map(((x: readonly [Core.Name, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>]) => x))(findIncidentVertexAdapter(cx)(g)(schema)(vidType)(eidType)(s))))(mOutSpec))(((outVertexAdapter: readonly [Core.Name, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>] | null) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((s: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => LibEithers.map(((x: readonly [Core.Name, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>]) => x))(findIncidentVertexAdapter(cx)(g)(schema)(vidType)(eidType)(s))))(mInSpec))(((inVertexAdapter: readonly [Core.Name, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>] | null) => (() => {
  const vertexAdapters = LibMaybes.cat([outVertexAdapter, inVertexAdapter]);
  return LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: parentLabel }))(((spec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: "no out-vertex label" }) }))(((a: string) => ({ tag: "right", value: a })))(LibPairs.second(LibPairs.second(spec)))))(mOutSpec))(((outLabel: PgModel.VertexLabel) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: parentLabel }))(((spec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: "no in-vertex label" }) }))(((a: string) => ({ tag: "right", value: a })))(LibPairs.second(LibPairs.second(spec)))))(mInSpec))(((inLabel: PgModel.VertexLabel) => ({ tag: "right", value: edgeCoder(g)(dir)(schema)(source)(eidType)(name)(label)(outLabel)(inLabel)(idAdapter)(outIdAdapter)(inIdAdapter)(propAdapters)(vertexAdapters) })))));
})()))))))))));
})()))))))))))))));
}

export function constructVertexCoder<t0, t1, t2>(cx: Context.Context): ((x: Graph.Graph) => ((x: PgMapping.Schema<t0, t1, t2>) => ((x: Core.Type) => ((x: t1) => ((x: t1) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t1>, Core.Field, PgModel.Property<t2>>>) => Errors.Error | Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>)))))))) {
  return ((g: Graph.Graph) => ((schema: PgMapping.Schema<t0, t1, t2>) => ((source: Core.Type) => ((vidType: t1) => ((eidType: t1) => ((name: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => ((propAdapters: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t1>, Core.Field, PgModel.Property<t2>>>) => LibEithers.bind(findLabelString(cx)(g)(source)(name)(((_x) => _x.vertexLabel)(((_x) => _x.annotations)(schema))))(((labelStr: string) => (() => {
  const label = labelStr;
  return LibEithers.bind(vertexIdAdapter(cx)(g)(schema)(vidType)(name)(((_x) => _x.vertexId)(((_x) => _x.annotations)(schema)))(fields))(((idAdapter: readonly [Core.Name, Coders.Adapter<Core.Type, t1, Core.Term, t2>]) => LibEithers.bind(findAdjacenEdgeAdapters(cx)(g)(schema)(vidType)(eidType)(label)(({ tag: "out" }))(fields))(((outEdgeAdapters: ReadonlyArray<readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>]]]>) => LibEithers.bind(findAdjacenEdgeAdapters(cx)(g)(schema)(vidType)(eidType)(label)(({ tag: "in" }))(fields))(((inEdgeAdapters: ReadonlyArray<readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>]]]>) => ({ tag: "right", value: vertexCoder(g)(schema)(source)(vidType)(name)(label)(idAdapter)(propAdapters)(LibLists.concat2(outEdgeAdapters)(inEdgeAdapters)) })))))));
})()))))))))));
}

export function edgeCoder<t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13>(g: t0): ((x: PgModel.Direction) => ((x: PgMapping.Schema<t1, t2, t3>) => ((x: t4) => ((x: t5) => ((x: Core.Name) => ((x: PgModel.EdgeLabel) => ((x: PgModel.VertexLabel) => ((x: PgModel.VertexLabel) => ((x: readonly [Core.Name, Coders.Adapter<t6, t7, Core.Term, t3>] | null) => ((x: readonly [Core.Name, Coders.Adapter<t8, t9, Core.Term, t3>] | null) => ((x: readonly [Core.Name, Coders.Adapter<t10, t11, Core.Term, t3>] | null) => ((x: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t5>, Core.Field, PgModel.Property<t3>>>) => ((x: ReadonlyArray<readonly [Core.Name, Coders.Adapter<t12, t13, Core.Term, PgModel.ElementTree<t3>>]>) => Coders.Adapter<t4, PgModel.ElementTypeTree<t5>, Core.Term, PgModel.ElementTree<t3>>))))))))))))) {
  return ((dir: PgModel.Direction) => ((schema: PgMapping.Schema<t1, t2, t3>) => ((source: t4) => ((eidType: t5) => ((tname: Core.Name) => ((label: PgModel.EdgeLabel) => ((outLabel: PgModel.VertexLabel) => ((inLabel: PgModel.VertexLabel) => ((mIdAdapter: readonly [Core.Name, Coders.Adapter<t6, t7, Core.Term, t3>] | null) => ((outAdapter: readonly [Core.Name, Coders.Adapter<t8, t9, Core.Term, t3>] | null) => ((inAdapter: readonly [Core.Name, Coders.Adapter<t10, t11, Core.Term, t3>] | null) => ((propAdapters: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t5>, Core.Field, PgModel.Property<t3>>>) => ((vertexAdapters: ReadonlyArray<readonly [Core.Name, Coders.Adapter<t12, t13, Core.Term, PgModel.ElementTree<t3>>]>) => (() => {
  const et = ({
    label: label,
    id: eidType,
    out: outLabel,
    in: inLabel,
    properties: propertyTypes(propAdapters)
  });
  return ({
    isLossy: true,
    source: source,
    target: elementTypeTreeEdge(et)([]),
    coder: ({
    encode: ((cx: Context.Context) => ((term: Core.Term) => (() => {
  const deannot = Strip.deannotateTerm(term);
  const unwrapped = (() => {
  const _m = deannot;
  switch (_m.tag) {
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.fromMaybe(deannot)(mt))((_m as any).value);
    default: return deannot(_m);
  }
})();
  const rec = (() => {
  const _m = unwrapped;
  switch (_m.tag) {
    case "record": return ((r: Core.Record) => r)((_m as any).value);
  }
})();
  return LibEithers.bind(checkRecordName(cx)(tname)(((_x) => _x.typeName)(rec)))(((_chk: void) => (() => {
  const fieldsm = Resolution.fieldMap(((_x) => _x.fields)(rec));
  return LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: ((_x) => _x.defaultEdgeId)(schema) }))(((v1: readonly [Core.Name, Coders.Adapter<t6, t7, Core.Term, t3>]) => selectEdgeId(cx)(fieldsm)(v1)))(mIdAdapter))(((edgeId: t3) => LibEithers.bind(encodeProperties(cx)(fieldsm)(propAdapters))(((props: ReadonlyMap<PgModel.PropertyKey, t3>) => (() => {
  const getVertexId = ((dirCheck: PgModel.Direction) => ((adapter: readonly [Core.Name, Coders.Adapter<t14, t15, Core.Term, t3>] | null) => LibMaybes.maybe(({ tag: "right", value: ((_x) => _x.defaultVertexId)(schema) }))(((v1: readonly [Core.Name, Coders.Adapter<t14, t15, Core.Term, t3>]) => selectVertexId(cx)(fieldsm)(v1)))(LibLogic.ifElse(LibEquality.equal(dir)(dirCheck))(null)(adapter))));
  return LibEithers.bind(getVertexId(({ tag: "out" }))(outAdapter))(((outId: t3) => LibEithers.bind(getVertexId(({ tag: "in" }))(inAdapter))(((inId: t3) => LibEithers.bind(LibEithers.map(((xs: ReadonlyArray<PgModel.ElementTree<t3> | null>) => LibMaybes.cat(xs)))(LibEithers.mapList(((va: readonly [Core.Name, Coders.Adapter<t12, t13, Core.Term, PgModel.ElementTree<t3>>]) => (() => {
  const fname = LibPairs.first(va);
  const ad = LibPairs.second(va);
  return LibMaybes.maybe(({ tag: "right", value: null }))(((fterm: Core.Term) => LibEithers.map(((x: PgModel.ElementTree<t3>) => x))(((_x) => _x.encode)(((_x) => _x.coder)(ad))(cx)(fterm))))(LibMaps.lookup(fname)(fieldsm));
})()))(vertexAdapters)))(((deps: ReadonlyArray<PgModel.ElementTree<t3>>) => ({ tag: "right", value: elementTreeEdge(({
    label: label,
    id: edgeId,
    out: outId,
    in: inId,
    properties: props
  }))(deps) })))))));
})()))));
})()));
})())),
    decode: ((cx: Context.Context) => ((_: PgModel.ElementTree<t3>) => ({ tag: "left", value: ({ tag: "other", value: "edge decoding is not yet supported" }) })))
  })
  });
})())))))))))))));
}

export function edgeIdAdapter<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: t5) => ((x: Core.Name) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | readonly [Core.Name, Coders.Adapter<Core.Type, t5, Core.Term, t4>] | null)))))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((eidType: t5) => ((name: Core.Name) => ((idKey: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(findIdProjectionSpec(cx)(false)(name)(idKey)(fields))(((mIdSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => LibMaybes.maybe(({ tag: "right", value: null }))(((idSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => LibEithers.map(((x: readonly [Core.Name, Coders.Adapter<Core.Type, t5, Core.Term, t4>]) => x))(projectionAdapter(cx)(g)(eidType)(((_x) => _x.edgeIds)(schema))(idSpec)("id"))))(mIdSpec)))))))));
}

export function elementCoder<t0, t1, t2>(mparent: readonly [PgModel.Direction, PgModel.VertexLabel] | null): ((x: PgMapping.Schema<t0, t1, t2>) => ((x: Core.Type) => ((x: t1) => ((x: t1) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>)))))) {
  return ((schema: PgMapping.Schema<t0, t1, t2>) => ((source: Core.Type) => ((vidType: t1) => ((eidType: t1) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const dir = LibMaybes.maybe(({ tag: "both" }))(((p: readonly [PgModel.Direction, PgModel.VertexLabel]) => LibPairs.first(p)))(mparent);
  const parentLabel = LibMaybes.maybe("NOLABEL")(((p: readonly [PgModel.Direction, PgModel.VertexLabel]) => LibPairs.second(p)))(mparent);
  return (() => {
  const _m = Strip.deannotateType(source);
  switch (_m.tag) {
    case "maybe": return ((ot: Core.Type) => elementCoder(mparent)(schema)(ot)(vidType)(eidType)(cx)(g))((_m as any).value);
    case "record": return ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const name = "placeholder";
  const outVertexKey = ((_x) => _x.outVertex)(((_x) => _x.annotations)(schema));
  const outVertexLabelKey = ((_x) => _x.outVertexLabel)(((_x) => _x.annotations)(schema));
  const inVertexKey = ((_x) => _x.inVertex)(((_x) => _x.annotations)(schema));
  const inVertexLabelKey = ((_x) => _x.inVertexLabel)(((_x) => _x.annotations)(schema));
  return LibEithers.bind(findProjectionSpec(cx)(g)(name)(outVertexKey)(outVertexLabelKey)(fields))(((mOutSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => LibEithers.bind(findProjectionSpec(cx)(g)(name)(inVertexKey)(inVertexLabelKey)(fields))(((mInSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => (() => {
  const kind = LibLogic.ifElse(hasVertexAdapters(dir)(mOutSpec)(mInSpec))(({ tag: "edge" }))(({ tag: "vertex" }));
  return LibEithers.bind(findPropertySpecs(cx)(g)(schema)(kind)(fields))(((propSpecs: ReadonlyArray<readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]>) => LibEithers.bind(LibEithers.mapList(((v1: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => propertyAdapter(cx)(g)(schema)(v1)))(propSpecs))(((propAdapters: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t1>, Core.Field, PgModel.Property<t2>>>) => (() => {
  const _m = kind;
  switch (_m.tag) {
    case "vertex": return ((_: void) => constructVertexCoder(cx)(g)(schema)(source)(vidType)(eidType)(name)(fields)(propAdapters))((_m as any).value);
    case "edge": return ((_: void) => constructEdgeCoder(cx)(g)(parentLabel)(schema)(source)(vidType)(eidType)(dir)(name)(fields)(propAdapters)(mOutSpec)(mInSpec))((_m as any).value);
  }
})()))));
})()))));
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("Expected ")("record type"))(", found: "))("other type") }) })(_m);
  }
})();
})()))))));
}

export function elementTreeEdge<t0>(edge: PgModel.Edge<t0>): ((x: ReadonlyArray<PgModel.ElementTree<t0>>) => PgModel.ElementTree<t0>) {
  return ((deps: ReadonlyArray<PgModel.ElementTree<t0>>) => ({
    self: ({ tag: "edge", value: edge }),
    dependencies: deps
  }));
}

export function elementTreeVertex<t0>(vertex: PgModel.Vertex<t0>): ((x: ReadonlyArray<PgModel.ElementTree<t0>>) => PgModel.ElementTree<t0>) {
  return ((deps: ReadonlyArray<PgModel.ElementTree<t0>>) => ({
    self: ({ tag: "vertex", value: vertex }),
    dependencies: deps
  }));
}

export function elementTypeTreeEdge<t0>(etype: PgModel.EdgeType<t0>): ((x: ReadonlyArray<PgModel.ElementTypeTree<t0>>) => PgModel.ElementTypeTree<t0>) {
  return ((deps: ReadonlyArray<PgModel.ElementTypeTree<t0>>) => ({
    self: ({ tag: "edge", value: etype }),
    dependencies: deps
  }));
}

export function elementTypeTreeVertex<t0>(vtype: PgModel.VertexType<t0>): ((x: ReadonlyArray<PgModel.ElementTypeTree<t0>>) => PgModel.ElementTypeTree<t0>) {
  return ((deps: ReadonlyArray<PgModel.ElementTypeTree<t0>>) => ({
    self: ({ tag: "vertex", value: vtype }),
    dependencies: deps
  }));
}

export function encodeProperties<t0, t1>(cx: Context.Context): ((x: ReadonlyMap<Core.Name, Core.Term>) => ((x: ReadonlyArray<Coders.Adapter<Core.FieldType, t0, Core.Field, PgModel.Property<t1>>>) => Errors.Error | ReadonlyMap<PgModel.PropertyKey, t1>)) {
  return ((fields: ReadonlyMap<Core.Name, Core.Term>) => ((adapters: ReadonlyArray<Coders.Adapter<Core.FieldType, t0, Core.Field, PgModel.Property<t1>>>) => LibEithers.map(((props: ReadonlyArray<PgModel.Property<t1>>) => LibMaps.fromList(LibLists.map(((prop: PgModel.Property<t1>) => [((_x) => _x.key)(prop), ((_x) => _x.value)(prop)]))(props))))(LibEithers.map(((xs: ReadonlyArray<PgModel.Property<t1> | null>) => LibMaybes.cat(xs)))(LibEithers.mapList(((v1: Coders.Adapter<Core.FieldType, t0, Core.Field, PgModel.Property<t1>>) => encodeProperty(cx)(fields)(v1)))(adapters)))));
}

export function encodeProperty<t0, t1>(cx: Context.Context): ((x: ReadonlyMap<Core.Name, Core.Term>) => ((x: Coders.Adapter<Core.FieldType, t0, Core.Field, t1>) => Errors.Error | t1 | null)) {
  return ((fields: ReadonlyMap<Core.Name, Core.Term>) => ((adapter: Coders.Adapter<Core.FieldType, t0, Core.Field, t1>) => (() => {
  const fname = ((_x) => _x.name)(((_x) => _x.source)(adapter));
  const ftyp = Strip.deannotateType(((_x) => _x.type)(((_x) => _x.source)(adapter)));
  const isMaybe = (() => {
  const _m = ftyp;
  switch (_m.tag) {
    case "maybe": return ((_: Core.Type) => true)((_m as any).value);
    default: return false(_m);
  }
})();
  const encodeValue = ((v: Core.Term) => LibEithers.map(((x: t1) => x))(((_x) => _x.encode)(((_x) => _x.coder)(adapter))(cx)(({
    name: fname,
    term: v
  }))));
  return LibMaybes.maybe(LibLogic.ifElse(isMaybe)(({ tag: "right", value: null }))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("expected field not found in record: ")(((_x) => _x)(fname)) }) })))(((value: Core.Term) => LibLogic.ifElse(isMaybe)((() => {
  const _m = Strip.deannotateTerm(value);
  switch (_m.tag) {
    case "maybe": return ((ov: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: null }))(encodeValue)(ov))((_m as any).value);
    default: return encodeValue(value)(_m);
  }
})())(encodeValue(value))))(LibMaps.lookup(fname)(fields));
})()));
}

export function extractString<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | string)) {
  return ((g: Graph.Graph) => ((t: Core.Term) => ExtractCore.string(g)(t)));
}

export function findAdjacenEdgeAdapters<t0, t1, t2>(cx: Context.Context): ((x: Graph.Graph) => ((x: PgMapping.Schema<t0, t1, t2>) => ((x: t1) => ((x: t1) => ((x: PgModel.VertexLabel) => ((x: PgModel.Direction) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | ReadonlyArray<readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>]]]>))))))) {
  return ((g: Graph.Graph) => ((schema: PgMapping.Schema<t0, t1, t2>) => ((vidType: t1) => ((eidType: t1) => ((parentLabel: PgModel.VertexLabel) => ((dir: PgModel.Direction) => ((fields: ReadonlyArray<Core.FieldType>) => LibEithers.map(((xs: ReadonlyArray<readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>]]] | null>) => LibMaybes.cat(xs)))(LibEithers.mapList(((field: Core.FieldType) => (() => {
  const key = (() => {
  const _m = dir;
  switch (_m.tag) {
    case "out": return ((_: void) => ((_x) => _x.outEdgeLabel)(((_x) => _x.annotations)(schema)))((_m as any).value);
    case "in": return ((_: void) => ((_x) => _x.inEdgeLabel)(((_x) => _x.annotations)(schema)))((_m as any).value);
  }
})();
  return LibMaybes.maybe(({ tag: "right", value: null }))(((a: Core.Term) => LibEithers.bind(extractString(cx)(g)(a))(((labelStr: string) => LibEithers.bind(elementCoder([dir, parentLabel])(schema)(((_x) => _x.type)(field))(vidType)(eidType)(cx)(g))(((elad: Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>) => ({ tag: "right", value: [dir, [field, [labelStr, elad]]] })))))))(Annotations.getTypeAnnotation(key)(((_x) => _x.type)(field)));
})()))(fields)))))))));
}

export function findIdProjectionSpec<t0>(cx: t0): ((x: boolean) => ((x: Core.Name) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null)))) {
  return ((required: boolean) => ((tname: Core.Name) => ((idKey: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(findSingleFieldWithAnnotationKey(cx)(tname)(idKey)(fields))(((mid: Core.FieldType | null) => LibMaybes.maybe(LibLogic.ifElse(required)(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2("no ")(((_x) => _x)(idKey)))(" field") }) }))(({ tag: "right", value: null })))(((mi: Core.FieldType) => LibEithers.map(((spec: PgMapping.ValueSpec) => [mi, [spec, LibMaybes.map(((s: string) => LibStrings.toUpper(s)))(null)]]))(LibMaybes.maybe(({ tag: "right", value: ({ tag: "value" }) }))(((v1: Core.Term) => PgTermsToElements.decodeValueSpec(cx)(({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  }))(v1)))(Annotations.getTypeAnnotation(idKey)(((_x) => _x.type)(mi))))))(mid)))))));
}

export function findIncidentVertexAdapter<t0, t1, t2>(cx: Context.Context): ((x: Graph.Graph) => ((x: PgMapping.Schema<t0, t1, t2>) => ((x: t1) => ((x: t1) => ((x: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => Errors.Error | readonly [Core.Name, Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>]))))) {
  return ((g: Graph.Graph) => ((schema: PgMapping.Schema<t0, t1, t2>) => ((vidType: t1) => ((eidType: t1) => ((spec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => (() => {
  const field = LibPairs.first(spec);
  return LibEithers.bind(elementCoder(null)(schema)(((_x) => _x.type)(field))(vidType)(eidType)(cx)(g))(((adapter: Coders.Adapter<Core.Type, PgModel.ElementTypeTree<t1>, Core.Term, PgModel.ElementTree<t2>>) => ({ tag: "right", value: [((_x) => _x.name)(field), adapter] })));
})())))));
}

export function findLabelString<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Type) => ((x: Core.Name) => ((x: Core.Name) => Errors.Error | string)))) {
  return ((g: Graph.Graph) => ((source: Core.Type) => ((tname: Core.Name) => ((labelKey: Core.Name) => LibMaybes.maybe(({ tag: "right", value: ((_x) => _x)(tname) }))(((v1: Core.Term) => extractString(cx)(g)(v1)))(Annotations.getTypeAnnotation(labelKey)(source))))));
}

export function findProjectionSpec<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Name) => ((x: Core.Name) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null))))) {
  return ((g: Graph.Graph) => ((tname: Core.Name) => ((key: Core.Name) => ((aliasKey: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(findSingleFieldWithAnnotationKey(cx)(tname)(key)(fields))(((mfield: Core.FieldType | null) => LibMaybes.maybe(({ tag: "right", value: null }))(((field: Core.FieldType) => LibEithers.bind(PgTermsToElements.decodeValueSpec(cx)(g)(LibMaybes.fromJust(Annotations.getTypeAnnotation(key)(((_x) => _x.type)(field)))))(((spec: PgMapping.ValueSpec) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((t: Core.Term) => LibEithers.map(((x: string) => x))(extractString(cx)(g)(t))))(Annotations.getTypeAnnotation(aliasKey)(((_x) => _x.type)(field))))(((alias: string | null) => ({ tag: "right", value: [field, [spec, alias]] })))))))(mfield))))))));
}

export function findPropertySpecs<t0, t1, t2, t3>(cx: t0): ((x: Graph.Graph) => ((x: PgMapping.Schema<t1, t2, t3>) => ((x: PgModel.ElementKind) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | ReadonlyArray<readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]>)))) {
  return ((g: Graph.Graph) => ((schema: PgMapping.Schema<t1, t2, t3>) => ((kind: PgModel.ElementKind) => ((fields: ReadonlyArray<Core.FieldType>) => LibEithers.mapList(((field: Core.FieldType) => (() => {
  const propKeyKey = ((_x) => _x.propertyKey)(((_x) => _x.annotations)(schema));
  const propValueKey = ((_x) => _x.propertyValue)(((_x) => _x.annotations)(schema));
  return LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((a: Core.Term) => LibEithers.map(((x: string) => x))(extractString(cx)(g)(a))))(Annotations.getTypeAnnotation(propKeyKey)(((_x) => _x.type)(field))))(((alias: string | null) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: ({ tag: "value" }) }))(((v1: Core.Term) => PgTermsToElements.decodeValueSpec(cx)(g)(v1)))(Annotations.getTypeAnnotation(propValueKey)(((_x) => _x.type)(field))))(((values: PgMapping.ValueSpec) => ({ tag: "right", value: [field, [values, alias]] })))));
})()))(LibLists.filter(((field: Core.FieldType) => (() => {
  const annots = ((_x) => _x.annotations)(schema);
  const ignoreKey = ((_x) => _x.ignore)(annots);
  const specialKeys = (() => {
  const _m = kind;
  switch (_m.tag) {
    case "vertex": return ((_: void) => [((_x) => _x.vertexId)(annots), ((_x) => _x.outEdgeLabel)(annots), ((_x) => _x.inEdgeLabel)(annots)])((_m as any).value);
    case "edge": return ((_: void) => [((_x) => _x.edgeId)(annots), ((_x) => _x.outVertex)(annots), ((_x) => _x.inVertex)(annots)])((_m as any).value);
  }
})();
  const allKeys = LibLists.concat([[ignoreKey], specialKeys]);
  const hasSpecialAnnotation = LibLists.foldl(((b: boolean) => ((k: Core.Name) => LibLogic.or(b)(LibMaybes.isJust(Annotations.getTypeAnnotation(k)(((_x) => _x.type)(field)))))))(false)(allKeys);
  const hasSpecialFieldName = LibLists.foldl(((b: boolean) => ((k: Core.Name) => LibLogic.or(b)(LibEquality.equal(((_x) => _x.name)(field))(k)))))(false)(specialKeys);
  return LibLogic.not(LibLogic.or(hasSpecialAnnotation)(hasSpecialFieldName));
})()))(fields))))));
}

export function findSingleFieldWithAnnotationKey<t0>(cx: t0): ((x: Core.Name) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | Core.FieldType | null))) {
  return ((tname: Core.Name) => ((key: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const matches = LibLists.filter(((f: Core.FieldType) => LibMaybes.isJust(Annotations.getTypeAnnotation(key)(((_x) => _x.type)(f)))))(fields);
  return LibLogic.ifElse(LibEquality.gt(LibLists.length(matches))(1))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("Multiple fields marked as '")(((_x) => _x)(key)))("' in record type "))(((_x) => _x)(tname)) }) }))(({ tag: "right", value: LibLists.safeHead(matches) }));
})())));
}

export function hasVertexAdapters<t0, t1>(dir: PgModel.Direction): ((x: t0 | null) => ((x: t1 | null) => boolean)) {
  return ((mOutSpec: t0 | null) => ((mInSpec: t1 | null) => (() => {
  const _m = dir;
  switch (_m.tag) {
    case "out": return ((_: void) => LibMaybes.isJust(mInSpec))((_m as any).value);
    case "in": return ((_: void) => LibMaybes.isJust(mOutSpec))((_m as any).value);
    case "both": return ((_: void) => LibLogic.and(LibMaybes.isJust(mOutSpec))(LibMaybes.isJust(mInSpec)))((_m as any).value);
  }
})()));
}

export function projectionAdapter<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: t2) => ((x: Coders.Coder<Core.Term, t3>) => ((x: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, t4]]) => ((x: string) => t5 | readonly [Core.Name, Coders.Adapter<Core.Type, t2, Core.Term, t3>]))))) {
  return ((g: t1) => ((idtype: t2) => ((coder: Coders.Coder<Core.Term, t3>) => ((spec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, t4]]) => ((key: string) => (() => {
  const field = LibPairs.first(spec);
  const values = LibPairs.first(LibPairs.second(spec));
  return LibEithers.bind(PgTermsToElements.parseValueSpec(cx)(g)(values))(((traversal: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>))) => ({ tag: "right", value: [((_x) => _x.name)(field), ({
    isLossy: true,
    source: ((_x) => _x.type)(field),
    target: idtype,
    coder: ({
    encode: ((cx_: Context.Context) => ((typ: Core.Term) => LibEithers.bind(traverseToSingleTerm(cx_)(LibStrings.cat2(key)("-projection"))(((v1: Core.Term) => traversal(cx_)(v1)))(typ))(((t: Core.Term) => ((_x) => _x.encode)(coder)(cx_)(t))))),
    decode: ((cx_: Context.Context) => ((_: t3) => ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2("edge '")(key))("' decoding is not yet supported") }) })))
  })
  })] })));
})())))));
}

export function propertyAdapter<t0, t1, t2, t3>(cx: Context.Context): ((x: t0) => ((x: PgMapping.Schema<t1, t2, t3>) => ((x: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => Errors.Error | Coders.Adapter<Core.FieldType, PgModel.PropertyType<t2>, Core.Field, PgModel.Property<t3>>))) {
  return ((g: t0) => ((schema: PgMapping.Schema<t1, t2, t3>) => ((spec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => (() => {
  const tfield = LibPairs.first(spec);
  const values = LibPairs.first(LibPairs.second(spec));
  const alias = LibPairs.second(LibPairs.second(spec));
  const key = LibMaybes.fromMaybe(((_x) => _x)(((_x) => _x.name)(tfield)))(alias);
  return LibEithers.bind(((_x) => _x.encode)(((_x) => _x.propertyTypes)(schema))(cx)(((_x) => _x.type)(tfield)))(((pt: t2) => LibEithers.bind(PgTermsToElements.parseValueSpec(cx)(g)(values))(((traversal: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>))) => ({ tag: "right", value: ({
    isLossy: true,
    source: tfield,
    target: ({
    key: key,
    value: pt,
    required: true
  }),
    coder: ({
    encode: ((cx_: Context.Context) => ((dfield: Core.Field) => LibEithers.bind(traverseToSingleTerm(cx_)("property traversal")(((v1: Core.Term) => traversal(cx_)(v1)))(((_x) => _x.term)(dfield)))(((result: Core.Term) => LibEithers.bind(((_x) => _x.encode)(((_x) => _x.propertyValues)(schema))(cx_)(result))(((value: t3) => ({ tag: "right", value: ({
    key: key,
    value: value
  }) }))))))),
    decode: ((cx_: Context.Context) => ((_: PgModel.Property<t3>) => ({ tag: "left", value: ({ tag: "other", value: "property decoding is not yet supported" }) })))
  })
  }) })))));
})())));
}

export function propertyTypes<t0, t1, t2, t3>(propAdapters: ReadonlyArray<Coders.Adapter<t0, PgModel.PropertyType<t1>, t2, t3>>): ReadonlyArray<PgModel.PropertyType<t1>> {
  return LibLists.map(((a: Coders.Adapter<t0, PgModel.PropertyType<t1>, t2, t3>) => ({
    key: ((_x) => _x.key)(((_x) => _x.target)(a)),
    value: ((_x) => _x.value)(((_x) => _x.target)(a)),
    required: true
  })))(propAdapters);
}

export function selectEdgeId<t0, t1, t2, t3>(cx: Context.Context): ((x: ReadonlyMap<Core.Name, t0>) => ((x: readonly [Core.Name, Coders.Adapter<t1, t2, t0, t3>]) => Errors.Error | t3)) {
  return ((fields: ReadonlyMap<Core.Name, t0>) => ((ad: readonly [Core.Name, Coders.Adapter<t1, t2, t0, t3>]) => (() => {
  const fname = LibPairs.first(ad);
  const adapter = LibPairs.second(ad);
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2("no ")(((_x) => _x)(fname)))(" in record") }) }))(((t: t0) => ((_x) => _x.encode)(((_x) => _x.coder)(adapter))(cx)(t)))(LibMaps.lookup(fname)(fields));
})()));
}

export function selectVertexId<t0, t1, t2, t3>(cx: Context.Context): ((x: ReadonlyMap<Core.Name, t0>) => ((x: readonly [Core.Name, Coders.Adapter<t1, t2, t0, t3>]) => Errors.Error | t3)) {
  return ((fields: ReadonlyMap<Core.Name, t0>) => ((ad: readonly [Core.Name, Coders.Adapter<t1, t2, t0, t3>]) => (() => {
  const fname = LibPairs.first(ad);
  const adapter = LibPairs.second(ad);
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2("no ")(((_x) => _x)(fname)))(" in record") }) }))(((t: t0) => ((_x) => _x.encode)(((_x) => _x.coder)(adapter))(cx)(t)))(LibMaps.lookup(fname)(fields));
})()));
}

export function traverseToSingleTerm<t0, t1, t2>(cx: t0): ((x: string) => ((x: ((x: t1) => Errors.Error | ReadonlyArray<t2>)) => ((x: t1) => Errors.Error | t2))) {
  return ((desc: string) => ((traversal: ((x: t1) => Errors.Error | ReadonlyArray<t2>)) => ((term: t1) => LibEithers.bind(traversal(term))(((terms: ReadonlyArray<t2>) => LibLogic.ifElse(LibLists.null_(terms))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(desc)(" did not resolve to a term") }) }))(LibLogic.ifElse(LibEquality.equal(LibLists.length(terms))(1))(({ tag: "right", value: LibLists.head(terms) }))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(desc)(" resolved to multiple terms") }) }))))))));
}

export function vertexCoder<t0, t1, t2, t3, t4, t5, t6, t7, t8, t9>(g: t0): ((x: PgMapping.Schema<t1, t2, t3>) => ((x: t4) => ((x: t5) => ((x: t6) => ((x: PgModel.VertexLabel) => ((x: readonly [Core.Name, Coders.Adapter<t7, t8, Core.Term, t3>]) => ((x: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t5>, Core.Field, PgModel.Property<t3>>>) => ((x: ReadonlyArray<readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<t9, PgModel.ElementTypeTree<t5>, Core.Term, PgModel.ElementTree<t3>>]]]>) => Coders.Adapter<t4, PgModel.ElementTypeTree<t5>, Core.Term, PgModel.ElementTree<t3>>)))))))) {
  return ((schema: PgMapping.Schema<t1, t2, t3>) => ((source: t4) => ((vidType: t5) => ((tname: t6) => ((vlabel: PgModel.VertexLabel) => ((idAdapter: readonly [Core.Name, Coders.Adapter<t7, t8, Core.Term, t3>]) => ((propAdapters: ReadonlyArray<Coders.Adapter<Core.FieldType, PgModel.PropertyType<t5>, Core.Field, PgModel.Property<t3>>>) => ((edgeAdapters: ReadonlyArray<readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<t9, PgModel.ElementTypeTree<t5>, Core.Term, PgModel.ElementTree<t3>>]]]>) => (() => {
  const vtype = ({
    label: vlabel,
    id: vidType,
    properties: propertyTypes(propAdapters)
  });
  const depTypes = LibLists.map(((ea: readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<t9, PgModel.ElementTypeTree<t5>, Core.Term, PgModel.ElementTree<t3>>]]]) => ((_x) => _x.target)(LibPairs.second(LibPairs.second(LibPairs.second(ea))))))(edgeAdapters);
  const target = elementTypeTreeVertex(vtype)(depTypes);
  return ({
    isLossy: true,
    source: source,
    target: target,
    coder: ({
    encode: ((cx: Context.Context) => ((term: Core.Term) => (() => {
  const deannot = Strip.deannotateTerm(term);
  const unwrapped = (() => {
  const _m = deannot;
  switch (_m.tag) {
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.fromMaybe(deannot)(mt))((_m as any).value);
    default: return deannot(_m);
  }
})();
  const rec = (() => {
  const _m = unwrapped;
  switch (_m.tag) {
    case "record": return ((r: Core.Record) => r)((_m as any).value);
  }
})();
  const fmap = Resolution.fieldMap(((_x) => _x.fields)(rec));
  return LibEithers.bind(selectVertexId(cx)(fmap)(idAdapter))(((vid: t3) => LibEithers.bind(encodeProperties(cx)(fmap)(propAdapters))(((props: ReadonlyMap<PgModel.PropertyKey, t3>) => LibEithers.bind(LibEithers.map(((xs: ReadonlyArray<ReadonlyArray<PgModel.ElementTree<t3>>>) => LibLists.concat(xs)))(LibEithers.mapList(((ea: readonly [PgModel.Direction, readonly [Core.FieldType, readonly [PgModel.EdgeLabel, Coders.Adapter<t9, PgModel.ElementTypeTree<t5>, Core.Term, PgModel.ElementTree<t3>>]]]) => (() => {
  const eaDir = LibPairs.first(ea);
  const eaField = LibPairs.first(LibPairs.second(ea));
  const eaLabel = LibPairs.first(LibPairs.second(LibPairs.second(ea)));
  const eaAdapter = LibPairs.second(LibPairs.second(LibPairs.second(ea)));
  return LibMaybes.maybe(({ tag: "right", value: [] }))(((fterm: Core.Term) => LibEithers.map(((tree: PgModel.ElementTree<t3>) => ((v1: PgModel.Element) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((vtx: PgModel.Vertex<t3>) => (() => {
  const otherid = ((_x) => _x.id)(vtx);
  const edgeid = ((_x) => _x.defaultEdgeId)(schema);
  const outId = (() => {
  const _m = eaDir;
  switch (_m.tag) {
    case "out": return ((_: void) => vid)((_m as any).value);
    case "in": return ((_: void) => otherid)((_m as any).value);
  }
})();
  const inId = (() => {
  const _m = eaDir;
  switch (_m.tag) {
    case "out": return ((_: void) => otherid)((_m as any).value);
    case "in": return ((_: void) => vid)((_m as any).value);
  }
})();
  const edge = ({ tag: "edge", value: ({
    label: eaLabel,
    id: edgeid,
    out: outId,
    in: inId,
    properties: LibMaps.empty
  }) });
  return [({
    self: edge,
    dependencies: [tree]
  })];
})())((_m as any).value);
    case "edge": return ((edg: PgModel.Edge<t3>) => (() => {
  const fixedEdge = (() => {
  const _m = eaDir;
  switch (_m.tag) {
    case "out": return ((_: void) => ({
    label: ((_x) => _x.label)(edg),
    id: ((_x) => _x.id)(edg),
    out: vid,
    in: ((_x) => _x.in)(edg),
    properties: ((_x) => _x.properties)(edg)
  }))((_m as any).value);
    case "in": return ((_: void) => ({
    label: ((_x) => _x.label)(edg),
    id: ((_x) => _x.id)(edg),
    out: ((_x) => _x.out)(edg),
    in: vid,
    properties: ((_x) => _x.properties)(edg)
  }))((_m as any).value);
  }
})();
  return [({
    self: ({ tag: "edge", value: fixedEdge }),
    dependencies: ((_x) => _x.dependencies)(tree)
  })];
})())((_m as any).value);
  }
})())(((_x) => _x.self)(tree))))(((_x) => _x.encode)(((_x) => _x.coder)(eaAdapter))(cx)(fterm))))(LibMaps.lookup(((_x) => _x.name)(eaField))(fmap));
})()))(edgeAdapters)))(((deps: ReadonlyArray<PgModel.ElementTree<t3>>) => ({ tag: "right", value: elementTreeVertex(({
    label: vlabel,
    id: vid,
    properties: props
  }))(deps) })))))));
})())),
    decode: ((cx: Context.Context) => ((_: PgModel.ElementTree<t3>) => ({ tag: "left", value: ({ tag: "other", value: "vertex decoding is not yet supported" }) })))
  })
  });
})()))))))));
}

export function vertexIdAdapter<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: t5) => ((x: Core.Name) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => Errors.Error | readonly [Core.Name, Coders.Adapter<Core.Type, t5, Core.Term, t4>])))))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((vidType: t5) => ((name: Core.Name) => ((idKey: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(findIdProjectionSpec(cx)(true)(name)(idKey)(fields))(((mIdSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]] | null) => LibEithers.bind(({ tag: "right", value: LibMaybes.fromJust(mIdSpec) }))(((idSpec: readonly [Core.FieldType, readonly [PgMapping.ValueSpec, string | null]]) => projectionAdapter(cx)(g)(vidType)(((_x) => _x.vertexIds)(schema))(idSpec)("id")))))))))));
}

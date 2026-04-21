// Note: this is an automatically generated file. Do not edit.

/**
 * Mappings from property graph schemas to SHACL shapes graphs, and from property graph data to RDF graphs
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
import * as LibEquality from "../../lib/equality.js";
import * as LibLists from "../../lib/lists.js";
import * as LibLogic from "../../lib/logic.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibPairs from "../../lib/pairs.js";
import * as LibSets from "../../lib/sets.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as PgModel from "../model.js";
import * as PgRdfEnvironment from "./environment.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as RdfSyntax from "../../rdf/syntax.js";
import * as RdfUtils from "../../rdf/utils.js";
import * as Relational from "../../relational.js";
import * as ShaclModel from "../../shacl/model.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function edgeTypesToPropertyShapes<t0, t1>(encodeVertexLabel: t0): ((x: ((x: PgModel.EdgeLabel) => RdfSyntax.Iri)) => ((x: PgModel.VertexLabel) => ((x: ReadonlyArray<PgModel.EdgeType<t1>>) => ReadonlyArray<ShaclModel.CommonConstraint>))) {
  return ((encodeEdgeLabel: ((x: PgModel.EdgeLabel) => RdfSyntax.Iri)) => ((vertexLabel: PgModel.VertexLabel) => ((edgeTypes: ReadonlyArray<PgModel.EdgeType<t1>>) => LibLists.concat(LibLists.map(((et: PgModel.EdgeType<t1>) => (() => {
  const outLabel = ((_x) => _x.out)(et);
  const matchesVertex = LibEquality.equal(((_x) => _x)(outLabel))(((_x) => _x)(vertexLabel));
  return (() => {
  const edgeShape = ({ tag: "property", value: LibSets.singleton(({ tag: "anonymous", value: ({
    common: ({
    constraints: LibSets.singleton(({ tag: "class", value: LibSets.singleton(undefined) })),
    deactivated: null,
    message: RdfUtils.emptyLangStrings,
    severity: ({ tag: "violation" }),
    targetClass: LibSets.empty,
    targetNode: LibSets.empty,
    targetObjectsOf: LibSets.empty,
    targetSubjectsOf: LibSets.empty
  }),
    constraints: LibSets.empty,
    defaultValue: null,
    description: RdfUtils.emptyLangStrings,
    name: RdfUtils.emptyLangStrings,
    order: null,
    path: encodeEdgeLabel(((_x) => _x.label)(et))
  }) })) });
  return LibLogic.ifElse(matchesVertex)([edgeShape])([]);
})();
})()))(edgeTypes)))));
}

export function encodeEdge<t0>(env: PgRdfEnvironment.PgRdfEnvironment<t0>): ((x: PgModel.Edge<t0>) => RdfSyntax.Description) {
  return ((edge: PgModel.Edge<t0>) => (() => {
  const elab = ((_x) => _x.label)(edge);
  const eout = ((_x) => _x.out)(edge);
  const ein = ((_x) => _x.in)(edge);
  const subj = ({ tag: "iri", value: ((_x) => _x.encodeVertexId)(env)(eout) });
  const obj = ({ tag: "iri", value: ((_x) => _x.encodeVertexId)(env)(ein) });
  const pred = ((_x) => _x.encodeEdgeLabel)(env)(elab);
  return ({
    subject: RdfUtils.resourceToNode(subj),
    graph: LibSets.singleton(({
    subject: subj,
    predicate: pred,
    object: obj
  }))
  });
})());
}

export function encodeLazyGraph<t0>(env: PgRdfEnvironment.PgRdfEnvironment<t0>): ((x: PgModel.LazyGraph<t0>) => RdfSyntax.Graph) {
  return ((lg: PgModel.LazyGraph<t0>) => (() => {
  const vertexDescs = LibLists.map(((v1: PgModel.Vertex<t0>) => encodeVertex(env)(v1)))(((_x) => _x.vertices)(lg));
  const edgeDescs = LibLists.map(((v1: PgModel.Edge<t0>) => encodeEdge(env)(v1)))(((_x) => _x.edges)(lg));
  const allDescs = LibLists.concat([vertexDescs, edgeDescs]);
  return RdfUtils.descriptionsToGraph(allDescs);
})());
}

export function encodeVertex<t0>(env: PgRdfEnvironment.PgRdfEnvironment<t0>): ((x: PgModel.Vertex<t0>) => RdfSyntax.Description) {
  return ((vertex: PgModel.Vertex<t0>) => (() => {
  const vlab = ((_x) => _x.label)(vertex);
  const vid = ((_x) => _x.id)(vertex);
  const vprops = ((_x) => _x.properties)(vertex);
  const subj = ({ tag: "iri", value: ((_x) => _x.encodeVertexId)(env)(vid) });
  const rtype = ({ tag: "iri", value: ((_x) => _x.encodeVertexLabel)(env)(vlab) });
  const typeTriple = ({
    subject: subj,
    predicate: RdfUtils.rdfIri("type"),
    object: rtype
  });
  const propTriples = LibLists.map(((kv: readonly [PgModel.PropertyKey, t0]) => (() => {
  const key = LibPairs.first(kv);
  const val = LibPairs.second(kv);
  const pred = ((_x) => _x.encodePropertyKey)(env)(key);
  const obj = ({ tag: "literal", value: ((_x) => _x.encodePropertyValue)(env)(val) });
  return ({
    subject: subj,
    predicate: pred,
    object: obj
  });
})()))(LibMaps.toList(vprops));
  const allTriples = LibLists.cons(typeTriple)(propTriples);
  return ({
    subject: RdfUtils.resourceToNode(subj),
    graph: LibSets.fromList(allTriples)
  });
})());
}

export function graphSchemaToShapesGraph<t0>(encodeType: ((x: t0) => RdfSyntax.Iri)): ((x: ((x: PgModel.VertexLabel) => RdfSyntax.Iri)) => ((x: ((x: PgModel.EdgeLabel) => RdfSyntax.Iri)) => ((x: ((x: PgModel.PropertyKey) => RdfSyntax.Iri)) => ((x: PgModel.GraphSchema<t0>) => ShaclModel.ShapesGraph)))) {
  return ((encodeVertexLabel: ((x: PgModel.VertexLabel) => RdfSyntax.Iri)) => ((encodeEdgeLabel: ((x: PgModel.EdgeLabel) => RdfSyntax.Iri)) => ((encodeKey: ((x: PgModel.PropertyKey) => RdfSyntax.Iri)) => ((schema: PgModel.GraphSchema<t0>) => (() => {
  const vertexTypes = LibMaps.elems(((_x) => _x.vertices)(schema));
  const edgeTypes = LibMaps.elems(((_x) => _x.edges)(schema));
  const defs = LibLists.map(((vt: PgModel.VertexType<t0>) => (() => {
  const baseDef = vertexTypeToNodeShape(encodeType)(encodeVertexLabel)(encodeKey)(vt);
  const edgeShapes = edgeTypesToPropertyShapes(encodeVertexLabel)(encodeEdgeLabel)(((_x) => _x.label)(vt))(edgeTypes);
  const baseShape = ((_x) => _x.target)(baseDef);
  const baseNode = (() => {
  const _m = baseShape;
  switch (_m.tag) {
    case "node": return ((ns: ShaclModel.NodeShape) => ns)((_m as any).value);
    case "property": return ((_: ShaclModel.PropertyShape) => ({
    common: ({
    constraints: LibSets.empty,
    deactivated: null,
    message: RdfUtils.emptyLangStrings,
    severity: ({ tag: "violation" }),
    targetClass: LibSets.empty,
    targetNode: LibSets.empty,
    targetObjectsOf: LibSets.empty,
    targetSubjectsOf: LibSets.empty
  })
  }))((_m as any).value);
  }
})();
  const baseCommon = ((_x) => _x.common)(baseNode);
  const mergedConstraints = LibSets.union(((_x) => _x.constraints)(baseCommon))(LibSets.fromList(edgeShapes));
  const updatedCommon = ({
    constraints: mergedConstraints,
    deactivated: ((_x) => _x.deactivated)(baseCommon),
    message: ((_x) => _x.message)(baseCommon),
    severity: ((_x) => _x.severity)(baseCommon),
    targetClass: ((_x) => _x.targetClass)(baseCommon),
    targetNode: ((_x) => _x.targetNode)(baseCommon),
    targetObjectsOf: ((_x) => _x.targetObjectsOf)(baseCommon),
    targetSubjectsOf: ((_x) => _x.targetSubjectsOf)(baseCommon)
  });
  const updatedShape = ({ tag: "node", value: ({
    common: updatedCommon
  }) });
  return ({
    iri: ((_x) => _x.iri)(baseDef),
    target: updatedShape
  });
})()))(vertexTypes);
  return LibSets.fromList(defs);
})()))));
}

export function propertyTypeToPropertyShape<t0>(encodeType: ((x: t0) => RdfSyntax.Iri)): ((x: ((x: PgModel.PropertyKey) => RdfSyntax.Iri)) => ((x: PgModel.PropertyType<t0>) => ShaclModel.PropertyShape)) {
  return ((encodeKey: ((x: PgModel.PropertyKey) => RdfSyntax.Iri)) => ((pt: PgModel.PropertyType<t0>) => (() => {
  const key = ((_x) => _x.key)(pt);
  const path = encodeKey(key);
  const required_ = ((_x) => _x.required)(pt);
  const dtIri = encodeType(((_x) => _x.value)(pt));
  const constraints = LibSets.singleton(({ tag: "datatype", value: dtIri }));
  const propConstraints = LibLogic.ifElse(required_)(LibSets.singleton(({ tag: "minCount", value: 1n })))(LibSets.empty);
  return ({
    common: ({
    constraints: constraints,
    deactivated: null,
    message: RdfUtils.emptyLangStrings,
    severity: ({ tag: "violation" }),
    targetClass: LibSets.empty,
    targetNode: LibSets.empty,
    targetObjectsOf: LibSets.empty,
    targetSubjectsOf: LibSets.empty
  }),
    constraints: propConstraints,
    defaultValue: null,
    description: RdfUtils.emptyLangStrings,
    name: RdfUtils.emptyLangStrings,
    order: null,
    path: path
  });
})()));
}

export function vertexTypeToNodeShape<t0>(encodeType: ((x: t0) => RdfSyntax.Iri)): ((x: ((x: PgModel.VertexLabel) => RdfSyntax.Iri)) => ((x: ((x: PgModel.PropertyKey) => RdfSyntax.Iri)) => ((x: PgModel.VertexType<t0>) => ShaclModel.Definition<ShaclModel.Shape>))) {
  return ((encodeLabel: ((x: PgModel.VertexLabel) => RdfSyntax.Iri)) => ((encodeKey: ((x: PgModel.PropertyKey) => RdfSyntax.Iri)) => ((vt: PgModel.VertexType<t0>) => (() => {
  const label = ((_x) => _x.label)(vt);
  const labelIri = encodeLabel(label);
  const propTypes = ((_x) => _x.properties)(vt);
  const propShapes = LibLists.map(((pt: PgModel.PropertyType<t0>) => ({ tag: "property", value: LibSets.singleton(({ tag: "anonymous", value: propertyTypeToPropertyShape(encodeType)(encodeKey)(pt) })) })))(propTypes);
  const common = ({
    constraints: LibSets.fromList(propShapes),
    deactivated: null,
    message: RdfUtils.emptyLangStrings,
    severity: ({ tag: "violation" }),
    targetClass: LibSets.singleton(undefined),
    targetNode: LibSets.empty,
    targetObjectsOf: LibSets.empty,
    targetSubjectsOf: LibSets.empty
  });
  return ({
    iri: labelIri,
    target: ({ tag: "node", value: ({
    common: common
  }) })
  });
})())));
}

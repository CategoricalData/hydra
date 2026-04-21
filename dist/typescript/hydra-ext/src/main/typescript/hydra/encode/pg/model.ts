// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.pg.model
 */



import * as Core from "../../core.js";
import * as EncodeCore from "../core.js";
import * as LibLists from "../../lib/lists.js";
import * as LibMaps from "../../lib/maps.js";
import * as PgModel from "../../pg/model.js";

export function adjacentEdge<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.AdjacentEdge<t0>) => Core.Term) {
  return ((x: PgModel.AdjacentEdge<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.AdjacentEdge",
    fields: [({
    name: "label",
    term: edgeLabel(((_x) => _x.label)(x))
  }), ({
    name: "id",
    term: v(((_x) => _x.id)(x))
  }), ({
    name: "vertex",
    term: v(((_x) => _x.vertex)(x))
  }), ({
    name: "properties",
    term: ({ tag: "map", value: LibMaps.bimap(propertyKey)(v)(((_x) => _x.properties)(x)) })
  })]
  }) }));
}

export function direction(v1: PgModel.Direction): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "out": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Direction",
    field: ({
    name: "out",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "in": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Direction",
    field: ({
    name: "in",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "both": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Direction",
    field: ({
    name: "both",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "undirected": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Direction",
    field: ({
    name: "undirected",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function edge<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.Edge<t0>) => Core.Term) {
  return ((x: PgModel.Edge<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.Edge",
    fields: [({
    name: "label",
    term: edgeLabel(((_x) => _x.label)(x))
  }), ({
    name: "id",
    term: v(((_x) => _x.id)(x))
  }), ({
    name: "out",
    term: v(((_x) => _x.out)(x))
  }), ({
    name: "in",
    term: v(((_x) => _x.in)(x))
  }), ({
    name: "properties",
    term: ({ tag: "map", value: LibMaps.bimap(propertyKey)(v)(((_x) => _x.properties)(x)) })
  })]
  }) }));
}

export function edgeLabel(x: PgModel.EdgeLabel): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.pg.model.EdgeLabel",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function edgeType<t0>(t: ((x: t0) => Core.Term)): ((x: PgModel.EdgeType<t0>) => Core.Term) {
  return ((x: PgModel.EdgeType<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.EdgeType",
    fields: [({
    name: "label",
    term: edgeLabel(((_x) => _x.label)(x))
  }), ({
    name: "id",
    term: t(((_x) => _x.id)(x))
  }), ({
    name: "out",
    term: vertexLabel(((_x) => _x.out)(x))
  }), ({
    name: "in",
    term: vertexLabel(((_x) => _x.in)(x))
  }), ({
    name: "properties",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.PropertyType<t0>) => propertyType(t)(v1)))(((_x) => _x.properties)(x)) })
  })]
  }) }));
}

export function element<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.Element<t0>) => Core.Term) {
  return ((v1: PgModel.Element) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((y: PgModel.Vertex<t0>) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Element",
    field: ({
    name: "vertex",
    term: vertex(v)(y)
  })
  }) }))((_m as any).value);
    case "edge": return ((y: PgModel.Edge<t0>) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Element",
    field: ({
    name: "edge",
    term: edge(v)(y)
  })
  }) }))((_m as any).value);
  }
})());
}

export function elementKind(v1: PgModel.ElementKind): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.ElementKind",
    field: ({
    name: "vertex",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "edge": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.ElementKind",
    field: ({
    name: "edge",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function elementTree<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.ElementTree<t0>) => Core.Term) {
  return ((x: PgModel.ElementTree<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.ElementTree",
    fields: [({
    name: "self",
    term: element(v)(((_x) => _x.self)(x))
  }), ({
    name: "dependencies",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.ElementTree<t0>) => elementTree(v)(v1)))(((_x) => _x.dependencies)(x)) })
  })]
  }) }));
}

export function elementType<t0>(t: ((x: t0) => Core.Term)): ((x: PgModel.ElementType<t0>) => Core.Term) {
  return ((v1: PgModel.ElementType) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((y: PgModel.VertexType<t0>) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.ElementType",
    field: ({
    name: "vertex",
    term: vertexType(t)(y)
  })
  }) }))((_m as any).value);
    case "edge": return ((y: PgModel.EdgeType<t0>) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.ElementType",
    field: ({
    name: "edge",
    term: edgeType(t)(y)
  })
  }) }))((_m as any).value);
  }
})());
}

export function elementTypeTree<t0>(t: ((x: t0) => Core.Term)): ((x: PgModel.ElementTypeTree<t0>) => Core.Term) {
  return ((x: PgModel.ElementTypeTree<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.ElementTypeTree",
    fields: [({
    name: "self",
    term: elementType(t)(((_x) => _x.self)(x))
  }), ({
    name: "dependencies",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.ElementTypeTree<t0>) => elementTypeTree(t)(v1)))(((_x) => _x.dependencies)(x)) })
  })]
  }) }));
}

export function graph<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.Graph<t0>) => Core.Term) {
  return ((x: PgModel.Graph<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.Graph",
    fields: [({
    name: "vertices",
    term: ({ tag: "map", value: LibMaps.bimap(v)(((v1: PgModel.Vertex<t0>) => vertex(v)(v1)))(((_x) => _x.vertices)(x)) })
  }), ({
    name: "edges",
    term: ({ tag: "map", value: LibMaps.bimap(v)(((v1: PgModel.Edge<t0>) => edge(v)(v1)))(((_x) => _x.edges)(x)) })
  })]
  }) }));
}

export function graphSchema<t0>(t: ((x: t0) => Core.Term)): ((x: PgModel.GraphSchema<t0>) => Core.Term) {
  return ((x: PgModel.GraphSchema<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.GraphSchema",
    fields: [({
    name: "vertices",
    term: ({ tag: "map", value: LibMaps.bimap(vertexLabel)(((v1: PgModel.VertexType<t0>) => vertexType(t)(v1)))(((_x) => _x.vertices)(x)) })
  }), ({
    name: "edges",
    term: ({ tag: "map", value: LibMaps.bimap(edgeLabel)(((v1: PgModel.EdgeType<t0>) => edgeType(t)(v1)))(((_x) => _x.edges)(x)) })
  })]
  }) }));
}

export function label(v1: PgModel.Label): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((y: PgModel.VertexLabel) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Label",
    field: ({
    name: "vertex",
    term: vertexLabel(y)
  })
  }) }))((_m as any).value);
    case "edge": return ((y: PgModel.EdgeLabel) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.model.Label",
    field: ({
    name: "edge",
    term: edgeLabel(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function lazyGraph<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.LazyGraph<t0>) => Core.Term) {
  return ((x: PgModel.LazyGraph<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.LazyGraph",
    fields: [({
    name: "vertices",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.Vertex<t0>) => vertex(v)(v1)))(((_x) => _x.vertices)(x)) })
  }), ({
    name: "edges",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.Edge<t0>) => edge(v)(v1)))(((_x) => _x.edges)(x)) })
  })]
  }) }));
}

export function property<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.Property<t0>) => Core.Term) {
  return ((x: PgModel.Property<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.Property",
    fields: [({
    name: "key",
    term: propertyKey(((_x) => _x.key)(x))
  }), ({
    name: "value",
    term: v(((_x) => _x.value)(x))
  })]
  }) }));
}

export function propertyKey(x: PgModel.PropertyKey): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.pg.model.PropertyKey",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function propertyType<t0>(t: ((x: t0) => Core.Term)): ((x: PgModel.PropertyType<t0>) => Core.Term) {
  return ((x: PgModel.PropertyType<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.PropertyType",
    fields: [({
    name: "key",
    term: propertyKey(((_x) => _x.key)(x))
  }), ({
    name: "value",
    term: t(((_x) => _x.value)(x))
  }), ({
    name: "required",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: ((_x) => _x.required)(x) }) })
  })]
  }) }));
}

export function vertex<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.Vertex<t0>) => Core.Term) {
  return ((x: PgModel.Vertex<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.Vertex",
    fields: [({
    name: "label",
    term: vertexLabel(((_x) => _x.label)(x))
  }), ({
    name: "id",
    term: v(((_x) => _x.id)(x))
  }), ({
    name: "properties",
    term: ({ tag: "map", value: LibMaps.bimap(propertyKey)(v)(((_x) => _x.properties)(x)) })
  })]
  }) }));
}

export function vertexLabel(x: PgModel.VertexLabel): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.pg.model.VertexLabel",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function vertexType<t0>(t: ((x: t0) => Core.Term)): ((x: PgModel.VertexType<t0>) => Core.Term) {
  return ((x: PgModel.VertexType<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.VertexType",
    fields: [({
    name: "label",
    term: vertexLabel(((_x) => _x.label)(x))
  }), ({
    name: "id",
    term: t(((_x) => _x.id)(x))
  }), ({
    name: "properties",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.PropertyType<t0>) => propertyType(t)(v1)))(((_x) => _x.properties)(x)) })
  })]
  }) }));
}

export function vertexWithAdjacentEdges<t0>(v: ((x: t0) => Core.Term)): ((x: PgModel.VertexWithAdjacentEdges<t0>) => Core.Term) {
  return ((x: PgModel.VertexWithAdjacentEdges<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.pg.model.VertexWithAdjacentEdges",
    fields: [({
    name: "vertex",
    term: vertex(v)(((_x) => _x.vertex)(x))
  }), ({
    name: "ins",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.AdjacentEdge<t0>) => adjacentEdge(v)(v1)))(((_x) => _x.ins)(x)) })
  }), ({
    name: "outs",
    term: ({ tag: "list", value: LibLists.map(((v1: PgModel.AdjacentEdge<t0>) => adjacentEdge(v)(v1)))(((_x) => _x.outs)(x)) })
  })]
  }) }));
}

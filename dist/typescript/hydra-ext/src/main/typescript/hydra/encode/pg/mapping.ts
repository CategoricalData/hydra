// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.pg.mapping
 */



import * as Core from "../../core.js";
import * as EncodeCoders from "../coders.js";
import * as EncodeCore from "../core.js";
import * as EncodePgModel from "./model.js";
import * as LibLists from "../../lib/lists.js";
import * as PgMapping from "../../pg/mapping.js";

export function annotationSchema(x: PgMapping.AnnotationSchema): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.pg.mapping.AnnotationSchema",
    fields: [({
    name: "vertexLabel",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.vertexLabel)(x) }) })
  }), ({
    name: "edgeLabel",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.edgeLabel)(x) }) })
  }), ({
    name: "vertexId",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.vertexId)(x) }) })
  }), ({
    name: "edgeId",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.edgeId)(x) }) })
  }), ({
    name: "propertyKey",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.propertyKey)(x) }) })
  }), ({
    name: "propertyValue",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.propertyValue)(x) }) })
  }), ({
    name: "outVertex",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.outVertex)(x) }) })
  }), ({
    name: "outVertexLabel",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.outVertexLabel)(x) }) })
  }), ({
    name: "inVertex",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.inVertex)(x) }) })
  }), ({
    name: "inVertexLabel",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.inVertexLabel)(x) }) })
  }), ({
    name: "outEdge",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.outEdge)(x) }) })
  }), ({
    name: "outEdgeLabel",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.outEdgeLabel)(x) }) })
  }), ({
    name: "inEdge",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.inEdge)(x) }) })
  }), ({
    name: "inEdgeLabel",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.inEdgeLabel)(x) }) })
  }), ({
    name: "ignore",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.ignore)(x) }) })
  })]
  }) });
}

export function edgeSpec(x: PgMapping.EdgeSpec): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.pg.mapping.EdgeSpec",
    fields: [({
    name: "label",
    term: EncodePgModel.edgeLabel(((_x) => _x.label)(x))
  }), ({
    name: "id",
    term: valueSpec(((_x) => _x.id)(x))
  }), ({
    name: "out",
    term: valueSpec(((_x) => _x.out)(x))
  }), ({
    name: "in",
    term: valueSpec(((_x) => _x.in)(x))
  }), ({
    name: "properties",
    term: ({ tag: "list", value: LibLists.map(propertySpec)(((_x) => _x.properties)(x)) })
  })]
  }) });
}

export function elementSpec(v1: PgMapping.ElementSpec): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "vertex": return ((y: PgMapping.VertexSpec) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.mapping.ElementSpec",
    field: ({
    name: "vertex",
    term: vertexSpec(y)
  })
  }) }))((_m as any).value);
    case "edge": return ((y: PgMapping.EdgeSpec) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.mapping.ElementSpec",
    field: ({
    name: "edge",
    term: edgeSpec(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function propertySpec(x: PgMapping.PropertySpec): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.pg.mapping.PropertySpec",
    fields: [({
    name: "key",
    term: EncodePgModel.propertyKey(((_x) => _x.key)(x))
  }), ({
    name: "value",
    term: valueSpec(((_x) => _x.value)(x))
  })]
  }) });
}

export function valueSpec(v1: PgMapping.ValueSpec): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "value": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.mapping.ValueSpec",
    field: ({
    name: "value",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "pattern": return ((y: string) => ({ tag: "inject", value: ({
    typeName: "hydra.pg.mapping.ValueSpec",
    field: ({
    name: "pattern",
    term: ({ tag: "literal", value: ({ tag: "string", value: y }) })
  })
  }) }))((_m as any).value);
  }
})();
}

export function vertexSpec(x: PgMapping.VertexSpec): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.pg.mapping.VertexSpec",
    fields: [({
    name: "label",
    term: EncodePgModel.vertexLabel(((_x) => _x.label)(x))
  }), ({
    name: "id",
    term: valueSpec(((_x) => _x.id)(x))
  }), ({
    name: "properties",
    term: ({ tag: "list", value: LibLists.map(propertySpec)(((_x) => _x.properties)(x)) })
  })]
  }) });
}

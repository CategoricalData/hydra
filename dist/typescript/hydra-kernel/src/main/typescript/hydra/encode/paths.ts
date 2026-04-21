// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.paths
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as Paths from "../paths.js";

export function subtermEdge(x: Paths.SubtermEdge): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.paths.SubtermEdge",
    fields: [({
    name: "source",
    term: subtermNode(((_x) => _x.source)(x))
  }), ({
    name: "path",
    term: subtermPath(((_x) => _x.path)(x))
  }), ({
    name: "target",
    term: subtermNode(((_x) => _x.target)(x))
  })]
  }) });
}

export function subtermGraph(x: Paths.SubtermGraph): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.paths.SubtermGraph",
    fields: [({
    name: "nodes",
    term: ({ tag: "list", value: LibLists.map(subtermNode)(((_x) => _x.nodes)(x)) })
  }), ({
    name: "edges",
    term: ({ tag: "list", value: LibLists.map(subtermEdge)(((_x) => _x.edges)(x)) })
  })]
  }) });
}

export function subtermNode(x: Paths.SubtermNode): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.paths.SubtermNode",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  }), ({
    name: "label",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.label)(x) }) })
  }), ({
    name: "id",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.id)(x) }) })
  })]
  }) });
}

export function subtermPath(x: Paths.SubtermPath): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.paths.SubtermPath",
    body: ({ tag: "list", value: LibLists.map(subtermStep)(((_x) => _x)(x)) })
  }) });
}

export function subtermStep(v1: Paths.SubtermStep): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotatedBody": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "annotatedBody",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "applicationFunction": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "applicationFunction",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "applicationArgument": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "applicationArgument",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "lambdaBody": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "lambdaBody",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "unionCasesDefault": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "unionCasesDefault",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "unionCasesBranch": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "unionCasesBranch",
    term: EncodeCore.name(y)
  })
  }) }))((_m as any).value);
    case "letBody": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "letBody",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "letBinding": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "letBinding",
    term: EncodeCore.name(y)
  })
  }) }))((_m as any).value);
    case "listElement": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "listElement",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "mapKey": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "mapKey",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "mapValue": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "mapValue",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "maybeTerm": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "maybeTerm",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "productTerm": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "productTerm",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "recordField": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "recordField",
    term: EncodeCore.name(y)
  })
  }) }))((_m as any).value);
    case "setElement": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "setElement",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "sumTerm": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "sumTerm",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "typeLambdaBody": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "typeLambdaBody",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "typeApplicationTerm": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "typeApplicationTerm",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "injectionTerm": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "injectionTerm",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "wrappedTerm": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtermStep",
    field: ({
    name: "wrappedTerm",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function subtypeEdge(x: Paths.SubtypeEdge): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.paths.SubtypeEdge",
    fields: [({
    name: "source",
    term: subtypeNode(((_x) => _x.source)(x))
  }), ({
    name: "path",
    term: subtypePath(((_x) => _x.path)(x))
  }), ({
    name: "target",
    term: subtypeNode(((_x) => _x.target)(x))
  })]
  }) });
}

export function subtypeGraph(x: Paths.SubtypeGraph): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.paths.SubtypeGraph",
    fields: [({
    name: "nodes",
    term: ({ tag: "list", value: LibLists.map(subtypeNode)(((_x) => _x.nodes)(x)) })
  }), ({
    name: "edges",
    term: ({ tag: "list", value: LibLists.map(subtypeEdge)(((_x) => _x.edges)(x)) })
  })]
  }) });
}

export function subtypeNode(x: Paths.SubtypeNode): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.paths.SubtypeNode",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  }), ({
    name: "label",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.label)(x) }) })
  }), ({
    name: "id",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.id)(x) }) })
  })]
  }) });
}

export function subtypePath(x: Paths.SubtypePath): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.paths.SubtypePath",
    body: ({ tag: "list", value: LibLists.map(subtypeStep)(((_x) => _x)(x)) })
  }) });
}

export function subtypeStep(v1: Paths.SubtypeStep): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotatedBody": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "annotatedBody",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "applicationFunction": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "applicationFunction",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "applicationArgument": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "applicationArgument",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "eitherLeft": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "eitherLeft",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "eitherRight": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "eitherRight",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "forallBody": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "forallBody",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "functionDomain": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "functionDomain",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "functionCodomain": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "functionCodomain",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "listElement": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "listElement",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "mapKeys": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "mapKeys",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "mapValues": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "mapValues",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "maybeElement": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "maybeElement",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "pairFirst": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "pairFirst",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "pairSecond": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "pairSecond",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "recordField": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "recordField",
    term: EncodeCore.name(y)
  })
  }) }))((_m as any).value);
    case "setElement": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "setElement",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "unionField": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "unionField",
    term: EncodeCore.name(y)
  })
  }) }))((_m as any).value);
    case "wrappedType": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.paths.SubtypeStep",
    field: ({
    name: "wrappedType",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.packaging
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as EncodeGraph from "./graph.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as Packaging from "../packaging.js";

export function definition(v1: Packaging.Definition): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "term": return ((y: Packaging.TermDefinition) => ({ tag: "inject", value: ({
    typeName: "hydra.packaging.Definition",
    field: ({
    name: "term",
    term: termDefinition(y)
  })
  }) }))((_m as any).value);
    case "type": return ((y: Packaging.TypeDefinition) => ({ tag: "inject", value: ({
    typeName: "hydra.packaging.Definition",
    field: ({
    name: "type",
    term: typeDefinition(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function fileExtension(x: Packaging.FileExtension): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.packaging.FileExtension",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function module(x: Packaging.Module): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.packaging.Module",
    fields: [({
    name: "namespace",
    term: namespace(((_x) => _x.namespace)(x))
  }), ({
    name: "definitions",
    term: ({ tag: "list", value: LibLists.map(definition)(((_x) => _x.definitions)(x)) })
  }), ({
    name: "termDependencies",
    term: ({ tag: "list", value: LibLists.map(namespace)(((_x) => _x.termDependencies)(x)) })
  }), ({
    name: "typeDependencies",
    term: ({ tag: "list", value: LibLists.map(namespace)(((_x) => _x.typeDependencies)(x)) })
  }), ({
    name: "description",
    term: ({ tag: "maybe", value: LibMaybes.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x.description)(x)) })
  })]
  }) });
}

export function namespace(x: Packaging.Namespace): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.packaging.Namespace",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function namespaces<t0>(n: ((x: t0) => Core.Term)): ((x: Packaging.Namespaces<t0>) => Core.Term) {
  return ((x: Packaging.Namespaces<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.packaging.Namespaces",
    fields: [({
    name: "focus",
    term: ({ tag: "pair", value: LibPairs.bimap(namespace)(n)(((_x) => _x.focus)(x)) })
  }), ({
    name: "mapping",
    term: ({ tag: "map", value: LibMaps.bimap(namespace)(n)(((_x) => _x.mapping)(x)) })
  })]
  }) }));
}

export function package_(x: Packaging.Package): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.packaging.Package",
    fields: [({
    name: "name",
    term: packageName(((_x) => _x.name)(x))
  }), ({
    name: "modules",
    term: ({ tag: "list", value: LibLists.map(module)(((_x) => _x.modules)(x)) })
  }), ({
    name: "dependencies",
    term: ({ tag: "list", value: LibLists.map(packageName)(((_x) => _x.dependencies)(x)) })
  }), ({
    name: "description",
    term: ({ tag: "maybe", value: LibMaybes.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x.description)(x)) })
  })]
  }) });
}

export function packageName(x: Packaging.PackageName): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.packaging.PackageName",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function qualifiedName(x: Packaging.QualifiedName): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.packaging.QualifiedName",
    fields: [({
    name: "namespace",
    term: ({ tag: "maybe", value: LibMaybes.map(namespace)(((_x) => _x.namespace)(x)) })
  }), ({
    name: "local",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.local)(x) }) })
  })]
  }) });
}

export function termDefinition(x: Packaging.TermDefinition): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.packaging.TermDefinition",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  }), ({
    name: "term",
    term: EncodeCore.term(((_x) => _x.term)(x))
  }), ({
    name: "type",
    term: ({ tag: "maybe", value: LibMaybes.map(EncodeCore.typeScheme)(((_x) => _x.type)(x)) })
  })]
  }) });
}

export function typeDefinition(x: Packaging.TypeDefinition): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.packaging.TypeDefinition",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  }), ({
    name: "type",
    term: EncodeCore.typeScheme(((_x) => _x.type)(x))
  })]
  }) });
}

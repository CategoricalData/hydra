// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.typing
 */



import * as Core from "../core.js";
import * as EncodeContext from "./context.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as Typing from "../typing.js";

export function functionStructure<t0>(env: ((x: t0) => Core.Term)): ((x: Typing.FunctionStructure<t0>) => Core.Term) {
  return ((x: Typing.FunctionStructure<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.typing.FunctionStructure",
    fields: [({
    name: "typeParams",
    term: ({ tag: "list", value: LibLists.map(EncodeCore.name)(((_x) => _x.typeParams)(x)) })
  }), ({
    name: "params",
    term: ({ tag: "list", value: LibLists.map(EncodeCore.name)(((_x) => _x.params)(x)) })
  }), ({
    name: "bindings",
    term: ({ tag: "list", value: LibLists.map(EncodeCore.binding)(((_x) => _x.bindings)(x)) })
  }), ({
    name: "body",
    term: EncodeCore.term(((_x) => _x.body)(x))
  }), ({
    name: "domains",
    term: ({ tag: "list", value: LibLists.map(EncodeCore.type)(((_x) => _x.domains)(x)) })
  }), ({
    name: "codomain",
    term: ({ tag: "maybe", value: LibMaybes.map(EncodeCore.type)(((_x) => _x.codomain)(x)) })
  }), ({
    name: "environment",
    term: env(((_x) => _x.environment)(x))
  })]
  }) }));
}

export function inferenceResult(x: Typing.InferenceResult): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.typing.InferenceResult",
    fields: [({
    name: "term",
    term: EncodeCore.term(((_x) => _x.term)(x))
  }), ({
    name: "type",
    term: EncodeCore.type(((_x) => _x.type)(x))
  }), ({
    name: "subst",
    term: typeSubst(((_x) => _x.subst)(x))
  }), ({
    name: "classConstraints",
    term: ({ tag: "map", value: LibMaps.bimap(EncodeCore.name)(EncodeCore.typeVariableMetadata)(((_x) => _x.classConstraints)(x)) })
  }), ({
    name: "context",
    term: EncodeContext.context(((_x) => _x.context)(x))
  })]
  }) });
}

export function termSubst(x: Typing.TermSubst): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.typing.TermSubst",
    body: ({ tag: "map", value: LibMaps.bimap(EncodeCore.name)(EncodeCore.term)(((_x) => _x)(x)) })
  }) });
}

export function typeConstraint(x: Typing.TypeConstraint): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.typing.TypeConstraint",
    fields: [({
    name: "left",
    term: EncodeCore.type(((_x) => _x.left)(x))
  }), ({
    name: "right",
    term: EncodeCore.type(((_x) => _x.right)(x))
  }), ({
    name: "comment",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.comment)(x) }) })
  })]
  }) });
}

export function typeSubst(x: Typing.TypeSubst): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.typing.TypeSubst",
    body: ({ tag: "map", value: LibMaps.bimap(EncodeCore.name)(EncodeCore.type)(((_x) => _x)(x)) })
  }) });
}

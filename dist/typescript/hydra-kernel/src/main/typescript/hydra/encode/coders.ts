// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.coders
 */



import * as Coders from "../coders.js";
import * as Core from "../core.js";
import * as EncodeContext from "./context.js";
import * as EncodeCore from "./core.js";
import * as EncodeErrors from "./errors.js";
import * as EncodeGraph from "./graph.js";
import * as EncodeVariants from "./variants.js";

export function coderDirection(v1: Coders.CoderDirection): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "encode": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.coders.CoderDirection",
    field: ({
    name: "encode",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "decode": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.coders.CoderDirection",
    field: ({
    name: "decode",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function languageName(x: Coders.LanguageName): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.coders.LanguageName",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function traversalOrder(v1: Coders.TraversalOrder): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "pre": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.coders.TraversalOrder",
    field: ({
    name: "pre",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "post": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.coders.TraversalOrder",
    field: ({
    name: "post",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

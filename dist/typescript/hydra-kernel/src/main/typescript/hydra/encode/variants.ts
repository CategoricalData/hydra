// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.variants
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as Variants from "../variants.js";

export function eliminationVariant(v1: Variants.EliminationVariant): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "record": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.EliminationVariant",
    field: ({
    name: "record",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "union": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.EliminationVariant",
    field: ({
    name: "union",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "wrap": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.EliminationVariant",
    field: ({
    name: "wrap",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function functionVariant(v1: Variants.FunctionVariant): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "elimination": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.FunctionVariant",
    field: ({
    name: "elimination",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "lambda": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.FunctionVariant",
    field: ({
    name: "lambda",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function literalVariant(v1: Variants.LiteralVariant): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "binary": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.LiteralVariant",
    field: ({
    name: "binary",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "boolean": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.LiteralVariant",
    field: ({
    name: "boolean",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "float": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.LiteralVariant",
    field: ({
    name: "float",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "integer": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.LiteralVariant",
    field: ({
    name: "integer",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "string": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.LiteralVariant",
    field: ({
    name: "string",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function termVariant(v1: Variants.TermVariant): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "annotated",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "application": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "application",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "cases": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "cases",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "either": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "either",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "inject": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "inject",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "lambda": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "lambda",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "let": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "let",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "list": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "list",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "literal": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "literal",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "map": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "map",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "maybe": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "maybe",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "pair": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "pair",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "project": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "project",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "record": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "record",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "set": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "set",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "typeApplication": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "typeApplication",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "typeLambda": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "typeLambda",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "unit": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "unwrap": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "unwrap",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "variable": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "variable",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "wrap": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TermVariant",
    field: ({
    name: "wrap",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function typeVariant(v1: Variants.TypeVariant): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "annotated",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "application": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "application",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "either": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "either",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "forall": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "forall",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "function": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "function",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "list": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "list",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "literal": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "literal",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "map": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "map",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "maybe": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "maybe",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "pair": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "pair",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "record": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "record",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "set": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "set",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "union": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "union",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "unit": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "variable": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "variable",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "void": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "void",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "wrap": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.variants.TypeVariant",
    field: ({
    name: "wrap",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

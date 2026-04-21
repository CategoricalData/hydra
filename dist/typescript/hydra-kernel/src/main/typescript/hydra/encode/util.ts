// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.util
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as Util from "../util.js";

export function caseConvention(v1: Util.CaseConvention): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "camel": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.CaseConvention",
    field: ({
    name: "camel",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "pascal": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.CaseConvention",
    field: ({
    name: "pascal",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "lowerSnake": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.CaseConvention",
    field: ({
    name: "lowerSnake",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "upperSnake": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.CaseConvention",
    field: ({
    name: "upperSnake",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function comparison(v1: Util.Comparison): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "lessThan": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "equalTo": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "equalTo",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "greaterThan": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "greaterThan",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function precision(v1: Util.Precision): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "arbitrary": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.util.Precision",
    field: ({
    name: "arbitrary",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "bits": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.util.Precision",
    field: ({
    name: "bits",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
  }
})();
}

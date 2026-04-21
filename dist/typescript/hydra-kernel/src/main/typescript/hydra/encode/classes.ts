// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.classes
 */



import * as Classes from "../classes.js";
import * as Core from "../core.js";
import * as EncodeCore from "./core.js";

export function typeClass(v1: Classes.TypeClass): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "equality": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.classes.TypeClass",
    field: ({
    name: "equality",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "ordering": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.classes.TypeClass",
    field: ({
    name: "ordering",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.json.model
 */



import * as Core from "../../core.js";
import * as EncodeCore from "../core.js";
import * as JsonModel from "../../json/model.js";
import * as LibLists from "../../lib/lists.js";
import * as LibMaps from "../../lib/maps.js";

export function value(v1: JsonModel.Value): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "array": return ((y: ReadonlyArray<JsonModel.Value>) => ({ tag: "inject", value: ({
    typeName: "hydra.json.model.Value",
    field: ({
    name: "array",
    term: ({ tag: "list", value: LibLists.map(value)(y) })
  })
  }) }))((_m as any).value);
    case "boolean": return ((y: boolean) => ({ tag: "inject", value: ({
    typeName: "hydra.json.model.Value",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: y }) })
  })
  }) }))((_m as any).value);
    case "null": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.json.model.Value",
    field: ({
    name: "null",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "number": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.json.model.Value",
    field: ({
    name: "number",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "object": return ((y: ReadonlyMap<string, JsonModel.Value>) => ({ tag: "inject", value: ({
    typeName: "hydra.json.model.Value",
    field: ({
    name: "object",
    term: ({ tag: "map", value: LibMaps.bimap(((x: string) => ({ tag: "literal", value: ({ tag: "string", value: x }) })))(value)(y) })
  })
  }) }))((_m as any).value);
    case "string": return ((y: string) => ({ tag: "inject", value: ({
    typeName: "hydra.json.model.Value",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: y }) })
  })
  }) }))((_m as any).value);
  }
})();
}

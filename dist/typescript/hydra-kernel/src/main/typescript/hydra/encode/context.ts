// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.context
 */



import * as Context from "../context.js";
import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";

export function context(x: Context.Context): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.context.Context",
    fields: [({
    name: "trace",
    term: ({ tag: "list", value: LibLists.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x.trace)(x)) })
  }), ({
    name: "messages",
    term: ({ tag: "list", value: LibLists.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x.messages)(x)) })
  }), ({
    name: "other",
    term: ({ tag: "map", value: LibMaps.bimap(EncodeCore.name)(EncodeCore.term)(((_x) => _x.other)(x)) })
  })]
  }) });
}

export function inContext<t0>(e: ((x: t0) => Core.Term)): ((x: Context.InContext<t0>) => Core.Term) {
  return ((x: Context.InContext<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.context.InContext",
    fields: [({
    name: "object",
    term: e(((_x) => _x.object)(x))
  }), ({
    name: "context",
    term: context(((_x) => _x.context)(x))
  })]
  }) }));
}

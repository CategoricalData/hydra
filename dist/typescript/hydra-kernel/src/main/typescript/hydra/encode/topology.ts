// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.topology
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";
import * as LibSets from "../lib/sets.js";
import * as Topology from "../topology.js";

export function graph(m: ReadonlyMap<number, ReadonlyArray<number>>): Core.Term {
  return ({ tag: "map", value: LibMaps.bimap(vertex)(((xs: ReadonlyArray<number>) => ({ tag: "list", value: LibLists.map(vertex)(xs) })))(m) });
}

export function tarjanState(x: Topology.TarjanState): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.topology.TarjanState",
    fields: [({
    name: "counter",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: ((_x) => _x.counter)(x) }) }) })
  }), ({
    name: "indices",
    term: ({ tag: "map", value: LibMaps.bimap(vertex)(((x2: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: x2 }) }) })))(((_x) => _x.indices)(x)) })
  }), ({
    name: "lowLinks",
    term: ({ tag: "map", value: LibMaps.bimap(vertex)(((x2: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: x2 }) }) })))(((_x) => _x.lowLinks)(x)) })
  }), ({
    name: "stack",
    term: ({ tag: "list", value: LibLists.map(vertex)(((_x) => _x.stack)(x)) })
  }), ({
    name: "onStack",
    term: ({ tag: "set", value: LibSets.map(vertex)(((_x) => _x.onStack)(x)) })
  }), ({
    name: "sccs",
    term: ({ tag: "list", value: LibLists.map(((xs2: ReadonlyArray<number>) => ({ tag: "list", value: LibLists.map(vertex)(xs2) })))(((_x) => _x.sccs)(x)) })
  })]
  }) });
}

export function vertex(x: number): Core.Term {
  return ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: x }) }) });
}

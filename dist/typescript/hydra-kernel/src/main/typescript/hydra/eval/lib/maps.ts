// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of Map functions for the Hydra interpreter.
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as ExtractCore from "../../extract/core.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as LibLists from "../../lib/lists.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibPairs from "../../lib/pairs.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function alter<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: t1) => ((funTerm: Core.Term) => ((keyTerm: Core.Term) => ((mapTerm: Core.Term) => (() => {
  const _m = mapTerm;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const currentVal = LibMaps.lookup(keyTerm)(m);
  return (() => {
  const newVal = ({ tag: "application", value: ({
    function: funTerm,
    argument: ({ tag: "maybe", value: currentVal })
  }) });
  return ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.delete" }),
    argument: keyTerm
  }) }),
    argument: mapTerm
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "newV",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.insert" }),
    argument: keyTerm
  }) }),
    argument: ({ tag: "variable", value: "newV" })
  }) }),
    argument: mapTerm
  }) })
  }) })
  }) }),
    argument: newVal
  }) }) });
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map value",
    actual: ShowCore.term(mapTerm)
  }) }) }) })(_m);
  }
})()))));
}

export function bimap<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: t1) => ((keyFun: Core.Term) => ((valFun: Core.Term) => ((mapTerm: Core.Term) => (() => {
  const _m = mapTerm;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const pairs = LibMaps.toList(m);
  return ({ tag: "right", value: ({ tag: "map", value: LibMaps.fromList(LibLists.map(((p: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(p);
  return (() => {
  const v = LibPairs.second(p);
  return [({ tag: "application", value: ({
    function: keyFun,
    argument: k
  }) }), ({ tag: "application", value: ({
    function: valFun,
    argument: v
  }) })];
})();
})()))(pairs)) }) });
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map value",
    actual: ShowCore.term(mapTerm)
  }) }) }) })(_m);
  }
})()))));
}

export function filter<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((valPred: Core.Term) => ((mapTerm: Core.Term) => (() => {
  const _m = mapTerm;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const pairs = LibMaps.toList(m);
  return ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: LibLists.map(((p: readonly [Core.Term, Core.Term]) => (() => {
  const v = LibPairs.second(p);
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: valPred,
    argument: v
  }) })
  }) }),
    argument: ({ tag: "list", value: LibLists.pure(({ tag: "pair", value: [LibPairs.first(p), v] })) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) });
})()))(pairs) })
  }) })
  }) }) });
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map value",
    actual: ShowCore.term(mapTerm)
  }) }) }) })(_m);
  }
})())));
}

export function filterWithKey<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((pred: Core.Term) => ((mapTerm: Core.Term) => (() => {
  const _m = mapTerm;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const pairs = LibMaps.toList(m);
  return ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: LibLists.map(((p: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(p);
  return (() => {
  const v = LibPairs.second(p);
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: pred,
    argument: k
  }) }),
    argument: v
  }) })
  }) }),
    argument: ({ tag: "list", value: LibLists.pure(({ tag: "pair", value: [k, v] })) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) });
})();
})()))(pairs) })
  }) })
  }) }) });
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map value",
    actual: ShowCore.term(mapTerm)
  }) }) }) })(_m);
  }
})())));
}

export function findWithDefault<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term)))) {
  return ((g: t1) => ((defaultTerm: Core.Term) => ((keyTerm: Core.Term) => ((mapTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.fromMaybe" }),
    argument: defaultTerm
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.lookup" }),
    argument: keyTerm
  }) }),
    argument: mapTerm
  }) })
  }) }) })))));
}

export function map<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((valFun: Core.Term) => ((mapTerm: Core.Term) => (() => {
  const _m = mapTerm;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const pairs = LibMaps.toList(m);
  return ({ tag: "right", value: ({ tag: "map", value: LibMaps.fromList(LibLists.map(((p: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(p);
  return (() => {
  const v = LibPairs.second(p);
  return [k, ({ tag: "application", value: ({
    function: valFun,
    argument: v
  }) })];
})();
})()))(pairs)) }) });
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map value",
    actual: ShowCore.term(mapTerm)
  }) }) }) })(_m);
  }
})())));
}

export function mapKeys<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((keyFun: Core.Term) => ((mapTerm: Core.Term) => (() => {
  const _m = mapTerm;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const pairs = LibMaps.toList(m);
  return ({ tag: "right", value: ({ tag: "map", value: LibMaps.fromList(LibLists.map(((p: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(p);
  return (() => {
  const v = LibPairs.second(p);
  return [({ tag: "application", value: ({
    function: keyFun,
    argument: k
  }) }), v];
})();
})()))(pairs)) }) });
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map value",
    actual: ShowCore.term(mapTerm)
  }) }) }) })(_m);
  }
})())));
}

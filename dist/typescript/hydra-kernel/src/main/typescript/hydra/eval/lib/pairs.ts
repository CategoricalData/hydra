// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of Pair functions for the Hydra interpreter.
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

export function bimap<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: t1) => ((firstFun: Core.Term) => ((secondFun: Core.Term) => ((pairTerm: Core.Term) => (() => {
  const _m = pairTerm;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const fst = LibPairs.first(p);
  return (() => {
  const snd = LibPairs.second(p);
  return ({ tag: "right", value: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: firstFun,
    argument: fst
  }) }), ({ tag: "application", value: ({
    function: secondFun,
    argument: snd
  }) })] }) });
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "pair value",
    actual: ShowCore.term(pairTerm)
  }) }) }) })(_m);
  }
})()))));
}

export function first<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((pairTerm: Core.Term) => (() => {
  const _m = pairTerm;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => ({ tag: "right", value: LibPairs.first(p) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "pair value",
    actual: ShowCore.term(pairTerm)
  }) }) }) })(_m);
  }
})()));
}

export function second<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((pairTerm: Core.Term) => (() => {
  const _m = pairTerm;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => ({ tag: "right", value: LibPairs.second(p) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "pair value",
    actual: ShowCore.term(pairTerm)
  }) }) }) })(_m);
  }
})()));
}

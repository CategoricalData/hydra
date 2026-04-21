// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of Either functions for the Hydra interpreter.
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
import * as LibEithers from "../../lib/eithers.js";
import * as LibLists from "../../lib/lists.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibPairs from "../../lib/pairs.js";
import * as LibSets from "../../lib/sets.js";
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
  return ((g: t1) => ((leftFun: Core.Term) => ((rightFun: Core.Term) => ((eitherTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((val: Core.Term) => ({ tag: "either", value: ({ tag: "left", value: ({ tag: "application", value: ({
    function: leftFun,
    argument: val
  }) }) }) })))(((val: Core.Term) => ({ tag: "either", value: ({ tag: "right", value: ({ tag: "application", value: ({
    function: rightFun,
    argument: val
  }) }) }) })))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})()))));
}

export function bind<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((eitherTerm: Core.Term) => ((funTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((val: Core.Term) => ({ tag: "either", value: ({ tag: "left", value: val }) })))(((val: Core.Term) => ({ tag: "application", value: ({
    function: funTerm,
    argument: val
  }) })))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})())));
}

export function either<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: t1) => ((leftFun: Core.Term) => ((rightFun: Core.Term) => ((eitherTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((val: Core.Term) => ({ tag: "application", value: ({
    function: leftFun,
    argument: val
  }) })))(((val: Core.Term) => ({ tag: "application", value: ({
    function: rightFun,
    argument: val
  }) })))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})()))));
}

export function foldl<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((initTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: funTerm,
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: el
  }) })
  }) })
  }) }),
    argument: acc
  }) }))))(({ tag: "either", value: ({ tag: "right", value: initTerm }) }))(elements) })))))));
}

export function fromLeft<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((defaultTerm: Core.Term) => ((eitherTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((val: Core.Term) => val))(((_: Core.Term) => defaultTerm))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})())));
}

export function fromRight<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((defaultTerm: Core.Term) => ((eitherTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((_: Core.Term) => defaultTerm))(((val: Core.Term) => val))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})())));
}

export function isLeft<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((eitherTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((_: Core.Term) => ({ tag: "literal", value: ({ tag: "boolean", value: true }) })))(((_: Core.Term) => ({ tag: "literal", value: ({ tag: "boolean", value: false }) })))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})()));
}

export function isRight<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((eitherTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((_: Core.Term) => ({ tag: "literal", value: ({ tag: "boolean", value: false }) })))(((_: Core.Term) => ({ tag: "literal", value: ({ tag: "boolean", value: true }) })))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})()));
}

export function lefts<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "list", value: LibLists.foldl(((acc: ReadonlyArray<Core.Term>) => ((el: Core.Term) => (() => {
  const _m = el;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((val: Core.Term) => LibLists.concat2(acc)(LibLists.pure(val))))(((_: Core.Term) => acc))(e))((_m as any).value);
    default: return acc(_m);
  }
})())))([])(elements) }) })))));
}

export function map<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((rightFun: Core.Term) => ((eitherTerm: Core.Term) => (() => {
  const _m = eitherTerm;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "right", value: LibEithers.either(((val: Core.Term) => ({ tag: "either", value: ({ tag: "left", value: val }) })))(((val: Core.Term) => ({ tag: "either", value: ({ tag: "right", value: ({ tag: "application", value: ({
    function: rightFun,
    argument: val
  }) }) }) })))(e) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(eitherTerm)
  }) }) }) })(_m);
  }
})())));
}

export function mapList<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "accErr",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "accErr" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "ys",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "variable", value: "ys" })
  }) }) }) })
  }) })
  }) }),
    argument: acc
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: funTerm,
    argument: el
  }) })
  }) }))))(({ tag: "either", value: ({ tag: "right", value: ({ tag: "list", value: [] }) }) }))(LibLists.reverse(elements)) }))))));
}

export function mapMaybe<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((funTerm: Core.Term) => ((maybeTerm: Core.Term) => (() => {
  const _m = maybeTerm;
  switch (_m.tag) {
    case "maybe": return ((opt: Core.Term | null) => ({ tag: "right", value: LibMaybes.maybe(({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: null }) }) }))(((val: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: ({ tag: "variable", value: "y" }) }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: funTerm,
    argument: val
  }) })
  }) })))(opt) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "maybe value",
    actual: ShowCore.term(maybeTerm)
  }) }) }) })(_m);
  }
})())));
}

export function mapSet<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((setTerm: Core.Term) => LibEithers.bind(ExtractCore.set(g)(setTerm))(((elements: ReadonlySet<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "accErr",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "accErr" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "ys",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.insert" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "variable", value: "ys" })
  }) }) }) })
  }) })
  }) }),
    argument: acc
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: funTerm,
    argument: el
  }) })
  }) }))))(({ tag: "either", value: ({ tag: "right", value: ({ tag: "set", value: LibSets.fromList([]) }) }) }))(LibSets.toList(elements)) }))))));
}

export function partitionEithers<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | readonly [ReadonlyArray<Core.Term>, ReadonlyArray<Core.Term>])) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: readonly [ReadonlyArray<Core.Term>, ReadonlyArray<Core.Term>]) => ((el: Core.Term) => (() => {
  const ls = LibPairs.first(acc);
  return (() => {
  const rs = LibPairs.second(acc);
  return (() => {
  const _m = el;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((val: Core.Term) => [LibLists.concat2(ls)(LibLists.pure(val)), rs]))(((val: Core.Term) => [ls, LibLists.concat2(rs)(LibLists.pure(val))]))(e))((_m as any).value);
    default: return acc(_m);
  }
})();
})();
})())))([[], []])(elements) })))));
}

export function rights<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "list", value: LibLists.foldl(((acc: ReadonlyArray<Core.Term>) => ((el: Core.Term) => (() => {
  const _m = el;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((_: Core.Term) => acc))(((val: Core.Term) => LibLists.concat2(acc)(LibLists.pure(val))))(e))((_m as any).value);
    default: return acc(_m);
  }
})())))([])(elements) }) })))));
}

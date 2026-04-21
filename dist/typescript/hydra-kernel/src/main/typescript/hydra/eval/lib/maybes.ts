// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of Maybe functions for the Hydra interpreter.
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

export function apply<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((funOptTerm: Core.Term) => ((argOptTerm: Core.Term) => (() => {
  const _m = funOptTerm;
  switch (_m.tag) {
    case "maybe": return ((mf: Core.Term | null) => (() => {
  const _m = argOptTerm;
  switch (_m.tag) {
    case "maybe": return ((mx: Core.Term | null) => ({ tag: "right", value: ({ tag: "maybe", value: LibMaybes.bind(mf)(((f: Core.Term) => LibMaybes.map(((x: Core.Term) => ({ tag: "application", value: ({
    function: f,
    argument: x
  }) })))(mx))) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(argOptTerm)
  }) }) }) })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional function",
    actual: ShowCore.term(funOptTerm)
  }) }) }) })(_m);
  }
})())));
}

export function bind<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((optTerm: Core.Term) => ((funTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: LibMaybes.maybe(({ tag: "maybe", value: null }))(((val: Core.Term) => ({ tag: "application", value: ({
    function: funTerm,
    argument: val
  }) })))(m) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})())));
}

export function cases<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: t1) => ((optTerm: Core.Term) => ((defaultTerm: Core.Term) => ((funTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: LibMaybes.maybe(defaultTerm)(((val: Core.Term) => ({ tag: "application", value: ({
    function: funTerm,
    argument: val
  }) })))(m) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})()))));
}

export function cat<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>)) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: ReadonlyArray<Core.Term>) => ((el: Core.Term) => (() => {
  const _m = el;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => LibMaybes.maybe(acc)(((v: Core.Term) => LibLists.concat2(acc)(LibLists.pure(v))))(m))((_m as any).value);
    default: return acc(_m);
  }
})())))([])(elements) })))));
}

export function compose<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term)))) {
  return ((g: t1) => ((funF: Core.Term) => ((funG: Core.Term) => ((xTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.bind" }),
    argument: ({ tag: "application", value: ({
    function: funF,
    argument: xTerm
  }) })
  }) }),
    argument: funG
  }) }) })))));
}

export function fromJust<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((optTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "Just value",
    actual: ShowCore.term(optTerm)
  }) }) }) }))(((val: Core.Term) => ({ tag: "right", value: val })))(m))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})()));
}

export function fromMaybe<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((defaultTerm: Core.Term) => ((optTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: LibMaybes.maybe(defaultTerm)(((val: Core.Term) => val))(m) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})())));
}

export function isJust<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((optTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: LibMaybes.maybe(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))(((_: Core.Term) => ({ tag: "literal", value: ({ tag: "boolean", value: true }) })))(m) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})()));
}

export function isNothing<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((optTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: LibMaybes.maybe(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))(((_: Core.Term) => ({ tag: "literal", value: ({ tag: "boolean", value: false }) })))(m) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})()));
}

export function map<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: t1) => ((funTerm: Core.Term) => ((optTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: ({ tag: "maybe", value: LibMaybes.map(((val: Core.Term) => ({ tag: "application", value: ({
    function: funTerm,
    argument: val
  }) })))(m) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})())));
}

export function mapMaybe<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.cat" }),
    argument: ({ tag: "list", value: LibLists.map(((el: Core.Term) => ({ tag: "application", value: ({
    function: funTerm,
    argument: el
  }) })))(elements) })
  }) }) }))))));
}

export function maybe<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: t1) => ((defaultTerm: Core.Term) => ((funTerm: Core.Term) => ((optTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: LibMaybes.maybe(defaultTerm)(((val: Core.Term) => ({ tag: "application", value: ({
    function: funTerm,
    argument: val
  }) })))(m) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})()))));
}

export function pure<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => t2 | Core.Term)) {
  return ((g: t1) => ((x: Core.Term) => ({ tag: "right", value: ({ tag: "maybe", value: x }) })));
}

export function toList<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: t1) => ((optTerm: Core.Term) => (() => {
  const _m = optTerm;
  switch (_m.tag) {
    case "maybe": return ((m: Core.Term | null) => ({ tag: "right", value: ({ tag: "list", value: LibMaybes.maybe([])(((val: Core.Term) => LibLists.pure(val)))(m) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "optional value",
    actual: ShowCore.term(optTerm)
  }) }) }) })(_m);
  }
})()));
}

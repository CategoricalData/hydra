// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of List functions for the Hydra interpreter.
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
import * as LibLogic from "../../lib/logic.js";
import * as LibPairs from "../../lib/pairs.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Reduction from "../../reduction.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function apply<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((funsTerm: Core.Term) => ((argsTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(funsTerm))(((funs: ReadonlyArray<Core.Term>) => LibEithers.bind(ExtractCore.list(g)(argsTerm))(((arguments_: ReadonlyArray<Core.Term>) => (() => {
  const applyOne = ((f: Core.Term) => LibLists.map(((arg: Core.Term) => ({ tag: "application", value: ({
    function: f,
    argument: arg
  }) })))(arguments_));
  return ({ tag: "right", value: ({ tag: "list", value: LibLists.concat(LibLists.map(applyOne)(funs)) }) });
})())))))));
}

export function bind<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => ((funTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: LibLists.map(((el: Core.Term) => ({ tag: "application", value: ({
    function: funTerm,
    argument: el
  }) })))(elements) })
  }) }) }))))));
}

export function concat2<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((list1: Core.Term) => ((list2: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [list1, list2] })
  }) }) }))));
}

export function dropWhile<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((predTerm: Core.Term) => ((listTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.span" }),
    argument: predTerm
  }) }),
    argument: listTerm
  }) })
  }) }) }))));
}

export function elem<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((x: Core.Term) => ((listTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.isJust" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.find" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.equal" }),
    argument: x
  }) })
  }) }),
    argument: listTerm
  }) })
  }) }) }))));
}

export function filter<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((predTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: LibLists.map(((el: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: predTerm,
    argument: el
  }) })
  }) }),
    argument: ({ tag: "list", value: LibLists.pure(el) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) })))(elements) })
  }) }) }))))));
}

export function find<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((predTerm: Core.Term) => ((listTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.safeHead" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.filter" }),
    argument: predTerm
  }) }),
    argument: listTerm
  }) })
  }) }) }))));
}

export function foldl(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((initTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => LibLists.foldl(((acc: Errors.Error | Core.Term) => ((el: Core.Term) => LibEithers.bind(acc)(((reducedAcc: Core.Term) => Reduction.reduceTerm(cx)(g)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: funTerm,
    argument: reducedAcc
  }) }),
    argument: el
  }) })))))))(({ tag: "right", value: initTerm }))(elements)))))));
}

export function foldr(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((initTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => LibLists.foldr(((el: Core.Term) => ((acc: Errors.Error | Core.Term) => LibEithers.bind(acc)(((reducedAcc: Core.Term) => Reduction.reduceTerm(cx)(g)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: funTerm,
    argument: el
  }) }),
    argument: reducedAcc
  }) })))))))(({ tag: "right", value: initTerm }))(elements)))))));
}

export function group<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => t2 | Core.Term)) {
  return ((g: t1) => ((listTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "foldResult",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.null" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "foldResult" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "foldResult" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "foldResult" })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "foldResult" })
  }) })] })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "acc",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "el",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "pair", value: [({ tag: "list", value: [({ tag: "variable", value: "el" })] }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "acc" })
  }) })] })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "h",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.equal" }),
    argument: ({ tag: "variable", value: "el" })
  }) }),
    argument: ({ tag: "variable", value: "h" })
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "acc" })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "el" })] })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "acc" })
  }) })] })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "list", value: [({ tag: "variable", value: "el" })] }), ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "acc" })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "acc" })
  }) })] })
  }) })] })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.safeHead" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "acc" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] })
  }) }),
    argument: listTerm
  }) })
  }) }) })));
}

export function intercalate<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((sep: Core.Term) => ((listsTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intersperse" }),
    argument: sep
  }) }),
    argument: listsTerm
  }) })
  }) }) }))));
}

export function intersperse<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((sep: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLogic.ifElse(LibLists.null_(elements))(({ tag: "list", value: [] }))(({ tag: "list", value: LibLists.cons(LibLists.head(elements))(LibLists.concat(LibLists.map(((el: Core.Term) => [sep, el]))(LibLists.tail(elements)))) })) }))))));
}

export function map<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "list", value: LibLists.reverse(LibLists.foldl(((acc: ReadonlyArray<Core.Term>) => ((el: Core.Term) => LibLists.cons(({ tag: "application", value: ({
    function: funTerm,
    argument: el
  }) }))(acc))))([])(elements)) }) }))))));
}

export function maybeHead<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLogic.ifElse(LibLists.null_(elements))(({ tag: "maybe", value: null }))(({ tag: "maybe", value: LibLists.head(elements) })) })))));
}

export function nub<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "acc",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "acc" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "acc" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "variable", value: "acc" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: listTerm
  }) }) })))));
}

export function partition<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((predTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => (() => {
  const initialState = ({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] });
  return (() => {
  const finalState = LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => (() => {
  const yeses = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: acc
  }) });
  return (() => {
  const nos = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: acc
  }) });
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: predTerm,
    argument: el
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: yeses
  }) }),
    argument: ({ tag: "list", value: [el] })
  }) }), nos] })
  }) }),
    argument: ({ tag: "pair", value: [yeses, ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: nos
  }) }),
    argument: ({ tag: "list", value: [el] })
  }) })] })
  }) });
})();
})())))(initialState)(elements);
  return ({ tag: "right", value: finalState });
})();
})())))));
}

export function pure<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => t2 | Core.Term)) {
  return ((g: t1) => ((x: Core.Term) => ({ tag: "right", value: ({ tag: "list", value: [x] }) })));
}

export function replicate<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((n: Core.Term) => ((x: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: x
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.range" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: n
  }) })
  }) }) }))));
}

export function safeHead<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLogic.ifElse(LibLists.null_(elements))(({ tag: "maybe", value: null }))(({ tag: "maybe", value: LibLists.head(elements) })) })))));
}

export function singleton<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => t2 | Core.Term)) {
  return ((g: t1) => ((x: Core.Term) => ({ tag: "right", value: ({ tag: "list", value: [x] }) })));
}

export function sort<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => t2 | Core.Term)) {
  return ((g: t1) => ((listTerm: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sortOn" }),
    argument: ({ tag: "variable", value: "hydra.lib.equality.identity" })
  }) }),
    argument: listTerm
  }) }) })));
}

export function sortOn<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((projTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((sorted: Core.Term) => ((x: Core.Term) => (() => {
  const splitResult = ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.span" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lte" }),
    argument: ({ tag: "application", value: ({
    function: projTerm,
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: projTerm,
    argument: x
  }) })
  }) })
  }) })
  }) }),
    argument: sorted
  }) });
  return (() => {
  const before = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: splitResult
  }) });
  return (() => {
  const after = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: splitResult
  }) });
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: before
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: x
  }) }),
    argument: after
  }) })
  }) });
})();
})();
})())))(({ tag: "list", value: [] }))(elements) }))))));
}

export function span<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((predTerm: Core.Term) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => (() => {
  const initialState = ({ tag: "pair", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "boolean", value: true }) }), ({ tag: "list", value: [] })] }), ({ tag: "list", value: [] })] });
  return (() => {
  const finalState = LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => (() => {
  const takingLeft = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: acc
  }) });
  return (() => {
  const right = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: acc
  }) });
  return (() => {
  const taking = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: takingLeft
  }) });
  return (() => {
  const left = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: takingLeft
  }) });
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.and" }),
    argument: taking
  }) }),
    argument: ({ tag: "application", value: ({
    function: predTerm,
    argument: el
  }) })
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "boolean", value: true }) }), ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: left
  }) }),
    argument: ({ tag: "list", value: [el] })
  }) })] }), right] })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "boolean", value: false }) }), left] }), ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: right
  }) }),
    argument: ({ tag: "list", value: [el] })
  }) })] })
  }) });
})();
})();
})();
})())))(initialState)(elements);
  return ({ tag: "right", value: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: finalState
  }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: finalState
  }) })] }) });
})();
})())))));
}

export function zipWith<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((g: Graph.Graph) => ((funTerm: Core.Term) => ((listTerm1: Core.Term) => ((listTerm2: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm1))(((elements1: ReadonlyArray<Core.Term>) => LibEithers.bind(ExtractCore.list(g)(listTerm2))(((elements2: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "list", value: LibLists.map(((p: readonly [Core.Term, Core.Term]) => (() => {
  const a = LibPairs.first(p);
  return (() => {
  const b = LibPairs.second(p);
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: funTerm,
    argument: a
  }) }),
    argument: b
  }) });
})();
})()))(LibLists.zip(elements1)(elements2)) }) })))))))));
}

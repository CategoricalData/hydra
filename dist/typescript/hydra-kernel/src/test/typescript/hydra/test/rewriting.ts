// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for core rewrite/fold combinators
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMath from "../lib/math.js";
import * as LibPairs from "../lib/pairs.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Rewriting from "../rewriting.js";
import * as ShowCore from "../show/core.js";
import * as Tabular from "../tabular.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "rewriting",
    description: null,
    subgroups: [({
    name: "foldOverTerm",
    description: null,
    subgroups: [],
    cases: [({
    name: "collect labels from single node - pre-order",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((lit: Core.Literal) => ({ tag: "literal", value: lit })))(Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: ReadonlyArray<Core.Literal>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const _m = LibPairs.first(p);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => [lit])((_m as any).value);
    default: return [](_m);
  }
})())((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "list", value: [] })] }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "collect labels from tree - pre-order",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((lit: Core.Literal) => ({ tag: "literal", value: lit })))(Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: ReadonlyArray<Core.Literal>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const _m = LibPairs.first(p);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => [lit])((_m as any).value);
    default: return [](_m);
  }
})())((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "list", value: [] })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "d" }) }), ({ tag: "list", value: [] })] })] })] })] })] }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "literal", value: ({ tag: "string", value: "d" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "collect labels from single node - post-order",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((lit: Core.Literal) => ({ tag: "literal", value: lit })))(Rewriting.foldOverTerm(({ tag: "post" }))(((acc: ReadonlyArray<Core.Literal>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const _m = LibPairs.first(p);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => [lit])((_m as any).value);
    default: return [](_m);
  }
})())((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "list", value: [] })] }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "collect labels from tree - post-order",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((lit: Core.Literal) => ({ tag: "literal", value: lit })))(Rewriting.foldOverTerm(({ tag: "post" }))(((acc: ReadonlyArray<Core.Literal>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const _m = LibPairs.first(p);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => [lit])((_m as any).value);
    default: return [](_m);
  }
})())((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "list", value: [] })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "d" }) }), ({ tag: "list", value: [] })] })] })] })] })] }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "d" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sum int32 literals",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })] })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 52 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "collect list lengths - pre-order",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: n }) }) })))(Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: ReadonlyArray<number>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "list": return ((elems: ReadonlyArray<Core.Term>) => [LibLists.length(elems)])((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })] }), ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "quux" }) })] })
  }) })] }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "collect list lengths - post-order",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: n }) }) })))(Rewriting.foldOverTerm(({ tag: "post" }))(((acc: ReadonlyArray<number>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "list": return ((elems: ReadonlyArray<Core.Term>) => [LibLists.length(elems)])((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })] }), ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "quux" }) })] })
  }) })] }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "rewriteType",
    description: null,
    subgroups: [],
    cases: [({
    name: "String type in left side of either is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String type in right side of either is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String types in both sides of either are replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String type in nested either (left of left) is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "either", value: ({
    left: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "either", value: ({
    left: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String type in nested either (right of right) is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }),
    right: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }),
    right: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String types in complex nested either are all replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "either", value: ({
    left: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    right: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "either", value: ({
    left: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    right: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String in list type is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String in function domain is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String in function codomain is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "String in optional type is replaced",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.type(Rewriting.rewriteType(((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => LibLogic.ifElse(LibEquality.equal(typ)(({ tag: "literal", value: ({ tag: "string" }) })))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(recurse(typ)))))(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "rewriteTerm",
    description: null,
    subgroups: [],
    cases: [({
    name: "string literal foo replaced with bar",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "literal", value: ({ tag: "string", value: "foo" }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in variable not changed",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "variable", value: "x" }))),
    expected: ShowCore.term(({ tag: "variable", value: "x" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple strings in list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in optional (just)",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in function application",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "print" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "print" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in lambda body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in nested applications",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in record field",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "strings in multiple record fields",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "record", value: ({
    typeName: "Data",
    fields: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }), ({
    name: "b",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  }), ({
    name: "c",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "Data",
    fields: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }), ({
    name: "b",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  }), ({
    name: "c",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in pair",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in let binding value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in let body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in first case branch",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }), ({
    name: "error",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }), ({
    name: "error",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in second case branch",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  }), ({
    name: "error",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  }), ({
    name: "error",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in default branch",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "cases", value: ({
    typeName: "Result",
    default: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    cases: [({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  }), ({
    name: "error",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "cases", value: ({
    typeName: "Result",
    default: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    cases: [({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  }), ({
    name: "error",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string deeply nested in record in list in application",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "process" }),
    argument: ({ tag: "list", value: [({ tag: "record", value: ({
    typeName: "Item",
    fields: [({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })]
  }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "process" }),
    argument: ({ tag: "list", value: [({ tag: "record", value: ({
    typeName: "Item",
    fields: [({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })]
  }) })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in union inject value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "inject", value: ({
    typeName: "Result",
    field: ({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "Result",
    field: ({
    name: "success",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in wrapped term",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "wrap", value: ({
    typeName: "Email",
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "wrap", value: ({
    typeName: "Email",
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in annotated term body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    annotation: new Map([])
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in first of multiple let bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in second of multiple let bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in all let bindings and body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in set",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "string", value: "baz" }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })]) }))),
    expected: ShowCore.term(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })]) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in type lambda body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in type application body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "typeApplication", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "typeApplication", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in nested type lambdas",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in case branch within let binding",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "let", value: ({
    bindings: [({
    name: "handler",
    term: ({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "ok",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "handler" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "handler",
    term: ({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "ok",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "handler" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string in annotated wrapped record field",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Rewriting.rewriteTerm(((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => LibLogic.ifElse(LibEquality.equal(term)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) })))(({ tag: "literal", value: ({ tag: "string", value: "bar" }) }))(recurse(term)))))(({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({
    typeName: "User",
    body: ({ tag: "record", value: ({
    typeName: "UserData",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })]
  }) })
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({
    typeName: "User",
    body: ({ tag: "record", value: ({
    typeName: "UserData",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })]
  }) })
  }) }),
    annotation: new Map([])
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "rewriteAndFoldTermWithPath",
    description: null,
    subgroups: [],
    cases: [({
    name: "path tracking through application - sum literals",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through nested applications",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through let bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 32 }) }) })] })
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through record fields",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "record", value: ({
    typeName: "Point",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  })]
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through case branches",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "ok",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })]
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through pair",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })] })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through optional",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through wrapped term",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "wrap", value: ({
    typeName: "Age",
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through type lambda",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100 }) }) })
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through type application",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "typeApplication", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 50 }) }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 50 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "path tracking through set elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })]) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "deep nesting - application in lambda in let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: number) => ((term: Core.Term) => LibMath.add(acc)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((intVal: Core.IntegerValue) => (() => {
  const _m = intVal;
  switch (_m.tag) {
    case "int32": return ((n: number) => n)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))))(0)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })) }) }) })),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 15 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "collect list lengths in nested structure",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: n }) }) })))(Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: ReadonlyArray<number>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "list": return ((elems: ReadonlyArray<Core.Term>) => [LibLists.length(elems)])((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "collect list lengths in let body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(({ tag: "list", value: LibLists.map(((n: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: n }) }) })))(Rewriting.foldOverTerm(({ tag: "pre" }))(((acc: ReadonlyArray<number>) => ((term: Core.Term) => LibLists.concat([acc, (() => {
  const _m = term;
  switch (_m.tag) {
    case "list": return ((elems: ReadonlyArray<Core.Term>) => [LibLists.length(elems)])((_m as any).value);
    default: return [](_m);
  }
})()]))))([])(({ tag: "let", value: ({
    bindings: [({
    name: "xs",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))) })),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

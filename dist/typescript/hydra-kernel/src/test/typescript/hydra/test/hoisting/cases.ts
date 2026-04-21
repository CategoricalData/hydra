// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for subterm hoisting and case statement hoisting
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
import * as Graph from "../../graph.js";
import * as Hoisting from "../../hoisting.js";
import * as JsonModel from "../../json/model.js";
import * as Lexical from "../../lexical.js";
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

export const allTests: Testing.TestGroup = ({
    name: "hoistCases",
    description: null,
    subgroups: [({
    name: "hoistSubterms",
    description: null,
    subgroups: [],
    cases: [({
    name: "hoistNothing: simple let unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((_: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => false))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistNothing: let with list in body unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((_: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => false))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistNothing: let with application in body unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((_: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => false))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "g" }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "g" }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: list in body is hoisted into local let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: multiple lists in body are hoisted together",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  }), ({
    name: "_hoist_x_body_2",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) }),
    argument: ({ tag: "variable", value: "_hoist_x_body_2" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: list in binding value is hoisted into local let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: nested lists hoisted from inside out",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  }), ({
    name: "_hoist_x_body_2",
    term: ({ tag: "list", value: [({ tag: "variable", value: "_hoist_x_body_1" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_2" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistApplications: application in list element is hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "application": return ((_: Core.Application) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }), ({ tag: "variable", value: "y" })] })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "_hoist_x_body_1" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistApplications: application in record field is hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "application": return ((_: Core.Application) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "record", value: ({
    typeName: "Data",
    fields: [({
    name: "value",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "record", value: ({
    typeName: "Data",
    fields: [({
    name: "value",
    term: ({ tag: "variable", value: "_hoist_x_body_1" })
  })]
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistApplications: nested applications hoisted from inside out",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "application": return ((_: Core.Application) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  }), ({
    name: "_hoist_x_body_2",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "_hoist_x_body_2" })] })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistCaseStatements: case in application argument is hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "cases": return ((_: Core.CaseStatement) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistCaseStatements: case in list element is hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "cases": return ((_: Core.CaseStatement) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "cases", value: ({
    typeName: "Result",
    default: ({ tag: "variable", value: "y" }),
    cases: [({
    name: "ok",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "cases", value: ({
    typeName: "Result",
    default: ({ tag: "variable", value: "y" }),
    cases: [({
    name: "ok",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "_hoist_x_body_1" })] })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: nested let - inner let processed independently",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_y_body_1",
    term: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_y_body_1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: non-let term is unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistApplications: bare application unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "application": return ((_: Core.Application) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: term referring to let-bound variable needs no capture",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: term referring to lambda above let needs no capture",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "y" }), ({ tag: "variable", value: "x" })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "list", value: [({ tag: "variable", value: "y" }), ({ tag: "variable", value: "x" })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: lambda-bound var not free in hoisted term needs no capture",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: lambda-bound var free in hoisted term requires capture",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_x_body_1" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: only free lambda-bound vars are captured",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "b" })] })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "b" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_x_body_1" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: stable naming for binding and body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: stable naming for multiple bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_1" })
  }) })
  }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_y_1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "_hoist_y_1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "hoistLists: polymorphic binding with self-reference below hoisted term",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistSubterms(((pt: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => (() => {
  const _m = LibPairs.second(pt);
  switch (_m.tag) {
    case "list": return ((_: ReadonlyArray<Core.Term>) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "hoistCaseStatements",
    description: null,
    subgroups: [],
    cases: [({
    name: "case at top level of let body is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }),
    type: null
  })],
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }),
    type: null
  })],
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in let binding value is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "y" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "y" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case inside lambda body is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case inside nested lambdas is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Result",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "ok",
    term: ({ tag: "variable", value: "b" })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Result",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "ok",
    term: ({ tag: "variable", value: "b" })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case as LHS of one application is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case wrapped in annotation is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    annotation: new Map([])
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    annotation: new Map([])
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in lambda with one application is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case as RHS of application IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "_hoist_f_1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in nested application LHS IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case inside list element IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "list", value: [({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "_hoist_f_1" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case inside lambda inside list IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list inside lambda is NOT hoisted (only case statements)",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in binding is not hoisted, case in arg position is hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "w" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_x_body_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "w" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_x_body_1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in nested let body is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "z" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "w" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "z" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "w" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in let inside lambda is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in lambda inside let body is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case with let+lambda+app is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in triple application LHS IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "c",
    domain: null,
    body: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "c",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "variable", value: "z" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "c",
    domain: null,
    body: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "c",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case as second argument IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "_hoist_f_1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in both arguments - both hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  }), ({
    name: "_hoist_f_2",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "_hoist_f_1" })
  }) }),
    argument: ({ tag: "variable", value: "_hoist_f_2" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in second list element IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "variable", value: "_hoist_f_1" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple cases in list - all hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "list", value: [({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }), ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })]
  }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  }), ({
    name: "_hoist_f_2",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "_hoist_f_1" }), ({ tag: "variable", value: "_hoist_f_2" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in pair first element IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "pair", value: [({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "_hoist_f_1" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in pair second element IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "variable", value: "_hoist_f_1" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in child let binding hoisted into child",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "inner" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_inner_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "_hoist_inner_1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "inner" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in child let body hoisted into child",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_inner_body_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "_hoist_inner_body_1" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case at top level of child let NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "inner" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "inner" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cases in both outer and child - each hoisted locally",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    argument: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "inner" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_outer_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "_hoist_outer_1" })
  }) }),
    argument: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_inner_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "b" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "_hoist_inner_1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "inner" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "outer" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda after app LHS takes us out of top level",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case inside case branch is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "b" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "b" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case inside case default branch is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "variable", value: "a" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "y" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "b" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "variable", value: "a" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "y" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "b" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in arg position inside case branch IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "b" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "a" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "b" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: ({ tag: "variable", value: "x" }),
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in let body inside applied case default IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "variable", value: "a" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "b",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "ok",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "variable", value: "a" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "b",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_b_body_1",
    term: ({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "ok",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_b_body_1" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case in let body inside applied case branch IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "b",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "ok",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "b",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_b_body_1",
    term: ({ tag: "cases", value: ({
    typeName: "Result",
    default: null,
    cases: [({
    name: "ok",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "err",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_b_body_1" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case application at top level of binding is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case application in arg position IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case application inside immediately-applied lambda IS hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "_hoist_f_1",
    term: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "_hoist_f_1" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case application in lambda body is NOT hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Hoisting.hoistCaseStatements(Lexical.emptyGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "Optional",
    default: null,
    cases: [({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }), ({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for variable analysis and manipulation
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
import * as LibLists from "../lib/lists.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Tabular from "../tabular.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variables from "../variables.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "variables",
    description: null,
    subgroups: [({
    name: "freeVariables",
    description: null,
    subgroups: [],
    cases: [({
    name: "string literal has no free variables",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(Variables.freeVariablesInTerm(({ tag: "literal", value: ({ tag: "string", value: "foo" }) }))))), "}"]),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(new Set([])))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single variable",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(Variables.freeVariablesInTerm(({ tag: "variable", value: "x" }))))), "}"]),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(new Set(["x"])))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bound variable is not free",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(Variables.freeVariablesInTerm(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }))))), "}"]),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(new Set([])))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unbound variable in lambda body",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(Variables.freeVariablesInTerm(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))))), "}"]),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(new Set(["x"])))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed free and bound variables",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(Variables.freeVariablesInTerm(({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })] }))))), "}"]),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(new Set(["x"])))), "}"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple free variables",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(Variables.freeVariablesInTerm(({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })] }))))), "}"]),
    expected: LibStrings.cat(["{", LibStrings.intercalate(", ")(LibLists.map(((n: Core.Name) => ((_x) => _x)(n)))(LibSets.toList(new Set(["x", "y"])))), "}"])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "normalizeTypeVariables",
    description: null,
    subgroups: [],
    cases: [({
    name: "literal without type variables unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "simple let without type annotations unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let with monomorphic type scheme unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let with monomorphic binding referencing string",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding with free type variable unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "variable", value: "bar" }),
    type: ({
    variables: [],
    type: ({ tag: "variable", value: "a" }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "variable", value: "bar" }),
    type: ({
    variables: [],
    type: ({ tag: "variable", value: "a" }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "monomorphic binding with typed lambda unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding with typed lambda in body unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "variable", value: "bar" }),
    type: ({
    variables: [],
    type: ({ tag: "variable", value: "a" }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "variable", value: "bar" }),
    type: ({
    variables: [],
    type: ({ tag: "variable", value: "a" }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic identity function normalized",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic const function normalized",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "const",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["a", "b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "const" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "const",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "const" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "binding rewriting does not affect body with typed lambda",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested polymorphic lets normalized",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id2" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id2" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested same substitution in bindings and environment",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "parent type variable shadows child variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id2" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "t1" }),
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "t0" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id2" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no shadowing distinct type variables",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "b" }),
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id2" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "t1" }),
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "t0" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id2" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "locally free type variable in nested binding",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.normalizeTypeVariablesInTerm(({ tag: "let", value: ({
    bindings: [({
    name: "fun1",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "b" }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "fun2",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: ({ tag: "variable", value: "c" }),
    body: ({ tag: "pair", value: [({ tag: "variable", value: "z" }), ({ tag: "variable", value: "y" })] })
  }) }),
    type: ({
    variables: ["c"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "c" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "c" }),
    second: ({ tag: "variable", value: "b" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "fun2" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a", "b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "a" }),
    second: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "fun1" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "fun1",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "t0" }),
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "t1" }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "fun2",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: ({ tag: "variable", value: "t2" }),
    body: ({ tag: "pair", value: [({ tag: "variable", value: "z" }), ({ tag: "variable", value: "y" })] })
  }) }),
    type: ({
    variables: ["t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t2" }),
    second: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "fun2" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "fun1" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "unshadowVariables",
    description: null,
    subgroups: [],
    cases: [({
    name: "literal unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "variable", value: "x" }))),
    expected: ShowCore.term(({ tag: "variable", value: "x" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single lambda unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "distinct lambda parameters unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let with no shadowing unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let and lambda with distinct names unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inner lambda shadows outer lambda",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inner lambda shadows outer - body references both",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "triple nested lambda same name",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x3",
    domain: null,
    body: ({ tag: "variable", value: "x3" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two parameters shadow sequentially",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y2",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x2" }), ({ tag: "variable", value: "y2" })] })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda shadows let-bound variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda shadows one of multiple let bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x2" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inner let body with lambda shadowing outer let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "let", value: ({
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
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda in function position of application",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "variable", value: "f" })
  }) }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "f2",
    domain: null,
    body: ({ tag: "variable", value: "f2" })
  }) }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambdas in list elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }), ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) }), ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda in record field",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "Pair",
    fields: [({
    name: "fst",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "snd",
    term: ({ tag: "variable", value: "x" })
  })]
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "Pair",
    fields: [({
    name: "fst",
    term: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  }), ({
    name: "snd",
    term: ({ tag: "variable", value: "x" })
  })]
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda in case branch",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Maybe",
    default: null,
    cases: [({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }), ({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "cases", value: ({
    typeName: "Maybe",
    default: null,
    cases: [({
    name: "nothing",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }), ({
    name: "just",
    term: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  })]
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda in pair",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }), ({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) }), ({ tag: "variable", value: "x" })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda inside optional",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "maybe", value: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "maybe", value: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda inside set element",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })]) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })]) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda in union injection",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "Result",
    field: ({
    name: "ok",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "Result",
    field: ({
    name: "ok",
    term: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda inside wrapped term",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "wrap", value: ({
    typeName: "Age",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "wrap", value: ({
    typeName: "Age",
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda inside type lambda",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda inside type application",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowed lambda inside annotated term",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) }),
    annotation: new Map([])
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "shadowing at multiple depths",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y2",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x2" }), ({ tag: "variable", value: "y2" })] })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let then lambda then lambda all same name",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    parameter: "x2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x3",
    domain: null,
    body: ({ tag: "variable", value: "x3" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with shadowing in let binding value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "y" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "variable", value: "x2" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "y" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "application without shadowing unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list of literals unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested record unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Variables.unshadowVariables(({ tag: "record", value: ({
    typeName: "Point",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "Point",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

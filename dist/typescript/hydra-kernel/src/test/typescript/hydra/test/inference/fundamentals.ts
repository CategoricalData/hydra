// Note: this is an automatically generated file. Do not edit.

/**
 * Inference tests for fundamental language features
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
import * as Inference from "../../inference.js";
import * as JsonModel from "../../json/model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibPairs from "../../lib/pairs.js";
import * as LibStrings from "../../lib/strings.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "Fundamentals",
    description: null,
    subgroups: [testGroupForLambdas, testGroupForLet, testGroupForLiterals, testGroupForPathologicalTerms, testGroupForPolymorphism, testGroupForPrimitives],
    cases: []
  });

export const testGroupForLambdas: Testing.TestGroup = ({
    name: "Lambdas",
    description: null,
    subgroups: [({
    name: "Simple lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 137n }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Nested lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "list", value: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Nested lambdas with shadowing",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const testGroupForLet: Testing.TestGroup = ({
    name: "Let terms",
    description: null,
    subgroups: [({
    name: "Simple",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 42.0 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Empty let",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Trivial let",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "foo" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "foo" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Multiple references to a let-bound term",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "bar",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "foo" }), ({ tag: "variable", value: "bar" }), ({ tag: "variable", value: "foo" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Nested let",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "bar",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "foo" }), ({ tag: "variable", value: "bar" })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "bar",
    term: ({ tag: "pair", value: [({ tag: "variable", value: "foo" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "bar" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "sng",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "sng" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    type: null
  }), ({
    name: "bar",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "sng" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }),
    type: null
  }), ({
    name: "quux",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "sng" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "foo" }), ({ tag: "pair", value: [({ tag: "variable", value: "bar" }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "quux" }),
    argument: ({ tag: "list", value: [] })
  }) })] })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    second: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Nested let with shadowing",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "foo" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    type: null
  }), ({
    name: "bar",
    term: ({ tag: "variable", value: "foo" }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "bar" }), ({ tag: "variable", value: "foo" })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Let-polymorphism",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 42.0 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "square",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "z" })
  }) }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "square" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "x" })
  }) })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#6",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#7",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "list",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "list" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "list" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    second: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "singleton",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) }),
    type: null
  }), ({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "singleton" }),
    argument: ({ tag: "variable", value: "x" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "singleton" }),
    argument: ({ tag: "variable", value: "y" })
  }) })] })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "variable", value: "t0" })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabled"]
  }), ({
    name: "#9",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  }), ({
    name: "fortytwo",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    type: null
  }), ({
    name: "foo",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "fortytwo" }), ({ tag: "variable", value: "foo" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "fortytwo",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    type: null
  }), ({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  }), ({
    name: "foo",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "fortytwo" }), ({ tag: "variable", value: "foo" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Recursive and mutually recursive let (@wisnesky's test cases)",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "variable", value: "y" }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "u",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "v" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "f" }), ({ tag: "variable", value: "g" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "v0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabled"]
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "plus",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "plus" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "plus" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) }),
    type: null
  }), ({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "p0",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "p0" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "p0" })
  }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#6",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    type: null
  }), ({
    name: "z",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "z" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#7",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    type: null
  }), ({
    name: "z",
    term: ({ tag: "variable", value: "x" }),
    type: null
  }), ({
    name: "w",
    term: ({ tag: "variable", value: "z" }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "pair", value: [({ tag: "variable", value: "w" }), ({ tag: "variable", value: "z" })] })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t2" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Recursive and mutually recursive let with polymorphism",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  }), ({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "variable", value: "g" })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.fromList" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "f" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "f" }), ({ tag: "variable", value: "g" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  }), ({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "variable", value: "g" })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.fromList" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "f" })] })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "f" }), ({ tag: "variable", value: "g" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "variable", value: "g" })
  }) })
  }) }),
    type: null
  }), ({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.fromList" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "f" })] })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "f" }), ({ tag: "variable", value: "g" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Recursion involving polymorphic functions",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "variable", value: "x" })] })] })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "variable", value: "x" })] })] })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "inst",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    type: null
  }), ({
    name: "rec",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b0",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "variable", value: "f" })
  }) }),
    argument: ({ tag: "variable", value: "b0" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "inst" }), ({ tag: "variable", value: "rec" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "inst",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "rec",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "inst" }), ({ tag: "variable", value: "rec" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "inst1",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "inst2",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "rec",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "inst1" }), ({ tag: "pair", value: [({ tag: "variable", value: "inst2" }), ({ tag: "variable", value: "rec" })] })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "variable", value: "bar" }),
    type: null
  }), ({
    name: "bar",
    term: ({ tag: "variable", value: "foo" }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "foo" }), ({ tag: "variable", value: "bar" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Over-generalization of hoisted let-bindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "r",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "val" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "r" })
  }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "val" })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "variable", value: "g" })
  }) }),
    argument: ({ tag: "variable", value: "val" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "forField",
    term: ({ tag: "lambda", value: ({
    parameter: "rec",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "r",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "variable", value: "val" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "r" })
  }) }), ({ tag: "variable", value: "x" })] })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "main",
    term: ({ tag: "lambda", value: ({
    parameter: "rec",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "forField" }),
    argument: ({ tag: "variable", value: "rec" })
  }) }),
    argument: ({ tag: "variable", value: "val" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "main" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1", "t2", "t3"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t2" }),
    second: ({ tag: "variable", value: "t3" })
  }) })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t2" }),
    second: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "val" })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "main",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "variable", value: "f" })
  }) }),
    argument: ({ tag: "variable", value: "val" })
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "variable", value: "val" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "main" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "forField",
    term: ({ tag: "lambda", value: ({
    parameter: "rec",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "r",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "variable", value: "val" })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "r" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "r" })
  }) })] })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "main",
    term: ({ tag: "lambda", value: ({
    parameter: "rec",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "forField" }),
    argument: ({ tag: "variable", value: "rec" })
  }) }),
    argument: ({ tag: "variable", value: "val" })
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "variable", value: "val" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "main" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#6",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "rcases",
    term: ({ tag: "lambda", value: ({
    parameter: "forFields",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "forFields" }),
    argument: ({ tag: "variable", value: "val" })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "r",
    term: ({ tag: "lambda", value: ({
    parameter: "forFields",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "val",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rcases" }),
    argument: ({ tag: "variable", value: "forFields" })
  }) }),
    argument: ({ tag: "variable", value: "val" })
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "variable", value: "val" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "r" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const testGroupForLiterals: Testing.TestGroup = ({
    name: "Literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "string", value: "foo" }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const testGroupForPathologicalTerms: Testing.TestGroup = ({
    name: "Pathological terms",
    description: null,
    subgroups: [({
    name: "Recursion",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "variable", value: "t0" }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  }), ({
    name: "weird",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "id" })
  }) }),
    argument: ({ tag: "variable", value: "id" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "weird" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "paradox",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "paradox" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "paradox" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#6",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Infinite lists",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "self",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "self" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "self" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "self",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "self" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "self" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "self",
    term: ({ tag: "lambda", value: ({
    parameter: "e",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "variable", value: "e" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "self" }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "self" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabled"]
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "build",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "build" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "build" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const testGroupForPolymorphism: Testing.TestGroup = ({
    name: "Polymorphism",
    description: null,
    subgroups: [({
    name: "Simple lists and optionals",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: null }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "maybe", value: ({ tag: "variable", value: "t0" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Lambdas, lists, and products",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }), ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "list", value: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "y" }), ({ tag: "variable", value: "x" })] })
  }) })
  }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "list", value: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t1" }),
    second: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Lambdas and application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Primitives and application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }), ({ tag: "list", value: [] })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Lambdas and primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.math.add" }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Mixed expressions with lambdas, constants, and primitive functions",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Application terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Phantom type variables",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.isLeft" }),
    argument: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.isLeft" }),
    argument: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: "x" }) }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.isLeft" }),
    argument: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.isLeft" }),
    argument: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }) }) })
  }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "boolean" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.eithers.isLeft" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const testGroupForPrimitives: Testing.TestGroup = ({
    name: "Primitives",
    description: null,
    subgroups: [({
    name: "Monomorphic primitive functions",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.strings.length" }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.math.sub" }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Polymorphic primitive functions",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "el",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "el" })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "el",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "variable", value: "el" })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.concat" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "lists",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "variable", value: "lists" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "lists",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "variable", value: "lists" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#6",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "list",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "list" }), ({ tag: "list", value: [] })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#7",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "list",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "list" }), ({ tag: "list", value: [] })] })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "lists",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "variable", value: "lists" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

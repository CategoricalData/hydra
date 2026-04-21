// Note: this is an automatically generated file. Do not edit.

/**
 * Inference tests for expected failures
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
    name: "Expected failures",
    description: null,
    subgroups: [undefinedVariableTests, unificationFailureTests, invalidApplicationTests, selfApplicationTests, arityMismatchTests, recursiveTypeTests, occurCheckTests, typeConstructorMisuseTests, polymorphismViolationTests, letBindingMismatchTests, constraintSolverEdgeCaseTests, primitiveTypeErrorTests, complexConstraintFailureTests],
    cases: []
  });

export const arityMismatchTests: Testing.TestGroup = ({
    name: "Arity mismatch",
    description: null,
    subgroups: [({
    name: "Too many arguments",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 999 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })] })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "extra" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Wrong argument types with extra args",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "extra" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.not" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "arg" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const complexConstraintFailureTests: Testing.TestGroup = ({
    name: "Complex constraint failures",
    description: null,
    subgroups: [({
    name: "Multi-level constraint conflicts",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    type: null
  }), ({
    name: "h",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "weird",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "weird" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "y" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "nested",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "int_f",
    term: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "n" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "str_g",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "s" }), ({ tag: "literal", value: ({ tag: "string", value: "!" }) })] })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "nested" }),
    argument: ({ tag: "variable", value: "int_f" })
  }) }),
    argument: ({ tag: "variable", value: "str_g" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Function composition failures",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "triple",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "increment",
    term: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "n" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "stringify",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "s" }), ({ tag: "literal", value: ({ tag: "string", value: "!" }) })] })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "triple" }),
    argument: ({ tag: "variable", value: "increment" })
  }) }),
    argument: ({ tag: "variable", value: "stringify" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "compose",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "reverse_compose",
    term: ({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "compose" }),
    argument: ({ tag: "variable", value: "reverse_compose" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.length" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const constraintSolverEdgeCaseTests: Testing.TestGroup = ({
    name: "Constraint solver edge cases",
    description: null,
    subgroups: [({
    name: "Complex constraint propagation",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "complex",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "complex" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Fixed point combinators",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "fix",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "fix" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "y" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "rec",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "rec" }),
    argument: ({ tag: "variable", value: "rec" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "omega",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "omega" }),
    argument: ({ tag: "variable", value: "omega" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Constraint cycles",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "b" }),
    argument: ({ tag: "variable", value: "c" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "c" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: null
  }), ({
    name: "c",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "a" }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "a" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "circular",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "circular" })
  }) }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "circular" }),
    argument: ({ tag: "variable", value: "circular" })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const invalidApplicationTests: Testing.TestGroup = ({
    name: "Invalid application",
    description: null,
    subgroups: [({
    name: "Non-function application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Collection application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "list", value: [] }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "index" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Primitive misapplication",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.empty" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.empty" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "maybe", value: null }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "value" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "list", value: [] }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const letBindingMismatchTests: Testing.TestGroup = ({
    name: "Let binding type mismatches",
    description: null,
    subgroups: [({
    name: "Application type mismatches",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "result" }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "extra" }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "g" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "num",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "num" }),
    argument: ({ tag: "variable", value: "num" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Collection type mismatches",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "list1",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }),
    type: null
  }), ({
    name: "list2",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "variable", value: "list1" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "list2" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "nums",
    term: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }),
    type: null
  }), ({
    name: "mixed",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bad" }) })
  }) }),
    argument: ({ tag: "variable", value: "nums" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "mixed" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "pair1",
    term: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }),
    type: null
  }), ({
    name: "pair2",
    term: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "variable", value: "pair1" })] }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "pair2" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Function binding mismatches",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "add",
    term: ({ tag: "variable", value: "hydra.lib.math.add" }),
    type: null
  }), ({
    name: "badCall",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "add" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a number" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "badCall" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    type: null
  }), ({
    name: "bad",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "bad" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const occurCheckTests: Testing.TestGroup = ({
    name: "Occur check failures",
    description: null,
    subgroups: [({
    name: "Function occur checks",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "h",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "g" })
  }) }),
    argument: ({ tag: "variable", value: "h" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "g" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Mutual occur checks",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "f" })
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
    argument: ({ tag: "variable", value: "g" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "b" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "a" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "a" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "cycle1",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "cycle2" }),
    argument: ({ tag: "variable", value: "cycle1" })
  }) }),
    type: null
  }), ({
    name: "cycle2",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "cycle1" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "cycle1" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Complex occur checks",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "omega",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "omega" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "omega" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "loop",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "loop" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "loop" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "loop" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const polymorphismViolationTests: Testing.TestGroup = ({
    name: "Polymorphism violations",
    description: null,
    subgroups: [({
    name: "Identity function violations",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    function: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Constrained polymorphism violations",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "string", value: "constant" }) })] })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bad" }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "h",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "incompatible" }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Higher-order polymorphism violations",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "g",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bad" }) })
  }) })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "h",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "error" }) })
  }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const primitiveTypeErrorTests: Testing.TestGroup = ({
    name: "Primitive function type errors",
    description: null,
    subgroups: [({
    name: "Logic primitive errors",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.and" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.or" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not boolean" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Collection primitive errors",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.lookup" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a map" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.member" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a list" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.fromMaybe" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not optional" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Math primitive errors",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a number" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.div" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a number" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const recursiveTypeTests: Testing.TestGroup = ({
    name: "Recursive type construction",
    description: null,
    subgroups: [({
    name: "Direct recursive types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "list", value: [({ tag: "variable", value: "x" })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Recursive function types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "f" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "f" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Mutually recursive types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "list", value: [({ tag: "variable", value: "y" })] }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "b" })
  }) }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "variable", value: "a" }),
    type: null
  })],
    body: ({ tag: "variable", value: "a" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "list", value: [({ tag: "variable", value: "g" })] }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "pair", value: [({ tag: "variable", value: "f" }), ({ tag: "variable", value: "f" })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const selfApplicationTests: Testing.TestGroup = ({
    name: "Self-application",
    description: null,
    subgroups: [({
    name: "Direct self-application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "f" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Indirect self-application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "y" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "b" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "a" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "cycle",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "cycle" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "cycle" }),
    argument: ({ tag: "variable", value: "cycle" })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const typeConstructorMisuseTests: Testing.TestGroup = ({
    name: "Type constructor misuse",
    description: null,
    subgroups: [({
    name: "List constructor errors",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a list" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.tail" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "String constructor errors",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.fromList" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a list" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toList" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Math constructor errors",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a number" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a number" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.div" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const undefinedVariableTests: Testing.TestGroup = ({
    name: "Undefined variable",
    description: null,
    subgroups: [({
    name: "Basic unbound variables",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "x" }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Unbound in let expressions",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "variable", value: "y" }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "variable", value: "y" }),
    type: null
  }), ({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "variable", value: "z" }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Shadowing scope errors",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const unificationFailureTests: Testing.TestGroup = ({
    name: "Unification failure",
    description: null,
    subgroups: [({
    name: "Basic type mismatches",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Collection type mismatches",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "not a list" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })] }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })] }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Conditional type mismatches",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Polymorphic instantiation conflicts",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    function: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "FAIL"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => LibStrings.cat2("unexpected: ")(ShowCore.typeScheme(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "cons",
    term: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "cons" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "cons" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })] })
  }) }))),
    expected: "FAIL"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

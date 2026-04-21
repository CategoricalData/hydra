// Note: this is an automatically generated file. Do not edit.

/**
 * Algorithm W inference tests
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
    name: "Algorithm W test cases",
    description: null,
    subgroups: [testGroupForSystemF],
    cases: []
  });

export const testGroupForSystemF: Testing.TestGroup = ({
    name: "STLC to System F",
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
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
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
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
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
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
    name: "sng",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "sng" })
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
    name: "sng",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "sng" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "sng" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "alice" }) })
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
    name: "#7",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "+",
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
    function: ({ tag: "variable", value: "+" }),
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
    function: ({ tag: "variable", value: "+" }),
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
    name: "#9",
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
    name: "#10",
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
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "xx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "yy",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "xx" })
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
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#11",
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
    variables: ["t0", "t1", "t2", "t3"],
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
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t3" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#12",
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
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
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
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#13",
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
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
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
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  });

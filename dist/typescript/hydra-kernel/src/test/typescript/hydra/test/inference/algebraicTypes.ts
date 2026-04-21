// Note: this is an automatically generated file. Do not edit.

/**
 * Inference tests for algebraic data types
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
    name: "Algebraic terms",
    description: null,
    subgroups: [testGroupForCollectionPrimitives, testGroupForEithers, testGroupForFolds, testGroupForLists, testGroupForMaps, testGroupForOptionals, testGroupForPairs, testGroupForSets],
    cases: []
  });

export const testGroupForCollectionPrimitives: Testing.TestGroup = ({
    name: "Collection primitives",
    description: null,
    subgroups: [({
    name: "maps.map applied to a function",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.sets.fromList" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "set", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })], ["t1", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "sets.map applied to a function",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.lists.length" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) }),
    codomain: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Composing collection primitives in let",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.sets.fromList" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "set", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })], ["t1", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.sets.fromList" })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })]]) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "g" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Map operations in lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "m",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.lists.length" })
  }) }),
    argument: ({ tag: "variable", value: "m" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "m",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "f" })
  }) }),
    argument: ({ tag: "variable", value: "m" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t2" }),
    values: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t2" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: new Map([["t2", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Fully applied collection conversions",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.fromList" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.sets.fromList" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })] })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  })],
    cases: []
  });

export const testGroupForEithers: Testing.TestGroup = ({
    name: "Either terms",
    description: null,
    subgroups: [({
    name: "Left values",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "error" }) }) }) }), ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "error" }) }) }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Right values",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) }), ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "error" }) }) }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Polymorphic either values",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "list", value: [] }) }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "either", value: ({
    left: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    right: ({ tag: "variable", value: "t1" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "right", value: ({ tag: "list", value: [] }) }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Nested either values",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "either", value: ({ tag: "left", value: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }) }) }) }), ({ tag: "either", value: ({ tag: "left", value: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: "nested" }) }) }) }) }) }), ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }) }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    right: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "either", value: ({ tag: "right", value: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) }) }) }), ({ tag: "either", value: ({ tag: "right", value: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }) }) }) }) }), ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }) }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Either in lambda",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "x" }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "variable", value: "t1" })
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
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "x" }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t1" }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Either in data structures",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "error" }) }) }) }), ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "error" }) }) }) }), ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })] }), ({ tag: "list", value: [] })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }),
    second: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
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

export const testGroupForFolds: Testing.TestGroup = ({
    name: "Eliminations",
    description: null,
    subgroups: [({
    name: "List eliminations (folds)",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Optional eliminations",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }),
    argument: ({ tag: "maybe", value: null })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.maybes.pure" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "maybe", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "maybe", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  })],
    cases: []
  });

export const testGroupForLists: Testing.TestGroup = ({
    name: "List terms",
    description: null,
    subgroups: [({
    name: "List of strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "List of lists of strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }), ({ tag: "list", value: [] })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Empty list",
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
  })]
  }), ({
    name: "List containing an empty list",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "list", value: [] })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Lambda producing a polymorphic list",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
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
  })]
  }), ({
    name: "Lambda producing a list of integers",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "List with repeated variables",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
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

export const testGroupForMaps: Testing.TestGroup = ({
    name: "Map terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "firstName" }) }), ({ tag: "literal", value: ({ tag: "string", value: "Arthur" }) })], [({ tag: "literal", value: ({ tag: "string", value: "lastName" }) }), ({ tag: "literal", value: ({ tag: "string", value: "Dent" }) })]]) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([]) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.1 }) }) })], [({ tag: "variable", value: "y" }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.2 }) }) })]]) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) })
  }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  });

export const testGroupForOptionals: Testing.TestGroup = ({
    name: "Optional terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
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
  })]
  });

export const testGroupForPairs: Testing.TestGroup = ({
    name: "Pair terms",
    description: null,
    subgroups: [({
    name: "Monotyped pairs",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))),
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
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 42.0 }) }) }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 137.0 }) }) })] })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Polytyped pairs",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Nested pairs",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "nested" }) })] }), ({ tag: "literal", value: ({ tag: "boolean", value: true }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 42.0 }) }) })] })] })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Pairs in lambda",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "string", value: "constant" }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "p" }), ({ tag: "variable", value: "p" })] })
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
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Pairs in data structures",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })] }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Additional cases",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }))),
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
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  })],
    cases: []
  });

export const testGroupForSets: Testing.TestGroup = ({
    name: "Set terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "boolean", value: true }) })]) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "boolean" }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "set", value: new Set([]) })]) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "set", value: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  });

// Note: this is an automatically generated file. Do not edit.

/**
 * Inference tests for type class constraints (ordering and equality)
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
    name: "Type classes",
    description: null,
    subgroups: [testGroupForMonomorphicConstraints, testGroupForPrimitiveReferences, testGroupForPartialApplication, testGroupForLetBindings, testGroupForComposition, testGroupForNestedContainers, testGroupForCollectionTerms],
    cases: []
  });

export const testGroupForCollectionTerms: Testing.TestGroup = ({
    name: "Collection term constraints",
    description: null,
    subgroups: [({
    name: "Set literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "variable", value: "x" })]) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
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
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })]) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })]) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Map literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "variable", value: "k" }), ({ tag: "variable", value: "v" })]]) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
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
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })]]) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Collection terms with primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "variable", value: "x" })
  }) }), ({ tag: "variable", value: "x" })]) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "variable", value: "k" }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "k" })] })
  }) })]]) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
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
  }), ({
    name: "Constraint propagation through collection elements",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "variable", value: "xs" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.fromList" }),
    argument: ({ tag: "variable", value: "xs" })
  }) })]]) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    values: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "variable", value: "hydra.lib.lists.sort" })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "list", value: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }) }),
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "variable", value: "hydra.lib.sets.fromList" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "set", value: new Set([({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.fromList" }),
    argument: ({ tag: "variable", value: "xs" })
  }) })]) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "set", value: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  })],
    cases: []
  });

export const testGroupForComposition: Testing.TestGroup = ({
    name: "Composition and constraint merging",
    description: null,
    subgroups: [({
    name: "Composing constrained primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })] })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "xs" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.fromList" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "f" })
  }) }),
    argument: ({ tag: "variable", value: "xs" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) }),
    constraints: new Map([["t1", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Composing map and sort",
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
    argument: ({ tag: "variable", value: "hydra.lib.lists.sort" })
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
    values: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
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
    tags: []
  })]
  })],
    cases: []
  });

export const testGroupForLetBindings: Testing.TestGroup = ({
    name: "Let binding constraint propagation",
    description: null,
    subgroups: [({
    name: "Simple let-bound wrappers",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "lookup",
    term: ({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "m",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.lookup" }),
    argument: ({ tag: "variable", value: "k" })
  }) }),
    argument: ({ tag: "variable", value: "m" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "lookup" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "member",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.member" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "member" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "fromList",
    term: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    type: null
  })],
    body: ({ tag: "variable", value: "fromList" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Let-bound with partial instantiation",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "m",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }),
    argument: ({ tag: "variable", value: "m" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
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
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.fromList" }),
    argument: ({ tag: "variable", value: "xs" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Multiple uses of a constrained let binding",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })] })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "boolean", value: true }) }), ({ tag: "literal", value: ({ tag: "string", value: "x" }) })] })] })
  }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    second: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    values: ({ tag: "literal", value: ({ tag: "string" }) })
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

export const testGroupForMonomorphicConstraints: Testing.TestGroup = ({
    name: "Monomorphic (constraints vanish)",
    description: null,
    subgroups: [({
    name: "Map operations with concrete types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.lookup" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "k" }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.singleton" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "k" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.insert" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "k" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.maps.empty" })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Set operations with concrete types",
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
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.member" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.singleton" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Equality operations with concrete types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.equal" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
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
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "b" }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: "hydra.util.Comparison" }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "List operations with concrete types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
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

export const testGroupForNestedContainers: Testing.TestGroup = ({
    name: "Nested containers",
    description: null,
    subgroups: [({
    name: "Maps of sets",
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
    argument: ({ tag: "variable", value: "hydra.lib.sets.fromList" })
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
    tags: []
  })]
  }), ({
    name: "Sets of sets",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "xss",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.sets.fromList" })
  }) }),
    argument: ({ tag: "variable", value: "xss" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }) }),
    codomain: ({ tag: "set", value: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Map from sorted list",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.singleton" }),
    argument: ({ tag: "variable", value: "x" })
  }) })] })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "xs" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const testGroupForPartialApplication: Testing.TestGroup = ({
    name: "Partial application preserving constraints",
    description: null,
    subgroups: [({
    name: "Map partial application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.lookup" }),
    argument: ({ tag: "variable", value: "k" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.singleton" }),
    argument: ({ tag: "variable", value: "k" })
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Set partial application",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.member" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Equality partial application",
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
    function: ({ tag: "variable", value: "hydra.lib.equality.equal" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["equality"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Partial application fixing the constrained variable",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.singleton" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "key" }) })
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "variable", value: "t0" })
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

export const testGroupForPrimitiveReferences: Testing.TestGroup = ({
    name: "Primitive references with constraints",
    description: null,
    subgroups: [({
    name: "Map primitives (ordering on key type)",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maps.fromList" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maps.lookup" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maps.insert" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maps.map" }))),
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
    tags: []
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maps.empty" }))),
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
    tags: []
  })]
  }), ({
    name: "Set primitives (ordering on element type)",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.sets.fromList" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.sets.member" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.sets.insert" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.sets.map" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t1" }) })
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
    tags: []
  })]
  }), ({
    name: "Equality primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.equality.equal" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["equality"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.equality.compare" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "hydra.util.Comparison" })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "List primitives with constraints",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.sort" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["ordering"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.nub" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["equality"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.elem" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: new Map([["t0", ({
    classes: new Set(["equality"])
  })]])
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

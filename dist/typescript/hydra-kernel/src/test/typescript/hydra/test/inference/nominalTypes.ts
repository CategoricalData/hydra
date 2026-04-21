// Note: this is an automatically generated file. Do not edit.

/**
 * Inference tests for nominal types
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
import * as TestTestTerms from "../testTerms.js";
import * as TestTestTypes from "../testTypes.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "Nominal terms",
    description: null,
    subgroups: [testGroupForCaseStatements, testGroupForProjections, testGroupForRecords, testGroupForVariants, testGroupForWrappers],
    cases: []
  });

export const testGroupForCaseStatements: Testing.TestGroup = ({
    name: "Case statements",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeSimpleNumberName,
    default: null,
    cases: [({
    name: "int",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }), ({
    name: "float",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeSimpleNumberName }),
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: null,
    cases: [({
    name: "bool",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) })
  }), ({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) })
  }), ({
    name: "unit",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  });

export const testGroupForProjections: Testing.TestGroup = ({
    name: "Projections",
    description: null,
    subgroups: [({
    name: "Record eliminations",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypePersonName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Pair projections",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.pairs.first" }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0", "t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
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
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  })],
    cases: []
  });

export const testGroupForRecords: Testing.TestGroup = ({
    name: "Records",
    description: null,
    subgroups: [({
    name: "Simple records",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonName,
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 37.774898529052734 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -122.41940307617188 }) }) })
  })]
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypeLatLonName }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 37.774898529052734 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -122.41940307617188 }) }) })
  })]
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeLatLonPolyName }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "lon",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 37.774898529052734 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "variable", value: "lon" })
  })]
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeLatLonPolyName }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "latlon",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    fields: [({
    name: "lat",
    term: ({ tag: "variable", value: "latlon" })
  }), ({
    name: "lon",
    term: ({ tag: "variable", value: "latlon" })
  })]
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeLatLonPolyName }),
    argument: ({ tag: "variable", value: "t0" })
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
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(TestTestTerms.testDataArthur)),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypePersonName }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Record instances of simply recursive record types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeIntListName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeIntListName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 43 }) }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypeIntListName }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeIntListName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeIntListName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypeIntListName }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeListName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeListName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 43 }) }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeListName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeListName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeListName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeListName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeListName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeListName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeListName }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Record instances of mutually recursive record types",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListBName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListAName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
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
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListBName,
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListAName }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
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

export const testGroupForVariants: Testing.TestGroup = ({
    name: "Variant terms",
    description: null,
    subgroups: [({
    name: "Variants",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeTimestampName,
    field: ({
    name: "unixTimeMillis",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 1638200308368n }) }) })
  })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypeTimestampName }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Polymorphic and recursive variants",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "bool",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: ["t0"],
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
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
    name: "other",
    term: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) }),
    type: null
  })],
    body: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "other",
    term: ({ tag: "variable", value: "other" })
  })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
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

export const testGroupForWrappers: Testing.TestGroup = ({
    name: "Wrapper introductions and eliminations",
    description: null,
    subgroups: [({
    name: "Wrapper introductions",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  }), ({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "variable", value: "v" })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Wrapper eliminations",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
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
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }),
    argument: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  })],
    cases: []
  });

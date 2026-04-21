// Note: this is an automatically generated file. Do not edit.

/**
 * Nominal type checking test cases: records, unions, field access, injection, projection
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
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Rewriting from "../../rewriting.js";
import * as Scoping from "../../scoping.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as TestTestGraph from "../testGraph.js";
import * as TestTestTypes from "../testTypes.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "Nominal types",
    description: null,
    subgroups: [recordsTests, unionsTests, wrappedTermsTests, eliminationsTests],
    cases: []
  });

export const chainedUnwrappingTests: Testing.TestGroup = ({
    name: "Chained unwrapping",
    description: null,
    subgroups: [],
    cases: [({
    name: "unwrap then process",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "wrapped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }),
    argument: ({ tag: "variable", value: "wrapped" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: " suffix" }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unwrap polymorphic then map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "wrappedList",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "variable", value: "wrappedList" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const eliminationsTests: Testing.TestGroup = ({
    name: "Eliminations",
    description: null,
    subgroups: [recordEliminationsTests, unionEliminationsTests, wrapEliminationsTests],
    cases: []
  });

export const higherOrderRecordProjectionsTests: Testing.TestGroup = ({
    name: "Higher-order record projections",
    description: null,
    subgroups: [],
    cases: [({
    name: "map projection over list of records",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) }), ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Bob" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Jones" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map polymorphic projection",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lat"
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 40 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -74 }) }) })
  })]
  }) }), ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 34 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -118 }) }) })
  })]
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "filter using projection",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.filter" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "age"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 35 }) }) })
  })]
  }) }), ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Bob" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Jones" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "variable", value: TestTestTypes.testTypePersonName }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const higherOrderUnionEliminationsTests: Testing.TestGroup = ({
    name: "Higher-order union eliminations",
    description: null,
    subgroups: [],
    cases: [({
    name: "map match over list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: null,
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "less" }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "equal" }) })
  }) })
  }), ({
    name: "greaterThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "greater" }) })
  }) })
  })]
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }), ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    field: ({
    name: "equalTo",
    term: ({ tag: "unit" })
  })
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "compose match with other functions",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "comp",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: null,
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "less" }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "equal" }) })
  }) })
  }), ({
    name: "greaterThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "greater" }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "comp" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeComparisonName }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match in lambda body",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "unionValue",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    default: null,
    cases: [({
    name: "int",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "i" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) })
  }), ({
    name: "float",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "unionValue" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeNumberName }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const monomorphicRecordsTests: Testing.TestGroup = ({
    name: "Monomorphic records",
    description: null,
    subgroups: [],
    cases: [({
    name: "latlon record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "LatLon",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 19.54290008544922 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -155.6658935546875 }) }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: "LatLon" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "latlon with variable",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "LatLon",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 19.54290008544922 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "variable", value: "x" })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }),
    codomain: ({ tag: "variable", value: "LatLon" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "person record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: "Person" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "Unit",
    fields: []
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: "Unit" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "person with variables",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "name",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "age",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "firstName",
    term: ({ tag: "variable", value: "name" })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  }), ({
    name: "age",
    term: ({ tag: "variable", value: "age" })
  })]
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "Person" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const monomorphicUnwrappingTests: Testing.TestGroup = ({
    name: "Monomorphic unwrapping",
    description: null,
    subgroups: [],
    cases: [({
    name: "unwrap string alias",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const monomorphicWrappedTermsTests: Testing.TestGroup = ({
    name: "Monomorphic wrapped terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "string alias",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "wrapped integer",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "wrapped" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "wrapped in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "first" }) })
  }) }), ({ tag: "literal", value: ({ tag: "string", value: "second" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multiParameterPolymorphicCaseStatementsTests: Testing.TestGroup = ({
    name: "Multi-parameter polymorphic case statements",
    description: null,
    subgroups: [],
    cases: [({
    name: "case Either converting both to string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    default: null,
    cases: [({
    name: "left",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }), ({
    name: "right",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case Either applied to injection",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    default: null,
    cases: [({
    name: "left",
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
  }) })
  }), ({
    name: "right",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "left",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "case Either with Triple and nested projections",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "triple",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    default: null,
    cases: [({
    name: "left",
    term: ({ tag: "lambda", value: ({
    parameter: "coords",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lat"
  }) }),
    argument: ({ tag: "variable", value: "coords" })
  }) })
  }) })
  }), ({
    name: "right",
    term: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "first"
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "second"
  }) }),
    argument: ({ tag: "variable", value: "triple" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "forall", value: ({
    parameter: "t3",
    body: ({ tag: "forall", value: ({
    parameter: "t4",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeTripleName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeLatLonPolyName }),
    argument: ({ tag: "variable", value: "t1" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeTripleName }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    argument: ({ tag: "variable", value: "t2" })
  }) }),
    argument: ({ tag: "variable", value: "t3" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "t4" })
  }) }),
    codomain: ({ tag: "variable", value: "t1" })
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
    name: "case Either with polymorphic let bindings",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "makeLeft",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "left",
    term: ({ tag: "variable", value: "x" })
  })
  }) })
  }) }),
    type: null
  }), ({
    name: "makeRight",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "right",
    term: ({ tag: "variable", value: "y" })
  })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "flag",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    default: null,
    cases: [({
    name: "left",
    term: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "makeRight" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "n" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) })
  }), ({
    name: "right",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "makeLeft" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "flag" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multiParameterPolymorphicInjectionsTests: Testing.TestGroup = ({
    name: "Multi-parameter polymorphic injections",
    description: null,
    subgroups: [],
    cases: [({
    name: "either left with int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "left",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either right with string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "right",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either containing LatLonPoly in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "right",
    term: ({ tag: "list", value: [({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 40 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -74 }) }) })
  })]
  }) })] })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "list", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeLatLonPolyName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either in triple in map with shared type variables",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x0",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x2",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "key" }) }), ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "left",
    term: ({ tag: "variable", value: "x0" })
  })
  }) })
  }), ({
    name: "second",
    term: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "left",
    term: ({ tag: "variable", value: "x0" })
  })
  }) })
  }), ({
    name: "third",
    term: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeEitherName,
    field: ({
    name: "right",
    term: ({ tag: "variable", value: "x1" })
  })
  }) })
  })]
  }) })]]) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "forall", value: ({
    parameter: "t3",
    body: ({ tag: "forall", value: ({
    parameter: "t4",
    body: ({ tag: "forall", value: ({
    parameter: "t5",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeTripleName }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t3" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t4" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeEitherName }),
    argument: ({ tag: "variable", value: "t5" })
  }) }),
    argument: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multiParameterPolymorphicProjectionsTests: Testing.TestGroup = ({
    name: "Multi-parameter polymorphic projections",
    description: null,
    subgroups: [],
    cases: [({
    name: "project first from Triple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "first"
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeTripleName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    argument: ({ tag: "variable", value: "t2" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project second from Triple applied",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "second"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "second",
    term: ({ tag: "literal", value: ({ tag: "string", value: "middle" }) })
  }), ({
    name: "third",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project from Triple and use second field, which is another polymorphic record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "triple",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "key",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    default: null,
    cases: [({
    name: "person",
    term: ({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "maybe", value: null })
  }) })
  }), ({
    name: "other",
    term: ({ tag: "lambda", value: ({
    parameter: "m",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.lookup" }),
    argument: ({ tag: "variable", value: "key" })
  }) }),
    argument: ({ tag: "variable", value: "m" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "second"
  }) }),
    argument: ({ tag: "variable", value: "triple" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "forall", value: ({
    parameter: "t3",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeTripleName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t1" }),
    values: ({ tag: "variable", value: "t2" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "t3" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t2" }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multiParameterPolymorphicRecordsTests: Testing.TestGroup = ({
    name: "Multi-parameter polymorphic records",
    description: null,
    subgroups: [],
    cases: [({
    name: "triple with three monomorphic types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "Triple",
    fields: [({
    name: "first",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "second",
    term: ({ tag: "literal", value: ({ tag: "string", value: "middle" }) })
  }), ({
    name: "third",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "Triple" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "triple with PersonOrSomething containing map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "k",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "Triple",
    fields: [({
    name: "first",
    term: ({ tag: "literal", value: ({ tag: "string", value: "prefix" }) })
  }), ({
    name: "second",
    term: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "map", value: new Map([[({ tag: "variable", value: "k" }), ({ tag: "variable", value: "v" })]]) })
  })
  }) })
  }), ({
    name: "third",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 999 }) }) })
  })]
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "Triple" }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "PersonOrSomething" }),
    argument: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multiParameterPolymorphicUnwrappersTests: Testing.TestGroup = ({
    name: "Multi-parameter polymorphic unwrappers",
    description: null,
    subgroups: [],
    cases: [({
    name: "unwrap symmetric triple to tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "st",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "first"
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "st" })
  }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "third"
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "st" })
  }) })
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unwrap and collect edges in set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "getEdge",
    term: ({ tag: "lambda", value: ({
    parameter: "st",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "second"
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "st" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "triples",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.map" }),
    argument: ({ tag: "variable", value: "getEdge" })
  }) }),
    argument: ({ tag: "variable", value: "triples" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "set", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t1" })
  }) }) }),
    codomain: ({ tag: "set", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unwrap with maybe to handle optional symmetric triple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "mst",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "st",
    domain: null,
    body: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "second"
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "st" })
  }) })
  }) }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "mst" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t1" })
  }) }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multiParameterPolymorphicWrappersTests: Testing.TestGroup = ({
    name: "Multi-parameter polymorphic wrappers",
    description: null,
    subgroups: [],
    cases: [({
    name: "symmetric triple wrapping simple types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeSymmetricTripleName,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "second",
    term: ({ tag: "literal", value: ({ tag: "string", value: "edge" }) })
  }), ({
    name: "third",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "symmetric triple from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "e",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v2",
    domain: null,
    body: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeSymmetricTripleName,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "variable", value: "v1" })
  }), ({
    name: "second",
    term: ({ tag: "variable", value: "e" })
  }), ({
    name: "third",
    term: ({ tag: "variable", value: "v2" })
  })]
  }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t1" })
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
    name: "symmetric triple with nested polymorphic types and foldl",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "sumList",
    term: ({ tag: "lambda", value: ({
    parameter: "lst",
    domain: null,
    body: ({ tag: "application", value: ({
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
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "acc" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "lst" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "nums1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "nums2",
    domain: null,
    body: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeSymmetricTripleName,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "sumList" }),
    argument: ({ tag: "variable", value: "nums1" })
  }) })
  }), ({
    name: "second",
    term: ({ tag: "list", value: [({ tag: "variable", value: "nums1" }), ({ tag: "variable", value: "nums2" })] })
  }), ({
    name: "third",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "sumList" }),
    argument: ({ tag: "variable", value: "nums2" })
  }) })
  })]
  }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeSymmetricTripleName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "list", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multipleUnwrapOperationsTests: Testing.TestGroup = ({
    name: "Multiple unwrap operations",
    description: null,
    subgroups: [],
    cases: [({
    name: "unwrap different types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "stringWrapped",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "listWrapped",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }),
    argument: ({ tag: "variable", value: "stringWrapped" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "variable", value: "listWrapped" })
  }) })] })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multipleWrappingLevelsTests: Testing.TestGroup = ({
    name: "Multiple wrapping levels",
    description: null,
    subgroups: [],
    cases: [({
    name: "wrapped in optional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "wrapped" }) })
  }) }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list of wrapped polymorphic",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }), ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedUnionEliminationsTests: Testing.TestGroup = ({
    name: "Nested union eliminations",
    description: null,
    subgroups: [],
    cases: [({
    name: "nested match statements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    default: null,
    cases: [({
    name: "person",
    term: ({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "p" })
  }) })
  }) })
  }), ({
    name: "other",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    default: null,
    cases: [({
    name: "int",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  }), ({
    name: "float",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "variable", value: TestTestTypes.testTypeNumberName })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: null,
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }), ({
    name: "greaterThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })
  }) })
  })]
  }) }), ({ tag: "literal", value: ({ tag: "string", value: "context" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeComparisonName }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedWrappedTermsTests: Testing.TestGroup = ({
    name: "Nested wrapped terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "wrapped tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) })] })] })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "wrapped optional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "wrapped map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "key" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })]]) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicRecordProjectionsAppliedTests: Testing.TestGroup = ({
    name: "Polymorphic record projections applied",
    description: null,
    subgroups: [],
    cases: [({
    name: "project lat from LatLonPoly with int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lat"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: "LatLonPoly",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 40 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -74 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project lon from LatLonPoly with float64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lon"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: "LatLonPoly",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 40.7128 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -74.006 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project head from BuddyListA with string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    field: "head"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: "BuddyListA",
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicRecordProjectionsTests: Testing.TestGroup = ({
    name: "Polymorphic record projections",
    description: null,
    subgroups: [],
    cases: [({
    name: "project lat from polymorphic LatLonPoly",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lat"
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project lon from polymorphic LatLonPoly",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lon"
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project head from BuddyListA",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    field: "head"
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "BuddyListA" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project tail from BuddyListA",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    field: "tail"
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "BuddyListA" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "BuddyListB" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicRecordsTests: Testing.TestGroup = ({
    name: "Polymorphic records",
    description: null,
    subgroups: [],
    cases: [({
    name: "latlon poly float",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "LatLonPoly",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 19.54290008544922 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -155.6658935546875 }) }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "latlon poly int64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "LatLonPoly",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 195429n }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: -1556659n }) }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "latlon poly variable",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "LatLonPoly",
    fields: [({
    name: "lat",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "lon",
    term: ({ tag: "variable", value: "x" })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "buddylist string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "BuddyListA",
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "string", value: "first" }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "BuddyListA" }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "buddylist variable",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "BuddyListA",
    fields: [({
    name: "head",
    term: ({ tag: "variable", value: "x" })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "BuddyListA" }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicRecursiveUnionInjectionsTests: Testing.TestGroup = ({
    name: "Polymorphic recursive union injections",
    description: null,
    subgroups: [],
    cases: [({
    name: "inject boolean into UnionPolymorphicRecursive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "bool",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject string value into UnionPolymorphicRecursive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject int value into UnionPolymorphicRecursive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 123 }) }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicUnionEliminationsTests: Testing.TestGroup = ({
    name: "Polymorphic union eliminations",
    description: null,
    subgroups: [simplePolymorphicUnionTests, usingUnionPolymorphicRecursiveTests, usingKernelTypesTests],
    cases: []
  });

export const polymorphicUnionInjectionsTests: Testing.TestGroup = ({
    name: "Polymorphic union injections",
    description: null,
    subgroups: [],
    cases: [({
    name: "inject person into PersonOrSomething",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "person",
    term: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject string into PersonOrSomething other variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "literal", value: ({ tag: "string", value: "something else" }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject int into PersonOrSomething other variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicUnionsFromLambdaTests: Testing.TestGroup = ({
    name: "Polymorphic unions from lambda",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda creating PersonOrSomething other variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "variable", value: "x" })
  })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda creating UnionPolymorphicRecursive value variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "variable", value: "x" })
  })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicUnwrappingTests: Testing.TestGroup = ({
    name: "Polymorphic unwrapping",
    description: null,
    subgroups: [],
    cases: [({
    name: "unwrap polymorphic wrapper",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "unwrap", value: TestTestTypes.testTypePolymorphicWrapperName }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicWrappedTermsTests: Testing.TestGroup = ({
    name: "Polymorphic wrapped terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "polymorphic wrapper with int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic wrapper with string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic wrapper from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" })] })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const projectionsWithVariablesTests: Testing.TestGroup = ({
    name: "Projections with variables",
    description: null,
    subgroups: [],
    cases: [({
    name: "project from lambda parameter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project from polymorphic lambda parameter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "coords",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lat"
  }) }),
    argument: ({ tag: "variable", value: "coords" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple projections from same record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "lastName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recordEliminationsTests: Testing.TestGroup = ({
    name: "Record eliminations",
    description: null,
    subgroups: [simpleRecordProjectionsTests, recordProjectionsAppliedToRecordsTests, polymorphicRecordProjectionsTests, polymorphicRecordProjectionsAppliedTests, recordProjectionsWithVariablesTests, recordProjectionsInComplexContextsTests, multiParameterPolymorphicProjectionsTests, higherOrderRecordProjectionsTests, recursiveRecordProjectionsTests, recordProjectionsWithMutualRecursionTests, projectionsWithVariablesTests],
    cases: []
  });

export const recordProjectionsAppliedToRecordsTests: Testing.TestGroup = ({
    name: "Record projections applied to records",
    description: null,
    subgroups: [],
    cases: [({
    name: "project firstName applied to person record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project age applied to person record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "age"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Bob" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Jones" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project lat applied to LatLon record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonName,
    field: "lat"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: "LatLon",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 40.712799072265625 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -74.00599670410156 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recordProjectionsInComplexContextsTests: Testing.TestGroup = ({
    name: "Record projections in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "projection in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "person",
    term: ({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Charlie" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Brown" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 35 }) }) })
  })]
  }) }),
    type: null
  }), ({
    name: "getName",
    term: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "getName" }),
    argument: ({ tag: "variable", value: "person" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "projection in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }), ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "age"
  }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "projection in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }), ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "lastName"
  }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recordProjectionsWithMutualRecursionTests: Testing.TestGroup = ({
    name: "Record projections with mutual recursion",
    description: null,
    subgroups: [],
    cases: [({
    name: "project head from BuddyListA",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    field: "head"
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListAName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project tail from BuddyListB",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListBName,
    field: "tail"
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListBName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListAName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "chained projections across mutual recursion",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "listA",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "listB",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    field: "tail"
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListBName,
    field: "tail"
  }) }),
    argument: ({ tag: "variable", value: "listB" })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    field: "tail"
  }) }),
    argument: ({ tag: "variable", value: "listA" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListAName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListBName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recordProjectionsWithVariablesTests: Testing.TestGroup = ({
    name: "Record projections with variables",
    description: null,
    subgroups: [],
    cases: [({
    name: "project from lambda parameter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project from polymorphic lambda parameter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "coords",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonPolyName,
    field: "lat"
  }) }),
    argument: ({ tag: "variable", value: "coords" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple projections from same record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "lastName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recordsInComplexContextsTests: Testing.TestGroup = ({
    name: "Records in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "records in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "record", value: ({
    typeName: "Person",
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Bob" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Jones" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) }), ({ tag: "record", value: ({
    typeName: "LatLon",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 1.0 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 2.0 }) }) })
  })]
  }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "Person" }),
    second: ({ tag: "variable", value: "LatLon" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "poly records in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "record", value: ({
    typeName: "LatLonPoly",
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })]
  }) }), ({ tag: "record", value: ({
    typeName: "BuddyListA",
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "LatLonPoly" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    second: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "BuddyListA" }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "recursive record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "IntList",
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: "IntList",
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
    expected: ShowCore.type(({ tag: "variable", value: "IntList" }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recordsTests: Testing.TestGroup = ({
    name: "Records",
    description: null,
    subgroups: [monomorphicRecordsTests, polymorphicRecordsTests, recordsInComplexContextsTests, multiParameterPolymorphicRecordsTests],
    cases: []
  });

export const recursiveRecordProjectionsTests: Testing.TestGroup = ({
    name: "Recursive record projections",
    description: null,
    subgroups: [],
    cases: [({
    name: "nested projection from recursive record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "intList",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeIntListName,
    field: "head"
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeIntListName,
    field: "tail"
  }) }),
    argument: ({ tag: "variable", value: "intList" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeIntListName }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recursiveUnionEliminationsTests: Testing.TestGroup = ({
    name: "Recursive union eliminations",
    description: null,
    subgroups: [],
    cases: [({
    name: "match HydraType recursively",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeHydraTypeName,
    default: null,
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "lit",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeHydraLiteralTypeName,
    default: null,
    cases: [({
    name: "boolean",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBoolean" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }), ({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "lit" })
  }) })
  }) })
  }), ({
    name: "list",
    term: ({ tag: "lambda", value: ({
    parameter: "nested",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "list" }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeHydraTypeName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simplePolymorphicUnionTests: Testing.TestGroup = ({
    name: "Simple polymorphic unions",
    description: null,
    subgroups: [],
    cases: [({
    name: "match PersonOrSomething with string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    default: null,
    cases: [({
    name: "person",
    term: ({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "p" })
  }) })
  }) })
  }), ({
    name: "other",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match PersonOrSomething instantiated with string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    default: null,
    cases: [({
    name: "person",
    term: ({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "p" })
  }) })
  }) })
  }), ({
    name: "other",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simpleRecordProjectionsTests: Testing.TestGroup = ({
    name: "Simple record projections",
    description: null,
    subgroups: [],
    cases: [({
    name: "project firstName from Person",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project lastName from Person",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "lastName"
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project age from Person",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "age"
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project lat from LatLon",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonName,
    field: "lat"
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "LatLon" }),
    codomain: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "project lon from LatLon",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeLatLonName,
    field: "lon"
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "LatLon" }),
    codomain: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simpleUnionInjectionsTests: Testing.TestGroup = ({
    name: "Simple union injections",
    description: null,
    subgroups: [],
    cases: [({
    name: "inject into Comparison lessThan variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeComparisonName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject into Comparison equalTo variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    field: ({
    name: "equalTo",
    term: ({ tag: "unit" })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeComparisonName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject into Comparison greaterThan variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    field: ({
    name: "greaterThan",
    term: ({ tag: "unit" })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeComparisonName }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simpleUnitVariantEliminationsTests: Testing.TestGroup = ({
    name: "Simple unit inject eliminations",
    description: null,
    subgroups: [],
    cases: [({
    name: "match Comparison with all cases",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: null,
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "less" }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "equal" }) })
  }) })
  }), ({
    name: "greaterThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "greater" }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeComparisonName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match Comparison returning int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: null,
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }), ({
    name: "greaterThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeComparisonName }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match applied to Comparison variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: null,
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "less" }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "equal" }) })
  }) })
  }), ({
    name: "greaterThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "greater" }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    field: ({
    name: "equalTo",
    term: ({ tag: "unit" })
  })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unionEliminationsInComplexContextsTests: Testing.TestGroup = ({
    name: "Union eliminations in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "match in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "matcher",
    term: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: null,
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "less" }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "equal" }) })
  }) })
  }), ({
    name: "greaterThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "greater" }) })
  }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "matcher" })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeComparisonName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match in record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    default: null,
    cases: [({
    name: "person",
    term: ({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "p" })
  }) })
  }) })
  }), ({
    name: "other",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "literal", value: ({ tag: "string", value: "John" }) })
  })
  }) })
  }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypePersonName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match with polymorphic result in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    default: null,
    cases: [({
    name: "person",
    term: ({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "age"
  }) }),
    argument: ({ tag: "variable", value: "p" })
  }) })
  }) })
  }), ({
    name: "other",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })
  }) })
  }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unionEliminationsTests: Testing.TestGroup = ({
    name: "Union eliminations",
    description: null,
    subgroups: [simpleUnitVariantEliminationsTests, unionEliminationsWithDataTests, polymorphicUnionEliminationsTests, unionEliminationsWithDefaultsTests, nestedUnionEliminationsTests, unionEliminationsInComplexContextsTests, multiParameterPolymorphicCaseStatementsTests, higherOrderUnionEliminationsTests, recursiveUnionEliminationsTests],
    cases: []
  });

export const unionEliminationsWithDataTests: Testing.TestGroup = ({
    name: "Union eliminations with data",
    description: null,
    subgroups: [],
    cases: [({
    name: "match Number extracting int values",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    default: null,
    cases: [({
    name: "int",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "variable", value: "i" })
  }) })
  }), ({
    name: "float",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeNumberName }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match Number converting to string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    default: null,
    cases: [({
    name: "int",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  }), ({
    name: "float",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "variable", value: "f" })
  }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeNumberName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match Number applied to int variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    default: null,
    cases: [({
    name: "int",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "i" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }), ({
    name: "float",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    field: ({
    name: "int",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match Timestamp with mixed data types",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeTimestampName,
    default: null,
    cases: [({
    name: "unixTimeMillis",
    term: ({ tag: "lambda", value: ({
    parameter: "millis",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint64" }),
    argument: ({ tag: "variable", value: "millis" })
  }) })
  }) })
  }), ({
    name: "date",
    term: ({ tag: "lambda", value: ({
    parameter: "dateStr",
    domain: null,
    body: ({ tag: "variable", value: "dateStr" })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeTimestampName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unionEliminationsWithDefaultsTests: Testing.TestGroup = ({
    name: "Union eliminations with defaults",
    description: null,
    subgroups: [],
    cases: [({
    name: "match Comparison with default case",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeComparisonName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "unknown" }) }),
    cases: [({
    name: "lessThan",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "less" }) })
  }) })
  }), ({
    name: "equalTo",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "equal" }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeComparisonName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match Number with default case",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    default: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }),
    cases: [({
    name: "int",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "variable", value: "i" })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeNumberName }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "match UnionMonomorphic with default",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "fallback" }) }),
    cases: [({
    name: "bool",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBoolean" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }), ({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unionInjectionsWithDataTests: Testing.TestGroup = ({
    name: "Union injections with data",
    description: null,
    subgroups: [],
    cases: [({
    name: "inject into Number int variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    field: ({
    name: "int",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeNumberName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject into Number float variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    field: ({
    name: "float",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.140000104904175 }) }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeNumberName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject into Timestamp unixTimeMillis variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeTimestampName,
    field: ({
    name: "unixTimeMillis",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 1609459200000n }) }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeTimestampName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "inject into Timestamp date variant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeTimestampName,
    field: ({
    name: "date",
    term: ({ tag: "literal", value: ({ tag: "string", value: "2021-01-01" }) })
  })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeTimestampName }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unionsInComplexContextsTests: Testing.TestGroup = ({
    name: "Unions in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "union in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    field: ({
    name: "int",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) }), ({ tag: "literal", value: ({ tag: "string", value: "context" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: TestTestTypes.testTypeNumberName }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "union in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    field: ({
    name: "int",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) }), ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeNumberName,
    field: ({
    name: "float",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 2.5 }) }) })
  })
  }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "variable", value: TestTestTypes.testTypeNumberName }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic union in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "value",
    term: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypePersonOrSomethingName,
    field: ({
    name: "other",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "value" })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypePersonOrSomethingName }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unionsTests: Testing.TestGroup = ({
    name: "Unions",
    description: null,
    subgroups: [simpleUnionInjectionsTests, unionInjectionsWithDataTests, polymorphicUnionInjectionsTests, polymorphicRecursiveUnionInjectionsTests, polymorphicUnionsFromLambdaTests, unionsInComplexContextsTests, multiParameterPolymorphicInjectionsTests],
    cases: []
  });

export const unwrapEliminationsInApplicationsTests: Testing.TestGroup = ({
    name: "Unwrap eliminations in applications",
    description: null,
    subgroups: [],
    cases: [({
    name: "unwrap applied to wrapped term",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }),
    argument: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unwrap polymorphic applied",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypePolymorphicWrapperName }),
    argument: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypePolymorphicWrapperName,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unwrapInComplexContextsTests: Testing.TestGroup = ({
    name: "Unwrap in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "unwrap in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "unwrapper",
    term: ({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }),
    type: null
  }), ({
    name: "wrapped",
    term: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "unwrapper" }),
    argument: ({ tag: "variable", value: "wrapped" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unwrap in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }), ({ tag: "literal", value: ({ tag: "string", value: "context" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unwrap in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "wrapped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: TestTestTypes.testTypeStringAliasName }),
    argument: ({ tag: "variable", value: "wrapped" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const usingKernelTypesTests: Testing.TestGroup = ({
    name: "Using kernel types",
    description: null,
    subgroups: [],
    cases: [({
    name: "case statement on CoderDirection applied to argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "dir",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "coder",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.coders.CoderDirection",
    default: null,
    cases: [({
    name: "encode",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v12",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.coders.Coder",
    field: "encode"
  }) }),
    argument: ({ tag: "variable", value: "coder" })
  }) }),
    argument: ({ tag: "variable", value: "v12" })
  }) })
  }) })
  }) })
  }), ({
    name: "decode",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v12",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.coders.Coder",
    field: "decode"
  }) }),
    argument: ({ tag: "variable", value: "coder" })
  }) }),
    argument: ({ tag: "variable", value: "v12" })
  }) })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "dir" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.coders.CoderDirection" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Coder" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.context.InContext" }),
    argument: ({ tag: "variable", value: "hydra.errors.Error" })
  }) }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: ["disabled"]
  })]
  });

export const usingUnionPolymorphicRecursiveTests: Testing.TestGroup = ({
    name: "using UnionPolymorphicRecursive",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-applied UnionPolymorphicRecursive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied UnionPolymorphicRecursive with int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied UnionPolymorphicRecursive with int32 in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied generic UnionPolymorphicRecursive in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const wrapEliminationsTests: Testing.TestGroup = ({
    name: "Wrap eliminations",
    description: null,
    subgroups: [monomorphicUnwrappingTests, polymorphicUnwrappingTests, unwrapEliminationsInApplicationsTests, unwrapInComplexContextsTests, multiParameterPolymorphicUnwrappersTests, chainedUnwrappingTests, multipleUnwrapOperationsTests],
    cases: []
  });

export const wrappedTermsInComplexContextsTests: Testing.TestGroup = ({
    name: "Wrapped terms in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "wrapped in record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "John" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypePersonName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "wrapped in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "alias",
    term: ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "alias" })
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "wrapped in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "first" }) })
  }) }), ({ tag: "wrap", value: ({
    typeName: TestTestTypes.testTypeStringAliasName,
    body: ({ tag: "literal", value: ({ tag: "string", value: "second" }) })
  }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "variable", value: TestTestTypes.testTypeStringAliasName }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const wrappedTermsTests: Testing.TestGroup = ({
    name: "Wrapped terms",
    description: null,
    subgroups: [monomorphicWrappedTermsTests, polymorphicWrappedTermsTests, wrappedTermsInComplexContextsTests, nestedWrappedTermsTests, multipleWrappingLevelsTests, multiParameterPolymorphicWrappersTests],
    cases: []
  });

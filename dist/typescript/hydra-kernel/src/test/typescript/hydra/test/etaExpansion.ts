// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for eta expansion of terms
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
import * as LibEithers from "../lib/eithers.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Reduction from "../reduction.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Tabular from "../tabular.js";
import * as TestTestGraph from "./testGraph.js";
import * as TestTestTypes from "./testTypes.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "eta expansion",
    description: null,
    subgroups: [({
    name: "Partial application of primitives",
    description: null,
    subgroups: [({
    name: "Bare primitives are not expanded",
    description: null,
    subgroups: [],
    cases: [({
    name: "unary primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.strings.toLower" }))),
    expected: ShowCore.term(({ tag: "variable", value: "hydra.lib.strings.toLower" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "binary primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.strings.splitOn" }))),
    expected: ShowCore.term(({ tag: "variable", value: "hydra.lib.strings.splitOn" }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Partially applied primitives expand with lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "binary primitive with one argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ternary primitive with one argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v2",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "f" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) }),
    argument: ({ tag: "variable", value: "v2" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Fully applied primitives are not expanded",
    description: null,
    subgroups: [],
    cases: [({
    name: "unary primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "FOO" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "FOO" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "binary primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a,b,c" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a,b,c" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Record projections",
    description: null,
    subgroups: [({
    name: "Bare projections expand with a lambda",
    description: null,
    subgroups: [],
    cases: [({
    name: "projection without argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Applied projections are not expanded",
    description: null,
    subgroups: [],
    cases: [({
    name: "projection with argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "projection applied to a record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "John" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "John" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  })]
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Projections nested in other structures",
    description: null,
    subgroups: [],
    cases: [({
    name: "projection in a list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }), ({ tag: "variable", value: "hydra.lib.strings.toLower" })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }), ({ tag: "variable", value: "hydra.lib.strings.toLower" })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "projection in a tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }), ({ tag: "literal", value: ({ tag: "string", value: "default" }) })] }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }), ({ tag: "literal", value: ({ tag: "string", value: "default" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "projection in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "getter",
    term: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "getter" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "getter",
    term: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "getter" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "projection in lambda body",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Function-valued projections",
    description: null,
    subgroups: [],
    cases: [({
    name: "projection of function-valued field applied to arguments should not be expanded",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "first"
  }) }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    argument: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "variable", value: "hydra.lib.strings.toLower" })
  }), ({
    name: "second",
    term: ({ tag: "literal", value: ({ tag: "string", value: "middle" }) })
  }), ({
    name: "third",
    term: ({ tag: "literal", value: ({ tag: "string", value: "last" }) })
  })]
  }) }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "DATA" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "first"
  }) }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    argument: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "variable", value: "hydra.lib.strings.toLower" })
  }), ({
    name: "second",
    term: ({ tag: "literal", value: ({ tag: "string", value: "middle" }) })
  }), ({
    name: "third",
    term: ({ tag: "literal", value: ({ tag: "string", value: "last" }) })
  })]
  }) }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "DATA" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  })],
    cases: []
  }), ({
    name: "Polymorphic terms (System F)",
    description: null,
    subgroups: [({
    name: "Type lambdas in let bindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "polymorphic identity function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    body: ({ tag: "variable", value: "id" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    body: ({ tag: "variable", value: "id" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "monomorphic partially applied primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "partial",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "partial" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "partial",
    term: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "partial" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "monomorphic projection",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "getter",
    term: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "getter" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "getter",
    term: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "Person" }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "getter" })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Type applications of polymorphic bindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "polymorphic variable with type application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type application of identity applied to binary function with no arguments",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type application of identity applied to partially applied binary function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type application of identity applied to fully applied binary function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo,bar" }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo,bar" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type application of identity applied to binary function, then applied to one argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type application of identity applied to binary function, then fully applied to two arguments",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo,bar" }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "id" }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo,bar" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  }), ({
    name: "Higher-Order Functions",
    description: null,
    subgroups: [({
    name: "Functions that return functions",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda returning bare binary primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "hydra.lib.strings.splitOn" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda returning bare unary primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "hydra.lib.strings.toLower" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "hydra.lib.strings.toLower" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda returning partially applied primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda returning fully applied primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda returning bare projection",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested lambdas with partial application in body",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "variable", value: "x" })
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
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda returning lambda returning partial application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "variable", value: "x" })
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
    parameter: "z",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  }), ({
    name: "Let terms",
    description: null,
    subgroups: [({
    name: "partial application of a let-bound function",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "arg1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg3",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "arg1" }), ({ tag: "variable", value: "arg2" }), ({ tag: "variable", value: "arg3" })] })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "arg1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg3",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "arg1" }), ({ tag: "variable", value: "arg2" }), ({ tag: "variable", value: "arg3" })] })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v2",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) }),
    argument: ({ tag: "variable", value: "v2" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "in a fold",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "arg1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg3",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "arg1" }), ({ tag: "variable", value: "arg2" }), ({ tag: "variable", value: "arg3" })] })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "arg1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg3",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "arg1" }), ({ tag: "variable", value: "arg2" }), ({ tag: "variable", value: "arg3" })] })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v2",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) }),
    argument: ({ tag: "variable", value: "v2" })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "bar" }) }), ({ tag: "literal", value: ({ tag: "string", value: "baz" }) })] })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "within another let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "tryme",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "arg1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg3",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "arg1" }), ({ tag: "variable", value: "arg2" }), ({ tag: "variable", value: "arg3" })] })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "unit" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "tryme",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "helper",
    term: ({ tag: "lambda", value: ({
    parameter: "arg1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg2",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "arg3",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "arg1" }), ({ tag: "variable", value: "arg2" }), ({ tag: "variable", value: "arg3" })] })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "v2",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "helper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) }),
    argument: ({ tag: "variable", value: "v2" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "unit" })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  }), ({
    name: "Case statements",
    description: null,
    subgroups: [({
    name: "monomorphic at top level",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-applied case statement",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied case statement",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied case statement in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "monomorphic in let binding",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-applied case statement",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "ignored" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "ignored" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied case statement",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "ignored" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "ignored" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied case statement in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "ignored" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: TestTestTypes.testTypeUnionMonomorphicName }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "variable", value: "s" })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "ignored" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "polymorphic in let binding",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-applied UnionPolymorphicRecursive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied UnionPolymorphicRecursive with int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "typeApplication", value: ({
    body: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "typeApplication", value: ({
    body: ({ tag: "inject", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    field: ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied UnionPolymorphicRecursive with int32 in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "i" })
  }) })
  }) })
  })]
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "test" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "applied generic UnionPolymorphicRecursive in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t1",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: ({ tag: "variable", value: "t1" }),
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  })]
  }) }),
    type: ({ tag: "variable", value: "t1" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "test" }),
    type: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "test",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t1",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionPolymorphicRecursiveName,
    default: ({ tag: "literal", value: ({ tag: "string", value: "other" }) }),
    cases: [({
    name: "value",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: ({ tag: "variable", value: "t1" }),
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  })]
  }) }),
    type: ({ tag: "variable", value: "t1" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t1"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeUnionPolymorphicRecursiveName }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "test" }),
    type: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "Forced expansion in case statement branches",
    description: null,
    subgroups: [],
    cases: [({
    name: "variable reference in case branch is expanded",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "handler",
    term: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: null,
    cases: [({
    name: "bool",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "boolean value" }) })
  }) })
  }), ({
    name: "string",
    term: ({ tag: "variable", value: "handler" })
  }), ({
    name: "unit",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "unit value" }) })
  }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "handler",
    term: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: null,
    cases: [({
    name: "bool",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "boolean value" }) })
  }) })
  }), ({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "handler" }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }), ({
    name: "unit",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "unit value" }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bare primitive in case branch is expanded",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: null,
    cases: [({
    name: "bool",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "boolean value" }) })
  }) })
  }), ({
    name: "string",
    term: ({ tag: "variable", value: "hydra.lib.strings.toLower" })
  }), ({
    name: "unit",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "unit value" }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: TestTestTypes.testTypeUnionMonomorphicName,
    default: null,
    cases: [({
    name: "bool",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "boolean value" }) })
  }) })
  }), ({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }), ({
    name: "unit",
    term: ({ tag: "lambda", value: ({
    parameter: "ignored",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "unit value" }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable reference outside case branch is not expanded",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "handler",
    term: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    type: null
  })],
    body: ({ tag: "variable", value: "handler" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "handler",
    term: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    type: null
  })],
    body: ({ tag: "variable", value: "handler" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bare primitive outside case branch is not expanded",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.strings.toLower" }))),
    expected: ShowCore.term(({ tag: "variable", value: "hydra.lib.strings.toLower" }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  }), ({
    name: "Non-expansion of eliminations which produce functions",
    description: null,
    subgroups: [],
    cases: [({
    name: "applied case statement",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "lambda", value: ({
    parameter: "dir",
    domain: ({ tag: "variable", value: "hydra.coders.CoderDirection" }),
    body: ({ tag: "lambda", value: ({
    parameter: "coder",
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Coder" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: ({ tag: "variable", value: "t0" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.coders.CoderDirection",
    default: null,
    cases: [({
    name: "encode",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: ({ tag: "unit" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: "hydra.coders.Coder",
    field: "encode"
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "coder" })
  }) }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }), ({
    name: "decode",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: ({ tag: "unit" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: "hydra.coders.Coder",
    field: "decode"
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "coder" })
  }) }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "dir" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "lambda", value: ({
    parameter: "dir",
    domain: ({ tag: "variable", value: "hydra.coders.CoderDirection" }),
    body: ({ tag: "lambda", value: ({
    parameter: "coder",
    domain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Coder" }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "t0" })
  }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    body: ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: ({ tag: "variable", value: "t0" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.coders.CoderDirection",
    default: null,
    cases: [({
    name: "encode",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: ({ tag: "unit" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: "hydra.coders.Coder",
    field: "encode"
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "coder" })
  }) }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }), ({
    name: "decode",
    term: ({ tag: "lambda", value: ({
    parameter: "_",
    domain: ({ tag: "unit" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: "hydra.coders.Coder",
    field: "decode"
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    type: ({ tag: "variable", value: "t0" })
  }) }),
    argument: ({ tag: "variable", value: "coder" })
  }) }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "dir" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: ["disabled"]
  }), ({
    name: "applied projection",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("ETA ERROR: ")("failed")))(((result: Core.Term) => ShowCore.term(result)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "third"
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }), ({
    name: "second",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }), ({
    name: "third",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    field: "third"
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    argument: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeTripleName,
    fields: [({
    name: "first",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }), ({
    name: "second",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) })
  }), ({
    name: "third",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) })
  })]
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

// Note: this is an automatically generated file. Do not edit.

/**
 * Fundamental type checking test cases: literals, variables, lambdas, applications, let terms, and primitives
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
    name: "Fundamentals",
    description: null,
    subgroups: [literalsTests, variablesTests, lambdasTests, applicationsTests, letTermsTests, primitivesTests],
    cases: []
  });

export const applicationsInComplexContextsTests: Testing.TestGroup = ({
    name: "Applications in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "application in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "b" }) })
  }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "application in record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "John" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "ny" }) })
  }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  }), ({
    name: "age",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypePersonName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "application in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "result",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "result" })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested applications",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const applicationsTests: Testing.TestGroup = ({
    name: "Applications",
    description: null,
    subgroups: [simpleFunctionApplicationsTests, partialApplicationsTests, higherOrderApplicationsTests, polymorphicApplicationsTests, applicationsInComplexContextsTests, applicationsWithComplexArgumentsTests],
    cases: []
  });

export const applicationsWithComplexArgumentsTests: Testing.TestGroup = ({
    name: "Applications with complex arguments",
    description: null,
    subgroups: [],
    cases: [({
    name: "application with record argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "getName",
    term: ({ tag: "lambda", value: ({
    parameter: "person",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "person" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "getName" }),
    argument: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "application with list argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "head",
    term: ({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "variable", value: "xs" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "head" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "first" }) }), ({ tag: "literal", value: ({ tag: "string", value: "second" }) })] })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const binaryPrimitivesTests: Testing.TestGroup = ({
    name: "Binary primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "math add",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.math.add" }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lists cons",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.cons" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "maps insert",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maps.insert" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
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
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const booleanLiteralsTests: Testing.TestGroup = ({
    name: "Boolean literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "boolean" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "boolean" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const floatLiteralsTests: Testing.TestGroup = ({
    name: "Float literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "bigfloat",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.14159 }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 2.718280076980591 }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.41421 }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const higherOrderApplicationsTests: Testing.TestGroup = ({
    name: "Higher-order applications",
    description: null,
    subgroups: [],
    cases: [({
    name: "apply function to function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "apply",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "double",
    term: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "n" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "apply" }),
    argument: ({ tag: "variable", value: "double" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "function composition",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    name: "add1",
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
    name: "mul2",
    term: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "n" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "compose" }),
    argument: ({ tag: "variable", value: "add1" })
  }) }),
    argument: ({ tag: "variable", value: "mul2" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const higherOrderLambdasTests: Testing.TestGroup = ({
    name: "Higher-order lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "function composition",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
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
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
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
    name: "function application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "curried function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const higherOrderPrimitivesTests: Testing.TestGroup = ({
    name: "Higher-order primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "lists map function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    codomain: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lists filter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.filter" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "optionals maybe",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maybes.maybe" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "maybe", value: ({ tag: "variable", value: "t1" }) }),
    codomain: ({ tag: "variable", value: "t0" })
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

export const integerLiteralsTests: Testing.TestGroup = ({
    name: "Integer literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "bigint",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 127 }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int16",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 32767n }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2147483647 }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 9223372036854775807n }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "uint8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 255n }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "uint16",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 65535 }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "uint32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 4294967295n }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "uint64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 18446744073709551615n }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const lambdasInComplexContextsTests: Testing.TestGroup = ({
    name: "Lambdas in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }), ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda in record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "name",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "variable", value: "name" })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "variable", value: TestTestTypes.testTypePersonName })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const lambdasTests: Testing.TestGroup = ({
    name: "Lambdas",
    description: null,
    subgroups: [simpleLambdasTests, multiParameterLambdasTests, lambdasWithOperationsTests, nestedLambdasTests, lambdasInComplexContextsTests, higherOrderLambdasTests],
    cases: []
  });

export const lambdasWithOperationsTests: Testing.TestGroup = ({
    name: "Lambdas with operations",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda with primitive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with construction",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
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
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
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

export const letTermsTests: Testing.TestGroup = ({
    name: "Let terms",
    description: null,
    subgroups: [simpleLetBindingsTests, letTermsWithShadowingTests, recursiveBindingsTests, mutualRecursionTests, nestedLetTermsTests, letWithComplexExpressionsTests],
    cases: []
  });

export const letTermsWithShadowingTests: Testing.TestGroup = ({
    name: "Let terms with shadowing",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda parameter shadowing let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested lambda shadowing",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
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
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t1" }),
    second: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple levels of let shadowing",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "second" }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "boolean" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let shadowing with lambda and reference to outer binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "variable", value: "y" }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "z" })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const letWithComplexExpressionsTests: Testing.TestGroup = ({
    name: "Let with complex expressions",
    description: null,
    subgroups: [],
    cases: [({
    name: "let in record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "first",
    term: ({ tag: "literal", value: ({ tag: "string", value: "John" }) }),
    type: null
  }), ({
    name: "middle",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Q" }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "variable", value: "first" })
  }) }),
    argument: ({ tag: "variable", value: "middle" })
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
    name: "let in function application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "composition",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    name: "add1",
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
    name: "double",
    term: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "n" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "compose" }),
    argument: ({ tag: "variable", value: "add1" })
  }) }),
    argument: ({ tag: "variable", value: "double" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const literalsInComplexContextsTests: Testing.TestGroup = ({
    name: "Literals in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "literals in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "boolean", value: true }) }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "test" }) }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.140000104904175 }) }) })] })] })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "literals in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "one" }) }), ({ tag: "literal", value: ({ tag: "string", value: "two" }) }), ({ tag: "literal", value: ({ tag: "string", value: "three" }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const literalsTests: Testing.TestGroup = ({
    name: "Literals",
    description: null,
    subgroups: [booleanLiteralsTests, stringLiteralsTests, integerLiteralsTests, floatLiteralsTests, literalsInComplexContextsTests],
    cases: []
  });

export const monomorphicVsPolymorphicTests: Testing.TestGroup = ({
    name: "Monomorphic vs polymorphic",
    description: null,
    subgroups: [],
    cases: [({
    name: "monomorphic math",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.math.add" }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic identity",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.equality.identity" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.map" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const multiParameterLambdasTests: Testing.TestGroup = ({
    name: "Multi-parameter lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "two parameters",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "three parameters",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
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
    name: "parameter reuse",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })] })
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
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
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

export const mutualRecursionTests: Testing.TestGroup = ({
    name: "Mutual recursion",
    description: null,
    subgroups: [],
    cases: [({
    name: "mutually recursive data",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "listA",
    term: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "variable", value: "listB" }) })
  })]
  }) }),
    type: null
  }), ({
    name: "listB",
    term: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListBName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "listA" })
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListAName }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "(monomorphic) mutually recursive functions",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedLambdasTests: Testing.TestGroup = ({
    name: "Nested lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda returning lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
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
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t0" })
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
    name: "lambda with let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with inner lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "outer",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "inner",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "inner" }),
    argument: ({ tag: "variable", value: "outer" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedLetTermsTests: Testing.TestGroup = ({
    name: "Nested let terms",
    description: null,
    subgroups: [],
    cases: [({
    name: "monomorphic nesting",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic nesting",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "apply",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "apply" }),
    argument: ({ tag: "variable", value: "id" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable capture avoidance",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "simple let in lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "variable", value: "z" }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nullaryPrimitivesTests: Testing.TestGroup = ({
    name: "Nullary primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.maps.empty" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.sets.empty" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "set", value: ({ tag: "variable", value: "t0" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const partialApplicationsTests: Testing.TestGroup = ({
    name: "Partial applications",
    description: null,
    subgroups: [],
    cases: [({
    name: "partially applied add",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "partially applied string cat",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "prefix" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicApplicationsTests: Testing.TestGroup = ({
    name: "Polymorphic applications",
    description: null,
    subgroups: [],
    cases: [({
    name: "polymorphic identity",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic const",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "const" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "keep" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 999 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic flip",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "flip",
    term: ({ tag: "lambda", value: ({
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
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "flip" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.cat2" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "world" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicVariablesTests: Testing.TestGroup = ({
    name: "Polymorphic variables",
    description: null,
    subgroups: [],
    cases: [({
    name: "polymorphic function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "id" })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    argument: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "higher order polymorphic",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "apply",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "apply" })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const primitivesInComplexContextsTests: Testing.TestGroup = ({
    name: "Primitives in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "primitive composition",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "double",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "increment",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "double" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "increment" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested higher-order",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })] })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const primitivesTests: Testing.TestGroup = ({
    name: "Primitives",
    description: null,
    subgroups: [nullaryPrimitivesTests, unaryPrimitivesTests, binaryPrimitivesTests, ternaryPrimitivesTests, monomorphicVsPolymorphicTests, higherOrderPrimitivesTests, primitivesInComplexContextsTests],
    cases: []
  });

export const recursiveBindingsTests: Testing.TestGroup = ({
    name: "Recursive bindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple arithmetic recursion",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "double",
    term: ({ tag: "lambda", value: ({
    parameter: "n",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "n" })
  }) }),
    argument: ({ tag: "variable", value: "n" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "double" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const recursiveVariablesTests: Testing.TestGroup = ({
    name: "Recursive variables",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple recursion",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mutual recursion",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simpleFunctionApplicationsTests: Testing.TestGroup = ({
    name: "Simple function applications",
    description: null,
    subgroups: [],
    cases: [({
    name: "identity application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "primitive application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string concatenation",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "world" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simpleLambdasTests: Testing.TestGroup = ({
    name: "Simple lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "identity function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "constant function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simpleLetBindingsTests: Testing.TestGroup = ({
    name: "Simple let bindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "single binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple bindings",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const simpleVariableLookupTests: Testing.TestGroup = ({
    name: "Simple variable lookup",
    description: null,
    subgroups: [],
    cases: [({
    name: "int variable",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple variables",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const stringLiteralsTests: Testing.TestGroup = ({
    name: "String literals",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "string", value: "" }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unicode string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "string", value: "café" }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const ternaryPrimitivesTests: Testing.TestGroup = ({
    name: "Ternary primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "logic ifElse",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.logic.ifElse" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lists foldl",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.foldl" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t1" }) }),
    codomain: ({ tag: "variable", value: "t0" })
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

export const unaryPrimitivesTests: Testing.TestGroup = ({
    name: "Unary primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "lists head",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.lists.head" }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "math neg",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.math.negate" }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logic not",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.logic.not" }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const variableScopingTests: Testing.TestGroup = ({
    name: "Variable scoping",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda parameter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
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
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let binding scope",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable shadowing",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
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
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested scoping",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "variable", value: "x" }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "pair", value: [({ tag: "variable", value: "y" }), ({ tag: "variable", value: "z" })] })] })
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
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
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

export const variablesInComplexContextsTests: Testing.TestGroup = ({
    name: "Variables in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "variable in record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "name",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "variable", value: "name" })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "variable", value: TestTestTypes.testTypePersonName })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable in map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "key",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "value",
    domain: null,
    body: ({ tag: "map", value: new Map([[({ tag: "variable", value: "key" }), ({ tag: "variable", value: "value" })]]) })
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
    codomain: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "t0" }),
    values: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable in optional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "x" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const variablesTests: Testing.TestGroup = ({
    name: "Variables",
    description: null,
    subgroups: [simpleVariableLookupTests, variableScopingTests, polymorphicVariablesTests, variablesInComplexContextsTests, recursiveVariablesTests],
    cases: []
  });

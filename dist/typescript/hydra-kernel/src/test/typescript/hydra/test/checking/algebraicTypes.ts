// Note: this is an automatically generated file. Do not edit.

/**
 * Algebraic type checking test cases: unit, pairs, eithers, optionals
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
    name: "Algebraic types",
    description: null,
    subgroups: [unitTests, pairsTests, eithersTests, optionalsTests],
    cases: []
  });

export const basicPairsTests: Testing.TestGroup = ({
    name: "Basic pairs",
    description: null,
    subgroups: [],
    cases: [({
    name: "pair of int and string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair of string and boolean",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "test" }) }), ({ tag: "literal", value: ({ tag: "boolean", value: true }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair of boolean and int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "boolean", value: false }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100 }) }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const eithersInComplexContextsTests: Testing.TestGroup = ({
    name: "Eithers in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "either in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "error" }) }) }) }), ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "result",
    term: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "result" })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const eithersTests: Testing.TestGroup = ({
    name: "Eithers",
    description: null,
    subgroups: [leftValuesTests, rightValuesTests, polymorphicEithersTests, eithersInComplexContextsTests, nestedEithersTests, eithersWithComplexTypesTests],
    cases: []
  });

export const eithersWithComplexTypesTests: Testing.TestGroup = ({
    name: "Eithers with complex types",
    description: null,
    subgroups: [],
    cases: [({
    name: "either with record on left",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "record", value: ({
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
  }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: TestTestTypes.testTypePersonName }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either with record on right",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "right", value: ({ tag: "record", value: ({
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
  }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "variable", value: TestTestTypes.testTypePersonName })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const leftValuesTests: Testing.TestGroup = ({
    name: "Left values",
    description: null,
    subgroups: [],
    cases: [({
    name: "left int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "left string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "error" }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "left boolean",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "boolean", value: false }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const monomorphicOptionalsTests: Testing.TestGroup = ({
    name: "Monomorphic optionals",
    description: null,
    subgroups: [],
    cases: [({
    name: "nothing",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: null }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "t0" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "just int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "just string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "just boolean",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "boolean" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedEithersTests: Testing.TestGroup = ({
    name: "Nested eithers",
    description: null,
    subgroups: [],
    cases: [({
    name: "either of either (left left)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "either", value: ({
    left: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    right: ({ tag: "variable", value: "t0" })
  }) }),
    right: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either of either (left right)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: "nested" }) }) }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "either", value: ({
    left: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    right: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either of either (right)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either of list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "left", value: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list of eithers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "a" }) }) }) }), ({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }) }), ({ tag: "either", value: ({ tag: "left", value: ({ tag: "literal", value: ({ tag: "string", value: "b" }) }) }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedOptionalsTests: Testing.TestGroup = ({
    name: "Nested optionals",
    description: null,
    subgroups: [],
    cases: [({
    name: "optional of optional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "nested" }) }) }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "optional of list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list of optionals",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "a" }) }) }), ({ tag: "maybe", value: null }), ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "b" }) }) })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedPairsTests: Testing.TestGroup = ({
    name: "Nested pairs",
    description: null,
    subgroups: [],
    cases: [({
    name: "pair of pairs",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "one" }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "boolean", value: true }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    second: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair with list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "literal", value: ({ tag: "string", value: "numbers" }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list of pairs",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const optionalsInComplexContextsTests: Testing.TestGroup = ({
    name: "Optionals in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "optional in record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListAName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "string", value: "first" }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeBuddyListBName,
    fields: [({
    name: "head",
    term: ({ tag: "literal", value: ({ tag: "string", value: "second" }) })
  }), ({
    name: "tail",
    term: ({ tag: "maybe", value: null })
  })]
  }) }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "application", value: ({
    function: ({ tag: "variable", value: TestTestTypes.testTypeBuddyListAName }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "optional in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "maybeValue",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "maybeValue" })
  }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const optionalsTests: Testing.TestGroup = ({
    name: "Optionals",
    description: null,
    subgroups: [monomorphicOptionalsTests, polymorphicOptionalsTests, optionalsInComplexContextsTests, nestedOptionalsTests, optionalsWithComplexTypesTests],
    cases: []
  });

export const optionalsWithComplexTypesTests: Testing.TestGroup = ({
    name: "Optionals with complex types",
    description: null,
    subgroups: [],
    cases: [({
    name: "optional map",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "maybe", value: ({ tag: "map", value: new Map([[({ tag: "literal", value: ({ tag: "string", value: "key" }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })]]) }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const pairsInComplexContextsTests: Testing.TestGroup = ({
    name: "Pairs in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "pair in list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "one" }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "two" }) })] })] }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair in let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "result",
    term: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "answer" }) })] }),
    type: null
  })],
    body: ({ tag: "variable", value: "result" })
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

export const pairsTests: Testing.TestGroup = ({
    name: "Pairs",
    description: null,
    subgroups: [basicPairsTests, polymorphicPairsTests, pairsInComplexContextsTests, nestedPairsTests, pairsWithComplexTypesTests],
    cases: []
  });

export const pairsWithComplexTypesTests: Testing.TestGroup = ({
    name: "Pairs with complex types",
    description: null,
    subgroups: [],
    cases: [({
    name: "pair with record on first",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "record", value: ({
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
  }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "Person" }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair with record on second",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "name" }) }), ({ tag: "record", value: ({
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
  }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "Person" })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicEithersTests: Testing.TestGroup = ({
    name: "Polymorphic eithers",
    description: null,
    subgroups: [],
    cases: [({
    name: "left from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "x" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "right from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "x" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t1" }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "either from two lambdas",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "flag",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "flag" })
  }) }),
    argument: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "x" }) }) })
  }) }),
    argument: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "x" }) }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicOptionalsTests: Testing.TestGroup = ({
    name: "Polymorphic optionals",
    description: null,
    subgroups: [],
    cases: [({
    name: "optional from lambda",
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
  }), ({
    name: "nothing from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "maybe", value: null })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t1" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "conditional optional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "flag",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "variable", value: "flag" })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "variable", value: "x" }) })
  }) }),
    argument: ({ tag: "maybe", value: null })
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "maybe", value: ({ tag: "variable", value: "t0" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const polymorphicPairsTests: Testing.TestGroup = ({
    name: "Polymorphic pairs",
    description: null,
    subgroups: [],
    cases: [({
    name: "pair from lambda (first element)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "string", value: "constant" }) })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair from lambda (second element)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "string", value: "constant" }) }), ({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair from two lambdas",
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
  }), ({
    name: "pair with repeated variable",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })] })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t0" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const rightValuesTests: Testing.TestGroup = ({
    name: "Right values",
    description: null,
    subgroups: [],
    cases: [({
    name: "right int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "right string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "string", value: "success" }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "right boolean",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "either", value: ({ tag: "right", value: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }) }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "t0" }),
    right: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unitTermInPolymorphicContextTests: Testing.TestGroup = ({
    name: "Unit term in polymorphic context",
    description: null,
    subgroups: [],
    cases: [({
    name: "unit from lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "unit" })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "t0",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "unit" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unitTermTests: Testing.TestGroup = ({
    name: "Unit term",
    description: null,
    subgroups: [],
    cases: [({
    name: "unit literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "unit" }))),
    expected: ShowCore.type(({ tag: "unit" }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const unitTests: Testing.TestGroup = ({
    name: "Unit",
    description: null,
    subgroups: [unitTermTests, unitTermInPolymorphicContextTests],
    cases: []
  });

// Note: this is an automatically generated file. Do not edit.

/**
 * Advanced type checking test cases: annotated terms and flows
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
    name: "Advanced",
    description: null,
    subgroups: [annotatedTermsTests],
    cases: []
  });

export const annotatedTermsTests: Testing.TestGroup = ({
    name: "Annotated terms",
    description: null,
    subgroups: [topLevelAnnotationsTests, nestedAnnotationsTests, annotationsInComplexContextsTests],
    cases: []
  });

export const annotationsInComplexContextsTests: Testing.TestGroup = ({
    name: "Annotations in complex contexts",
    description: null,
    subgroups: [],
    cases: [({
    name: "annotated let binding",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }),
    annotation: new Map([])
  }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "world" }) }),
    annotation: new Map([])
  }) }),
    type: null
  })],
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] }),
    annotation: new Map([])
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated record fields",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) }),
    annotation: new Map([])
  }) })
  }), ({
    name: "lastName",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "Smith" }) }),
    annotation: new Map([])
  }) })
  }), ({
    name: "age",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) }),
    annotation: new Map([])
  }) })
  })]
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypePersonName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated function in application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "add",
    term: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.lib.math.add" }),
    annotation: new Map([])
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "add" }),
    argument: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }),
    annotation: new Map([])
  }) })
  }) }),
    argument: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }),
    annotation: new Map([])
  }) })
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const nestedAnnotationsTests: Testing.TestGroup = ({
    name: "Nested annotations",
    description: null,
    subgroups: [],
    cases: [({
    name: "annotation within annotation",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "annotated", value: ({
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100 }) }) }),
    annotation: new Map([])
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated terms in tuple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "pair", value: [({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    annotation: new Map([])
  }) }), ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    annotation: new Map([])
  }) })] }))),
    expected: ShowCore.type(({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated term in function application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) }),
    argument: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    annotation: new Map([])
  }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const topLevelAnnotationsTests: Testing.TestGroup = ({
    name: "Top-level annotations",
    description: null,
    subgroups: [],
    cases: [({
    name: "annotated literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "annotated", value: ({
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated record",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "annotated", value: ({
    body: ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "John" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Doe" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) })
  })]
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.type(({ tag: "variable", value: TestTestTypes.testTypePersonName }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotated lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<inference error>>"))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.type(Scoping.typeSchemeToFType(LibPairs.second(LibPairs.first(result))))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
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

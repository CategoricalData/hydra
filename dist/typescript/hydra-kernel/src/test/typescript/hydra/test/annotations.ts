// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.annotations functions
 */



import * as Annotations from "../annotations.js";
import * as Core from "../core.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibMaps from "../lib/maps.js";
import * as Reduction from "../reduction.js";
import * as ShowCore from "../show/core.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";

export const allTests: Testing.TestGroup = ({
    name: "annotations",
    description: null,
    subgroups: [({
    name: "arbitrary annotations",
    description: null,
    subgroups: [],
    cases: [({
    name: "set single annotation #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set single annotation #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "myKey" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -17 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "myKey" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -17 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set single annotation #3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get existing annotation #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "value" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "value" }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get existing annotation #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get existing annotation #3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "key" }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "key" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 123 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 123 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get missing annotation #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int16",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 42n }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get missing annotation #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "nonexistent" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get missing annotation #3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k2" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set multiple annotations #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k2" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 200 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "first" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.fromList([[({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) }), ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "first" }) })
  })
  }) })
  })
  }) })], [({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k2" }) })
  }) }), ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 200 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })]]) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set multiple annotations #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "b" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "a" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.fromList([[({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "a" }) })
  }) }), ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })], [({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "b" }) })
  }) }), ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })]]) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "outer annotation overrides inner #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "outer" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "inner" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "outer" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "outer annotation overrides inner #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "new" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "old" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "new" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "outer annotation overrides inner #3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "key" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 999 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "key" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "key" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 999 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unset single annotation #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 137n }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 137n }) }) })
  })
  }) })
  })
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unset single annotation #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unset one of multiple annotations #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k2" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 200 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k1" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "first" }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 137n }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 137n }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "k2" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 200 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unset one of multiple annotations #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "b" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "b" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "a" }) })
  }) })
  }) }),
    argument: ({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  })
  }) })
  })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "a" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "descriptions",
    description: null,
    subgroups: [],
    cases: [({
    name: "set description #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "my description" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "description" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "my description" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set description #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "description" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set description #3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "A longer description with spaces" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "description" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "A longer description with spaces" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get existing description #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermDescription" }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyContext" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyGraph" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get existing description #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermDescription" }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyContext" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyGraph" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "" }) }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get existing description #3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermDescription" }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyContext" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyGraph" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "desc" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "desc" }) }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get missing description #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermDescription" }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyContext" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyGraph" })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int16",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 42n }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: null }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get missing description #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermDescription" }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyContext" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyGraph" })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "no description here" }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: null }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get missing description #3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermDescription" }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyContext" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lexical.emptyGraph" })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "either", value: ({ tag: "right", value: ({ tag: "maybe", value: null }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "outer description overrides inner #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "outer" }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "inner" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "description" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "outer" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "outer description overrides inner #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "new" }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "old" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "description" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "new" }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unset description #1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "desc" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 137n }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 137n }) }) })
  })
  }) })
  })
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unset description #2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: null })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.setTermDescription" }),
    argument: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "to be removed" }) }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  })
  }) })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "layered annotations",
    description: null,
    subgroups: [],
    cases: [({
    name: "get annotation from unannotated term",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get annotation from singly annotated term",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get inner annotation from doubly annotated term",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "two" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get outer annotation from doubly annotated term",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "two" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "two" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "get non-overridden annotation from triply annotated term",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "two" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "two" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "outer annotation overrides inner in layered term",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.annotations.getTermAnnotation" }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) })
  }) }),
    argument: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "two" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.singleton(({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })
  })
  }) })
  })
  }) })
  })
  }) })) })
  })]
  }) })
  })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })
  })
  }) })
  })
  }) })
  })
  }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

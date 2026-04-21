// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for term reduction/evaluation mechanics
 */



import * as Core from "../core.js";
import * as Inference from "../inference.js";
import * as LibEithers from "../lib/eithers.js";
import * as Reduction from "../reduction.js";
import * as ShowCore from "../show/core.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";

export const allTests: Testing.TestGroup = ({
    name: "reduction",
    description: null,
    subgroups: [({
    name: "beta reduction",
    description: null,
    subgroups: [],
    cases: [({
    name: "identity function applied to literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "constant function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "monomorphic primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "toUpper on lowercase",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toUpper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "HELLO" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "toUpper on mixed case",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toUpper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "Hello World" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "HELLO WORLD" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "toUpper on empty string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toUpper" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "toLower on uppercase",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toLower" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "HELLO" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string length",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string length of empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.length" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "add two positive integers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 8 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "add negative and positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -7 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "add with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "subtract integers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiply integers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiply by zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "divide integers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.div" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "modulo",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 17 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "splitOn basic",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a,b,c" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cat2 strings",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "world" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "helloworld" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "polymorphic primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "length of integer list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "length of string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "length of empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "length of single element list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "boolean", value: true }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "head of integer list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "head of string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "first" }) }), ({ tag: "literal", value: ({ tag: "string", value: "second" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "first" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "last of integer list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.last" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "concat two integer lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "concat with empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "reverse integer list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.reverse" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "reverse empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.reverse" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "nullary primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty set has size zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.size" }),
    argument: ({ tag: "variable", value: "hydra.lib.sets.empty" })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "literals as values",
    description: null,
    subgroups: [],
    cases: [({
    name: "integer literal is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative integer literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -17 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -17 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero integer literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string literal is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "string", value: "" }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with special characters",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "string", value: "hello\nworld\ttab" }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello\nworld\ttab" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "boolean true is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "boolean false is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float literal is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative float literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.718 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.718 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero float literal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "list reduction",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty list is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "list", value: [] }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list of literals is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list with reducible element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "list", value: [({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "optional reduction",
    description: null,
    subgroups: [],
    cases: [({
    name: "nothing is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "maybe", value: null }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "just literal is a value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "just with reducible content",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "maybe", value: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "alpha conversion",
    description: null,
    subgroups: [],
    cases: [({
    name: "variable at top level",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("y")(({ tag: "variable", value: "x" }))),
    expected: ShowCore.term(({ tag: "variable", value: "y" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable in list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("y")(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "variable", value: "x" })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "variable", value: "y" })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with different variable is transparent",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("y")(({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "variable", value: "x" }), ({ tag: "variable", value: "z" })] })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "variable", value: "y" }), ({ tag: "variable", value: "z" })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with same variable is opaque",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("y")(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "variable", value: "x" }), ({ tag: "variable", value: "z" })] })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "variable", value: "x" }), ({ tag: "variable", value: "z" })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested lambda outer variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("y")(({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested lambda shadows outer",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("z")(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "application with variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("y")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "y" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "application with both variables same",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Reduction.alphaConvert("x")("y")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "x" }),
    argument: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "y" }),
    argument: ({ tag: "variable", value: "y" })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "type reduction",
    description: null,
    subgroups: [],
    cases: [({
    name: "unit type unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "unit" }))),
    expected: ShowCore.type(({ tag: "unit" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string type unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "string" }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "string" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int32 type unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "identity type applied to string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "forall", value: ({
    parameter: "t",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "variable", value: "t" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "constant type ignores argument",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "forall", value: ({
    parameter: "x",
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested forall first application",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "forall", value: ({
    parameter: "x",
    body: ({ tag: "forall", value: ({
    parameter: "y",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "x" }),
    codomain: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "forall", value: ({
    parameter: "y",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "variable", value: "y" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested forall both applications",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "forall", value: ({
    parameter: "x",
    body: ({ tag: "forall", value: ({
    parameter: "y",
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "x" }),
    codomain: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list type applied",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "list", value: ({ tag: "variable", value: "a" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }))),
    expected: ShowCore.type(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "optional type applied",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<type reduction error>>"))(((t: Core.Type) => ShowCore.type(t)))(Reduction.betaReduceType(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "a" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }))),
    expected: ShowCore.type(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "etaExpandTerm",
    description: null,
    subgroups: [],
    cases: [({
    name: "integer literal unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fully applied binary function unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with fully applied primitive unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
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
    name: "lambda returning constant unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bare unary primitive unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.strings.toLower" }))),
    expected: ShowCore.term(({ tag: "variable", value: "hydra.lib.strings.toLower" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bare binary primitive unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "variable", value: "hydra.lib.strings.splitOn" }))),
    expected: ShowCore.term(({ tag: "variable", value: "hydra.lib.strings.splitOn" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "partially applied binary primitive expands to one lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "variable", value: "foo" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "variable", value: "foo" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "projection expands to lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "project", value: ({
    typeName: "Person",
    field: "firstName"
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "Person",
    field: "firstName"
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "partial application inside lambda expands",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "variable", value: "x" })
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
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let with constant body unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let with bare primitive value unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    type: null
  })],
    body: ({ tag: "variable", value: "foo" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "foo",
    term: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    type: null
  })],
    body: ({ tag: "variable", value: "foo" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fully applied unary unchanged",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "application", value: ({
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
    name: "partial application in list expands",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((_: hydra.errors.Error) => "eta expansion failed"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.etaExpandTypedTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] })
  }) }), ({ tag: "lambda", value: ({
    parameter: "v1",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.splitOn" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

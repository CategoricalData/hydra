// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.lists primitives
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
import * as JsonModel from "../../json/model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Reduction from "../../reduction.js";
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
    name: "hydra.lib.lists primitives",
    description: null,
    subgroups: [({
    name: "apply",
    description: null,
    subgroups: [({
    name: "string transformations",
    description: null,
    subgroups: [],
    cases: [({
    name: "string transformations",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.apply" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "hydra.lib.strings.toUpper" }), ({ tag: "variable", value: "hydra.lib.strings.toLower" })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "One" }) }), ({ tag: "literal", value: ({ tag: "string", value: "Two" }) }), ({ tag: "literal", value: ({ tag: "string", value: "Three" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "ONE" }) }), ({ tag: "literal", value: ({ tag: "string", value: "TWO" }) }), ({ tag: "literal", value: ({ tag: "string", value: "THREE" }) }), ({ tag: "literal", value: ({ tag: "string", value: "one" }) }), ({ tag: "literal", value: ({ tag: "string", value: "two" }) }), ({ tag: "literal", value: ({ tag: "string", value: "three" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "edge cases",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty function list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.apply" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty input list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.apply" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "hydra.lib.strings.toUpper" })] })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.apply" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "hydra.lib.strings.toUpper" })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "HELLO" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single input",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.apply" }),
    argument: ({ tag: "list", value: [({ tag: "variable", value: "hydra.lib.strings.toUpper" }), ({ tag: "variable", value: "hydra.lib.strings.toLower" })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "Test" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "TEST" }) }), ({ tag: "literal", value: ({ tag: "string", value: "test" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  }), ({
    name: "at",
    description: null,
    subgroups: [],
    cases: [({
    name: "first element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.at" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "middle element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.at" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "last element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.at" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.at" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list access",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.at" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "world" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bind",
    description: null,
    subgroups: [],
    cases: [({
    name: "negation function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.bind" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.bind" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.bind" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.bind" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "concat",
    description: null,
    subgroups: [],
    cases: [({
    name: "multiple non-empty lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 8 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 8 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty lists included",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all empty lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] }), ({ tag: "list", value: [] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list of lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "concat2",
    description: null,
    subgroups: [],
    cases: [({
    name: "two non-empty lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
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
    name: "first list empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
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
    name: "second list empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "both lists empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.concat2" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "literal", value: ({ tag: "string", value: "d" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "literal", value: ({ tag: "string", value: "d" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "cons",
    description: null,
    subgroups: [],
    cases: [({
    name: "cons to non-empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cons to empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cons negative number",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cons string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.cons" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "world" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "drop",
    description: null,
    subgroups: [],
    cases: [({
    name: "drop from beginning",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.drop" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "drop zero elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.drop" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "drop all elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.drop" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "drop more than length",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.drop" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "drop from empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.drop" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "drop negative amount",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.drop" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "dropWhile",
    description: null,
    subgroups: [],
    cases: [({
    name: "drop while less than 3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.dropWhile" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "drop all elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.dropWhile" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "drop no elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.dropWhile" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.dropWhile" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "elem",
    description: null,
    subgroups: [],
    cases: [({
    name: "element present",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "element not present",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element present",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element not present",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string element present",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "world" }) }), ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "test" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string element not present",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.elem" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "missing" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "world" }) }), ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "filter",
    description: null,
    subgroups: [],
    cases: [({
    name: "filter positive numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.filter" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "filter all elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.filter" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "filter no elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.filter" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.filter" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "find",
    description: null,
    subgroups: [],
    cases: [({
    name: "find existing element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.find" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "find first matching",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.find" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "find no match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.find" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "find in empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.find" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "find single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.find" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.equal" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "foldl",
    description: null,
    subgroups: [],
    cases: [({
    name: "sum with addition",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "product with multiplication",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.mul" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 24 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 15 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "subtraction fold",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldl" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.sub" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "foldr",
    description: null,
    subgroups: [],
    cases: [({
    name: "subtraction fold right",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldr" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.sub" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldr" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldr" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 15 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sum with addition",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldr" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "subtraction vs foldl",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.foldr" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.sub" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -8 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "group",
    description: null,
    subgroups: [],
    cases: [({
    name: "consecutive duplicates",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.group" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no duplicates",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.group" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all same",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.group" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.group" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.group" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "head",
    description: null,
    subgroups: [],
    cases: [({
    name: "three element list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "init",
    description: null,
    subgroups: [],
    cases: [({
    name: "multiple elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.init" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.init" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.init" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.init" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "intercalate",
    description: null,
    subgroups: [],
    cases: [({
    name: "double zero separator",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intercalate" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 8 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 8 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty separator",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intercalate" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element separator",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intercalate" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list of lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intercalate" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intercalate" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lists with empty lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intercalate" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "list", value: [] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "intersperse",
    description: null,
    subgroups: [],
    cases: [({
    name: "string interspersion",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intersperse" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "and" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "one" }) }), ({ tag: "literal", value: ({ tag: "string", value: "two" }) }), ({ tag: "literal", value: ({ tag: "string", value: "three" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "one" }) }), ({ tag: "literal", value: ({ tag: "string", value: "and" }) }), ({ tag: "literal", value: ({ tag: "string", value: "two" }) }), ({ tag: "literal", value: ({ tag: "string", value: "and" }) }), ({ tag: "literal", value: ({ tag: "string", value: "three" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intersperse" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "only" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "only" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intersperse" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "x" }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intersperse" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "+" }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "+" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "number interspersion",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.intersperse" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "last",
    description: null,
    subgroups: [],
    cases: [({
    name: "three element list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.last" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.last" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.last" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.last" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "world" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "length",
    description: null,
    subgroups: [],
    cases: [({
    name: "three elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "many elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 8 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 9 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.length" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "map",
    description: null,
    subgroups: [],
    cases: [({
    name: "string to uppercase",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.toUpper" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "one" }) }), ({ tag: "literal", value: ({ tag: "string", value: "two" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "ONE" }) }), ({ tag: "literal", value: ({ tag: "string", value: "TWO" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.toUpper" })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.toUpper" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "HELLO" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "number negation",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "identity function",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: ({ tag: "variable", value: "hydra.lib.equality.identity" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeAt",
    description: null,
    subgroups: [],
    cases: [({
    name: "valid index",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeAt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "first element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeAt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "last element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeAt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "out of bounds",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeAt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative index",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeAt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeAt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeHead",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-empty int list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeHead" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty int list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeHead" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeHead" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "non-empty string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeHead" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeHead" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeInit",
    description: null,
    subgroups: [],
    cases: [({
    name: "three elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeInit" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeInit" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "list", value: [] }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeInit" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeLast",
    description: null,
    subgroups: [],
    cases: [({
    name: "three elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeLast" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeLast" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeLast" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeTail",
    description: null,
    subgroups: [],
    cases: [({
    name: "three elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeTail" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeTail" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "list", value: [] }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.maybeTail" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "nub",
    description: null,
    subgroups: [],
    cases: [({
    name: "remove duplicates",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.nub" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no duplicates",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.nub" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all duplicates",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.nub" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.nub" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.nub" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string duplicates",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.nub" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "null",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty int list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.null" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.null" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.null" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.null" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "non-empty string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.null" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "partition",
    description: null,
    subgroups: [],
    cases: [({
    name: "partition greater than 3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.partition" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "partition all elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.partition" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }), ({ tag: "list", value: [] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "partition no elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.partition" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "partition even numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.partition" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.even" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.partition" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "pure",
    description: null,
    subgroups: [],
    cases: [({
    name: "string element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "one" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "one" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "number element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative number",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "replicate",
    description: null,
    subgroups: [],
    cases: [({
    name: "replicate three times",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.replicate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "replicate zero times",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.replicate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "replicate once",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.replicate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 99 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "replicate string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.replicate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "reverse",
    description: null,
    subgroups: [],
    cases: [({
    name: "multiple elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.reverse" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.reverse" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.reverse" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.reverse" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.reverse" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "c" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "safeHead",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-empty int list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.safeHead" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty int list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.safeHead" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.safeHead" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "non-empty string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.safeHead" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.safeHead" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "singleton",
    description: null,
    subgroups: [],
    cases: [({
    name: "number element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.singleton" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative number",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.singleton" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.singleton" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.singleton" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "sort",
    description: null,
    subgroups: [],
    cases: [({
    name: "unsorted numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "already sorted",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "reverse sorted",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicates",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string sort",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sort" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "zebra" }) }), ({ tag: "literal", value: ({ tag: "string", value: "apple" }) }), ({ tag: "literal", value: ({ tag: "string", value: "banana" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "apple" }) }), ({ tag: "literal", value: ({ tag: "string", value: "banana" }) }), ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "sortOn",
    description: null,
    subgroups: [],
    cases: [({
    name: "sort by string length",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sortOn" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.length" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "hi" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hi" }) }), ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }), ({ tag: "literal", value: ({ tag: "string", value: "world" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sortOn" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.length" })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single string element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sortOn" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.length" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "test" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "test" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sort by negation",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sortOn" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.negate" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sort by absolute value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.sortOn" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.abs" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "span",
    description: null,
    subgroups: [],
    cases: [({
    name: "span less than 3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.span" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "span all elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.span" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }), ({ tag: "list", value: [] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "span no elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.span" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.span" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "list", value: [] }), ({ tag: "list", value: [] })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "tail",
    description: null,
    subgroups: [],
    cases: [({
    name: "multiple elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.tail" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.tail" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.tail" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.tail" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "take",
    description: null,
    subgroups: [],
    cases: [({
    name: "take from beginning",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.take" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "take zero elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.take" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "take all elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.take" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "take more than length",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.take" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "take from empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.take" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "take negative amount",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.take" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "transpose",
    description: null,
    subgroups: [],
    cases: [({
    name: "square matrix",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.transpose" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.transpose" }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single row",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.transpose" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single column",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.transpose" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ragged matrix",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.transpose" }),
    argument: ({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "zip",
    description: null,
    subgroups: [],
    cases: [({
    name: "equal length lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zip" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "first list shorter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zip" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second list shorter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zip" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "a" }) })] }), ({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty first list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zip" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty second list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zip" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "both empty lists",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zip" }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "zipWith",
    description: null,
    subgroups: [],
    cases: [({
    name: "addition",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zipWith" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 9 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "first list shorter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zipWith" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second list shorter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zipWith" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty first list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zipWith" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty second list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zipWith" }),
    argument: ({ tag: "variable", value: "hydra.lib.math.add" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })
  }) }),
    argument: ({ tag: "list", value: [] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string concatenation",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.zipWith" }),
    argument: ({ tag: "variable", value: "hydra.lib.strings.cat2" })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] })
  }) }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "1" }) }), ({ tag: "literal", value: ({ tag: "string", value: "2" }) })] })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a1" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b2" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

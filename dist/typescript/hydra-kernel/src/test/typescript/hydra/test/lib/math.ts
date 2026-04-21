// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.math primitives
 */



import * as Core from "../../core.js";
import * as LibEithers from "../../lib/eithers.js";
import * as Reduction from "../../reduction.js";
import * as ShowCore from "../../show/core.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";

export const allTests: Testing.TestGroup = ({
    name: "hydra.lib.math primitives",
    description: null,
    subgroups: [({
    name: "abs",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.abs" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.abs" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.abs" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "add",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive numbers",
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
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -8 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed sign",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "div",
    description: null,
    subgroups: [],
    cases: [({
    name: "exact division",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.div" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "truncates toward negative infinity",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.div" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative dividend",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.div" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative divisor",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.div" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "even",
    description: null,
    subgroups: [],
    cases: [({
    name: "even positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.even" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "odd positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.even" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "even negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.even" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "odd negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.even" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.even" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "max",
    description: null,
    subgroups: [],
    cases: [({
    name: "first is larger",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second is larger",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal values",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed sign",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeDiv",
    description: null,
    subgroups: [],
    cases: [({
    name: "basic division",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeDiv" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exact division",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeDiv" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "division by zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeDiv" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero divided",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeDiv" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative dividend",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeDiv" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative divisor",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeDiv" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "min",
    description: null,
    subgroups: [],
    cases: [({
    name: "first is smaller",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second is smaller",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal values",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 7 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed sign",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeMod",
    description: null,
    subgroups: [],
    cases: [({
    name: "basic modulo",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeMod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exact division",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeMod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "division by zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeMod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative dividend",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeMod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative divisor",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeMod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "mod",
    description: null,
    subgroups: [],
    cases: [({
    name: "basic modulo",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exact division",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative dividend",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative divisor",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mod" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "mul",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 15 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 15 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed sign",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -15 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with one",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "negate",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negate" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "odd",
    description: null,
    subgroups: [],
    cases: [({
    name: "odd positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.odd" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "even positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.odd" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "odd negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.odd" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "even negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.odd" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.odd" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybePred",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybePred" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybePred" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybePred" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -6 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "minBound",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybePred" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2147483648 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "pred",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pred" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pred" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pred" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -6 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "range",
    description: null,
    subgroups: [],
    cases: [({
    name: "ascending range",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.range" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.range" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.range" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative start",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.range" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeRem",
    description: null,
    subgroups: [],
    cases: [({
    name: "basic remainder",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeRem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exact division",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeRem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "division by zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeRem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative dividend",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeRem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative divisor",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeRem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "rem",
    description: null,
    subgroups: [],
    cases: [({
    name: "basic remainder",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.rem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exact division",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.rem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative dividend",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.rem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative divisor",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.rem" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "signum",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.signum" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.signum" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.signum" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "sub",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive numbers",
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
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -7 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed sign",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 13 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybeSucc",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeSucc" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeSucc" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeSucc" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "maxBound",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.maybeSucc" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2147483647 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "succ",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.succ" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 6 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.succ" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.succ" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -4 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "addFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 8.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -8.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed sign",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 7.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fractional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "mulFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 15.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 15.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed sign",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -30.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with one",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fractional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "negateFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negateFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -5.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negateFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negateFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fractional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negateFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.5 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "subFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative result",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative numbers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "same value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "fractional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "e",
    description: null,
    subgroups: [],
    cases: [({
    name: "Euler's number",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.math.e" })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.71828182846 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "pi",
    description: null,
    subgroups: [],
    cases: [({
    name: "pi constant",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.math.pi" })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159265359 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "sin",
    description: null,
    subgroups: [],
    cases: [({
    name: "sin 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin pi/2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5707963267948966 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin pi",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.141592653589793 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.22464679915e-16 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.841470984808 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.479425538604 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "cos",
    description: null,
    subgroups: [],
    cases: [({
    name: "cos 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos pi/2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5707963267948966 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 6.12323399574e-17 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos pi",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.141592653589793 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.540302305868 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.87758256189 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "tan",
    description: null,
    subgroups: [],
    cases: [({
    name: "tan 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tan pi/4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.7853981633974483 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tan 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.55740772465 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tan 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.546302489844 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tan NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tan +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tan -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "asin",
    description: null,
    subgroups: [],
    cases: [({
    name: "asin 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin -1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.523598775598 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin below domain",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin above domain",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asin -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asin" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "acos",
    description: null,
    subgroups: [],
    cases: [({
    name: "acos 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos -1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159265359 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0471975512 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos below domain",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos above domain",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acos -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acos" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "atan",
    description: null,
    subgroups: [],
    cases: [({
    name: "atan 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.785398163397 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.463647609001 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "atan2",
    description: null,
    subgroups: [],
    cases: [({
    name: "atan2 1 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.785398163397 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 1 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 0 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 3 4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.643501108793 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 NaN 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 +Inf 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 -Inf 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.57079632679 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 1 NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 1 +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 1 -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159265359 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 +Inf +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 +Inf -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 -Inf +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atan2 -Inf -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atan2" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "sinh",
    description: null,
    subgroups: [],
    cases: [({
    name: "sinh 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sinh 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.17520119364 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sinh 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.62686040785 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sinh NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sinh +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sinh -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "cosh",
    description: null,
    subgroups: [],
    cases: [({
    name: "cosh 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cosh 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.54308063482 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cosh 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.76219569108 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cosh NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cosh +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cosh -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "tanh",
    description: null,
    subgroups: [],
    cases: [({
    name: "tanh 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tanh 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.761594155956 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tanh 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.46211715726 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tanh NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tanh +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tanh -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.tanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "asinh",
    description: null,
    subgroups: [],
    cases: [({
    name: "asinh 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asinh 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.88137358702 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asinh 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.48121182506 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asinh NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asinh +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "asinh -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.asinh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "acosh",
    description: null,
    subgroups: [],
    cases: [({
    name: "acosh 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acosh 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.31695789692 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acosh 3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.76274717404 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acosh below domain",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acosh negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acosh NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acosh +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "acosh -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.acosh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "atanh",
    description: null,
    subgroups: [],
    cases: [({
    name: "atanh 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.549306144334 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh 0.1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.1 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.100335347731 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh upper boundary",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh lower boundary",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh above domain",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh below domain",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "atanh -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.atanh" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "exp",
    description: null,
    subgroups: [],
    cases: [({
    name: "exp 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.71828182846 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp -1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.367879441171 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 7.38905609893 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.6487212707 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "log",
    description: null,
    subgroups: [],
    cases: [({
    name: "log 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log e",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.718281828459045 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.69314718056 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log 10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.30258509299 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "logBase",
    description: null,
    subgroups: [],
    cases: [({
    name: "log10 1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log10 10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log10 100",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 100.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log2 8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 8.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log2 10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.32192809489 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase 10 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase 10 negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase negative 10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase 10 NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase 10 +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase 10 -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase NaN 10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase +Inf 10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "logBase -Inf 10",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.logBase" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "pow",
    description: null,
    subgroups: [],
    cases: [({
    name: "2^3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 8.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "10^0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "2^-1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "2^0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.41421356237 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "0^0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "0^-1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "(-1)^0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "NaN^2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "+Inf^2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "-Inf^2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "+Inf^-1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "2^NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "2^+Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "2^-Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "sqrt",
    description: null,
    subgroups: [],
    cases: [({
    name: "sqrt 4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt 9",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 9.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.4142135623730951 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt 0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt 3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.73205080757 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sqrt -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "ceiling",
    description: null,
    subgroups: [],
    cases: [({
    name: "ceiling 3.2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.ceiling" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ceiling 3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.ceiling" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ceiling -3.2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.ceiling" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ceiling -3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.ceiling" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ceiling NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.ceiling" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ceiling +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.ceiling" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "ceiling -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.ceiling" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "floor",
    description: null,
    subgroups: [],
    cases: [({
    name: "floor 3.8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.floor" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.8 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "floor 3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.floor" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "floor -3.2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.floor" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -4.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "floor -3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.floor" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "floor NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.floor" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "floor +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.floor" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "floor -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.floor" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "round",
    description: null,
    subgroups: [],
    cases: [({
    name: "round 3.4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 3.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 3.6",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.6 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round -3.4",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.4 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round -3.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -4.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.round" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "roundBigfloat",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round pi to 4 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.141592653589793 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.142 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 1234.5 to 3 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 1234.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 1230.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 0.001234 to 2 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 1.234e-3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 1.2e-3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -1234.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -1230.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "roundFloat32",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round pi to 4 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.1415927410125732 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.1419999599456787 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 1234.5 to 3 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 1234.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 1230.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -1234.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -1230.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "+Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "-Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "roundFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round pi to 4 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 4 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.141592653589793 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.142 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round pi to 10 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.141592653589793 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.141592654 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 1234.5 to 3 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1234.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1230.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 0.001234 to 2 digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.234e-3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.2e-3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1234.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1230.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round 1 digit",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 9.876 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "+Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "-Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "truncate",
    description: null,
    subgroups: [],
    cases: [({
    name: "truncate 3.8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.truncate" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.8 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "truncate 3.2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.truncate" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "truncate -3.8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.truncate" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.8 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "truncate -3.2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.truncate" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "truncate NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.truncate" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "truncate +Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.truncate" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "truncate -Inf",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.truncate" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

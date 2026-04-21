// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.equality primitives
 */



import * as Core from "../../core.js";
import * as LibEithers from "../../lib/eithers.js";
import * as Reduction from "../../reduction.js";
import * as ShowCore from "../../show/core.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";

export const allTests: Testing.TestGroup = ({
    name: "hydra.lib.equality primitives",
    description: null,
    subgroups: [({
    name: "compare",
    description: null,
    subgroups: [],
    cases: [({
    name: "less than",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "equalTo",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greater than",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "greaterThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "equal",
    description: null,
    subgroups: [],
    cases: [({
    name: "equal integers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.equal" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unequal integers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.equal" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "gt",
    description: null,
    subgroups: [],
    cases: [({
    name: "greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "gte",
    description: null,
    subgroups: [],
    cases: [({
    name: "greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gte" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gte" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gte" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "identity",
    description: null,
    subgroups: [],
    cases: [({
    name: "integer",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.identity" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lt",
    description: null,
    subgroups: [],
    cases: [({
    name: "less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lte",
    description: null,
    subgroups: [],
    cases: [({
    name: "less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lte" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lte" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lte" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "max",
    description: null,
    subgroups: [],
    cases: [({
    name: "first greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.max" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "min",
    description: null,
    subgroups: [],
    cases: [({
    name: "first less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.min" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "compare strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "less than (lexicographic)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "banana" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "equalTo",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greater than (lexicographic)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "greaterThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty vs non-empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "prefix vs longer",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "ab" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lt strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "less (lexicographic)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "banana" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "gt strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "greater (lexicographic)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "banana" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "max strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "first greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.max" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "zebra" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.max" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "zebra" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.max" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "min strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "first less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.min" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "apple" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.min" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "zebra" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "apple" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "apple" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.min" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "compare floats",
    description: null,
    subgroups: [],
    cases: [({
    name: "less than",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "equalTo",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greater than",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "greaterThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative vs positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.compare" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "inject", value: ({
    typeName: "hydra.util.Comparison",
    field: ({
    name: "lessThan",
    term: ({ tag: "unit" })
  })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lt floats",
    description: null,
    subgroups: [],
    cases: [({
    name: "less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "gt floats",
    description: null,
    subgroups: [],
    cases: [({
    name: "greater",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "equal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "less",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gt" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

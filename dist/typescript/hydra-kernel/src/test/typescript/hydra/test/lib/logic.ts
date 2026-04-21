// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.logic primitives
 */



import * as Core from "../../core.js";
import * as LibEithers from "../../lib/eithers.js";
import * as Reduction from "../../reduction.js";
import * as ShowCore from "../../show/core.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";

export const allTests: Testing.TestGroup = ({
    name: "hydra.lib.logic primitives",
    description: null,
    subgroups: [({
    name: "and",
    description: null,
    subgroups: [],
    cases: [({
    name: "true and true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.and" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "true and false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.and" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false and true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.and" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false and false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.and" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "ifElse",
    description: null,
    subgroups: [({
    name: "boolean values",
    description: null,
    subgroups: [],
    cases: [({
    name: "true condition returns then",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false condition returns else",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "integer values",
    description: null,
    subgroups: [],
    cases: [({
    name: "true selects first int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false selects second int",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "string values",
    description: null,
    subgroups: [],
    cases: [({
    name: "true selects first string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "yes" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "no" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "yes" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false selects second string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "yes" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "no" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "no" }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  }), ({
    name: "not",
    description: null,
    subgroups: [],
    cases: [({
    name: "not true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.not" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "not false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.not" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "or",
    description: null,
    subgroups: [],
    cases: [({
    name: "true or true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.or" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "true or false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.or" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false or true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.or" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false or false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.or" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

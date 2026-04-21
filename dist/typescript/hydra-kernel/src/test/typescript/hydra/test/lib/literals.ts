// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.literals primitives
 */



import * as Core from "../../core.js";
import * as LibEithers from "../../lib/eithers.js";
import * as Reduction from "../../reduction.js";
import * as ShowCore from "../../show/core.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";

export const allTests: Testing.TestGroup = ({
    name: "hydra.lib.literals primitives",
    description: null,
    subgroups: [({
    name: "bigintToInt8",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -42 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToInt16",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 1000n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -1000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: -1000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToInt32",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToInt64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 1000000n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToInt64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: -1000000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToUint8",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 100n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 100n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToUint16",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 1000 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToUint32",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 100000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 100000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToUint64",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 1000000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "int8ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int8ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int8ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -42n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "max value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int8ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 127 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 127n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "min value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int8ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -128 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -128n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "int16ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int16ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 1000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int16ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: -1000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -1000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "int32ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int32ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int32ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -42n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int32ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "int64ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int64ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000000n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.int64ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: -1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -1000000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "uint8ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint8ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "max value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint8ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 255n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 255n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "uint16ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint16ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint16ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 1000 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "uint32ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint32ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint32ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 100000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 100000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "uint64ToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint64ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.uint64ToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 1000000n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "float32ToBigfloat",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.float32ToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 2.5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.float32ToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -2.5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.float32ToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "float64ToBigfloat",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.float64ToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.14159 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.float64ToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.71828 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -2.71828 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.float64ToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigfloatToFloat32",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.14 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.140000104904175 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -2.5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigfloatToFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.14159 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -2.71828 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.71828 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigintToBigfloat",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 42.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -42.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigintToBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bigfloatToBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 42.7 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 43n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: -42.7 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -43n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "round down",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 42.3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "half even up",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 42.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "half even down",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.bigfloatToBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 43.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 44n }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showInt8",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "42" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "-42" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showInt16",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 1000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "1000" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: -1000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "-1000" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showInt32",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "42" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "-42" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showInt64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "1000000" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: -1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "-1000000" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showUint8",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "max value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 255n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "255" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showUint16",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 1000 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "1000" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showUint32",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 100000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "100000" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showUint64",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 1000000n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "1000000" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "42" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -42n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "-42" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showFloat32",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.140000104904175 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "3.14" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -2.5 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "-2.5" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0.0" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "small positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 5.000000074505806e-2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "5.0e-2" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "small positive 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 2.9999999329447746e-2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "3.0e-2" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "very small",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 1.0000000474974513e-3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "1.0e-3" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "normal decimal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 0.10000000149011612 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0.1" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "3.14159" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0.0" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "small positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0e-2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "5.0e-2" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "small positive 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0e-2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "3.0e-2" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "very small",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0e-3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "1.0e-3" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "normal decimal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.1 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0.1" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showBigfloat",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.14 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "3.14" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0.0" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "small positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 5.0e-2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "5.0e-2" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "small positive 2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.0e-2 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "3.0e-2" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "very small",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 1.0e-3 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "1.0e-3" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "normal decimal",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 0.1 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "0.1" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showBoolean",
    description: null,
    subgroups: [],
    cases: [({
    name: "true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBoolean" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "true" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showBoolean" }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "false" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "showString",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"hello\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "latin accented",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "café" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"caf\\233\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "greek lambda",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "λ" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\955\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed ascii and non-ascii",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "AéB" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"A\\233B\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "tab",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "\t" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\t\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "newline",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "\n" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\n\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "carriage return",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "\r" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\r\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "backslash",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "\\" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\\\\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "double quote",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "\"" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\\"\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "null",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: " " }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\NUL\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bell",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\a\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "backspace",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\b\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "form feed",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\f\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "vertical tab",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\v\"" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "delete",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "\"\\DEL\"" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readInt8",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "42" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-42" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "max value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "127" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 127 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "min value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-128" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -128 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "overflow",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "128" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readInt16",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt16" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "1000" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 1000n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt16" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-1000" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: -1000n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt16" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readInt32",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "42" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-42" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: -42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readInt64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "1000000" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 1000000n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-1000000" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: -1000000n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readInt64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readUint8",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "0" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 0n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "100" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 100n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "max value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "255" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 255n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint8" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-1" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readUint16",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "0" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 0 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "1000" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 1000 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint16" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-1" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readUint32",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "0" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 0n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "100000" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 100000n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-1" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readUint64",
    description: null,
    subgroups: [],
    cases: [({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "0" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 0n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "typical",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "1000000" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: 1000000n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readUint64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-1" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readBigint",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "42" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 42n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-42" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: -42n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "0" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 0n }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "large",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "123456789012345678901234567890" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: 123456789012345678901234567890n }) }) }) }))
  }) }),
    description: null,
    tags: ["disabled"]
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBigint" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readFloat32",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "3.14" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 3.140000104904175 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-2.5" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -2.5 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readFloat32" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readFloat64",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "3.14159" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "-2.71828" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -2.71828 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readBigfloat",
    description: null,
    subgroups: [],
    cases: [({
    name: "positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "3.14" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: 3.14 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBigfloat" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readBoolean",
    description: null,
    subgroups: [],
    cases: [({
    name: "true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBoolean" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "true" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "boolean", value: true }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBoolean" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "false" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "boolean", value: false }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "invalid",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readBoolean" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "yes" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "readString",
    description: null,
    subgroups: [],
    cases: [({
    name: "quoted string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "\"hello\"" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty quoted",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "\"\"" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unquoted",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.readString" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "stringToBinary",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple base64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.stringToBinary" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "aGVsbG8=" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "binary", value: new Uint8Array() }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.stringToBinary" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "binary", value: new Uint8Array() }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "binaryToString",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple binary",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.binaryToString" }),
    argument: ({ tag: "literal", value: ({ tag: "binary", value: new Uint8Array() }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "aGVsbG8=" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty binary",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: hydra.errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.binaryToString" }),
    argument: ({ tag: "literal", value: ({ tag: "binary", value: new Uint8Array() }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "" }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

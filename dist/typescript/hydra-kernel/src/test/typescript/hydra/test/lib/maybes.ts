// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.maybes primitives
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
import * as LibEquality from "../../lib/equality.js";
import * as LibLiterals from "../../lib/literals.js";
import * as LibLogic from "../../lib/logic.js";
import * as LibMath from "../../lib/math.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Reduction from "../../reduction.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "hydra.lib.maybes primitives",
    description: null,
    subgroups: [({
    name: "apply",
    description: null,
    subgroups: [],
    cases: [({
    name: "both just",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.apply(((x: number) => LibMath.add(3)(x)))(5)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(8)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing function",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.apply(null)(5)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.apply(((x: number) => LibMath.add(3)(x)))(null)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(null)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bind",
    description: null,
    subgroups: [],
    cases: [({
    name: "just to just",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.bind(5)(((x: number) => LibMath.mul(x)(2)))),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(10)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing to nothing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.bind(null)(((x: number) => LibMath.mul(x)(2)))),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(null)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "cases",
    description: null,
    subgroups: [],
    cases: [({
    name: "just applies function",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaybes.cases(5)(0)(((x: number) => LibMath.mul(x)(2)))),
    expected: LibLiterals.showInt32(10)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing returns default",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaybes.cases(null)(99)(((x: number) => LibMath.mul(x)(2)))),
    expected: LibLiterals.showInt32(99)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "cat",
    description: null,
    subgroups: [],
    cases: [({
    name: "filters nothings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.cat([1, null, 2])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all justs",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.cat([1, 2])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all nothings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.cat([null, null])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.cat([])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "compose",
    description: null,
    subgroups: [],
    cases: [({
    name: "both succeed",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.compose(((x: number) => LibLogic.ifElse(LibEquality.lte(x)(5))(LibMath.add(x)(1))(null)))(((y: number) => LibLogic.ifElse(LibEquality.gte(y)(5))(LibMath.mul(y)(2))(null)))(5)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(12)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "first fails",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.compose(((x: number) => LibLogic.ifElse(LibEquality.lte(x)(5))(LibMath.add(x)(1))(null)))(((y: number) => LibLogic.ifElse(LibEquality.gte(y)(5))(LibMath.mul(y)(2))(null)))(10)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "second fails",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.compose(((x: number) => LibLogic.ifElse(LibEquality.lte(x)(5))(LibMath.add(x)(1))(null)))(((y: number) => LibLogic.ifElse(LibEquality.gte(y)(5))(LibMath.mul(y)(2))(null)))(3)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(null)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "fromJust",
    description: null,
    subgroups: [],
    cases: [({
    name: "extract from just",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaybes.fromJust(42)),
    expected: LibLiterals.showInt32(42)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "fromMaybe",
    description: null,
    subgroups: [],
    cases: [({
    name: "just value",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaybes.fromMaybe(0)(42)),
    expected: LibLiterals.showInt32(42)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing with default",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaybes.fromMaybe(99)(null)),
    expected: LibLiterals.showInt32(99)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "isJust",
    description: null,
    subgroups: [],
    cases: [({
    name: "just value",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaybes.isJust(42)),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaybes.isJust(null)),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "isNothing",
    description: null,
    subgroups: [],
    cases: [({
    name: "just value",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaybes.isNothing(42)),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaybes.isNothing(null)),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "map",
    description: null,
    subgroups: [],
    cases: [({
    name: "maps just value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.map(((x: number) => LibMath.mul(x)(2)))(5)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(10)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.map(((x: number) => LibMath.mul(x)(2)))(null)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(null)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "mapMaybe",
    description: null,
    subgroups: [],
    cases: [({
    name: "filter and transform",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.mapMaybe(((x: number) => LibLogic.ifElse(LibEquality.gt(x)(2))(LibMath.mul(x)(2))(null)))([1, 2, 3, 4, 5])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([6, 8, 10])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty result",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.mapMaybe(((x: number) => LibLogic.ifElse(LibEquality.gt(x)(2))(LibMath.mul(x)(2))(null)))([1, 2])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty input",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.mapMaybe(((x: number) => LibLogic.ifElse(LibEquality.gt(x)(2))(LibMath.mul(x)(2))(null)))([])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "maybe",
    description: null,
    subgroups: [],
    cases: [({
    name: "just value applies function",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaybes.maybe(0)(((x: number) => LibMath.mul(x)(2)))(5)),
    expected: LibLiterals.showInt32(10)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing returns default",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaybes.maybe(99)(((x: number) => LibMath.mul(x)(2)))(null)),
    expected: LibLiterals.showInt32(99)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "pure",
    description: null,
    subgroups: [],
    cases: [({
    name: "wraps integer",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.pure(42)),
    expected: ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(42)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "wraps string",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))(LibMaybes.pure("hello")),
    expected: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))("hello")
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "toList",
    description: null,
    subgroups: [],
    cases: [({
    name: "just value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.toList(42)),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([42])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaybes.toList(null)),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.eithers primitives
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
import * as LibEquality from "../../lib/equality.js";
import * as LibLiterals from "../../lib/literals.js";
import * as LibLogic from "../../lib/logic.js";
import * as LibMath from "../../lib/math.js";
import * as LibStrings from "../../lib/strings.js";
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
    name: "hydra.lib.eithers primitives",
    description: null,
    subgroups: [({
    name: "bind",
    description: null,
    subgroups: [],
    cases: [({
    name: "bind Right with success",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(LibEithers.bind(({ tag: "right", value: "ab" }))(((s: string) => LibLogic.ifElse(LibStrings.null_(s))(({ tag: "left", value: 0 }))(({ tag: "right", value: LibStrings.length(s) }))))),
    expected: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(({ tag: "right", value: 2 }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bind Right with failure",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(LibEithers.bind(({ tag: "right", value: "" }))(((s: string) => LibLogic.ifElse(LibStrings.null_(s))(({ tag: "left", value: 0 }))(({ tag: "right", value: LibStrings.length(s) }))))),
    expected: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(({ tag: "left", value: 0 }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bind Left returns Left unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(LibEithers.bind(({ tag: "left", value: 42 }))(((s: string) => LibLogic.ifElse(LibStrings.null_(s))(({ tag: "left", value: 0 }))(({ tag: "right", value: LibStrings.length(s) }))))),
    expected: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(({ tag: "left", value: 42 }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bimap",
    description: null,
    subgroups: [],
    cases: [({
    name: "map left value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(LibEithers.bimap(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))(({ tag: "left", value: 5 }))),
    expected: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(({ tag: "left", value: 10 }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map right value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(LibEithers.bimap(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))(({ tag: "right", value: "ab" }))),
    expected: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(({ tag: "right", value: 2 }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "isLeft",
    description: null,
    subgroups: [],
    cases: [({
    name: "left value",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibEithers.isLeft(({ tag: "left", value: 42 }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "right value",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibEithers.isLeft(({ tag: "right", value: "test" }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "isRight",
    description: null,
    subgroups: [],
    cases: [({
    name: "right value",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibEithers.isRight(({ tag: "right", value: "test" }))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "left value",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibEithers.isRight(({ tag: "left", value: 42 }))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "fromLeft",
    description: null,
    subgroups: [],
    cases: [({
    name: "extract left",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibEithers.fromLeft(99)(({ tag: "left", value: 42 }))),
    expected: LibLiterals.showInt32(42)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "use default for right",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibEithers.fromLeft(99)(({ tag: "right", value: "test" }))),
    expected: LibLiterals.showInt32(99)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "fromRight",
    description: null,
    subgroups: [],
    cases: [({
    name: "extract right",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showString(LibEithers.fromRight("default")(({ tag: "right", value: "test" }))),
    expected: LibLiterals.showString("test")
  }) }),
    description: null,
    tags: []
  }), ({
    name: "use default for left",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showString(LibEithers.fromRight("default")(({ tag: "left", value: 42 }))),
    expected: LibLiterals.showString("default")
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "either",
    description: null,
    subgroups: [],
    cases: [({
    name: "apply left function",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibEithers.either(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))(({ tag: "left", value: 5 }))),
    expected: LibLiterals.showInt32(10)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "apply right function",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibEithers.either(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))(({ tag: "right", value: "ab" }))),
    expected: LibLiterals.showInt32(2)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lefts",
    description: null,
    subgroups: [],
    cases: [({
    name: "filter left values",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibEithers.lefts([({ tag: "left", value: 1 }), ({ tag: "right", value: "a" }), ({ tag: "left", value: 2 }), ({ tag: "right", value: "b" })])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all lefts",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibEithers.lefts([({ tag: "left", value: 1 }), ({ tag: "left", value: 2 })])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all rights",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibEithers.lefts([({ tag: "right", value: "a" }), ({ tag: "right", value: "b" })])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibEithers.lefts([])),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "rights",
    description: null,
    subgroups: [],
    cases: [({
    name: "filter right values",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((s: string) => LibLiterals.showString(s)))(LibEithers.rights([({ tag: "left", value: 1 }), ({ tag: "right", value: "a" }), ({ tag: "left", value: 2 }), ({ tag: "right", value: "b" })])),
    expected: ShowCore.list(((s: string) => LibLiterals.showString(s)))(["a", "b"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all rights",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((s: string) => LibLiterals.showString(s)))(LibEithers.rights([({ tag: "right", value: "a" }), ({ tag: "right", value: "b" })])),
    expected: ShowCore.list(((s: string) => LibLiterals.showString(s)))(["a", "b"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all lefts",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((s: string) => LibLiterals.showString(s)))(LibEithers.rights([({ tag: "left", value: 1 }), ({ tag: "left", value: 2 })])),
    expected: ShowCore.list(((s: string) => LibLiterals.showString(s)))([])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((s: string) => LibLiterals.showString(s)))(LibEithers.rights([])),
    expected: ShowCore.list(((s: string) => LibLiterals.showString(s)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "partitionEithers",
    description: null,
    subgroups: [],
    cases: [({
    name: "partition mixed",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))(LibEithers.partitionEithers([({ tag: "left", value: 1 }), ({ tag: "right", value: "a" }), ({ tag: "left", value: 2 }), ({ tag: "right", value: "b" })])),
    expected: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))([[1, 2], ["a", "b"]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all lefts",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))(LibEithers.partitionEithers([({ tag: "left", value: 1 }), ({ tag: "left", value: 2 })])),
    expected: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))([[1, 2], []])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "all rights",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))(LibEithers.partitionEithers([({ tag: "right", value: "a" }), ({ tag: "right", value: "b" })])),
    expected: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))([[], ["a", "b"]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))(LibEithers.partitionEithers([])),
    expected: ShowCore.pair(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(((xs: ReadonlyArray<string>) => ShowCore.list(((s: string) => LibLiterals.showString(s)))(xs)))([[], []])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "map",
    description: null,
    subgroups: [],
    cases: [({
    name: "map right value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(LibEithers.map(((x: number) => LibMath.mul(x)(2)))(({ tag: "right", value: 5 }))),
    expected: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(({ tag: "right", value: 10 }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "preserve left",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(LibEithers.map(((x: number) => LibMath.mul(x)(2)))(({ tag: "left", value: 99 }))),
    expected: ShowCore.either(((n: number) => LibLiterals.showInt32(n)))(((n: number) => LibLiterals.showInt32(n)))(({ tag: "left", value: 99 }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "mapList",
    description: null,
    subgroups: [],
    cases: [({
    name: "all succeed",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(LibEithers.mapList(((x: number) => LibLogic.ifElse(LibEquality.equal(x)(0))(({ tag: "left", value: "zero" }))(({ tag: "right", value: LibMath.mul(x)(2) }))))([1, 2, 3])),
    expected: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(({ tag: "right", value: [2, 4, 6] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "first fails",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(LibEithers.mapList(((x: number) => LibLogic.ifElse(LibEquality.equal(x)(0))(({ tag: "left", value: "zero" }))(({ tag: "right", value: LibMath.mul(x)(2) }))))([1, 0, 3])),
    expected: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(({ tag: "left", value: "zero" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(LibEithers.mapList(((x: number) => LibLogic.ifElse(LibEquality.equal(x)(0))(({ tag: "left", value: "zero" }))(({ tag: "right", value: LibMath.mul(x)(2) }))))([])),
    expected: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((xs: ReadonlyArray<number>) => ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(xs)))(({ tag: "right", value: [] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "mapMaybe",
    description: null,
    subgroups: [],
    cases: [({
    name: "just succeeds",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((mx: number | null) => ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(mx)))(LibEithers.mapMaybe(((x: number) => LibLogic.ifElse(LibEquality.equal(x)(0))(({ tag: "left", value: "zero" }))(({ tag: "right", value: LibMath.mul(x)(2) }))))(5)),
    expected: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((mx: number | null) => ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(mx)))(({ tag: "right", value: 10 }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "just fails",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((mx: number | null) => ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(mx)))(LibEithers.mapMaybe(((x: number) => LibLogic.ifElse(LibEquality.equal(x)(0))(({ tag: "left", value: "zero" }))(({ tag: "right", value: LibMath.mul(x)(2) }))))(0)),
    expected: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((mx: number | null) => ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(mx)))(({ tag: "left", value: "zero" }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nothing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((mx: number | null) => ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(mx)))(LibEithers.mapMaybe(((x: number) => LibLogic.ifElse(LibEquality.equal(x)(0))(({ tag: "left", value: "zero" }))(({ tag: "right", value: LibMath.mul(x)(2) }))))(null)),
    expected: ShowCore.either(((s: string) => LibLiterals.showString(s)))(((mx: number | null) => ShowCore.maybe(((n: number) => LibLiterals.showInt32(n)))(mx)))(({ tag: "right", value: null }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

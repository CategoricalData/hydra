// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.maps primitives
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
import * as LibChars from "../../lib/chars.js";
import * as LibEquality from "../../lib/equality.js";
import * as LibLiterals from "../../lib/literals.js";
import * as LibMaps from "../../lib/maps.js";
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
    name: "hydra.lib.maps primitives",
    description: null,
    subgroups: [({
    name: "alter",
    description: null,
    subgroups: [],
    cases: [({
    name: "insert new key",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.alter(((_: string | null) => "new"))(3)(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [2, "b"], [3, "new"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "update existing key",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.alter(((_: string | null) => "updated"))(2)(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [2, "updated"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "delete key",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.alter(((_: string | null) => null))(2)(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"]]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "bimap",
    description: null,
    subgroups: [],
    cases: [({
    name: "transform both",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.bimap(((k: number) => LibMath.mul(k)(2)))(((v: string) => LibStrings.toUpper(v)))(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[2, "A"], [4, "B"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.bimap(((k: number) => LibMath.mul(k)(2)))(((v: string) => LibStrings.toUpper(v)))(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "elems",
    description: null,
    subgroups: [],
    cases: [({
    name: "get all elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((s: string) => LibLiterals.showString(s)))(LibMaps.elems(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.list(((s: string) => LibLiterals.showString(s)))(["a", "b"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unsorted keys",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((s: string) => LibLiterals.showString(s)))(LibMaps.elems(new Map([[1, "a"], [2, "b"], [3, "c"]]))),
    expected: ShowCore.list(((s: string) => LibLiterals.showString(s)))(["a", "b", "c"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((s: string) => LibLiterals.showString(s)))(LibMaps.elems(new Map([]))),
    expected: ShowCore.list(((s: string) => LibLiterals.showString(s)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "empty",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.empty),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "filter",
    description: null,
    subgroups: [],
    cases: [({
    name: "filter values starting with a",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.filter(((v: string) => LibEquality.equal(LibChars.toLower(LibStrings.charAt(0)(v)))(97)))(new Map([[1, "a"], [2, "b"], [3, "ab"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [3, "ab"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "filter all",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.filter(((v: string) => LibEquality.equal(LibChars.toLower(LibStrings.charAt(0)(v)))(97)))(new Map([[1, "b"], [2, "c"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.filter(((v: string) => LibEquality.equal(LibChars.toLower(LibStrings.charAt(0)(v)))(97)))(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "filterWithKey",
    description: null,
    subgroups: [],
    cases: [({
    name: "filter by key > 1",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.filterWithKey(((k: number) => ((v: string) => LibEquality.gt(k)(1))))(new Map([[1, "a"], [2, "b"], [3, "c"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[2, "b"], [3, "c"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "filter all",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.filterWithKey(((k: number) => ((v: string) => LibEquality.gt(k)(1))))(new Map([[1, "a"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.filterWithKey(((k: number) => ((v: string) => LibEquality.gt(k)(1))))(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "findWithDefault",
    description: null,
    subgroups: [],
    cases: [({
    name: "find existing",
    case: ({ tag: "universal", value: ({
    actual: LibMaps.findWithDefault("default")(2)(new Map([[1, "a"], [2, "b"]])),
    expected: "b"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "use default",
    case: ({ tag: "universal", value: ({
    actual: LibMaps.findWithDefault("default")(3)(new Map([[1, "a"], [2, "b"]])),
    expected: "default"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "fromList",
    description: null,
    subgroups: [],
    cases: [({
    name: "create from pairs",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.fromList([[1, "a"], [2, "b"]])),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [2, "b"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate keys",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.fromList([[1, "a"], [1, "b"]])),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "b"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.fromList([])),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "insert",
    description: null,
    subgroups: [],
    cases: [({
    name: "insert new key",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.insert(3)("c")(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [2, "b"], [3, "c"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "update existing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.insert(2)("updated")(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [2, "updated"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "insert into empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.insert(1)("x")(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "x"]]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "keys",
    description: null,
    subgroups: [],
    cases: [({
    name: "get all keys",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaps.keys(new Map([[1, "a"], [2, "b"], [3, "c"]]))),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2, 3])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unsorted keys",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaps.keys(new Map([[1, "a"], [2, "b"], [3, "c"]]))),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2, 3])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibMaps.keys(new Map([]))),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lookup",
    description: null,
    subgroups: [],
    cases: [({
    name: "find existing key",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))(LibMaps.lookup(2)(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))("b")
  }) }),
    description: null,
    tags: []
  }), ({
    name: "key not found",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))(LibMaps.lookup(3)(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lookup in empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))(LibMaps.lookup(1)(new Map([]))),
    expected: ShowCore.maybe(((s: string) => LibLiterals.showString(s)))(null)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "map",
    description: null,
    subgroups: [],
    cases: [({
    name: "map over values",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.map(((s: string) => LibStrings.toUpper(s)))(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "A"], [2, "B"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.map(((s: string) => LibStrings.toUpper(s)))(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "mapKeys",
    description: null,
    subgroups: [],
    cases: [({
    name: "double keys",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.mapKeys(((k: number) => LibMath.mul(k)(2)))(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[2, "a"], [4, "b"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.mapKeys(((k: number) => LibMath.mul(k)(2)))(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "member",
    description: null,
    subgroups: [],
    cases: [({
    name: "key exists",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaps.member(2)(new Map([[1, "a"], [2, "b"]]))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "key missing",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaps.member(3)(new Map([[1, "a"], [2, "b"]]))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaps.member(1)(new Map([]))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "null",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaps.null_(new Map([]))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "non-empty map",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibMaps.null_(new Map([[1, "a"]]))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "remove",
    description: null,
    subgroups: [],
    cases: [({
    name: "remove existing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.delete_(2)(new Map([[1, "a"], [2, "b"], [3, "c"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [3, "c"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "remove non-existing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.delete_(4)(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [2, "b"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "remove from empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.delete_(1)(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "singleton",
    description: null,
    subgroups: [],
    cases: [({
    name: "single entry",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.singleton(42)("hello")),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[42, "hello"]]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "size",
    description: null,
    subgroups: [],
    cases: [({
    name: "three entries",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaps.size(new Map([[1, "a"], [2, "b"], [3, "c"]]))),
    expected: LibLiterals.showInt32(3)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single entry",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaps.size(new Map([[42, "test"]]))),
    expected: LibLiterals.showInt32(1)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibMaps.size(new Map([]))),
    expected: LibLiterals.showInt32(0)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "toList",
    description: null,
    subgroups: [],
    cases: [({
    name: "convert to pairs",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((p: readonly [number, string]) => ShowCore.pair(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(p)))(LibMaps.toList(new Map([[1, "a"], [2, "b"]]))),
    expected: ShowCore.list(((p: readonly [number, string]) => ShowCore.pair(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(p)))([[1, "a"], [2, "b"]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unsorted keys",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((p: readonly [number, string]) => ShowCore.pair(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(p)))(LibMaps.toList(new Map([[1, "a"], [2, "b"], [3, "c"]]))),
    expected: ShowCore.list(((p: readonly [number, string]) => ShowCore.pair(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(p)))([[1, "a"], [2, "b"], [3, "c"]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((p: readonly [number, string]) => ShowCore.pair(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(p)))(LibMaps.toList(new Map([]))),
    expected: ShowCore.list(((p: readonly [number, string]) => ShowCore.pair(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(p)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "union",
    description: null,
    subgroups: [],
    cases: [({
    name: "union two maps",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.union(new Map([[1, "a"], [2, "b"]]))(new Map([[2, "x"], [3, "c"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"], [2, "b"], [3, "c"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "union with empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.union(new Map([[1, "a"]]))(new Map([]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"]]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty with map",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(LibMaps.union(new Map([]))(new Map([[1, "a"]]))),
    expected: ShowCore.map(((n: number) => LibLiterals.showInt32(n)))(((s: string) => LibLiterals.showString(s)))(new Map([[1, "a"]]))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

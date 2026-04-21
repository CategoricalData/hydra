// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.sets primitives
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
import * as LibLiterals from "../../lib/literals.js";
import * as LibMath from "../../lib/math.js";
import * as LibSets from "../../lib/sets.js";
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
    name: "hydra.lib.sets primitives",
    description: null,
    subgroups: [({
    name: "empty",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.empty),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "singleton",
    description: null,
    subgroups: [],
    cases: [({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.singleton(42)),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([42]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "fromList",
    description: null,
    subgroups: [],
    cases: [({
    name: "create from list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.fromList([1, 2, 3])),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicates removed",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.fromList([1, 2, 1, 3])),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.fromList([])),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "toList",
    description: null,
    subgroups: [],
    cases: [({
    name: "convert to list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibSets.toList(new Set([1, 2, 3]))),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2, 3])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unsorted input",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibSets.toList(new Set([1, 2, 3]))),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([1, 2, 3])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))(LibSets.toList(new Set([]))),
    expected: ShowCore.list(((n: number) => LibLiterals.showInt32(n)))([])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "insert",
    description: null,
    subgroups: [],
    cases: [({
    name: "insert new element",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.insert(4)(new Set([1, 2, 3]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3, 4]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "insert existing element",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.insert(2)(new Set([1, 2, 3]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "insert into empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.insert(1)(new Set([]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "delete",
    description: null,
    subgroups: [],
    cases: [({
    name: "delete existing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.delete_(2)(new Set([1, 2, 3]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "delete non-existing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.delete_(4)(new Set([1, 2, 3]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "delete from empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.delete_(1)(new Set([]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "member",
    description: null,
    subgroups: [],
    cases: [({
    name: "element exists",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibSets.member(2)(new Set([1, 2, 3]))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "element missing",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibSets.member(4)(new Set([1, 2, 3]))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibSets.member(1)(new Set([]))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "size",
    description: null,
    subgroups: [],
    cases: [({
    name: "three elements",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibSets.size(new Set([1, 2, 3]))),
    expected: LibLiterals.showInt32(3)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibSets.size(new Set([42]))),
    expected: LibLiterals.showInt32(1)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibSets.size(new Set([]))),
    expected: LibLiterals.showInt32(0)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "null",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibSets.null_(new Set([]))),
    expected: LibLiterals.showBoolean(true)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "non-empty set",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showBoolean(LibSets.null_(new Set([1, 2]))),
    expected: LibLiterals.showBoolean(false)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "union",
    description: null,
    subgroups: [],
    cases: [({
    name: "union two sets",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.union(new Set([1, 2]))(new Set([2, 3]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "union with empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.union(new Set([1, 2]))(new Set([]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty with non-empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.union(new Set([]))(new Set([1, 2]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "unions",
    description: null,
    subgroups: [],
    cases: [({
    name: "union of multiple sets",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.unions([new Set([1, 2]), new Set([2, 3]), new Set([3, 4])])),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3, 4]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "union with empty sets",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.unions([new Set([1, 2]), new Set([]), new Set([3])])),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list of sets",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.unions([])),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single set",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.unions([new Set([1, 2, 3])])),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2, 3]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "intersection",
    description: null,
    subgroups: [],
    cases: [({
    name: "common elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.intersection(new Set([1, 2, 3]))(new Set([2, 3, 4]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([2, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no common elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.intersection(new Set([1, 2]))(new Set([3, 4]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "intersection with empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.intersection(new Set([1, 2]))(new Set([]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "difference",
    description: null,
    subgroups: [],
    cases: [({
    name: "remove elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.difference(new Set([1, 2, 3]))(new Set([2, 4]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 3]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no overlap",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.difference(new Set([1, 2]))(new Set([3, 4]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "difference with empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.difference(new Set([1, 2]))(new Set([]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([1, 2]))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "map",
    description: null,
    subgroups: [],
    cases: [({
    name: "map function",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.map(((x: number) => LibMath.mul(x)(2)))(new Set([1, 2, 3]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([2, 4, 6]))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "map on empty",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(LibSets.map(((x: number) => LibMath.mul(x)(2)))(new Set([]))),
    expected: ShowCore.set(((n: number) => LibLiterals.showInt32(n)))(new Set([]))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

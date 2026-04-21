// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for topological sorting algorithms
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Sorting from "../sorting.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "sorting",
    description: null,
    subgroups: [({
    name: "topological sort",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "right", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "singleton set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[1, []]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "right", value: [1] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "discrete set with multiple elements",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[3, []], [1, []], [2, []]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "right", value: [1, 2, 3] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "linked list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[3, [1]], [2, [3]], [1, []]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "right", value: [1, 3, 2] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "binary tree",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[3, [1, 4]], [4, [6, 2]], [1, [5]], [2, []], [6, []], [5, []]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "right", value: [5, 1, 2, 6, 4, 3] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two trees",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[3, [1, 4]], [5, [6, 2]], [2, [7]], [1, []], [4, []], [6, []], [7, []]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "right", value: [1, 7, 2, 4, 3, 6, 5] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "diamond DAG",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[1, [3, 4]], [3, [2]], [4, [2]], [2, [5]], [5, []]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "right", value: [5, 2, 3, 4, 1] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "two-node cycle",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[1, [2]], [2, [1]]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "left", value: [[1, 2]] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cycle with incoming and outgoing edges",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(Sorting.topologicalSort([[1, [3]], [3, [2]], [2, [3, 4]], [4, [5]], [5, []]])),
    expected: LibEithers.either(((cs: ReadonlyArray<ReadonlyArray<number>>) => LibStrings.cat2("left(")(LibStrings.cat2(ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(cs))(")"))))(((xs: ReadonlyArray<number>) => LibStrings.cat2("right(")(LibStrings.cat2(ShowCore.list(LibLiterals.showInt32)(xs))(")"))))(({ tag: "left", value: [[2, 3]] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "topological sort SCC",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "singleton set",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[1, []]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "discrete set with multiple elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[3, []], [1, []], [2, []]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1], [2], [3]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single two-element component #1",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[1, [2]], [2, []]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[2], [1]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single two-element component #2",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[2, [1]], [1, []]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1], [2]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple-element component",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[2, [1, 3]], [1, [3]], [3, []]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[3], [1], [2]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cycle of two nodes #1",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[1, [2]], [2, [1]]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1, 2]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cycle of two nodes #2",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[2, [1]], [1, [2]]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1, 2]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cycle of three nodes #1",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[1, [2]], [2, [3]], [3, [1]]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1, 2, 3]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cycle of three nodes #2",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[2, [1]], [3, [2]], [1, [3]]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1, 2, 3]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple disconnected cycles",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[200, []], [100, []], [300, []], [10, [20]], [20, [10]], [1, [2]], [2, [3]], [3, [1]]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1, 2, 3], [10, 20], [100], [200], [300]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "complex cycles",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[1, [2, 3]], [2, [3]], [3, [1]]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[1, 2, 3]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "chain of three SCCs",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[1, [2, 10]], [2, [3]], [3, [1]], [10, [20]], [20, [100, 10]], [100, []]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[100], [10, 20], [1, 2, 3]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "SCCs with dependencies to/from non-SCC nodes",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))(Sorting.topologicalSortComponents([[1, [2, 3, 10]], [2, [3]], [3, [1]], [10, [20, 30]], [20, [30]], [30, []], [100, [200, 2]], [200, []], [300, [100]], [1000, []], [2000, []]])),
    expected: ShowCore.list(((v1: ReadonlyArray<number>) => ShowCore.list(LibLiterals.showInt32)(v1)))([[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]])
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

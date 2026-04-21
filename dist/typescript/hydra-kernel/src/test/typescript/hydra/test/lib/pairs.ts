// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.pairs primitives
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
import * as LibPairs from "../../lib/pairs.js";
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
    name: "hydra.lib.pairs primitives",
    description: null,
    subgroups: [({
    name: "bimap",
    description: null,
    subgroups: [],
    cases: [({
    name: "transform both elements",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["(", LibLiterals.showInt32(LibPairs.first(LibPairs.bimap(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))([5, "ab"]))), ", ", LibLiterals.showInt32(LibPairs.second(LibPairs.bimap(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))([5, "ab"]))), ")"]),
    expected: LibStrings.cat(["(", LibLiterals.showInt32(LibPairs.first([10, 2])), ", ", LibLiterals.showInt32(LibPairs.second([10, 2])), ")"])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibStrings.cat(["(", LibLiterals.showInt32(LibPairs.first(LibPairs.bimap(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))([0, "hello"]))), ", ", LibLiterals.showInt32(LibPairs.second(LibPairs.bimap(((x: number) => LibMath.mul(x)(2)))(((s: string) => LibStrings.length(s)))([0, "hello"]))), ")"]),
    expected: LibStrings.cat(["(", LibLiterals.showInt32(LibPairs.first([0, 5])), ", ", LibLiterals.showInt32(LibPairs.second([0, 5])), ")"])
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "first",
    description: null,
    subgroups: [],
    cases: [({
    name: "extract first element",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibPairs.first([42, "hello"])),
    expected: LibLiterals.showInt32(42)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "with zero",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibPairs.first([0, "world"])),
    expected: LibLiterals.showInt32(0)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative number",
    case: ({ tag: "universal", value: ({
    actual: LibLiterals.showInt32(LibPairs.first([-5, "test"])),
    expected: LibLiterals.showInt32(-5)
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "second",
    description: null,
    subgroups: [],
    cases: [({
    name: "extract second element",
    case: ({ tag: "universal", value: ({
    actual: LibPairs.second([42, "hello"]),
    expected: "hello"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty string",
    case: ({ tag: "universal", value: ({
    actual: LibPairs.second([0, ""]),
    expected: ""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "long string",
    case: ({ tag: "universal", value: ({
    actual: LibPairs.second([123, "testing"]),
    expected: "testing"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

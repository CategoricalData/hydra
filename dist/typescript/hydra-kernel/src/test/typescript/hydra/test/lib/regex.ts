// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for hydra.lib.regex primitives
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
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Reduction from "../../reduction.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "hydra.lib.regex primitives",
    description: null,
    subgroups: [({
    name: "matches",
    description: null,
    subgroups: [],
    cases: [({
    name: "exact match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pattern match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[a-z]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "partial content does not match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[a-z]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello123" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "digit pattern",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "12345" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed pattern",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[a-z]+[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello123" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty pattern matches empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty pattern does not match non-empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "star matches empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a*" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "alternation",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "cat|dog" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "cat" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "alternation second",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "cat|dog" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "dog" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "alternation no match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "cat|dog" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "bird" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "quantifier",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "ab?c" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "ac" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "quantifier with optional",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.matches" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "ab?c" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "find",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple find",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.find" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc123def" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "123" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.find" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abcdef" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "find first",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.find" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[a-z]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "123abc456def" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty input",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.find" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "full match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.find" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: ".*" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "findAll",
    description: null,
    subgroups: [],
    cases: [({
    name: "multiple matches",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.findAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a1b2c3" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "1" }) }), ({ tag: "literal", value: ({ tag: "string", value: "2" }) }), ({ tag: "literal", value: ({ tag: "string", value: "3" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no matches",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.findAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "overlapping words",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.findAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[a-z]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc def ghi" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "abc" }) }), ({ tag: "literal", value: ({ tag: "string", value: "def" }) }), ({ tag: "literal", value: ({ tag: "string", value: "ghi" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.findAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "say hello world" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "hello" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "replace",
    description: null,
    subgroups: [],
    cases: [({
    name: "basic replace",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replace" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "X" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc123def456" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "abcXdef456" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replace" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "X" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abcdef" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "abcdef" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "replace at start",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replace" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "^[a-z]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "X" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc123" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "X123" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty replacement",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replace" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc123def" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "abcdef" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "replaceAll",
    description: null,
    subgroups: [],
    cases: [({
    name: "replace all digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replaceAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "X" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a1b2c3" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "aXbXcX" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replaceAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "X" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "abc" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "replace all words",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replaceAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[a-z]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "X" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc 123 def" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "X 123 X" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty replacement",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.replaceAll" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a1b2c3" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "abc" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "split",
    description: null,
    subgroups: [],
    cases: [({
    name: "split on comma",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.split" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a,b,c" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "split on spaces",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.split" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: " +" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a b  c" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no match",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.split" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "abc" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "abc" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "split on digits",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.split" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "[0-9]+" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a1b2c" }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "c" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "trailing delimiter",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.regex.split" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "," }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "a,b," }) })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) }), ({ tag: "literal", value: ({ tag: "string", value: "" }) })] }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

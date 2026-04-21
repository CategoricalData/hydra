// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for string formatting and case conversion
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
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "formatting",
    description: null,
    subgroups: [caseConversionTests],
    cases: []
  });

export const caseConversionTests: Testing.TestGroup = ({
    name: "case conversion",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1 (lower_snake_case -> UPPER_SNAKE_CASE)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "lowerSnake" }))(({ tag: "upperSnake" }))("a_hello_world_42_a42_42a_b"),
    expected: "A_HELLO_WORLD_42_A42_42A_B"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#2 (lower_snake_case -> camelCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "lowerSnake" }))(({ tag: "camel" }))("a_hello_world_42_a42_42a_b"),
    expected: "aHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#3 (lower_snake_case -> PascalCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "lowerSnake" }))(({ tag: "pascal" }))("a_hello_world_42_a42_42a_b"),
    expected: "AHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#4 (lower_snake_case -> lower_snake_case)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "lowerSnake" }))(({ tag: "lowerSnake" }))("a_hello_world_42_a42_42a_b"),
    expected: "a_hello_world_42_a42_42a_b"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#5 (UPPER_SNAKE_CASE -> lower_snake_case)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "upperSnake" }))(({ tag: "lowerSnake" }))("A_HELLO_WORLD_42_A42_42A_B"),
    expected: "a_hello_world_42_a42_42a_b"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#6 (UPPER_SNAKE_CASE -> camelCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "upperSnake" }))(({ tag: "camel" }))("A_HELLO_WORLD_42_A42_42A_B"),
    expected: "aHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#7 (UPPER_SNAKE_CASE -> PascalCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "upperSnake" }))(({ tag: "pascal" }))("A_HELLO_WORLD_42_A42_42A_B"),
    expected: "AHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "upperSnake" }))(({ tag: "upperSnake" }))("A_HELLO_WORLD_42_A42_42A_B"),
    expected: "A_HELLO_WORLD_42_A42_42A_B"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#9 (camelCase -> lower_snake_case)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "camel" }))(({ tag: "lowerSnake" }))("aHelloWorld42A4242aB"),
    expected: "a_hello_world42_a4242a_b"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#10 (camelCase -> UPPER_SNAKE_CASE)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "camel" }))(({ tag: "upperSnake" }))("aHelloWorld42A4242aB"),
    expected: "A_HELLO_WORLD42_A4242A_B"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#11 (camelCase -> PascalCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "camel" }))(({ tag: "pascal" }))("aHelloWorld42A4242aB"),
    expected: "AHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#12 (camelCase -> camelCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "camel" }))(({ tag: "camel" }))("aHelloWorld42A4242aB"),
    expected: "aHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#13 (PascalCase -> lower_snake_case)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "pascal" }))(({ tag: "lowerSnake" }))("AHelloWorld42A4242aB"),
    expected: "a_hello_world42_a4242a_b"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#14 (PascalCase -> UPPER_SNAKE_CASE)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "pascal" }))(({ tag: "upperSnake" }))("AHelloWorld42A4242aB"),
    expected: "A_HELLO_WORLD42_A4242A_B"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#15 (PascalCase -> camelCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "pascal" }))(({ tag: "camel" }))("AHelloWorld42A4242aB"),
    expected: "aHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "#16 (PascalCase -> PascalCase)",
    case: ({ tag: "universal", value: ({
    actual: Formatting.convertCase(({ tag: "pascal" }))(({ tag: "pascal" }))("AHelloWorld42A4242aB"),
    expected: "AHelloWorld42A4242aB"
  }) }),
    description: null,
    tags: []
  })]
  });

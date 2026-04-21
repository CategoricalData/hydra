// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for JSON serialization
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
import * as JsonWriter from "../../json/writer.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "JSON serialization",
    description: null,
    subgroups: [({
    name: "primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "null",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "null" })),
    expected: "null"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "true",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "boolean", value: true })),
    expected: "true"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "false",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "boolean", value: false })),
    expected: "false"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "number", value: 0.0 })),
    expected: "0"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "positive integer",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "number", value: 42.0 })),
    expected: "42"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative integer",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "number", value: -17.0 })),
    expected: "-17"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "large integer",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "number", value: 1000000.0 })),
    expected: "1000000"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "decimal",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "number", value: 3.14 })),
    expected: "3.14"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "negative decimal",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "number", value: -2.5 })),
    expected: "-2.5"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "small decimal",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "number", value: 1.0e-3 })),
    expected: "1.0e-3"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "strings",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty string",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "" })),
    expected: "\"\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "simple string",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "hello" })),
    expected: "\"hello\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with spaces",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "hello world" })),
    expected: "\"hello world\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with double quote",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "say \"hi\"" })),
    expected: "\"say \\\"hi\\\"\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with backslash",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "path\\to\\file" })),
    expected: "\"path\\\\to\\\\file\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with newline",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "line1\nline2" })),
    expected: "\"line1\\nline2\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with carriage return",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "line1\rline2" })),
    expected: "\"line1\\rline2\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with tab",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "col1\tcol2" })),
    expected: "\"col1\\tcol2\""
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with mixed escapes",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "string", value: "a\"b\\c\nd" })),
    expected: "\"a\\\"b\\\\c\\nd\""
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "arrays",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty array",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "array", value: [] })),
    expected: "[]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single element",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "array", value: [({ tag: "number", value: 1.0 })] })),
    expected: "[1]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple numbers",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "array", value: [({ tag: "number", value: 1.0 }), ({ tag: "number", value: 2.0 }), ({ tag: "number", value: 3.0 })] })),
    expected: "[1, 2, 3]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple strings",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "array", value: [({ tag: "string", value: "a" }), ({ tag: "string", value: "b" })] })),
    expected: "[\"a\", \"b\"]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed types",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "array", value: [({ tag: "number", value: 1.0 }), ({ tag: "string", value: "two" }), ({ tag: "boolean", value: true }), ({ tag: "null" })] })),
    expected: "[1, \"two\", true, null]"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "objects",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty object",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "object", value: new Map([]) })),
    expected: "{}"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single key-value",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "object", value: new Map([["name", ({ tag: "string", value: "Alice" })]]) })),
    expected: "{\"name\": \"Alice\"}"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple keys",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "object", value: new Map([["a", ({ tag: "number", value: 1.0 })], ["b", ({ tag: "number", value: 2.0 })]]) })),
    expected: "{\"a\": 1, \"b\": 2}"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed value types",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "object", value: new Map([["active", ({ tag: "boolean", value: true })], ["count", ({ tag: "number", value: 42.0 })], ["name", ({ tag: "string", value: "test" })]]) })),
    expected: "{\"active\": true, \"count\": 42, \"name\": \"test\"}"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "nested structures",
    description: null,
    subgroups: [],
    cases: [({
    name: "nested arrays",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "array", value: [({ tag: "array", value: [({ tag: "number", value: 1.0 }), ({ tag: "number", value: 2.0 })] }), ({ tag: "array", value: [({ tag: "number", value: 3.0 }), ({ tag: "number", value: 4.0 })] })] })),
    expected: "[[1, 2], [3, 4]]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "object with array",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "object", value: new Map([["items", ({ tag: "array", value: [({ tag: "number", value: 1.0 }), ({ tag: "number", value: 2.0 })] })]]) })),
    expected: "{\"items\": [1, 2]}"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "array of objects",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "array", value: [({ tag: "object", value: new Map([["id", ({ tag: "number", value: 1.0 })]]) }), ({ tag: "object", value: new Map([["id", ({ tag: "number", value: 2.0 })]]) })] })),
    expected: "[{\"id\": 1}, {\"id\": 2}]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested object",
    case: ({ tag: "universal", value: ({
    actual: JsonWriter.printJson(({ tag: "object", value: new Map([["user", ({ tag: "object", value: new Map([["name", ({ tag: "string", value: "Bob" })]]) })]]) })),
    expected: "{\"user\": {\"name\": \"Bob\"}}"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

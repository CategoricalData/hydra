// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for core term and type validation
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
import * as LibMaybes from "../../lib/maybes.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as ShowErrorCore from "../../show/error/core.js";
import * as Tabular from "../../tabular.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as ValidateCore from "../../validate/core.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "validate.core",
    description: null,
    subgroups: [duplicateBindingsTests, duplicateFieldsTests, emptyLetBindingsTests, identityApplicationTests, variableShadowingTests],
    cases: []
  });

export const duplicateBindingsTests: Testing.TestGroup = ({
    name: "duplicate bindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "no bindings (literal)",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single binding",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "distinct bindings",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate bindings at top level",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "duplicateBinding", value: ({
    location: [],
    name: "x"
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate bindings in lambda body",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "a" })
  }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "duplicateBinding", value: ({
    location: [({ tag: "lambdaBody" })],
    name: "a"
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate bindings in let body",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "y" })
  }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "duplicateBinding", value: ({
    location: [({ tag: "letBody" })],
    name: "y"
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate bindings in let binding value",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "a" })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "duplicateBinding", value: ({
    location: [({ tag: "letBinding", value: "x" })],
    name: "a"
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "same name in different scopes is valid",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  })]
  });

export const duplicateFieldsTests: Testing.TestGroup = ({
    name: "duplicate fields",
    description: null,
    subgroups: [],
    cases: [({
    name: "no fields (literal)",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "distinct record fields",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "Point",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })]
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate record fields at top level",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "record", value: ({
    typeName: "Point",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })]
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "duplicateField", value: ({
    location: [],
    name: "x"
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate fields in record inside lambda",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "record", value: ({
    typeName: "Point",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })]
  }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "duplicateField", value: ({
    location: [({ tag: "lambdaBody" })],
    name: "x"
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "duplicate fields in record inside let body",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "r",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }),
    type: null
  })],
    body: ({ tag: "record", value: ({
    typeName: "Point",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }), ({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  })]
  }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "duplicateField", value: ({
    location: [({ tag: "letBody" })],
    name: "x"
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const emptyLetBindingsTests: Testing.TestGroup = ({
    name: "empty let bindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "let with bindings is valid",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty let bindings",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "let", value: ({
    bindings: [],
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "emptyLetBindings", value: ({
    location: []
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const identityApplicationTests: Testing.TestGroup = ({
    name: "identity application",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-identity lambda application is valid",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "identity lambda application",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(({ tag: "unnecessaryIdentityApplication", value: ({
    location: []
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  });

export const variableShadowingTests: Testing.TestGroup = ({
    name: "variable shadowing",
    description: null,
    subgroups: [],
    cases: [({
    name: "lambda with fresh variable is valid",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda shadows outer lambda",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let binding shadows lambda parameter",
    case: ({ tag: "universal", value: ({
    actual: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(ValidateCore.term(false)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: LibMaybes.maybe("valid")(((e: ErrorCore.InvalidTermError) => ShowErrorCore.invalidTermError(e)))(null)
  }) }),
    description: null,
    tags: []
  })]
  });

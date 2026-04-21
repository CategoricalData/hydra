// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for dependency analysis and let-term transformations
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as Dependencies from "../dependencies.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibMaps from "../lib/maps.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Tabular from "../tabular.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "dependencies",
    description: null,
    subgroups: [({
    name: "simplifyTerm",
    description: null,
    subgroups: [],
    cases: [({
    name: "const application with literal",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.simplifyTerm(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "foo" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "identity application",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.simplifyTerm(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "x" })] })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "variable", value: "y" }), ({ tag: "variable", value: "y" })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unused parameter",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.simplifyTerm(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "foo" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested lambda applications",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.simplifyTerm(({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "variable", value: "a" })] })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) }), ({ tag: "variable", value: "y" })] }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "flattenLetTerms",
    description: null,
    subgroups: [],
    cases: [({
    name: "non-let term unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.flattenLetTerms(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list term unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.flattenLetTerms(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "foo" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sequential lets in body are flattened",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.flattenLetTerms(({ tag: "let", value: ({
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
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested binding in let value is flattened",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.flattenLetTerms(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "b" })] })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "b_x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "b_y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "list", value: [({ tag: "variable", value: "b_x" }), ({ tag: "variable", value: "b_y" })] }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "b" })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple levels of nesting are flattened",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.flattenLetTerms(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "p",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  }), ({
    name: "q",
    term: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "q" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "b" })] })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "b_x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  }), ({
    name: "b_y_p",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 137 }) }) }),
    type: null
  }), ({
    name: "b_y_q",
    term: ({ tag: "list", value: [({ tag: "variable", value: "b_x" }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })] }),
    type: null
  }), ({
    name: "b_y",
    term: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "b_y_q" })] }),
    type: null
  }), ({
    name: "b",
    term: ({ tag: "list", value: [({ tag: "variable", value: "b_x" }), ({ tag: "variable", value: "b_y" })] }),
    type: null
  })],
    body: ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "b" })] })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "liftLambdaAboveLet",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple let with lambda in body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bare lambda unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "bare let unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda with let in body unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let with two nested lambdas",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda inside let body already above let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let without lambda in body unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "variable", value: "y" })] })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple let bindings with lambda",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested lets with lambda at innermost level",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda between two lets",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple lambdas between nested lets",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "b",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "a",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "b",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple lambdas already above let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotation above let containing lambda",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "annotated", value: ({
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    annotation: new Map([])
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotation above lambda in let body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotation between two lambdas",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) }),
    annotation: new Map([])
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotation on the body of lambda in let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "x" }),
    annotation: new Map([])
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "x" }),
    annotation: new Map([])
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "annotation on lambda already above let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    annotation: new Map([])
  }) }))),
    expected: ShowCore.term(({ tag: "annotated", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    annotation: new Map([])
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let-lambda inside a list",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let-lambda in multiple list elements",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "list", value: [({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let-lambda in a let binding value",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))),
    expected: ShowCore.term(({ tag: "let", value: ({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "f" })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let-lambda inside a pair",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "pair", value: [({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "literal", value: ({ tag: "string", value: "test" }) })] }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "literal", value: ({ tag: "string", value: "test" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let-lambda in both elements of a pair",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "pair", value: [({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })] }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }), ({ tag: "lambda", value: ({
    parameter: "w",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "z",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "z" })
  }) })
  }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "let-lambda inside lambda body",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Dependencies.liftLambdaAboveLet(({ tag: "lambda", value: ({
    parameter: "outer",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "inner",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "outer",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "inner",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: null
  })],
    body: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "topologicalSortBindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "isolated bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))(Dependencies.topologicalSortBindingMap(LibMaps.fromList([["a", ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })], ["b", ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })]]))),
    expected: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))([[["a", ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })]], [["b", ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })]]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single recursive binding",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))(Dependencies.topologicalSortBindingMap(LibMaps.fromList([["a", ({ tag: "list", value: [({ tag: "variable", value: "a" })] })]]))),
    expected: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))([[["a", ({ tag: "list", value: [({ tag: "variable", value: "a" })] })]]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mutually recursive bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))(Dependencies.topologicalSortBindingMap(LibMaps.fromList([["a", ({ tag: "list", value: [({ tag: "variable", value: "b" })] })], ["b", ({ tag: "list", value: [({ tag: "variable", value: "a" })] })]]))),
    expected: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))([[["a", ({ tag: "list", value: [({ tag: "variable", value: "b" })] })], ["b", ({ tag: "list", value: [({ tag: "variable", value: "a" })] })]]])
  }) }),
    description: null,
    tags: []
  }), ({
    name: "mixed bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))(Dependencies.topologicalSortBindingMap(LibMaps.fromList([["a", ({ tag: "variable", value: "b" })], ["b", ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "c" })] })], ["c", ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })], ["d", ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })]]))),
    expected: ShowCore.list(((group: ReadonlyArray<readonly [Core.Name, Core.Term]>) => ShowCore.list(((pair: readonly [Core.Name, Core.Term]) => LibStrings.cat(["(", ((_x) => _x)(LibPairs.first(pair)), ", ", ShowCore.term(LibPairs.second(pair)), ")"])))(group)))([[["c", ({ tag: "literal", value: ({ tag: "string", value: "foo" }) })]], [["a", ({ tag: "variable", value: "b" })], ["b", ({ tag: "list", value: [({ tag: "variable", value: "a" }), ({ tag: "variable", value: "c" })] })]], [["d", ({ tag: "literal", value: ({ tag: "string", value: "bar" }) })]]])
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

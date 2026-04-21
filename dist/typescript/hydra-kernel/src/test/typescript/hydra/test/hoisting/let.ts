// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for let-binding hoisting transformations
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
import * as Hoisting from "../../hoisting.js";
import * as JsonModel from "../../json/model.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "hoistLet",
    description: null,
    subgroups: [({
    name: "hoistLetBindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "nested let inside lambda: binding hoisted with lambda capture",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistAllLetBindings(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "g" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_g" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "f_g",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type application: nested let outside lambda CAN be hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistAllLetBindings(({
    bindings: [({
    name: "f",
    term: ({ tag: "typeApplication", value: ({
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "typeApplication", value: ({
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "f_y" })
  }) })
  }) }),
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    type: null
  }), ({
    name: "f_y",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "hoistPolymorphicLetBindings",
    description: null,
    subgroups: [],
    cases: [({
    name: "no polymorphic bindings: simple let unchanged",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "x" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "x" })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no polymorphic bindings: multiple monomorphic bindings",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hi" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "string", value: "hi" }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "string" }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single polymorphic binding: already at top level",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding inside lambda: no capture",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_id" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_id",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding captures lambda variable: wrapped in lambda",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "b" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_g" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding captures multiple lambda variables",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "triple" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["c"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "c" }),
    codomain: ({ tag: "variable", value: "c" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_g" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "c",
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "triple" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["c"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "c" }),
    codomain: ({ tag: "variable", value: "c" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding captures some but not all lambda variables",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["c"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "c" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "variable", value: "c" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_g" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "c",
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["c"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "c" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    second: ({ tag: "variable", value: "c" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding captures both lambda-bound and let-bound variables",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  }), ({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_g" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sibling polymorphic bindings inside lambda: one calls the other",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  }), ({
    name: "h",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_h" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_h",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_g" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sibling polymorphic bindings inside lambda: h passes its own args to g",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "v" })
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "h",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "v" })
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_h" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "v" })
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_h",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_g" }),
    argument: ({ tag: "variable", value: "outer" })
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "untyped binding: not hoisted",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
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
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }))),
    expected: ShowCore.let_(({
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
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "no name collision: distinct names after unshadowing",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id2" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "id",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_id2" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "id" }),
    argument: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_id2",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested polymorphic binding calls enclosing polymorphic binding",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "lambda", value: ({
    parameter: "inner",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "h",
    term: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "outer",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "inner",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_h" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_h",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "z",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_g" }),
    argument: ({ tag: "variable", value: "z" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 10 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 20 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding captures monomorphic sibling in same let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "left",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "right",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "sleft",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "left" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  }), ({
    name: "sright",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "right" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  }), ({
    name: "cannotUnify",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "sleft" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "sright" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "cannotUnify" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "left",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "right",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "sleft",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "left" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  }), ({
    name: "sright",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "right" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_cannotUnify" }),
    argument: ({ tag: "variable", value: "sleft" })
  }) }),
    argument: ({ tag: "variable", value: "sright" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_cannotUnify",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "sleft",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "sright",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "sleft" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "sright" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested lets: poly binding references poly sibling from outer let",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "left",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "sleft",
    term: ({ tag: "variable", value: "left" }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "cannotUnify",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "sleft" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "joinList",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "cannotUnify" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "joinList" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "wrapper",
    term: ({ tag: "lambda", value: ({
    parameter: "left",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "sleft",
    term: ({ tag: "variable", value: "left" }),
    type: ({
    variables: [],
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_joinList" }),
    argument: ({ tag: "variable", value: "sleft" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_cannotUnify",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "lambda", value: ({
    parameter: "sleft",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "sleft" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "a" })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "wrapper_joinList",
    term: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "sleft",
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper_cannotUnify" }),
    argument: ({ tag: "variable", value: "sleft" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "b" }),
    codomain: ({ tag: "variable", value: "b" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "wrapper" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "polymorphic binding with pair: type applications preserved",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "init",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "pair", value: [({ tag: "typeApplication", value: ({
    body: ({ tag: "list", value: [] }),
    type: ({ tag: "variable", value: "t0" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "singleton" }),
    argument: ({ tag: "variable", value: "b" })
  }) })] }),
    type: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    type: ({ tag: "set", value: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) })
  }) })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "set", value: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "init" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "set", value: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "name_x" })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_init" }),
    argument: ({ tag: "variable", value: "b" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "set", value: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_init",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "pair", value: [({ tag: "typeApplication", value: ({
    body: ({ tag: "list", value: [] }),
    type: ({ tag: "variable", value: "t0" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "singleton" }),
    argument: ({ tag: "variable", value: "b" })
  }) })] }),
    type: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) })
  }) }),
    type: ({ tag: "set", value: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "list", value: ({ tag: "variable", value: "t0" }) }),
    second: ({ tag: "set", value: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "name_x" })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "monomorphic binding captures type vars: replacement includes type applications",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "q",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "q" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a", "b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: ({ tag: "variable", value: "f_q" }),
    type: ({ tag: "variable", value: "a" })
  }) }),
    type: ({ tag: "variable", value: "b" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a", "b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_q",
    term: ({ tag: "typeLambda", value: ({
    parameter: "a",
    body: ({ tag: "typeLambda", value: ({
    parameter: "b",
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: ({ tag: "variable", value: "a" }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["a", "b"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "variable", value: "b" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "hoistPolymorphicTypeParameters",
    description: null,
    subgroups: [],
    cases: [({
    name: "nested function types: all type variables must be declared",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "choose",
    term: ({ tag: "lambda", value: ({
    parameter: "forLeft",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "forRight",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "e",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "forLeft" }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "choose" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "f_choose" }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_choose",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t1",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t2",
    body: ({ tag: "lambda", value: ({
    parameter: "forLeft",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "forRight",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "e",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "forLeft" }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type variable in return position only",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "returnT",
    term: ({ tag: "lambda", value: ({
    parameter: "unit",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "unit" }),
    codomain: ({ tag: "variable", value: "t" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "returnT" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "unit" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "f_returnT" }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "unit" }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_returnT",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t",
    body: ({ tag: "lambda", value: ({
    parameter: "unit",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "unit" }),
    codomain: ({ tag: "variable", value: "t" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "type variables in deeply nested generics",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "nested",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "pair", value: ({
    first: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) }),
    second: ({ tag: "variable", value: "t2" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "nested" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "pair", value: ({
    first: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "f_nested" }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "pair", value: ({
    first: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_nested",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t1",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t2",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "pair", value: ({
    first: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "t0" }),
    second: ({ tag: "variable", value: "t1" })
  }) }),
    second: ({ tag: "variable", value: "t2" })
  }) }),
    codomain: ({ tag: "variable", value: "t0" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiple bindings with overlapping type variable names",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "outer",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "id1",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "variable", value: "t" })
  }) }),
    constraints: null
  })
  }), ({
    name: "id2",
    term: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "variable", value: "t" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "id1" })
  }) }),
    argument: ({ tag: "variable", value: "id2" })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "outer" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "outer",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "outer_id1" })
  }) }),
    argument: ({ tag: "variable", value: "outer_id2" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "pair", value: ({
    first: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) }),
    second: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "outer_id1",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "variable", value: "t" })
  }) }),
    constraints: null
  })
  }), ({
    name: "outer_id2",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t",
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "variable", value: "t" })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "outer" })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "captured variable with type parameters",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "t" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "g" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f_g" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t",
    body: ({ tag: "lambda", value: ({
    parameter: "a",
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "pair" }),
    argument: ({ tag: "variable", value: "a" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "literal", value: ({ tag: "string" }) }),
    second: ({ tag: "variable", value: "t" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) })
  }) })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "short type variable names are treated as type parameters",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) })
  }) }),
    type: ({
    variables: ["s", "t", "v"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "s" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "variable", value: "v" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "g" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "f_g" }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "s",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t",
    body: ({ tag: "typeLambda", value: ({
    parameter: "v",
    body: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["s", "t", "v"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "s" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t" }),
    codomain: ({ tag: "variable", value: "v" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "numbered type variables like t0 t1 t2",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "f",
    term: ({ tag: "let", value: ({
    bindings: [({
    name: "g",
    term: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t2" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "g" })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "f",
    term: ({ tag: "variable", value: "f_g" }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "f_g",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t1",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t2",
    body: ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "undefined" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "variable", value: "t2" })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "f" })
  }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "choose pattern from mutateTrace",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.let_(Hoisting.hoistPolymorphicLetBindings(((b: Core.Binding) => true))(({
    bindings: [({
    name: "mutateTrace",
    term: ({ tag: "lambda", value: ({
    parameter: "mutate",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "restore",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "choose",
    term: ({ tag: "lambda", value: ({
    parameter: "forLeft",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "forRight",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "e",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "forLeft" }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "choose" }),
    argument: ({ tag: "variable", value: "forLeft" })
  }) }),
    argument: ({ tag: "variable", value: "forRight" })
  }) }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "mutateTrace" })
  }))),
    expected: ShowCore.let_(({
    bindings: [({
    name: "mutateTrace",
    term: ({ tag: "lambda", value: ({
    parameter: "mutate",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "restore",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "mutateTrace_choose" }),
    argument: ({ tag: "variable", value: "forLeft" })
  }) }),
    argument: ({ tag: "variable", value: "forRight" })
  }) }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    codomain: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  }) })
  }) })
  }) }),
    constraints: null
  })
  }), ({
    name: "mutateTrace_choose",
    term: ({ tag: "typeLambda", value: ({
    parameter: "t0",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t1",
    body: ({ tag: "typeLambda", value: ({
    parameter: "t2",
    body: ({ tag: "lambda", value: ({
    parameter: "forLeft",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "forRight",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "e",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "forLeft" }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: ({
    variables: ["t0", "t1", "t2"],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t0" }),
    codomain: ({ tag: "variable", value: "t1" })
  }) })
  }) })
  }) }),
    constraints: null
  })
  })],
    body: ({ tag: "variable", value: "mutateTrace" })
  }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

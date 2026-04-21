// Note: this is an automatically generated file. Do not edit.

/**
 * Inference tests for examples from the Hydra kernel
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
import * as Inference from "../../inference.js";
import * as JsonModel from "../../json/model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibPairs from "../../lib/pairs.js";
import * as LibStrings from "../../lib/strings.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
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
    name: "Examples from the Hydra kernel",
    description: null,
    subgroups: [testGroupForNestedLet],
    cases: []
  });

export const testGroupForNestedLet: Testing.TestGroup = ({
    name: "Nested let",
    description: null,
    subgroups: [({
    name: "hydra.formatting.mapFirstLetter",
    description: null,
    subgroups: [],
    cases: [({
    name: "#1",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "mapping",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "firstLetter",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "mapping" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.fromList" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.pure" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.head" }),
    argument: ({ tag: "variable", value: "list" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  }), ({
    name: "list",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.toList" }),
    argument: ({ tag: "variable", value: "s" })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.null" }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "s" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "variable", value: "firstLetter" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.fromList" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.tail" }),
    argument: ({ tag: "variable", value: "list" })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Recursive let with pair return (ifElse)",
    description: null,
    subgroups: [],
    cases: [({
    name: "#2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "input",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "go",
    term: ({ tag: "lambda", value: ({
    parameter: "depth",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "subst",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.null" }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) }),
    argument: ({ tag: "pair", value: [({ tag: "variable", value: "subst" }), ({ tag: "variable", value: "s" })] })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "go" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "depth" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.insert" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "key" }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "val" }) })
  }) }),
    argument: ({ tag: "variable", value: "subst" })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "result",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "go" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.maps.empty" })
  }) }),
    argument: ({ tag: "variable", value: "input" })
  }) }),
    type: null
  }), ({
    name: "subst",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "result" })
  }) }),
    type: null
  }), ({
    name: "body",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "result" })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "variable", value: "subst" }), ({ tag: "variable", value: "body" })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "literal", value: ({ tag: "string" }) }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "map", value: ({
    keys: ({ tag: "literal", value: ({ tag: "string" }) }),
    values: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    second: ({ tag: "literal", value: ({ tag: "string" }) })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference"]
  })]
  }), ({
    name: "Recursive let with pair return (case on Type)",
    description: null,
    subgroups: [],
    cases: [({
    name: "#3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => LibStrings.cat2("INFERENCE ERROR: ")("failed")))(((result: readonly [readonly [Core.Term, Core.TypeScheme], Context.Context]) => ShowCore.typeScheme(LibPairs.second(LibPairs.first(result)))))(Inference.inferTypeOf(TestTestGraph.testContext)(TestTestGraph.testGraph)(({ tag: "lambda", value: ({
    parameter: "typ",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "go",
    term: ({ tag: "lambda", value: ({
    parameter: "depth",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "subst",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Type",
    default: ({ tag: "pair", value: [({ tag: "variable", value: "subst" }), ({ tag: "variable", value: "t" })] }),
    cases: [({
    name: "forall",
    term: ({ tag: "lambda", value: ({
    parameter: "ft",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "go" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "depth" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.insert" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.core.ForallType",
    field: "parameter"
  }) }),
    argument: ({ tag: "variable", value: "ft" })
  }) })
  }) }),
    argument: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat2" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: "_" }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.literals.showInt32" }),
    argument: ({ tag: "variable", value: "depth" })
  }) })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "subst" })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.core.ForallType",
    field: "body"
  }) }),
    argument: ({ tag: "variable", value: "ft" })
  }) })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) })
  }) })
  }) })
  }) }),
    type: null
  })],
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "result",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "go" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) })
  }) }),
    argument: ({ tag: "variable", value: "hydra.lib.maps.empty" })
  }) }),
    argument: ({ tag: "variable", value: "typ" })
  }) }),
    type: null
  })],
    body: ({ tag: "pair", value: [({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.first" }),
    argument: ({ tag: "variable", value: "result" })
  }) }), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.second" }),
    argument: ({ tag: "variable", value: "result" })
  }) })] })
  }) })
  }) })
  }) }))),
    expected: ShowCore.typeScheme(({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.core.Type" }),
    codomain: ({ tag: "pair", value: ({
    first: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.Name" })
  }) }),
    second: ({ tag: "variable", value: "hydra.core.Type" })
  }) })
  }) }),
    constraints: null
  }))
  }) }),
    description: null,
    tags: ["disabledForMinimalInference", "disabled"]
  })]
  })],
    cases: []
  });

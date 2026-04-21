// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for automatic differentiation
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as Differentiation from "../differentiation.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Reduction from "../reduction.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as Tabular from "../tabular.js";
import * as TestTestGraph from "./testGraph.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variables from "../variables.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "differentiation",
    description: null,
    subgroups: [({
    name: "basic rules",
    description: null,
    subgroups: [],
    cases: [({
    name: "variable wrt itself",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "variable", value: "x" }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "variable wrt different variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "variable", value: "y" }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float64 literal",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "zero literal",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "identity function",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateFunction(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "constant function",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateFunction(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda binding different variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "lambda shadowing differentiation variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "unary primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "sin of variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp of variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin of different variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "y" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "y" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "binary primitives",
    description: null,
    subgroups: [],
    cases: [({
    name: "add variable to itself",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "multiply variable by itself",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "subtract constant from variable",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.subFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "chain rule",
    description: null,
    subgroups: [],
    cases: [({
    name: "nested sin(cos(x))",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }))),
    expected: ShowCore.term(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "lambda", value: ({
    parameter: "_x",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.negateFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "_x" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "structural",
    description: null,
    subgroups: [],
    cases: [({
    name: "list of terms",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "list", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "pair of terms",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "pair", value: [({ tag: "variable", value: "x" }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })] }))),
    expected: ShowCore.term(({ tag: "pair", value: [({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }), ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "unit term",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.differentiateTerm("x")(({ tag: "unit" }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "evaluate derivatives",
    description: null,
    subgroups: [],
    cases: [({
    name: "d/dx(x) at 3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "variable", value: "x" })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(42.0) at 3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 42.0 }) }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(x+x) at 3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.add" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(x*x) at 3.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 6.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(x*x) at 5.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 10.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(x-5) at 7.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 7.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sub" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 5.0 }) }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(sin(x)) at 0.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(sin(x)) at pi/2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5707963267948966 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 6.12323399574e-17 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(cos(x)) at 1.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -0.841470984808 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(cos(x)) at pi",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.141592653589793 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -1.22464679915e-16 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(exp(x)) at 0.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(exp(x)) at 1.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.71828182846 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(log(x)) at 1.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(log(x)) at e",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.718281828459045 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.367879441171 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(sqrt(x)) at 4.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sqrt" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.25 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(x^3) at 2.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 12.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(sin(cos(x))) at 0.5",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -0.30635890919 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(sin(cos(x))) at 1.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -0.721606149063 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(exp(sin(x))) at 0.0",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "d/dx(x*sin(x)) at pi",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 12 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.141592653589793 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -3.14159265359 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "gradient check",
    description: null,
    subgroups: [],
    cases: [({
    name: "x^2",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 4.0 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin(x)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5403 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "exp(x)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.exp" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.6487 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "x^3",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.5 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.pow" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.0 }) }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 6.75 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "log(x)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.log" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "cos(x)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -0.84147 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "sin(cos(x))",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.cos" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -0.72161 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "x*sin(x)",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: Errors.Error) => "<<eval error>>"))(((t: Core.Term) => ShowCore.term(t)))(Reduction.reduceTerm(TestTestGraph.testContext)(TestTestGraph.testGraph)(true)(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.roundFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 5 }) }) })
  }) }),
    argument: Variables.replaceFreeTermVariable("x")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 2.0 }) }) }))(Differentiation.differentiateTerm("x")(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mul" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.sin" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })))
  }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 7.7004e-2 }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "gradient",
    description: null,
    subgroups: [],
    cases: [({
    name: "add two variables",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.gradient("Gradient")(["x", "y"])(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "Gradient",
    fields: [({
    name: "x",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) })
  }), ({
    name: "y",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "product of two variables",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.gradient("Gradient")(["x", "y"])(({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "variable", value: "y" })
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "Gradient",
    fields: [({
    name: "x",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) })
  }), ({
    name: "y",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.addFloat64" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "x" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.math.mulFloat64" }),
    argument: ({ tag: "variable", value: "y" })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }) })
  }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "single variable partial",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.gradient("Gradient")(["x", "y"])(({ tag: "variable", value: "x" }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "Gradient",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 1.0 }) }) })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "constant",
    case: ({ tag: "universal", value: ({
    actual: ShowCore.term(Differentiation.gradient("Gradient")(["x", "y"])(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 7.0 }) }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "Gradient",
    fields: [({
    name: "x",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  }), ({
    name: "y",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 0.0 }) }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

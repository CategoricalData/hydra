// Note: this is an automatically generated file. Do not edit.

/**
 * Test cases for AST serialization
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
import * as LibMath from "../lib/math.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Serialization from "../serialization.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "serialization",
    description: null,
    subgroups: [({
    name: "associativity",
    description: null,
    subgroups: [],
    cases: [({
    name: "right-associative operator",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(arrowOp)(Serialization.ifx(arrowOp)(Serialization.cst("a"))(Serialization.cst("b")))(Serialization.ifx(arrowOp)(Serialization.cst("c"))(Serialization.cst("d"))))),
    expected: "(a -> b) -> c -> d"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "case statements",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple case statement",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(({
    symbol: "of",
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "breakAndIndent", value: "  " })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(Serialization.spaceSep([Serialization.cst("case"), Serialization.ifx(gtOp)(Serialization.cst("x"))(Serialization.num(42))]))(Serialization.newlineSep([Serialization.ifx(caseOp)(Serialization.cst("False"))(Serialization.cst("Big")), Serialization.ifx(caseOp)(Serialization.cst("True"))(Serialization.cst("Small"))])))),
    expected: "case x > 42 of\n  False -> Big\n  True -> Small"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested case statement",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(({
    symbol: "of",
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "breakAndIndent", value: "  " })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(Serialization.spaceSep([Serialization.cst("case"), Serialization.ifx(gtOp)(Serialization.cst("x"))(Serialization.num(42))]))(Serialization.newlineSep([Serialization.ifx(caseOp)(Serialization.cst("True"))(Serialization.ifx(({
    symbol: "of",
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "breakAndIndent", value: "  " })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(Serialization.spaceSep([Serialization.cst("case"), Serialization.ifx(gtOp)(Serialization.cst("x"))(Serialization.num(100))]))(Serialization.newlineSep([Serialization.ifx(caseOp)(Serialization.cst("True"))(Serialization.cst("ReallyBig")), Serialization.ifx(caseOp)(Serialization.cst("False"))(Serialization.cst("Big"))]))), Serialization.ifx(caseOp)(Serialization.cst("False"))(Serialization.cst("Small"))])))),
    expected: "case x > 42 of\n  True -> case x > 100 of\n    True -> ReallyBig\n    False -> Big\n  False -> Small"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lambdas",
    description: null,
    subgroups: [],
    cases: [({
    name: "simple lambda",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(lambdaOp)(Serialization.cst("\\x y"))(Serialization.ifx(plusOp)(Serialization.cst("x"))(Serialization.cst("y"))))),
    expected: "\\x y -> x + y"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "lists",
    description: null,
    subgroups: [],
    cases: [({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.bracketList(Serialization.inlineStyle)([]))),
    expected: "[]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "simple non-empty list",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.bracketList(Serialization.inlineStyle)([Serialization.num(1), Serialization.num(2), Serialization.num(3)]))),
    expected: "[1, 2, 3]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested list",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.bracketList(Serialization.inlineStyle)([Serialization.bracketList(Serialization.inlineStyle)([Serialization.num(1), Serialization.num(3)]), Serialization.num(2)]))),
    expected: "[[1, 3], 2]"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list with parenthesized expression inside",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.bracketList(Serialization.inlineStyle)([Serialization.bracketList(Serialization.inlineStyle)([Serialization.num(1), Serialization.ifx(multOp)(Serialization.ifx(plusOp)(Serialization.num(2))(Serialization.num(3)))(Serialization.ifx(plusOp)(Serialization.num(1))(Serialization.num(10)))]), Serialization.num(2)]))),
    expected: "[[1, (2 + 3) * (1 + 10)], 2]"
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "precedence",
    description: null,
    subgroups: [],
    cases: [({
    name: "operators with different precedence - no parens needed",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(plusOp)(Serialization.ifx(multOp)(Serialization.num(2))(Serialization.num(3)))(Serialization.ifx(multOp)(Serialization.num(1))(Serialization.num(10))))),
    expected: "2 * 3 + 1 * 10"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "operators with different precedence - parens needed",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(multOp)(Serialization.ifx(plusOp)(Serialization.num(2))(Serialization.num(3)))(Serialization.ifx(plusOp)(Serialization.num(1))(Serialization.num(10))))),
    expected: "(2 + 3) * (1 + 10)"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "associative operator left nesting",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(multOp)(Serialization.cst("x"))(Serialization.ifx(multOp)(Serialization.cst("y"))(Serialization.cst("z"))))),
    expected: "x * y * z"
  }) }),
    description: null,
    tags: []
  }), ({
    name: "associative operator right nesting",
    case: ({ tag: "universal", value: ({
    actual: Serialization.printExpr(Serialization.parenthesize(Serialization.ifx(multOp)(Serialization.ifx(multOp)(Serialization.cst("x"))(Serialization.cst("y")))(Serialization.cst("z")))),
    expected: "x * y * z"
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });

export const arrowOp: Ast.Op = Serialization.op("->")(LibMath.negate(1))(({ tag: "right" }));

export const caseOp: Ast.Op = Serialization.op("->")(0)(({ tag: "none" }));

export const gtOp: Ast.Op = Serialization.op(">")(4)(({ tag: "none" }));

export const lambdaOp: Ast.Op = Serialization.op("->")(LibMath.negate(1))(({ tag: "right" }));

export const multOp: Ast.Op = Serialization.op("*")(7)(({ tag: "both" }));

export const plusOp: Ast.Op = Serialization.op("+")(6)(({ tag: "both" }));

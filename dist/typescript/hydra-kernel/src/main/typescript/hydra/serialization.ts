// Note: this is an automatically generated file. Do not edit.

/**
 * Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLiterals from "./lib/literals.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMath from "./lib/math.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export const angleBraces: Ast.Brackets = ({
    open: "<",
    close: ">"
  });

export function angleBracesList(style: Ast.BlockStyle): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((els: ReadonlyArray<Ast.Expr>) => LibLogic.ifElse(LibLists.null_(els))(cst("<>"))(brackets(angleBraces)(style)(commaSep(style)(els))));
}

export function bracesListAdaptive(els: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return (() => {
  const inlineList = curlyBracesList(null)(inlineStyle)(els);
  return LibLogic.ifElse(LibEquality.gt(expressionLength(inlineList))(70))(curlyBracesList(null)(halfBlockStyle)(els))(inlineList);
})();
}

export function bracketList(style: Ast.BlockStyle): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((els: ReadonlyArray<Ast.Expr>) => LibLogic.ifElse(LibLists.null_(els))(cst("[]"))(brackets(squareBrackets)(style)(commaSep(style)(els))));
}

export function bracketListAdaptive(els: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return (() => {
  const inlineList = bracketList(inlineStyle)(els);
  return LibLogic.ifElse(LibEquality.gt(expressionLength(inlineList))(70))(bracketList(halfBlockStyle)(els))(inlineList);
})();
}

export function brackets(br: Ast.Brackets): ((x: Ast.BlockStyle) => ((x: Ast.Expr) => Ast.Expr)) {
  return ((style: Ast.BlockStyle) => ((e: Ast.Expr) => ({ tag: "brackets", value: ({
    brackets: br,
    enclosed: e,
    style: style
  }) })));
}

export function commaSep(v1: Ast.BlockStyle): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((v2: ReadonlyArray<Ast.Expr>) => symbolSep(",")(v1)(v2));
}

export function cst(s: string): Ast.Expr {
  return ({ tag: "const", value: sym(s) });
}

export function curlyBlock(style: Ast.BlockStyle): ((x: Ast.Expr) => Ast.Expr) {
  return ((e: Ast.Expr) => curlyBracesList(null)(style)([e]));
}

export const curlyBraces: Ast.Brackets = ({
    open: "{",
    close: "}"
  });

export function curlyBracesList(msymb: string | null): ((x: Ast.BlockStyle) => ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr)) {
  return ((style: Ast.BlockStyle) => ((els: ReadonlyArray<Ast.Expr>) => LibLogic.ifElse(LibLists.null_(els))(cst("{}"))(brackets(curlyBraces)(style)(symbolSep(LibMaybes.fromMaybe(",")(msymb))(style)(els)))));
}

export function customIndent(idt: string): ((x: string) => string) {
  return ((s: string) => LibStrings.cat(LibLists.intersperse("\n")(LibLists.map(((line: string) => LibStrings.cat2(idt)(line)))(LibStrings.lines(s)))));
}

export function customIndentBlock(idt: string): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((els: ReadonlyArray<Ast.Expr>) => (() => {
  const idtOp = ({
    symbol: sym(""),
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "breakAndIndent", value: idt })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });
  return LibMaybes.maybe(cst(""))(((head: Ast.Expr) => LibLogic.ifElse(LibEquality.equal(LibLists.length(els))(1))(head)(ifx(idtOp)(head)(newlineSep(LibLists.drop(1)(els))))))(LibLists.safeHead(els));
})());
}

export function dotSep(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return sep(({
    symbol: sym("."),
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "none" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(v1);
}

export function doubleNewlineSep(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return sep(({
    symbol: sym(""),
    padding: ({
    left: ({ tag: "break" }),
    right: ({ tag: "break" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(v1);
}

export const doubleSpace: string = "  ";

export function expressionLength(e: Ast.Expr): number {
  return (() => {
  const symbolLength = ((s: Ast.Symbol) => LibStrings.length(((_x) => _x)(s)));
  return (() => {
  const wsLength = ((ws: Ast.Ws) => (() => {
  const _m = ws;
  switch (_m.tag) {
    case "none": return ((_: void) => 0)((_m as any).value);
    case "space": return ((_: void) => 1)((_m as any).value);
    case "break": return ((_: void) => 10000)((_m as any).value);
    case "breakAndIndent": return ((s: string) => 10000)((_m as any).value);
    case "doubleBreak": return ((_: void) => 10000)((_m as any).value);
  }
})());
  return (() => {
  const blockStyleLength = ((style: Ast.BlockStyle) => (() => {
  const mindentLen = LibMaybes.maybe(0)(LibStrings.length)(((_x) => _x.indent)(style));
  return (() => {
  const nlBeforeLen = LibLogic.ifElse(((_x) => _x.newlineBeforeContent)(style))(1)(0);
  return (() => {
  const nlAfterLen = LibLogic.ifElse(((_x) => _x.newlineAfterContent)(style))(1)(0);
  return LibMath.add(mindentLen)(LibMath.add(nlBeforeLen)(nlAfterLen));
})();
})();
})());
  return (() => {
  const bracketsLength = ((brackets: Ast.Brackets) => LibMath.add(symbolLength(((_x) => _x.open)(brackets)))(symbolLength(((_x) => _x.close)(brackets))));
  return (() => {
  const bracketExprLength = ((be: Ast.BracketExpr) => LibMath.add(bracketsLength(((_x) => _x.brackets)(be)))(LibMath.add(expressionLength(((_x) => _x.enclosed)(be)))(blockStyleLength(((_x) => _x.style)(be)))));
  return (() => {
  const indentedExpressionLength = ((ie: Ast.IndentedExpression) => (() => {
  const baseLen = expressionLength(((_x) => _x.expr)(ie));
  return (() => {
  const indentLen = (() => {
  const _m = ((_x) => _x.style)(ie);
  switch (_m.tag) {
    case "allLines": return ((s: string) => LibStrings.length(s))((_m as any).value);
    case "subsequentLines": return ((s: string) => LibStrings.length(s))((_m as any).value);
  }
})();
  return LibMath.add(baseLen)(indentLen);
})();
})());
  return (() => {
  const opLength = ((op: Ast.Op) => (() => {
  const symLen = symbolLength(((_x) => _x.symbol)(op));
  return (() => {
  const padding = ((_x) => _x.padding)(op);
  return (() => {
  const leftLen = wsLength(((_x) => _x.left)(padding));
  return (() => {
  const rightLen = wsLength(((_x) => _x.right)(padding));
  return LibMath.add(symLen)(LibMath.add(leftLen)(rightLen));
})();
})();
})();
})());
  return (() => {
  const opExprLength = ((oe: Ast.OpExpr) => (() => {
  const opLen = opLength(((_x) => _x.op)(oe));
  return (() => {
  const leftLen = expressionLength(((_x) => _x.lhs)(oe));
  return (() => {
  const rightLen = expressionLength(((_x) => _x.rhs)(oe));
  return LibMath.add(opLen)(LibMath.add(leftLen)(rightLen));
})();
})();
})());
  return (() => {
  const seqExprLength = ((se: Ast.SeqExpr) => (() => {
  const sopLen = opLength(((_x) => _x.op)(se));
  return (() => {
  const elementLens = LibLists.map(expressionLength)(((_x) => _x.elements)(se));
  return (() => {
  const totalElLen = LibLists.foldl(LibMath.add)(0)(elementLens);
  return (() => {
  const numSeps = LibMath.sub(LibLists.length(((_x) => _x.elements)(se)))(1);
  return LibMath.add(totalElLen)(LibMath.mul(sopLen)(LibLogic.ifElse(LibEquality.gt(numSeps)(0))(numSeps)(0)));
})();
})();
})();
})());
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "const": return ((s: Ast.Symbol) => symbolLength(s))((_m as any).value);
    case "indent": return ((ie: Ast.IndentedExpression) => indentedExpressionLength(ie))((_m as any).value);
    case "op": return ((oe: Ast.OpExpr) => opExprLength(oe))((_m as any).value);
    case "brackets": return ((be: Ast.BracketExpr) => bracketExprLength(be))((_m as any).value);
    case "seq": return ((se: Ast.SeqExpr) => seqExprLength(se))((_m as any).value);
  }
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
}

export const fullBlockStyle: Ast.BlockStyle = ({
    indent: doubleSpace,
    newlineBeforeContent: true,
    newlineAfterContent: true
  });

export const halfBlockStyle: Ast.BlockStyle = ({
    indent: doubleSpace,
    newlineBeforeContent: true,
    newlineAfterContent: false
  });

export function ifx(op: Ast.Op): ((x: Ast.Expr) => ((x: Ast.Expr) => Ast.Expr)) {
  return ((lhs: Ast.Expr) => ((rhs: Ast.Expr) => ({ tag: "op", value: ({
    op: op,
    lhs: lhs,
    rhs: rhs
  }) })));
}

export function indent(v1: string): string {
  return customIndent(doubleSpace)(v1);
}

export function indentBlock(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return customIndentBlock(doubleSpace)(v1);
}

export function indentSubsequentLines(idt: string): ((x: Ast.Expr) => Ast.Expr) {
  return ((e: Ast.Expr) => ({ tag: "indent", value: ({
    style: ({ tag: "subsequentLines", value: idt }),
    expr: e
  }) }));
}

export function infixWs(op: string): ((x: Ast.Expr) => ((x: Ast.Expr) => Ast.Expr)) {
  return ((l: Ast.Expr) => ((r: Ast.Expr) => spaceSep([l, cst(op), r])));
}

export function infixWsList(op: string): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((opers: ReadonlyArray<Ast.Expr>) => (() => {
  const opExpr = cst(op);
  return (() => {
  const foldFun = ((e: ReadonlyArray<Ast.Expr>) => ((r: Ast.Expr) => LibLogic.ifElse(LibLists.null_(e))([r])(LibLists.cons(r)(LibLists.cons(opExpr)(e)))));
  return spaceSep(LibLists.foldl(foldFun)([])(LibLists.reverse(opers)));
})();
})());
}

export const inlineStyle: Ast.BlockStyle = ({
    indent: null,
    newlineBeforeContent: false,
    newlineAfterContent: false
  });

export function newlineSep(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return sep(({
    symbol: sym(""),
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "break" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(v1);
}

export const noPadding: Ast.Padding = ({
    left: ({ tag: "none" }),
    right: ({ tag: "none" })
  });

export function noSep(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return sep(({
    symbol: sym(""),
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "none" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(v1);
}

export function num(i: number): Ast.Expr {
  return cst(LibLiterals.showInt32(i));
}

export function op(s: string): ((x: number) => ((x: Ast.Associativity) => Ast.Op)) {
  return ((p: number) => ((assoc: Ast.Associativity) => ({
    symbol: sym(s),
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "space" })
  }),
    precedence: p,
    associativity: assoc
  })));
}

export function orOp(newlines: boolean): Ast.Op {
  return ({
    symbol: sym("|"),
    padding: ({
    left: ({ tag: "space" }),
    right: LibLogic.ifElse(newlines)(({ tag: "break" }))(({ tag: "space" }))
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });
}

export function orSep(style: Ast.BlockStyle): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((l: ReadonlyArray<Ast.Expr>) => (() => {
  const newlines = ((_x) => _x.newlineBeforeContent)(style);
  return LibMaybes.maybe(cst(""))(((h: Ast.Expr) => LibLists.foldl(((acc: Ast.Expr) => ((el: Ast.Expr) => ifx(orOp(newlines))(acc)(el))))(h)(LibLists.drop(1)(l))))(LibLists.safeHead(l));
})());
}

export function parenList(newlines: boolean): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((els: ReadonlyArray<Ast.Expr>) => (() => {
  const style = LibLogic.ifElse(LibLogic.and(newlines)(LibEquality.gt(LibLists.length(els))(1)))(halfBlockStyle)(inlineStyle);
  return LibLogic.ifElse(LibLists.null_(els))(cst("()"))(brackets(parentheses)(style)(commaSep(style)(els)));
})());
}

export function parens(v1: Ast.Expr): Ast.Expr {
  return brackets(parentheses)(inlineStyle)(v1);
}

export const parentheses: Ast.Brackets = ({
    open: "(",
    close: ")"
  });

export function parenthesize(exp: Ast.Expr): Ast.Expr {
  return (() => {
  const assocLeft = ((a: Ast.Associativity) => (() => {
  const _m = a;
  switch (_m.tag) {
    case "right": return ((_: void) => false)((_m as any).value);
    default: return true(_m);
  }
})());
  return (() => {
  const assocRight = ((a: Ast.Associativity) => (() => {
  const _m = a;
  switch (_m.tag) {
    case "left": return ((_: void) => false)((_m as any).value);
    default: return true(_m);
  }
})());
  return (() => {
  const _m = exp;
  switch (_m.tag) {
    case "brackets": return ((bracketExpr: Ast.BracketExpr) => ({ tag: "brackets", value: ({
    brackets: ((_x) => _x.brackets)(bracketExpr),
    enclosed: parenthesize(((_x) => _x.enclosed)(bracketExpr)),
    style: ((_x) => _x.style)(bracketExpr)
  }) }))((_m as any).value);
    case "const": return ((ignored: Ast.Symbol) => exp)((_m as any).value);
    case "indent": return ((indentExpr: Ast.IndentedExpression) => ({ tag: "indent", value: ({
    style: ((_x) => _x.style)(indentExpr),
    expr: parenthesize(((_x) => _x.expr)(indentExpr))
  }) }))((_m as any).value);
    case "seq": return ((seqExpr: Ast.SeqExpr) => ({ tag: "seq", value: ({
    op: ((_x) => _x.op)(seqExpr),
    elements: LibLists.map(parenthesize)(((_x) => _x.elements)(seqExpr))
  }) }))((_m as any).value);
    case "op": return ((opExpr: Ast.OpExpr) => (() => {
  const op = ((_x) => _x.op)(opExpr);
  return (() => {
  const prec = ((_x) => _x)(((_x) => _x.precedence)(op));
  return (() => {
  const assoc = ((_x) => _x.associativity)(op);
  return (() => {
  const lhs = ((_x) => _x.lhs)(opExpr);
  return (() => {
  const rhs = ((_x) => _x.rhs)(opExpr);
  return (() => {
  const lhs_ = parenthesize(lhs);
  return (() => {
  const rhs_ = parenthesize(rhs);
  return (() => {
  const lhs2 = (() => {
  const _m = lhs_;
  switch (_m.tag) {
    case "op": return ((lopExpr: Ast.OpExpr) => (() => {
  const lop = ((_x) => _x.op)(lopExpr);
  return (() => {
  const lprec = ((_x) => _x)(((_x) => _x.precedence)(lop));
  return (() => {
  const lassoc = ((_x) => _x.associativity)(lop);
  return (() => {
  const comparison = LibEquality.compare(prec)(lprec);
  return (() => {
  const _m = comparison;
  switch (_m.tag) {
    case "lessThan": return ((_: void) => lhs_)((_m as any).value);
    case "greaterThan": return ((_: void) => parens(lhs_))((_m as any).value);
    case "equalTo": return ((_: void) => LibLogic.ifElse(LibLogic.and(assocLeft(assoc))(assocLeft(lassoc)))(lhs_)(parens(lhs_)))((_m as any).value);
  }
})();
})();
})();
})();
})())((_m as any).value);
    default: return lhs_(_m);
  }
})();
  return (() => {
  const rhs2 = (() => {
  const _m = rhs_;
  switch (_m.tag) {
    case "op": return ((ropExpr: Ast.OpExpr) => (() => {
  const rop = ((_x) => _x.op)(ropExpr);
  return (() => {
  const rprec = ((_x) => _x)(((_x) => _x.precedence)(rop));
  return (() => {
  const rassoc = ((_x) => _x.associativity)(rop);
  return (() => {
  const comparison = LibEquality.compare(prec)(rprec);
  return (() => {
  const _m = comparison;
  switch (_m.tag) {
    case "lessThan": return ((_: void) => rhs_)((_m as any).value);
    case "greaterThan": return ((_: void) => parens(rhs_))((_m as any).value);
    case "equalTo": return ((_: void) => LibLogic.ifElse(LibLogic.and(assocRight(assoc))(assocRight(rassoc)))(rhs_)(parens(rhs_)))((_m as any).value);
  }
})();
})();
})();
})();
})())((_m as any).value);
    default: return rhs_(_m);
  }
})();
  return ({ tag: "op", value: ({
    op: op,
    lhs: lhs2,
    rhs: rhs2
  }) });
})();
})();
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
  }
})();
})();
})();
}

export function prefix(p: string): ((x: Ast.Expr) => Ast.Expr) {
  return ((expr: Ast.Expr) => (() => {
  const preOp = ({
    symbol: sym(p),
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "none" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });
  return ifx(preOp)(cst(""))(expr);
})());
}

export function printExpr(e: Ast.Expr): string {
  return (() => {
  const pad = ((ws: Ast.Ws) => (() => {
  const _m = ws;
  switch (_m.tag) {
    case "none": return ((_: void) => "")((_m as any).value);
    case "space": return ((_: void) => " ")((_m as any).value);
    case "break": return ((_: void) => "\n")((_m as any).value);
    case "breakAndIndent": return ((ignored: string) => "\n")((_m as any).value);
    case "doubleBreak": return ((_: void) => "\n\n")((_m as any).value);
  }
})());
  return (() => {
  const idt = ((ws: Ast.Ws) => ((s: string) => (() => {
  const _m = ws;
  switch (_m.tag) {
    case "breakAndIndent": return ((indentStr: string) => customIndent(indentStr)(s))((_m as any).value);
    default: return s(_m);
  }
})()));
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "const": return ((symbol: Ast.Symbol) => ((_x) => _x)(symbol))((_m as any).value);
    case "indent": return ((indentExpr: Ast.IndentedExpression) => (() => {
  const style = ((_x) => _x.style)(indentExpr);
  return (() => {
  const expr = ((_x) => _x.expr)(indentExpr);
  return (() => {
  const lns = LibStrings.lines(printExpr(expr));
  return (() => {
  const ilns = (() => {
  const _m = style;
  switch (_m.tag) {
    case "allLines": return ((idt2: string) => LibLists.map(((line: string) => LibStrings.cat2(idt2)(line)))(lns))((_m as any).value);
    case "subsequentLines": return ((idt2: string) => LibLogic.ifElse(LibEquality.equal(LibLists.length(lns))(1))(lns)(LibLists.cons(LibLists.head(lns))(LibLists.map(((line: string) => LibStrings.cat2(idt2)(line)))(LibLists.tail(lns)))))((_m as any).value);
  }
})();
  return LibStrings.intercalate("\n")(ilns);
})();
})();
})();
})())((_m as any).value);
    case "seq": return ((seqExpr: Ast.SeqExpr) => (() => {
  const sop = ((_x) => _x.op)(seqExpr);
  return (() => {
  const ssym = ((_x) => _x)(((_x) => _x.symbol)(sop));
  return (() => {
  const spadding = ((_x) => _x.padding)(sop);
  return (() => {
  const spadl = ((_x) => _x.left)(spadding);
  return (() => {
  const spadr = ((_x) => _x.right)(spadding);
  return (() => {
  const selements = ((_x) => _x.elements)(seqExpr);
  return (() => {
  const separator = LibStrings.cat2(LibStrings.cat2(pad(spadl))(ssym))(pad(spadr));
  return (() => {
  const printedElements = LibLists.map(((el: Ast.Expr) => idt(spadr)(printExpr(el))))(selements);
  return LibStrings.intercalate(separator)(printedElements);
})();
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
    case "op": return ((opExpr: Ast.OpExpr) => (() => {
  const op = ((_x) => _x.op)(opExpr);
  return (() => {
  const sym = ((_x) => _x)(((_x) => _x.symbol)(op));
  return (() => {
  const padding = ((_x) => _x.padding)(op);
  return (() => {
  const padl = ((_x) => _x.left)(padding);
  return (() => {
  const padr = ((_x) => _x.right)(padding);
  return (() => {
  const l = ((_x) => _x.lhs)(opExpr);
  return (() => {
  const r = ((_x) => _x.rhs)(opExpr);
  return (() => {
  const lhs = idt(padl)(printExpr(l));
  return (() => {
  const rhs = idt(padr)(printExpr(r));
  return LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(lhs)(pad(padl)))(sym))(pad(padr)))(rhs);
})();
})();
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
    case "brackets": return ((bracketExpr: Ast.BracketExpr) => (() => {
  const brs = ((_x) => _x.brackets)(bracketExpr);
  return (() => {
  const l = ((_x) => _x)(((_x) => _x.open)(brs));
  return (() => {
  const r = ((_x) => _x)(((_x) => _x.close)(brs));
  return (() => {
  const e = ((_x) => _x.enclosed)(bracketExpr);
  return (() => {
  const style = ((_x) => _x.style)(bracketExpr);
  return (() => {
  const body = printExpr(e);
  return (() => {
  const doIndent = ((_x) => _x.indent)(style);
  return (() => {
  const nlBefore = ((_x) => _x.newlineBeforeContent)(style);
  return (() => {
  const nlAfter = ((_x) => _x.newlineAfterContent)(style);
  return (() => {
  const ibody = LibMaybes.maybe(body)(((idt2: string) => customIndent(idt2)(body)))(doIndent);
  return (() => {
  const pre = LibLogic.ifElse(nlBefore)("\n")("");
  return (() => {
  const suf = LibLogic.ifElse(nlAfter)("\n")("");
  return LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(l)(pre))(ibody))(suf))(r);
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
  }
})();
})();
})();
}

export function semicolonSep(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return symbolSep(";")(inlineStyle)(v1);
}

export function sep(op: Ast.Op): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((els: ReadonlyArray<Ast.Expr>) => LibMaybes.maybe(cst(""))(((h: Ast.Expr) => LibLists.foldl(((acc: Ast.Expr) => ((el: Ast.Expr) => ifx(op)(acc)(el))))(h)(LibLists.drop(1)(els))))(LibLists.safeHead(els)));
}

export function spaceSep(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return sep(({
    symbol: sym(""),
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "none" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(v1);
}

export const squareBrackets: Ast.Brackets = ({
    open: "[",
    close: "]"
  });

export function structuralSep(op: Ast.Op): ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr) {
  return ((els: ReadonlyArray<Ast.Expr>) => LibLogic.ifElse(LibLists.null_(els))(cst(""))(LibLogic.ifElse(LibEquality.equal(LibLists.length(els))(1))(LibLists.head(els))(({ tag: "seq", value: ({
    op: op,
    elements: els
  }) }))));
}

export function structuralSpaceSep(v1: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return structuralSep(({
    symbol: sym(""),
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "none" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  }))(v1);
}

export function suffix(s: string): ((x: Ast.Expr) => Ast.Expr) {
  return ((expr: Ast.Expr) => (() => {
  const sufOp = ({
    symbol: sym(s),
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "none" })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });
  return ifx(sufOp)(expr)(cst(""));
})());
}

export function sym(s: string): Ast.Symbol {
  return s;
}

export function symbolSep(symb: string): ((x: Ast.BlockStyle) => ((x: ReadonlyArray<Ast.Expr>) => Ast.Expr)) {
  return ((style: Ast.BlockStyle) => ((l: ReadonlyArray<Ast.Expr>) => (() => {
  const breakCount = LibLists.length(LibLists.filter(((x_: boolean) => x_))([((_x) => _x.newlineBeforeContent)(style), ((_x) => _x.newlineAfterContent)(style)]));
  return (() => {
  const break_ = LibLogic.ifElse(LibEquality.equal(breakCount)(0))(({ tag: "space" }))(LibLogic.ifElse(LibEquality.equal(breakCount)(1))(({ tag: "break" }))(({ tag: "doubleBreak" })));
  return (() => {
  const commaOp = ({
    symbol: sym(symb),
    padding: ({
    left: ({ tag: "none" }),
    right: break_
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });
  return LibMaybes.maybe(cst(""))(((h: Ast.Expr) => LibLists.foldl(((acc: Ast.Expr) => ((el: Ast.Expr) => ifx(commaOp)(acc)(el))))(h)(LibLists.drop(1)(l))))(LibLists.safeHead(l));
})();
})();
})()));
}

export function tabIndent(e: Ast.Expr): Ast.Expr {
  return ({ tag: "indent", value: ({
    style: ({ tag: "allLines", value: "    " }),
    expr: e
  }) });
}

export function tabIndentDoubleSpace(exprs: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return tabIndent(doubleNewlineSep(exprs));
}

export function tabIndentSingleSpace(exprs: ReadonlyArray<Ast.Expr>): Ast.Expr {
  return tabIndent(newlineSep(exprs));
}

export function unsupportedType(label: string): Ast.Expr {
  return cst(LibStrings.cat2(LibStrings.cat2("[")(label))("]"));
}

export function unsupportedVariant(label: string): ((x: string) => Ast.Expr) {
  return ((obj: string) => cst(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("[unsupported ")(label))(": "))(LibLiterals.showString(obj)))("]")));
}

export function withComma(e: Ast.Expr): Ast.Expr {
  return noSep([e, cst(",")]);
}

export function withSemi(e: Ast.Expr): Ast.Expr {
  return noSep([e, cst(";")]);
}

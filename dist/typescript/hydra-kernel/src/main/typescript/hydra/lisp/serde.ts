// Note: this is an automatically generated file. Do not edit.

/**
 * Lisp serializer: converts Lisp AST to concrete syntax for Clojure, Emacs Lisp, Common Lisp, or Scheme
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Constants from "../constants.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as LispSyntax from "./syntax.js";
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

export function andExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.AndExpression) => Ast.Expr) {
  return ((andExpr: LispSyntax.AndExpression) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("and")])(LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.expressions)(andExpr))))));
}

export function applicationToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.Application) => Ast.Expr) {
  return ((app: LispSyntax.Application) => (() => {
  const funExpr = ((_x) => _x.function)(app);
  const fun = expressionToExpr(d)(funExpr);
  const args = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.arguments)(app));
  const needsFuncall = (() => {
  const _m = d;
  switch (_m.tag) {
    case "emacsLisp": return ((u: void) => (() => {
  const _m = funExpr;
  switch (_m.tag) {
    case "variable": return ((s: LispSyntax.VariableReference) => false)((_m as any).value);
    default: return true(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})();
  const allParts = LibLogic.ifElse(needsFuncall)(LibLists.concat2([Serialization.cst("funcall"), fun])(args))(LibLists.concat2([fun])(args));
  return Serialization.parens(Serialization.spaceSep(allParts));
})());
}

export function caseExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.CaseExpression) => Ast.Expr) {
  return ((caseExpr: LispSyntax.CaseExpression) => (() => {
  const scrutinee = expressionToExpr(d)(((_x) => _x.scrutinee)(caseExpr));
  const clauses = ((_x) => _x.clauses)(caseExpr);
  const dflt = ((_x) => _x.default)(caseExpr);
  const clauseExprs = LibLists.map(((c: LispSyntax.CaseClause) => Serialization.parens(Serialization.spaceSep([Serialization.parens(Serialization.spaceSep(LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.keys)(c)))), expressionToExpr(d)(((_x) => _x.body)(c))]))))(clauses);
  const defaultPart = LibMaybes.maybe([])(((e: LispSyntax.Expression) => [Serialization.parens(Serialization.spaceSep([Serialization.cst("else"), expressionToExpr(d)(e)]))]))(dflt);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("case"), scrutinee], clauseExprs, defaultPart])));
})());
}

export function commentToExpr(c: LispSyntax.Comment): Ast.Expr {
  return (() => {
  const text = ((_x) => _x.text)(c);
  return Serialization.cst(LibStrings.cat2("; ")(text));
})();
}

export function condExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.CondExpression) => Ast.Expr) {
  return ((condExpr: LispSyntax.CondExpression) => (() => {
  const clauses = ((_x) => _x.clauses)(condExpr);
  const dflt = ((_x) => _x.default)(condExpr);
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => (() => {
  const clauseExprs = LibLists.concat(LibLists.map(((c: LispSyntax.CondClause) => [expressionToExpr(d)(((_x) => _x.condition)(c)), expressionToExpr(d)(((_x) => _x.body)(c))]))(clauses));
  const defaultPart = LibMaybes.maybe([])(((e: LispSyntax.Expression) => [Serialization.cst(":else"), expressionToExpr(d)(e)]))(dflt);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("cond")], clauseExprs, defaultPart])));
})())((_m as any).value);
    case "emacsLisp": return ((_: void) => (() => {
  const clauseExprs = LibLists.map(((c: LispSyntax.CondClause) => Serialization.parens(Serialization.spaceSep([expressionToExpr(d)(((_x) => _x.condition)(c)), expressionToExpr(d)(((_x) => _x.body)(c))]))))(clauses);
  const defaultPart = LibMaybes.maybe([])(((e: LispSyntax.Expression) => [Serialization.parens(Serialization.spaceSep([Serialization.cst("t"), expressionToExpr(d)(e)]))]))(dflt);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("cond")], clauseExprs, defaultPart])));
})())((_m as any).value);
    case "commonLisp": return ((_: void) => (() => {
  const clauseExprs = LibLists.map(((c: LispSyntax.CondClause) => Serialization.parens(Serialization.spaceSep([expressionToExpr(d)(((_x) => _x.condition)(c)), expressionToExpr(d)(((_x) => _x.body)(c))]))))(clauses);
  const defaultPart = LibMaybes.maybe([])(((e: LispSyntax.Expression) => [Serialization.parens(Serialization.spaceSep([Serialization.cst("t"), expressionToExpr(d)(e)]))]))(dflt);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("cond")], clauseExprs, defaultPart])));
})())((_m as any).value);
    case "scheme": return ((_: void) => (() => {
  const clauseExprs = LibLists.map(((c: LispSyntax.CondClause) => Serialization.parens(Serialization.spaceSep([expressionToExpr(d)(((_x) => _x.condition)(c)), expressionToExpr(d)(((_x) => _x.body)(c))]))))(clauses);
  const defaultPart = LibMaybes.maybe([])(((e: LispSyntax.Expression) => [Serialization.parens(Serialization.spaceSep([Serialization.cst("else"), expressionToExpr(d)(e)]))]))(dflt);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("cond")], clauseExprs, defaultPart])));
})())((_m as any).value);
  }
})();
})());
}

export function constantDefinitionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.ConstantDefinition) => Ast.Expr) {
  return ((cdef: LispSyntax.ConstantDefinition) => (() => {
  const name = symbolToExpr(((_x) => _x.name)(cdef));
  const value = expressionToExpr(d)(((_x) => _x.value)(cdef));
  return Serialization.parens(Serialization.spaceSep([Serialization.cst(defconstKeyword(d)), name, value]));
})());
}

export function defKeyword(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "def")((_m as any).value);
    case "emacsLisp": return ((_: void) => "defvar")((_m as any).value);
    case "commonLisp": return ((_: void) => "cl:defvar")((_m as any).value);
    case "scheme": return ((_: void) => "define")((_m as any).value);
  }
})();
}

export function defconstKeyword(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "def")((_m as any).value);
    case "emacsLisp": return ((_: void) => "defconst")((_m as any).value);
    case "commonLisp": return ((_: void) => "cl:defconstant")((_m as any).value);
    case "scheme": return ((_: void) => "define")((_m as any).value);
  }
})();
}

export function defnKeyword(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "defn")((_m as any).value);
    case "emacsLisp": return ((_: void) => "defun")((_m as any).value);
    case "commonLisp": return ((_: void) => "cl:defun")((_m as any).value);
    case "scheme": return ((_: void) => "define")((_m as any).value);
  }
})();
}

export function defrecordKeyword(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "defrecord")((_m as any).value);
    case "emacsLisp": return ((_: void) => "cl-defstruct")((_m as any).value);
    case "commonLisp": return ((_: void) => "cl:defstruct")((_m as any).value);
    case "scheme": return ((_: void) => "define-record-type")((_m as any).value);
  }
})();
}

export function doExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.DoExpression) => Ast.Expr) {
  return ((doExpr: LispSyntax.DoExpression) => (() => {
  const kw = (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "do")((_m as any).value);
    case "emacsLisp": return ((_: void) => "progn")((_m as any).value);
    case "commonLisp": return ((_: void) => "progn")((_m as any).value);
    case "scheme": return ((_: void) => "begin")((_m as any).value);
  }
})();
  return Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst(kw)])(LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.expressions)(doExpr)))));
})());
}

export function docstringToExpr(ds: LispSyntax.Docstring): Ast.Expr {
  return Serialization.cst(LibStrings.cat([";; ", ((_x) => _x)(ds)]));
}

export function exportDeclarationToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.ExportDeclaration) => Ast.Expr) {
  return ((edecl: LispSyntax.ExportDeclaration) => (() => {
  const syms = LibLists.map(symbolToExpr)(((_x) => _x.symbols)(edecl));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.cst(""))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.newlineSep(LibLists.map(((s: Ast.Expr) => Serialization.parens(Serialization.spaceSep([Serialization.cst("provide"), Serialization.noSep([Serialization.cst("'"), s])]))))(syms)))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst(":export")])(LibLists.map(((s: Ast.Expr) => Serialization.noSep([Serialization.cst(":"), s])))(syms)))))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("export")])(syms))))((_m as any).value);
  }
})();
})());
}

export function expressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.Expression) => Ast.Expr) {
  return ((expr: LispSyntax.Expression) => (() => {
  const _m = expr;
  switch (_m.tag) {
    case "application": return ((a: LispSyntax.Application) => applicationToExpr(d)(a))((_m as any).value);
    case "lambda": return ((l: LispSyntax.Lambda) => lambdaToExpr(d)(l))((_m as any).value);
    case "let": return ((l: LispSyntax.LetExpression) => letExpressionToExpr(d)(l))((_m as any).value);
    case "if": return ((i: LispSyntax.IfExpression) => ifExpressionToExpr(d)(i))((_m as any).value);
    case "cond": return ((c: LispSyntax.CondExpression) => condExpressionToExpr(d)(c))((_m as any).value);
    case "case": return ((c: LispSyntax.CaseExpression) => caseExpressionToExpr(d)(c))((_m as any).value);
    case "and": return ((a: LispSyntax.AndExpression) => andExpressionToExpr(d)(a))((_m as any).value);
    case "or": return ((o: LispSyntax.OrExpression) => orExpressionToExpr(d)(o))((_m as any).value);
    case "not": return ((n: LispSyntax.NotExpression) => notExpressionToExpr(d)(n))((_m as any).value);
    case "do": return ((e: LispSyntax.DoExpression) => doExpressionToExpr(d)(e))((_m as any).value);
    case "begin": return ((e: LispSyntax.BeginExpression) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("begin")])(LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.expressions)(e))))))((_m as any).value);
    case "variable": return ((v: LispSyntax.VariableReference) => variableReferenceToExpr(d)(v))((_m as any).value);
    case "literal": return ((l: LispSyntax.Literal) => literalToExpr(d)(l))((_m as any).value);
    case "list": return ((l: LispSyntax.ListLiteral) => listLiteralToExpr(d)(l))((_m as any).value);
    case "vector": return ((v: LispSyntax.VectorLiteral) => vectorLiteralToExpr(d)(v))((_m as any).value);
    case "map": return ((m: LispSyntax.MapLiteral) => mapLiteralToExpr(d)(m))((_m as any).value);
    case "set": return ((s: LispSyntax.SetLiteral) => setLiteralToExpr(d)(s))((_m as any).value);
    case "cons": return ((c: LispSyntax.ConsExpression) => Serialization.parens(Serialization.spaceSep([Serialization.cst("cons"), expressionToExpr(d)(((_x) => _x.head)(c)), expressionToExpr(d)(((_x) => _x.tail)(c))])))((_m as any).value);
    case "dottedPair": return ((p: LispSyntax.DottedPair) => Serialization.parens(Serialization.spaceSep([expressionToExpr(d)(((_x) => _x.car)(p)), Serialization.cst("."), expressionToExpr(d)(((_x) => _x.cdr)(p))])))((_m as any).value);
    case "fieldAccess": return ((fa: LispSyntax.FieldAccess) => fieldAccessToExpr(d)(fa))((_m as any).value);
    case "typeAnnotation": return ((ta: LispSyntax.TypeAnnotation) => expressionToExpr(d)(((_x) => _x.expression)(ta)))((_m as any).value);
    case "quote": return ((q: LispSyntax.QuoteExpression) => Serialization.noSep([Serialization.cst("'"), expressionToExpr(d)(((_x) => _x.body)(q))]))((_m as any).value);
    case "quasiquote": return ((q: LispSyntax.QuasiquoteExpression) => Serialization.noSep([Serialization.cst("`"), expressionToExpr(d)(((_x) => _x.body)(q))]))((_m as any).value);
    case "unquote": return ((u: LispSyntax.UnquoteExpression) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.noSep([Serialization.cst("~"), expressionToExpr(d)(((_x) => _x.body)(u))]))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.noSep([Serialization.cst(","), expressionToExpr(d)(((_x) => _x.body)(u))]))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.noSep([Serialization.cst(","), expressionToExpr(d)(((_x) => _x.body)(u))]))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.noSep([Serialization.cst(","), expressionToExpr(d)(((_x) => _x.body)(u))]))((_m as any).value);
  }
})())((_m as any).value);
    case "splicingUnquote": return ((su: LispSyntax.SplicingUnquoteExpression) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.noSep([Serialization.cst("~@"), expressionToExpr(d)(((_x) => _x.body)(su))]))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.noSep([Serialization.cst(",@"), expressionToExpr(d)(((_x) => _x.body)(su))]))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.noSep([Serialization.cst(",@"), expressionToExpr(d)(((_x) => _x.body)(su))]))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.noSep([Serialization.cst(",@"), expressionToExpr(d)(((_x) => _x.body)(su))]))((_m as any).value);
  }
})())((_m as any).value);
    case "sExpression": return ((s: LispSyntax.SExpression) => sExpressionToExpr(s))((_m as any).value);
  }
})());
}

export function falseExpr(d: LispSyntax.Dialect): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.cst("false"))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.cst("nil"))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.cst("cl:nil"))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.cst("#f"))((_m as any).value);
  }
})();
}

export function fieldAccessToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.FieldAccess) => Ast.Expr) {
  return ((fa: LispSyntax.FieldAccess) => (() => {
  const rtype = symbolToExpr(((_x) => _x.recordType)(fa));
  const field = symbolToExpr(((_x) => _x.field)(fa));
  const target = expressionToExpr(d)(((_x) => _x.target)(fa));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.noSep([Serialization.cst(":"), field]), target])))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.noSep([rtype, Serialization.cst("-"), field]), target])))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.noSep([rtype, Serialization.cst("-"), field]), target])))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.noSep([rtype, Serialization.cst("-"), field]), target])))((_m as any).value);
  }
})();
})());
}

export function formatLispFloat(d: LispSyntax.Dialect): ((x: number) => string) {
  return ((v: number) => (() => {
  const s = LibLiterals.showBigfloat(v);
  return LibLogic.ifElse(LibEquality.equal(s)("NaN"))((() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "Double/NaN")((_m as any).value);
    case "scheme": return ((_: void) => "+nan.0")((_m as any).value);
    case "commonLisp": return ((_: void) => "+hydra-nan+")((_m as any).value);
    case "emacsLisp": return ((_: void) => "0.0e+NaN")((_m as any).value);
  }
})())(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))((() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "Double/POSITIVE_INFINITY")((_m as any).value);
    case "scheme": return ((_: void) => "+inf.0")((_m as any).value);
    case "commonLisp": return ((_: void) => "+hydra-pos-inf+")((_m as any).value);
    case "emacsLisp": return ((_: void) => "1.0e+INF")((_m as any).value);
  }
})())(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))((() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "Double/NEGATIVE_INFINITY")((_m as any).value);
    case "scheme": return ((_: void) => "-inf.0")((_m as any).value);
    case "commonLisp": return ((_: void) => "+hydra-neg-inf+")((_m as any).value);
    case "emacsLisp": return ((_: void) => "-1.0e+INF")((_m as any).value);
  }
})())(s)));
})());
}

export function functionDefinitionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.FunctionDefinition) => Ast.Expr) {
  return ((fdef: LispSyntax.FunctionDefinition) => (() => {
  const name = symbolToExpr(((_x) => _x.name)(fdef));
  const params = LibLists.map(symbolToExpr)(((_x) => _x.params)(fdef));
  const body = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.body)(fdef));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("defn"), name], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("defun"), name], [Serialization.parens(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("defun"), name], [Serialization.parens(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("define")], [Serialization.parens(Serialization.spaceSep(LibLists.concat2([name])(params)))], body]))))((_m as any).value);
  }
})();
})());
}

export function ifExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.IfExpression) => Ast.Expr) {
  return ((ifExpr: LispSyntax.IfExpression) => (() => {
  const cond = expressionToExpr(d)(((_x) => _x.condition)(ifExpr));
  const then = expressionToExpr(d)(((_x) => _x.then)(ifExpr));
  const else_ = ((_x) => _x.else)(ifExpr);
  const elsePart = LibMaybes.maybe([])(((e: LispSyntax.Expression) => [expressionToExpr(d)(e)]))(else_);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("if"), cond, then], elsePart])));
})());
}

export function importDeclarationToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.ImportDeclaration) => Ast.Expr) {
  return ((idecl: LispSyntax.ImportDeclaration) => (() => {
  const modName = ((_x) => _x)(((_x) => _x.module)(idecl));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.cst(":require"), Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep([Serialization.cst(modName)]))])))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.cst("require"), Serialization.noSep([Serialization.cst("'"), Serialization.cst(modName)])])))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.cst(":use"), Serialization.cst(LibStrings.cat2(":")(modName))])))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.cst("import"), Serialization.parens(Serialization.cst(modName))])))((_m as any).value);
  }
})();
})());
}

export function keywordToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.Keyword) => Ast.Expr) {
  return ((k: LispSyntax.Keyword) => (() => {
  const name = ((_x) => _x.name)(k);
  const ns = ((_x) => _x.namespace)(k);
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "scheme": return ((_: void) => Serialization.noSep([Serialization.cst("'"), Serialization.cst(name)]))((_m as any).value);
    default: return Serialization.cst(LibMaybes.maybe(LibStrings.cat2(":")(name))(((n: string) => LibStrings.cat([n, "/:", name])))(ns))(_m);
  }
})();
})());
}

export function lambdaKeyword(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "fn")((_m as any).value);
    case "emacsLisp": return ((_: void) => "lambda")((_m as any).value);
    case "commonLisp": return ((_: void) => "cl:lambda")((_m as any).value);
    case "scheme": return ((_: void) => "lambda")((_m as any).value);
  }
})();
}

export function lambdaToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.Lambda) => Ast.Expr) {
  return ((lam: LispSyntax.Lambda) => (() => {
  const params = LibLists.map(symbolToExpr)(((_x) => _x.params)(lam));
  const body = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.body)(lam));
  const mname = ((_x) => _x.name)(lam);
  const kw = lambdaKeyword(d);
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => LibMaybes.maybe(Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw)], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(params))], body]))))(((sym: LispSyntax.Symbol) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw), symbolToExpr(sym)], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(params))], body])))))(mname))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw)], [Serialization.parens(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw)], [Serialization.parens(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw)], [Serialization.parens(Serialization.spaceSep(params))], body]))))((_m as any).value);
  }
})();
})());
}

export function letExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.LetExpression) => Ast.Expr) {
  return ((letExpr: LispSyntax.LetExpression) => (() => {
  const kind = ((_x) => _x.kind)(letExpr);
  const bindings = ((_x) => _x.bindings)(letExpr);
  const body = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.body)(letExpr));
  const bindingPairs = LibLists.map(((b: LispSyntax.LetBinding) => (() => {
  const _m = b;
  switch (_m.tag) {
    case "simple": return ((sb: LispSyntax.SimpleBinding) => [symbolToExpr(((_x) => _x.name)(sb)), expressionToExpr(d)(((_x) => _x.value)(sb))])((_m as any).value);
    case "destructuring": return ((_: LispSyntax.DestructuringBinding) => [Serialization.cst("<destructuring>"), Serialization.cst("<destructuring>")])((_m as any).value);
  }
})()))(bindings);
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => (() => {
  const _m = kind;
  switch (_m.tag) {
    case "recursive": return ((_2: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("let")], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(LibLists.concat(LibLists.map(((p: readonly [Ast.Expr, Ast.Expr]) => [LibPairs.first(p), LibPairs.second(p)]))(bindingPairs))))], body]))))((_m as any).value);
    case "parallel": return ((_2: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("let")], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(LibLists.concat(LibLists.map(((p: readonly [Ast.Expr, Ast.Expr]) => [LibPairs.first(p), LibPairs.second(p)]))(bindingPairs))))], body]))))((_m as any).value);
    case "sequential": return ((_2: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("let")], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(LibLists.concat(LibLists.map(((p: readonly [Ast.Expr, Ast.Expr]) => [LibPairs.first(p), LibPairs.second(p)]))(bindingPairs))))], body]))))((_m as any).value);
  }
})())((_m as any).value);
    case "emacsLisp": return ((_: void) => (() => {
  const kw = (() => {
  const _m = kind;
  switch (_m.tag) {
    case "parallel": return ((_2: void) => "let")((_m as any).value);
    case "sequential": return ((_2: void) => "let*")((_m as any).value);
    case "recursive": return ((_2: void) => "letrec")((_m as any).value);
  }
})();
  const bindingExprs = LibLists.map(((p: readonly [Ast.Expr, Ast.Expr]) => Serialization.parens(Serialization.spaceSep([LibPairs.first(p), LibPairs.second(p)]))))(bindingPairs);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw)], [Serialization.parens(Serialization.spaceSep(bindingExprs))], body])));
})())((_m as any).value);
    case "commonLisp": return ((_: void) => (() => {
  const kw = (() => {
  const _m = kind;
  switch (_m.tag) {
    case "parallel": return ((_2: void) => "let")((_m as any).value);
    case "sequential": return ((_2: void) => "let*")((_m as any).value);
    case "recursive": return ((_2: void) => "letrec")((_m as any).value);
  }
})();
  const bindingExprs = LibLists.map(((p: readonly [Ast.Expr, Ast.Expr]) => Serialization.parens(Serialization.spaceSep([LibPairs.first(p), LibPairs.second(p)]))))(bindingPairs);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw)], [Serialization.parens(Serialization.spaceSep(bindingExprs))], body])));
})())((_m as any).value);
    case "scheme": return ((_: void) => (() => {
  const kw = (() => {
  const _m = kind;
  switch (_m.tag) {
    case "parallel": return ((_2: void) => "let")((_m as any).value);
    case "sequential": return ((_2: void) => "let*")((_m as any).value);
    case "recursive": return ((_2: void) => "letrec")((_m as any).value);
  }
})();
  const bindingExprs = LibLists.map(((p: readonly [Ast.Expr, Ast.Expr]) => Serialization.parens(Serialization.spaceSep([LibPairs.first(p), LibPairs.second(p)]))))(bindingPairs);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst(kw)], [Serialization.parens(Serialization.spaceSep(bindingExprs))], body])));
})())((_m as any).value);
  }
})();
})());
}

export function listKeyword(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "list")((_m as any).value);
    case "emacsLisp": return ((_: void) => "list")((_m as any).value);
    case "commonLisp": return ((_: void) => "cl:list")((_m as any).value);
    case "scheme": return ((_: void) => "list")((_m as any).value);
  }
})();
}

export function listLiteralToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.ListLiteral) => Ast.Expr) {
  return ((ll: LispSyntax.ListLiteral) => (() => {
  const elems = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.elements)(ll));
  const quoted = ((_x) => _x.quoted)(ll);
  return LibLogic.ifElse(quoted)(Serialization.noSep([Serialization.cst("'"), Serialization.parens(Serialization.spaceSep(elems))]))(Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst(listKeyword(d))])(elems))));
})());
}

export function literalToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.Literal) => Ast.Expr) {
  return ((lit: LispSyntax.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((i: LispSyntax.IntegerLiteral) => Serialization.cst(LibLiterals.showBigint(((_x) => _x.value)(i))))((_m as any).value);
    case "float": return ((f: LispSyntax.FloatLiteral) => Serialization.cst(formatLispFloat(d)(((_x) => _x.value)(f))))((_m as any).value);
    case "string": return ((s: string) => (() => {
  const e1 = LibStrings.intercalate("\\\\")(LibStrings.splitOn("\\")(s));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "commonLisp": return ((_: void) => (() => {
  const escaped = LibStrings.intercalate("\\\"")(LibStrings.splitOn("\"")(e1));
  return Serialization.cst(LibStrings.cat(["\"", escaped, "\""]));
})())((_m as any).value);
    case "clojure": return ((_: void) => (() => {
  const e2 = LibStrings.intercalate("\\n")(LibStrings.splitOn(LibStrings.fromList([10]))(e1));
  return (() => {
  const e3 = LibStrings.intercalate("\\r")(LibStrings.splitOn(LibStrings.fromList([13]))(e2));
  return (() => {
  const e4 = LibStrings.intercalate("\\t")(LibStrings.splitOn(LibStrings.fromList([9]))(e3));
  return (() => {
  const escaped = LibStrings.intercalate("\\\"")(LibStrings.splitOn("\"")(e4));
  return Serialization.cst(LibStrings.cat(["\"", escaped, "\""]));
})();
})();
})();
})())((_m as any).value);
    case "emacsLisp": return ((_: void) => (() => {
  const e2 = LibStrings.intercalate("\\n")(LibStrings.splitOn(LibStrings.fromList([10]))(e1));
  return (() => {
  const e3 = LibStrings.intercalate("\\r")(LibStrings.splitOn(LibStrings.fromList([13]))(e2));
  return (() => {
  const e4 = LibStrings.intercalate("\\t")(LibStrings.splitOn(LibStrings.fromList([9]))(e3));
  return (() => {
  const escaped = LibStrings.intercalate("\\\"")(LibStrings.splitOn("\"")(e4));
  return Serialization.cst(LibStrings.cat(["\"", escaped, "\""]));
})();
})();
})();
})())((_m as any).value);
    case "scheme": return ((_: void) => (() => {
  const e2 = LibStrings.intercalate("\\n")(LibStrings.splitOn(LibStrings.fromList([10]))(e1));
  return (() => {
  const e3 = LibStrings.intercalate("\\r")(LibStrings.splitOn(LibStrings.fromList([13]))(e2));
  return (() => {
  const e4 = LibStrings.intercalate("\\t")(LibStrings.splitOn(LibStrings.fromList([9]))(e3));
  return (() => {
  const escaped = LibStrings.intercalate("\\\"")(LibStrings.splitOn("\"")(e4));
  return Serialization.cst(LibStrings.cat(["\"", escaped, "\""]));
})();
})();
})();
})())((_m as any).value);
  }
})();
})())((_m as any).value);
    case "character": return ((c: LispSyntax.CharacterLiteral) => (() => {
  const ch = ((_x) => _x.value)(c);
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.cst(LibStrings.cat2("\\")(ch)))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.cst(LibStrings.cat2("?")(ch)))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.cst(LibStrings.cat2("#\\")(ch)))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.cst(LibStrings.cat2("#\\")(ch)))((_m as any).value);
  }
})();
})())((_m as any).value);
    case "boolean": return ((b: boolean) => LibLogic.ifElse(b)(trueExpr(d))(falseExpr(d)))((_m as any).value);
    case "nil": return ((_: void) => nilExpr(d))((_m as any).value);
    case "keyword": return ((k: LispSyntax.Keyword) => keywordToExpr(d)(k))((_m as any).value);
    case "symbol": return ((s: LispSyntax.Symbol) => Serialization.noSep([Serialization.cst("'"), symbolToExpr(s)]))((_m as any).value);
  }
})());
}

export function macroDefinitionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.MacroDefinition) => Ast.Expr) {
  return ((mdef: LispSyntax.MacroDefinition) => (() => {
  const name = symbolToExpr(((_x) => _x.name)(mdef));
  const params = LibLists.map(symbolToExpr)(((_x) => _x.params)(mdef));
  const body = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.body)(mdef));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("defmacro"), name], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("defmacro"), name], [Serialization.parens(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("defmacro"), name], [Serialization.parens(Serialization.spaceSep(params))], body]))))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("define-syntax"), name], body]))))((_m as any).value);
  }
})();
})());
}

export function mapLiteralToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.MapLiteral) => Ast.Expr) {
  return ((ml: LispSyntax.MapLiteral) => (() => {
  const entries = ((_x) => _x.entries)(ml);
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.brackets(Serialization.curlyBraces)(Serialization.inlineStyle)(Serialization.spaceSep(LibLists.concat(LibLists.map(((e: LispSyntax.MapEntry) => [expressionToExpr(d)(((_x) => _x.key)(e)), expressionToExpr(d)(((_x) => _x.value)(e))]))(entries)))))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.noSep([Serialization.cst("'"), Serialization.parens(Serialization.spaceSep(LibLists.map(((e: LispSyntax.MapEntry) => Serialization.parens(Serialization.spaceSep([expressionToExpr(d)(((_x) => _x.key)(e)), Serialization.cst("."), expressionToExpr(d)(((_x) => _x.value)(e))]))))(entries)))]))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.noSep([Serialization.cst("'"), Serialization.parens(Serialization.spaceSep(LibLists.map(((e: LispSyntax.MapEntry) => Serialization.parens(Serialization.spaceSep([expressionToExpr(d)(((_x) => _x.key)(e)), Serialization.cst("."), expressionToExpr(d)(((_x) => _x.value)(e))]))))(entries)))]))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("list")])(LibLists.map(((e: LispSyntax.MapEntry) => Serialization.parens(Serialization.spaceSep([Serialization.cst("cons"), expressionToExpr(d)(((_x) => _x.key)(e)), expressionToExpr(d)(((_x) => _x.value)(e))]))))(entries)))))((_m as any).value);
  }
})();
})());
}

export function moduleDeclarationToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.ModuleDeclaration) => Ast.Expr) {
  return ((mdecl: LispSyntax.ModuleDeclaration) => (() => {
  const name = ((_x) => _x)(((_x) => _x.name)(mdecl));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.cst("ns"), Serialization.cst(name)])))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.newlineSep([Serialization.parens(Serialization.spaceSep([Serialization.cst("require"), Serialization.noSep([Serialization.cst("'"), Serialization.cst("cl-lib")])])), Serialization.parens(Serialization.spaceSep([Serialization.cst("provide"), Serialization.noSep([Serialization.cst("'"), Serialization.cst(name)])]))]))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.newlineSep([Serialization.parens(Serialization.spaceSep([Serialization.cst("defpackage"), Serialization.cst(LibStrings.cat2(":")(name))])), Serialization.parens(Serialization.spaceSep([Serialization.cst("in-package"), Serialization.cst(LibStrings.cat2(":")(name))]))]))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep([Serialization.cst("define-library"), Serialization.parens(Serialization.cst(name))])))((_m as any).value);
  }
})();
})());
}

export function nilExpr(d: LispSyntax.Dialect): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.cst("nil"))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.cst("nil"))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.cst("cl:nil"))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.cst("'()"))((_m as any).value);
  }
})();
}

export function notExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.NotExpression) => Ast.Expr) {
  return ((notExpr: LispSyntax.NotExpression) => Serialization.parens(Serialization.spaceSep([Serialization.cst("not"), expressionToExpr(d)(((_x) => _x.expression)(notExpr))])));
}

export function orExpressionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.OrExpression) => Ast.Expr) {
  return ((orExpr: LispSyntax.OrExpression) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("or")])(LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.expressions)(orExpr))))));
}

export function programToExpr(prog: LispSyntax.Program): Ast.Expr {
  return (() => {
  const d = ((_x) => _x.dialect)(prog);
  const modDecl = ((_x) => _x.module)(prog);
  const imports = ((_x) => _x.imports)(prog);
  const exports = ((_x) => _x.exports)(prog);
  const forms = ((_x) => _x.forms)(prog);
  const formPart = LibLists.map(((v1: LispSyntax.TopLevelFormWithComments) => topLevelFormWithCommentsToExpr(d)(v1)))(forms);
  const importNames = LibLists.map(((idecl: LispSyntax.ImportDeclaration) => ((_x) => _x)(((_x) => _x.module)(idecl))))(imports);
  const exportSyms = LibLists.concat(LibLists.map(((edecl: LispSyntax.ExportDeclaration) => LibLists.map(symbolToExpr)(((_x) => _x.symbols)(edecl))))(exports));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => LibMaybes.maybe(Serialization.doubleNewlineSep(formPart))(((m: LispSyntax.ModuleDeclaration) => (() => {
  const nameStr = ((_x) => _x)(((_x) => _x.name)(m));
  const requireClauses = LibLists.map(((imp: string) => Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep([Serialization.cst(imp), Serialization.cst(":refer"), Serialization.cst(":all")]))))(importNames);
  const nsForm = LibLogic.ifElse(LibLists.null_(requireClauses))(Serialization.parens(Serialization.spaceSep([Serialization.cst("ns"), Serialization.cst(nameStr)])))(Serialization.parens(Serialization.newlineSep([Serialization.spaceSep([Serialization.cst("ns"), Serialization.cst(nameStr)]), Serialization.spaceSep(LibLists.concat2([Serialization.cst("  (:require")])(requireClauses)), Serialization.cst(")")])));
  return (() => {
  const varNames = LibLists.concat(LibLists.map(((fwc: LispSyntax.TopLevelFormWithComments) => (() => {
  const form = ((_x) => _x.form)(fwc);
  return (() => {
  const _m = form;
  switch (_m.tag) {
    case "variable": return ((vd: LispSyntax.VariableDefinition) => [symbolToExpr(((_x) => _x.name)(vd))])((_m as any).value);
    case "function": return ((fd: LispSyntax.FunctionDefinition) => [symbolToExpr(((_x) => _x.name)(fd))])((_m as any).value);
    default: return [](_m);
  }
})();
})()))(forms));
  return (() => {
  const declareForm = LibLogic.ifElse(LibLists.null_(varNames))([])([Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("declare")])(varNames)))]);
  return Serialization.doubleNewlineSep(LibLists.concat([[nsForm], declareForm, formPart]));
})();
})();
})()))(modDecl))((_m as any).value);
    case "emacsLisp": return ((_: void) => LibMaybes.maybe(Serialization.doubleNewlineSep(formPart))(((m: LispSyntax.ModuleDeclaration) => (() => {
  const nameStr = ((_x) => _x)(((_x) => _x.name)(m));
  const requireClLib = Serialization.parens(Serialization.spaceSep([Serialization.cst("require"), Serialization.noSep([Serialization.cst("'"), Serialization.cst("cl-lib")])]));
  const requireImports = LibLists.map(((imp: string) => Serialization.parens(Serialization.spaceSep([Serialization.cst("require"), Serialization.noSep([Serialization.cst("'"), Serialization.cst(imp)])]))))(importNames);
  const provideForm = Serialization.parens(Serialization.spaceSep([Serialization.cst("provide"), Serialization.noSep([Serialization.cst("'"), Serialization.cst(nameStr)])]));
  return Serialization.doubleNewlineSep(LibLists.concat([[requireClLib], requireImports, formPart, [provideForm]]));
})()))(modDecl))((_m as any).value);
    case "commonLisp": return ((_: void) => LibMaybes.maybe(Serialization.doubleNewlineSep(formPart))(((m: LispSyntax.ModuleDeclaration) => (() => {
  const nameStr = ((_x) => _x)(((_x) => _x.name)(m));
  const colonName = LibStrings.cat2(":")(nameStr);
  const useClause = Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst(":use"), Serialization.cst(":cl")])(LibLists.map(((imp: string) => Serialization.cst(LibStrings.cat2(":")(imp))))(importNames))));
  const exportClause = LibLogic.ifElse(LibLists.null_(exportSyms))([])([Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst(":export")])(LibLists.map(((s: Ast.Expr) => Serialization.noSep([Serialization.cst(":"), s])))(exportSyms))))]);
  const defpkgForm = Serialization.parens(Serialization.newlineSep(LibLists.concat([[Serialization.spaceSep([Serialization.cst("defpackage"), Serialization.cst(colonName)])], [useClause], exportClause])));
  const inpkgForm = Serialization.parens(Serialization.spaceSep([Serialization.cst("in-package"), Serialization.cst(colonName)]));
  return Serialization.doubleNewlineSep(LibLists.concat([[defpkgForm, inpkgForm], formPart]));
})()))(modDecl))((_m as any).value);
    case "scheme": return ((_: void) => LibMaybes.maybe(Serialization.doubleNewlineSep(formPart))(((m: LispSyntax.ModuleDeclaration) => (() => {
  const nameStr = ((_x) => _x)(((_x) => _x.name)(m));
  const nameParts = LibLists.map(((p: string) => Formatting.convertCaseCamelToLowerSnake(p)))(LibStrings.splitOn(".")(nameStr));
  const nameExpr = Serialization.parens(Serialization.spaceSep(LibLists.map(((p: string) => Serialization.cst(p)))(nameParts)));
  const domainImportExprs = LibLists.map(((idecl: LispSyntax.ImportDeclaration) => (() => {
  const nsName = ((_x) => _x)(((_x) => _x.module)(idecl));
  const nsParts = LibLists.map(((p: string) => Formatting.convertCaseCamelToLowerSnake(p)))(LibStrings.splitOn(".")(nsName));
  return Serialization.parens(Serialization.spaceSep(LibLists.map(((p: string) => Serialization.cst(p)))(nsParts)));
})()))(imports);
  const schemeBaseExpr = Serialization.parens(Serialization.spaceSep([Serialization.cst("scheme"), Serialization.cst("base")]));
  const allImportExprs = LibLists.concat2([schemeBaseExpr])(domainImportExprs);
  const importClause = Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("import")])(allImportExprs)));
  const exportClauses = LibLists.map(((edecl: LispSyntax.ExportDeclaration) => exportDeclarationToExpr(d)(edecl)))(exports);
  const beginClause = Serialization.parens(Serialization.newlineSep(LibLists.concat2([Serialization.cst("begin")])(formPart)));
  return Serialization.parens(Serialization.newlineSep(LibLists.concat([[Serialization.spaceSep([Serialization.cst("define-library"), nameExpr])], exportClauses, [importClause], [beginClause]])));
})()))(modDecl))((_m as any).value);
  }
})();
})();
}

export function recordTypeDefinitionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.RecordTypeDefinition) => Ast.Expr) {
  return ((rdef: LispSyntax.RecordTypeDefinition) => (() => {
  const name = symbolToExpr(((_x) => _x.name)(rdef));
  const fields = LibLists.map(((f: LispSyntax.FieldDefinition) => symbolToExpr(((_x) => _x.name)(f))))(((_x) => _x.fields)(rdef));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => (() => {
  const nameStr = ((_x) => _x)(((_x) => _x.name)(rdef));
  const fieldNames = LibLists.map(((f: LispSyntax.FieldDefinition) => ((_x) => _x)(((_x) => _x.name)(f))))(((_x) => _x.fields)(rdef));
  const defrecordForm = Serialization.parens(Serialization.spaceSep([Serialization.cst("defrecord"), name, Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(fields))]));
  const makeAlias = Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("defn"), Serialization.cst(LibStrings.cat2("make-")(nameStr))], [Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(fields))], [Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst(LibStrings.cat2("->")(nameStr))])(LibLists.map(((fn: string) => Serialization.cst(fn)))(fieldNames))))]])));
  return Serialization.newlineSep([defrecordForm, makeAlias]);
})())((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("cl-defstruct"), name], fields]))))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("cl:defstruct"), name], fields]))))((_m as any).value);
    case "scheme": return ((_: void) => (() => {
  const nameStr = ((_x) => _x)(((_x) => _x.name)(rdef));
  const fieldNames = LibLists.map(((f: LispSyntax.FieldDefinition) => ((_x) => _x)(((_x) => _x.name)(f))))(((_x) => _x.fields)(rdef));
  const constructor = Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst(LibStrings.cat2("make-")(nameStr))])(LibLists.map(((fn: string) => Serialization.cst(fn)))(fieldNames))));
  const predicate = Serialization.cst(LibStrings.cat2(nameStr)("?"));
  const accessors = LibLists.map(((fn: string) => Serialization.parens(Serialization.spaceSep([Serialization.cst(fn), Serialization.cst(LibStrings.cat([nameStr, "-", fn]))]))))(fieldNames);
  return Serialization.parens(Serialization.spaceSep(LibLists.concat([[Serialization.cst("define-record-type"), name, constructor, predicate], accessors])));
})())((_m as any).value);
  }
})();
})());
}

export function sExpressionToExpr(sexpr: LispSyntax.SExpression): Ast.Expr {
  return (() => {
  const _m = sexpr;
  switch (_m.tag) {
    case "atom": return ((a: string) => Serialization.cst(a))((_m as any).value);
    case "list": return ((elems: ReadonlyArray<LispSyntax.SExpression>) => Serialization.parens(Serialization.spaceSep(LibLists.map(sExpressionToExpr)(elems))))((_m as any).value);
  }
})();
}

export function setLiteralToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.SetLiteral) => Ast.Expr) {
  return ((sl: LispSyntax.SetLiteral) => (() => {
  const elems = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.elements)(sl));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.noSep([Serialization.cst("#"), Serialization.brackets(Serialization.curlyBraces)(Serialization.inlineStyle)(Serialization.spaceSep(elems))]))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("list")])(elems))))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("cl:list")])(elems))))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.parens(Serialization.spaceSep(LibLists.concat2([Serialization.cst("list")])(elems))))((_m as any).value);
  }
})();
})());
}

export function symbolToExpr(s: LispSyntax.Symbol): Ast.Expr {
  return Serialization.cst(((_x) => _x)(s));
}

export function topLevelFormToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.TopLevelForm) => Ast.Expr) {
  return ((form: LispSyntax.TopLevelForm) => (() => {
  const _m = form;
  switch (_m.tag) {
    case "function": return ((f: LispSyntax.FunctionDefinition) => functionDefinitionToExpr(d)(f))((_m as any).value);
    case "variable": return ((v: LispSyntax.VariableDefinition) => variableDefinitionToExpr(d)(v))((_m as any).value);
    case "constant": return ((c: LispSyntax.ConstantDefinition) => constantDefinitionToExpr(d)(c))((_m as any).value);
    case "recordType": return ((r: LispSyntax.RecordTypeDefinition) => recordTypeDefinitionToExpr(d)(r))((_m as any).value);
    case "macro": return ((m: LispSyntax.MacroDefinition) => macroDefinitionToExpr(d)(m))((_m as any).value);
    case "expression": return ((e: LispSyntax.Expression) => expressionToExpr(d)(e))((_m as any).value);
  }
})());
}

export function topLevelFormWithCommentsToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.TopLevelFormWithComments) => Ast.Expr) {
  return ((fwc: LispSyntax.TopLevelFormWithComments) => (() => {
  const mdoc = ((_x) => _x.doc)(fwc);
  const mcomment = ((_x) => _x.comment)(fwc);
  const form = ((_x) => _x.form)(fwc);
  const docPart = LibMaybes.maybe([])(((ds: LispSyntax.Docstring) => [docstringToExpr(ds)]))(mdoc);
  const commentPart = LibMaybes.maybe([])(((c: LispSyntax.Comment) => [commentToExpr(c)]))(mcomment);
  const formExpr = topLevelFormToExpr(d)(form);
  return Serialization.newlineSep(LibLists.concat([commentPart, docPart, [formExpr]]));
})());
}

export function trueExpr(d: LispSyntax.Dialect): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.cst("true"))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.cst("t"))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.cst("cl:t"))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.cst("#t"))((_m as any).value);
  }
})();
}

export function variableDefinitionToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.VariableDefinition) => Ast.Expr) {
  return ((vdef: LispSyntax.VariableDefinition) => (() => {
  const name = symbolToExpr(((_x) => _x.name)(vdef));
  const value = expressionToExpr(d)(((_x) => _x.value)(vdef));
  return Serialization.parens(Serialization.spaceSep([Serialization.cst(defKeyword(d)), name, value]));
})());
}

export function variableReferenceToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.VariableReference) => Ast.Expr) {
  return ((vref: LispSyntax.VariableReference) => (() => {
  const name = symbolToExpr(((_x) => _x.name)(vref));
  const isFnNs = ((_x) => _x.functionNamespace)(vref);
  return LibLogic.ifElse(isFnNs)((() => {
  const _m = d;
  switch (_m.tag) {
    case "commonLisp": return ((_: void) => Serialization.noSep([Serialization.cst("#'"), name]))((_m as any).value);
    case "clojure": return ((_: void) => name)((_m as any).value);
    case "emacsLisp": return ((_: void) => name)((_m as any).value);
    case "scheme": return ((_: void) => name)((_m as any).value);
  }
})())(name);
})());
}

export function vectorLiteralToExpr(d: LispSyntax.Dialect): ((x: LispSyntax.VectorLiteral) => Ast.Expr) {
  return ((vl: LispSyntax.VectorLiteral) => (() => {
  const elems = LibLists.map(((v1: LispSyntax.Expression) => expressionToExpr(d)(v1)))(((_x) => _x.elements)(vl));
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(elems)))((_m as any).value);
    case "emacsLisp": return ((_: void) => Serialization.brackets(Serialization.squareBrackets)(Serialization.inlineStyle)(Serialization.spaceSep(elems)))((_m as any).value);
    case "commonLisp": return ((_: void) => Serialization.noSep([Serialization.cst("#"), Serialization.parens(Serialization.spaceSep(elems))]))((_m as any).value);
    case "scheme": return ((_: void) => Serialization.noSep([Serialization.cst("#"), Serialization.parens(Serialization.spaceSep(elems))]))((_m as any).value);
  }
})();
})());
}

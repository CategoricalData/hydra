// Note: this is an automatically generated file. Do not edit.

/**
 * Haskell operator precendence and associativity are drawn from:
 * https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
 * Other operators were investigated using GHCi, e.g. ":info (->)"
 * Operator names are drawn (loosely) from:
 * https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators
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
import * as Graph from "../graph.js";
import * as HaskellOperators from "./operators.js";
import * as HaskellSyntax from "./syntax.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
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

export function alternativeToExpr(alt: HaskellSyntax.Alternative): Ast.Expr {
  return Serialization.structuralSpaceSep([patternToExpr(((_x) => _x.pattern)(alt)), Serialization.cst("->"), caseRhsToExpr(((_x) => _x.rhs)(alt))]);
}

export function applicationExpressionToExpr(app: HaskellSyntax.ApplicationExpression): Ast.Expr {
  return Serialization.ifx(HaskellOperators.appOp)(expressionToExpr(((_x) => _x.function)(app)))(expressionToExpr(((_x) => _x.argument)(app)));
}

export function applicationPatternToExpr(appPat: HaskellSyntax.ApplicationPattern): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(appPat);
  const pats = ((_x) => _x.args)(appPat);
  return Serialization.spaceSep(LibLists.cons(nameToExpr(name))(LibLists.map(patternToExpr)(pats)));
})();
}

export function assertionToExpr(sert: HaskellSyntax.Assertion): Ast.Expr {
  return (() => {
  const _m = sert;
  switch (_m.tag) {
    case "class": return ((cls: HaskellSyntax.ClassAssertion) => classAssertionToExpr(cls))((_m as any).value);
    case "tuple": return ((serts: ReadonlyArray<HaskellSyntax.Assertion>) => Serialization.parenList(false)(LibLists.map(assertionToExpr)(serts)))((_m as any).value);
  }
})();
}

export function caseExpressionToExpr(caseExpr: HaskellSyntax.CaseExpression): Ast.Expr {
  return (() => {
  const cs = ((_x) => _x.case)(caseExpr);
  const alts = ((_x) => _x.alternatives)(caseExpr);
  const ofOp = ({
    symbol: "of",
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "breakAndIndent", value: "  " })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });
  const lhs = Serialization.spaceSep([Serialization.cst("case"), expressionToExpr(cs)]);
  const rhs = Serialization.newlineSep(LibLists.map(alternativeToExpr)(alts));
  return Serialization.ifx(ofOp)(lhs)(rhs);
})();
}

export function caseRhsToExpr(rhs: HaskellSyntax.CaseRhs): Ast.Expr {
  return expressionToExpr(((_x) => _x)(rhs));
}

export function classAssertionToExpr(clsAsrt: HaskellSyntax.ClassAssertion): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(clsAsrt);
  const types = ((_x) => _x.types)(clsAsrt);
  return Serialization.spaceSep(LibLists.cons(nameToExpr(name))([Serialization.commaSep(Serialization.halfBlockStyle)(LibLists.map(typeToExpr)(types))]));
})();
}

export function constructRecordExpressionToExpr(constructRecord: HaskellSyntax.ConstructRecordExpression): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(constructRecord);
  const updates = ((_x) => _x.fields)(constructRecord);
  const fromUpdate = ((update: HaskellSyntax.FieldUpdate) => (() => {
  const fn = ((_x) => _x.name)(update);
  const val = ((_x) => _x.value)(update);
  return Serialization.ifx(HaskellOperators.defineOp)(nameToExpr(fn))(expressionToExpr(val));
})());
  const body = Serialization.commaSep(Serialization.halfBlockStyle)(LibLists.map(fromUpdate)(updates));
  return Serialization.spaceSep(LibLists.cons(nameToExpr(name))([Serialization.brackets(Serialization.curlyBraces)(Serialization.halfBlockStyle)(body)]));
})();
}

export function constructorToExpr(cons: HaskellSyntax.Constructor): Ast.Expr {
  return (() => {
  const _m = cons;
  switch (_m.tag) {
    case "ordinary": return ((ord: HaskellSyntax.OrdinaryConstructor) => (() => {
  const name = ((_x) => _x.name)(ord);
  const types = ((_x) => _x.fields)(ord);
  return Serialization.spaceSep(LibLists.cons(nameToExpr(name))([Serialization.spaceSep(LibLists.map(typeToExpr)(types))]));
})())((_m as any).value);
    case "record": return ((rec: HaskellSyntax.RecordConstructor) => (() => {
  const name = ((_x) => _x.name)(rec);
  const fields = ((_x) => _x.fields)(rec);
  return Serialization.spaceSep(LibLists.cons(nameToExpr(name))([Serialization.curlyBracesList(null)(Serialization.halfBlockStyle)(LibLists.map(fieldWithCommentsToExpr)(fields))]));
})())((_m as any).value);
  }
})();
}

export function constructorWithCommentsToExpr(consWithComments: HaskellSyntax.ConstructorWithComments): Ast.Expr {
  return (() => {
  const body = ((_x) => _x.body)(consWithComments);
  const mc = ((_x) => _x.comments)(consWithComments);
  return LibMaybes.maybe(constructorToExpr(body))(((c: string) => Serialization.newlineSep(LibLists.cons(Serialization.cst(toHaskellComments(c)))([constructorToExpr(body)]))))(mc);
})();
}

export function dataOrNewtypeToExpr(kw: HaskellSyntax.DataOrNewtype): Ast.Expr {
  return (() => {
  const _m = kw;
  switch (_m.tag) {
    case "data": return ((_: void) => Serialization.cst("data"))((_m as any).value);
    case "newtype": return ((_: void) => Serialization.cst("newtype"))((_m as any).value);
  }
})();
}

export function declarationHeadToExpr(hd: HaskellSyntax.DeclarationHead): Ast.Expr {
  return (() => {
  const _m = hd;
  switch (_m.tag) {
    case "application": return ((appHead: HaskellSyntax.ApplicationDeclarationHead) => (() => {
  const fun = ((_x) => _x.function)(appHead);
  const op = ((_x) => _x.operand)(appHead);
  return Serialization.spaceSep(LibLists.cons(declarationHeadToExpr(fun))([variableToExpr(op)]));
})())((_m as any).value);
    case "simple": return ((name: HaskellSyntax.Name) => nameToExpr(name))((_m as any).value);
  }
})();
}

export function declarationToExpr(decl: HaskellSyntax.Declaration): Ast.Expr {
  return (() => {
  const _m = decl;
  switch (_m.tag) {
    case "data": return ((dataDecl: HaskellSyntax.DataDeclaration) => (() => {
  const kw = ((_x) => _x.keyword)(dataDecl);
  const hd = ((_x) => _x.head)(dataDecl);
  const cons = ((_x) => _x.constructors)(dataDecl);
  const deriv = ((_x) => _x.deriving)(dataDecl);
  const derivCat = LibLists.concat(LibLists.map(((_x) => _x))(deriv));
  const constructors = Serialization.orSep(Serialization.halfBlockStyle)(LibLists.map(constructorWithCommentsToExpr)(cons));
  const derivingClause = LibLogic.ifElse(LibLists.null_(derivCat))([])([Serialization.spaceSep(LibLists.cons(Serialization.cst("deriving"))([Serialization.parenList(false)(LibLists.map(nameToExpr)(derivCat))]))]);
  const mainParts = [Serialization.spaceSep(LibLists.cons(dataOrNewtypeToExpr(kw))(LibLists.cons(declarationHeadToExpr(hd))([Serialization.cst("=")]))), constructors];
  return Serialization.indentBlock(LibLists.concat2(mainParts)(derivingClause));
})())((_m as any).value);
    case "type": return ((typeDecl: HaskellSyntax.TypeDeclaration) => (() => {
  const hd = ((_x) => _x.name)(typeDecl);
  const typ = ((_x) => _x.type)(typeDecl);
  return Serialization.spaceSep(LibLists.cons(Serialization.cst("type"))(LibLists.cons(declarationHeadToExpr(hd))(LibLists.cons(Serialization.cst("="))([typeToExpr(typ)]))));
})())((_m as any).value);
    case "valueBinding": return ((vb: HaskellSyntax.ValueBinding) => valueBindingToExpr(vb))((_m as any).value);
    case "typedBinding": return ((typedBinding: HaskellSyntax.TypedBinding) => (() => {
  const typeSig = ((_x) => _x.typeSignature)(typedBinding);
  const vb = ((_x) => _x.valueBinding)(typedBinding);
  const name = ((_x) => _x.name)(typeSig);
  const htype = ((_x) => _x.type)(typeSig);
  return Serialization.newlineSep(LibLists.cons(Serialization.structuralSpaceSep([nameToExpr(name), Serialization.cst("::"), typeToExpr(htype)]))([valueBindingToExpr(vb)]));
})())((_m as any).value);
  }
})();
}

export function declarationWithCommentsToExpr(declWithComments: HaskellSyntax.DeclarationWithComments): Ast.Expr {
  return (() => {
  const body = ((_x) => _x.body)(declWithComments);
  const mc = ((_x) => _x.comments)(declWithComments);
  return LibMaybes.maybe(declarationToExpr(body))(((c: string) => Serialization.newlineSep(LibLists.cons(Serialization.cst(toHaskellComments(c)))([declarationToExpr(body)]))))(mc);
})();
}

export function expressionToExpr(expr: HaskellSyntax.Expression): Ast.Expr {
  return (() => {
  const _m = expr;
  switch (_m.tag) {
    case "application": return ((app: HaskellSyntax.ApplicationExpression) => applicationExpressionToExpr(app))((_m as any).value);
    case "case": return ((cases: HaskellSyntax.CaseExpression) => caseExpressionToExpr(cases))((_m as any).value);
    case "constructRecord": return ((r: HaskellSyntax.ConstructRecordExpression) => constructRecordExpressionToExpr(r))((_m as any).value);
    case "do": return ((statements: ReadonlyArray<HaskellSyntax.Statement>) => Serialization.indentBlock(LibLists.cons(Serialization.cst("do"))(LibLists.map(statementToExpr)(statements))))((_m as any).value);
    case "if": return ((ifte: HaskellSyntax.IfExpression) => ifExpressionToExpr(ifte))((_m as any).value);
    case "literal": return ((lit: HaskellSyntax.Literal) => literalToExpr(lit))((_m as any).value);
    case "lambda": return ((lam: HaskellSyntax.LambdaExpression) => Serialization.parenthesize(lambdaExpressionToExpr(lam)))((_m as any).value);
    case "let": return ((letExpr: HaskellSyntax.LetExpression) => (() => {
  const bindings = ((_x) => _x.bindings)(letExpr);
  const inner = ((_x) => _x.inner)(letExpr);
  const encodeBinding = ((binding: HaskellSyntax.LocalBinding) => Serialization.indentSubsequentLines("    ")(localBindingToExpr(binding)));
  return Serialization.indentBlock(LibLists.cons(Serialization.cst(""))(LibLists.cons(Serialization.spaceSep(LibLists.cons(Serialization.cst("let"))([Serialization.customIndentBlock("    ")(LibLists.map(encodeBinding)(bindings))])))([Serialization.spaceSep(LibLists.cons(Serialization.cst("in"))([expressionToExpr(inner)]))])));
})())((_m as any).value);
    case "list": return ((exprs: ReadonlyArray<HaskellSyntax.Expression>) => Serialization.bracketList(Serialization.halfBlockStyle)(LibLists.map(expressionToExpr)(exprs)))((_m as any).value);
    case "parens": return ((expr_: HaskellSyntax.Expression) => Serialization.parenthesize(expressionToExpr(expr_)))((_m as any).value);
    case "tuple": return ((exprs: ReadonlyArray<HaskellSyntax.Expression>) => Serialization.parenList(false)(LibLists.map(expressionToExpr)(exprs)))((_m as any).value);
    case "variable": return ((name: HaskellSyntax.Name) => nameToExpr(name))((_m as any).value);
  }
})();
}

export function fieldToExpr(field: HaskellSyntax.Field): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(field);
  const typ = ((_x) => _x.type)(field);
  return Serialization.spaceSep(LibLists.cons(nameToExpr(name))(LibLists.cons(Serialization.cst("::"))([typeToExpr(typ)])));
})();
}

export function fieldWithCommentsToExpr(fieldWithComments: HaskellSyntax.FieldWithComments): Ast.Expr {
  return (() => {
  const field = ((_x) => _x.field)(fieldWithComments);
  const mc = ((_x) => _x.comments)(fieldWithComments);
  return LibMaybes.maybe(fieldToExpr(field))(((c: string) => Serialization.newlineSep(LibLists.cons(Serialization.cst(toHaskellComments(c)))([fieldToExpr(field)]))))(mc);
})();
}

export function ifExpressionToExpr(ifExpr: HaskellSyntax.IfExpression): Ast.Expr {
  return (() => {
  const eif = ((_x) => _x.condition)(ifExpr);
  const ethen = ((_x) => _x.then)(ifExpr);
  const eelse = ((_x) => _x.else)(ifExpr);
  const ifOp = ({
    symbol: "",
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "breakAndIndent", value: "  " })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });
  const body = Serialization.newlineSep(LibLists.cons(Serialization.spaceSep(LibLists.cons(Serialization.cst("then"))([expressionToExpr(ethen)])))([Serialization.spaceSep(LibLists.cons(Serialization.cst("else"))([expressionToExpr(eelse)]))]));
  return Serialization.ifx(ifOp)(Serialization.spaceSep(LibLists.cons(Serialization.cst("if"))([expressionToExpr(eif)])))(body);
})();
}

export function importExportSpecToExpr(spec: HaskellSyntax.ImportExportSpec): Ast.Expr {
  return nameToExpr(((_x) => _x.name)(spec));
}

export function importToExpr(import_: HaskellSyntax.Import): Ast.Expr {
  return (() => {
  const qual = ((_x) => _x.qualified)(import_);
  const modName = ((_x) => _x.module)(import_);
  const mod = ((_x) => _x.as)(import_);
  const mspec = ((_x) => _x.spec)(import_);
  const name = ((_x) => _x)(modName);
  const hidingSec = ((spec: HaskellSyntax.SpecImport) => (() => {
  const _m = spec;
  switch (_m.tag) {
    case "hiding": return ((names: ReadonlyArray<HaskellSyntax.ImportExportSpec>) => Serialization.spaceSep(LibLists.cons(Serialization.cst("hiding "))([Serialization.parens(Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(importExportSpecToExpr)(names)))])))((_m as any).value);
  }
})());
  const parts = LibMaybes.cat([Serialization.cst("import"), LibLogic.ifElse(qual)(Serialization.cst("qualified"))(null), Serialization.cst(name), LibMaybes.map(((m: HaskellSyntax.ModuleName) => Serialization.cst(LibStrings.cat2("as ")(((_x) => _x)(m)))))(mod), LibMaybes.map(hidingSec)(mspec)]);
  return Serialization.spaceSep(parts);
})();
}

export function lambdaExpressionToExpr(lambdaExpr: HaskellSyntax.LambdaExpression): Ast.Expr {
  return (() => {
  const bindings = ((_x) => _x.bindings)(lambdaExpr);
  const inner = ((_x) => _x.inner)(lambdaExpr);
  const head = Serialization.spaceSep(LibLists.map(patternToExpr)(bindings));
  const body = expressionToExpr(inner);
  return Serialization.ifx(HaskellOperators.lambdaOp)(Serialization.prefix("\\")(head))(body);
})();
}

export function literalToExpr(lit: HaskellSyntax.Literal): Ast.Expr {
  return (() => {
  const parensIfNeg = ((b: boolean) => ((e: string) => LibLogic.ifElse(b)(LibStrings.cat(["(", e, ")"]))(e)));
  return (() => {
  const showFloat = ((showFn: ((x: t0) => string)) => ((v: t0) => (() => {
  const raw = showFn(v);
  return LibLogic.ifElse(LibEquality.equal(raw)("NaN"))("(0/0)")(LibLogic.ifElse(LibEquality.equal(raw)("Infinity"))("(1/0)")(LibLogic.ifElse(LibEquality.equal(raw)("-Infinity"))("(-(1/0))")(parensIfNeg(LibEquality.equal(LibStrings.charAt(0)(raw))(45))(raw))));
})()));
  return Serialization.cst((() => {
  const _m = lit;
  switch (_m.tag) {
    case "char": return ((c: number) => LibLiterals.showString(LibLiterals.showUint16(c)))((_m as any).value);
    case "double": return ((d: number) => showFloat(((v: number) => LibLiterals.showFloat64(v)))(d))((_m as any).value);
    case "float": return ((f: number) => showFloat(((v: number) => LibLiterals.showFloat32(v)))(f))((_m as any).value);
    case "int": return ((i: number) => parensIfNeg(LibEquality.lt(i)(0))(LibLiterals.showInt32(i)))((_m as any).value);
    case "integer": return ((i: bigint) => parensIfNeg(LibEquality.lt(i)(0n))(LibLiterals.showBigint(i)))((_m as any).value);
    case "string": return ((s: string) => LibLiterals.showString(s))((_m as any).value);
  }
})());
})();
})();
}

export function localBindingToExpr(binding: HaskellSyntax.LocalBinding): Ast.Expr {
  return (() => {
  const _m = binding;
  switch (_m.tag) {
    case "signature": return ((ts: HaskellSyntax.TypeSignature) => typeSignatureToExpr(ts))((_m as any).value);
    case "value": return ((vb: HaskellSyntax.ValueBinding) => valueBindingToExpr(vb))((_m as any).value);
  }
})();
}

export function moduleHeadToExpr(moduleHead: HaskellSyntax.ModuleHead): Ast.Expr {
  return (() => {
  const mc = ((_x) => _x.comments)(moduleHead);
  const modName = ((_x) => _x.name)(moduleHead);
  const mname = ((_x) => _x)(modName);
  const head = Serialization.spaceSep(LibLists.cons(Serialization.cst("module"))(LibLists.cons(Serialization.cst(mname))([Serialization.cst("where")])));
  return LibMaybes.maybe(head)(((c: string) => Serialization.newlineSep(LibLists.cons(Serialization.cst(toHaskellComments(c)))(LibLists.cons(Serialization.cst(""))([head])))))(mc);
})();
}

export function moduleToExpr(module: HaskellSyntax.Module): Ast.Expr {
  return (() => {
  const mh = ((_x) => _x.head)(module);
  const imports = ((_x) => _x.imports)(module);
  const decls = ((_x) => _x.declarations)(module);
  const warning = [Serialization.cst(toSimpleComments(Constants.warningAutoGeneratedFile))];
  const headerLine = LibMaybes.maybe([])(((h: HaskellSyntax.ModuleHead) => [moduleHeadToExpr(h)]))(mh);
  const declLines = LibLists.map(declarationWithCommentsToExpr)(decls);
  const importLines = LibLogic.ifElse(LibLists.null_(imports))([])([Serialization.newlineSep(LibLists.map(importToExpr)(imports))]);
  return Serialization.doubleNewlineSep(LibLists.concat([warning, headerLine, importLines, declLines]));
})();
}

export function nameToExpr(name: HaskellSyntax.Name): Ast.Expr {
  return Serialization.cst((() => {
  const _m = name;
  switch (_m.tag) {
    case "implicit": return ((qn: HaskellSyntax.QualifiedName) => LibStrings.cat2("?")(writeQualifiedName(qn)))((_m as any).value);
    case "normal": return ((qn: HaskellSyntax.QualifiedName) => writeQualifiedName(qn))((_m as any).value);
    case "parens": return ((qn: HaskellSyntax.QualifiedName) => LibStrings.cat(["(", writeQualifiedName(qn), ")"]))((_m as any).value);
  }
})());
}

export function patternToExpr(pat: HaskellSyntax.Pattern): Ast.Expr {
  return (() => {
  const _m = pat;
  switch (_m.tag) {
    case "application": return ((app: HaskellSyntax.ApplicationPattern) => applicationPatternToExpr(app))((_m as any).value);
    case "list": return ((pats: ReadonlyArray<HaskellSyntax.Pattern>) => Serialization.bracketList(Serialization.halfBlockStyle)(LibLists.map(patternToExpr)(pats)))((_m as any).value);
    case "literal": return ((lit: HaskellSyntax.Literal) => literalToExpr(lit))((_m as any).value);
    case "name": return ((name: HaskellSyntax.Name) => nameToExpr(name))((_m as any).value);
    case "parens": return ((pat_: HaskellSyntax.Pattern) => Serialization.parenthesize(patternToExpr(pat_)))((_m as any).value);
    case "tuple": return ((pats: ReadonlyArray<HaskellSyntax.Pattern>) => Serialization.parenList(false)(LibLists.map(patternToExpr)(pats)))((_m as any).value);
    case "wildcard": return ((_: void) => Serialization.cst("_"))((_m as any).value);
  }
})();
}

export function rightHandSideToExpr(rhs: HaskellSyntax.RightHandSide): Ast.Expr {
  return expressionToExpr(((_x) => _x)(rhs));
}

export function statementToExpr(stmt: HaskellSyntax.Statement): Ast.Expr {
  return expressionToExpr(((_x) => _x)(stmt));
}

export function toHaskellComments(c: string): string {
  return LibStrings.intercalate("\n")(LibLists.map(((s: string) => LibStrings.cat2("-- | ")(s)))(LibStrings.lines(c)));
}

export function toSimpleComments(c: string): string {
  return LibStrings.intercalate("\n")(LibLists.map(((s: string) => LibStrings.cat2("-- ")(s)))(LibStrings.lines(c)));
}

export function typeSignatureToExpr(typeSig: HaskellSyntax.TypeSignature): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(typeSig);
  const typ = ((_x) => _x.type)(typeSig);
  const nameExpr = nameToExpr(name);
  const typeExpr = typeToExpr(typ);
  const inlineSig = Serialization.structuralSpaceSep([nameExpr, Serialization.cst("::"), typeExpr]);
  return LibLogic.ifElse(LibEquality.gt(Serialization.expressionLength(inlineSig))(120))(Serialization.newlineSep([Serialization.spaceSep([nameExpr, Serialization.cst("::")]), Serialization.tabIndent(typeExpr)]))(inlineSig);
})();
}

export function typeToExpr(htype: HaskellSyntax.Type): Ast.Expr {
  return (() => {
  const _m = htype;
  switch (_m.tag) {
    case "application": return ((appType: HaskellSyntax.ApplicationType) => (() => {
  const lhs = ((_x) => _x.context)(appType);
  const rhs = ((_x) => _x.argument)(appType);
  return Serialization.ifx(HaskellOperators.appOp)(typeToExpr(lhs))(typeToExpr(rhs));
})())((_m as any).value);
    case "ctx": return ((ctxType: HaskellSyntax.ContextType) => (() => {
  const ctx = ((_x) => _x.ctx)(ctxType);
  const typ = ((_x) => _x.type)(ctxType);
  return Serialization.ifx(HaskellOperators.assertOp)(assertionToExpr(ctx))(typeToExpr(typ));
})())((_m as any).value);
    case "function": return ((funType: HaskellSyntax.FunctionType) => (() => {
  const dom = ((_x) => _x.domain)(funType);
  const cod = ((_x) => _x.codomain)(funType);
  return Serialization.ifx(HaskellOperators.arrowOp)(typeToExpr(dom))(typeToExpr(cod));
})())((_m as any).value);
    case "list": return ((htype_: HaskellSyntax.Type) => Serialization.bracketList(Serialization.inlineStyle)([typeToExpr(htype_)]))((_m as any).value);
    case "tuple": return ((types: ReadonlyArray<HaskellSyntax.Type>) => Serialization.parenList(false)(LibLists.map(typeToExpr)(types)))((_m as any).value);
    case "variable": return ((name: HaskellSyntax.Name) => nameToExpr(name))((_m as any).value);
  }
})();
}

export function valueBindingToExpr(vb: HaskellSyntax.ValueBinding): Ast.Expr {
  return (() => {
  const _m = vb;
  switch (_m.tag) {
    case "simple": return ((simpleVB: HaskellSyntax.SimpleValueBinding) => (() => {
  const pat = ((_x) => _x.pattern)(simpleVB);
  const rhs = ((_x) => _x.rhs)(simpleVB);
  const local = ((_x) => _x.localBindings)(simpleVB);
  const lhsExpr = patternToExpr(pat);
  const rhsExpr = rightHandSideToExpr(rhs);
  const inlineBody = Serialization.structuralSpaceSep([lhsExpr, Serialization.cst("="), rhsExpr]);
  const body = LibLogic.ifElse(LibEquality.gt(Serialization.expressionLength(inlineBody))(120))(Serialization.newlineSep([Serialization.spaceSep([lhsExpr, Serialization.cst("=")]), Serialization.tabIndent(rhsExpr)]))(inlineBody);
  return LibMaybes.maybe(body)(((localBindings: HaskellSyntax.LocalBindings) => (() => {
  const bindings = ((_x) => _x)(localBindings);
  return Serialization.indentBlock(LibLists.cons(body)([Serialization.indentBlock(LibLists.cons(Serialization.cst("where"))(LibLists.map(localBindingToExpr)(bindings)))]));
})()))(local);
})())((_m as any).value);
  }
})();
}

export function variableToExpr(variable: HaskellSyntax.Variable): Ast.Expr {
  return nameToExpr(((_x) => _x)(variable));
}

export function writeQualifiedName(qname: HaskellSyntax.QualifiedName): string {
  return (() => {
  const qualifiers = ((_x) => _x.qualifiers)(qname);
  const unqual = ((_x) => _x.unqualified)(qname);
  const h = ((namePart: HaskellSyntax.NamePart) => ((_x) => _x)(namePart));
  const allParts = LibLists.concat2(LibLists.map(h)(qualifiers))([h(unqual)]);
  return LibStrings.intercalate(".")(allParts);
})();
}

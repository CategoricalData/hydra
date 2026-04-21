// Note: this is an automatically generated file. Do not edit.

/**
 * Python serializer: converts Python AST to concrete syntax
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
import * as PythonSyntax from "./syntax.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Serialization from "../serialization.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function encodeAnnotatedRhs(arhs: PythonSyntax.AnnotatedRhs): Ast.Expr {
  return Serialization.spaceSep([Serialization.cst("="), (() => {
  const _m = arhs;
  switch (_m.tag) {
    case "star": return ((ses: ReadonlyArray<PythonSyntax.StarExpression>) => Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeStarExpression)(ses)))((_m as any).value);
    case "yield": return ((_: PythonSyntax.YieldExpression) => Serialization.cst("yield ..."))((_m as any).value);
  }
})()]);
}

export function encodeAnnotatedStatement(as_: PythonSyntax.AnnotatedStatement): Ast.Expr {
  return (() => {
  const doc_ = ((_x) => _x.comment)(as_);
  const stmt = ((_x) => _x.statement)(as_);
  return Serialization.newlineSep([Serialization.cst(toPythonComments(doc_)), encodeStatement(stmt)]);
})();
}

export function encodeAnnotation(ann: PythonSyntax.Annotation): Ast.Expr {
  return Serialization.spaceSep([Serialization.cst(":"), encodeExpression(((_x) => _x)(ann))]);
}

export function encodeArgs(args: PythonSyntax.Args): Ast.Expr {
  return (() => {
  const pos = ((_x) => _x.positional)(args);
  const ks = ((_x) => _x.kwargOrStarred)(args);
  const kss = ((_x) => _x.kwargOrDoubleStarred)(args);
  return Serialization.commaSep(Serialization.inlineStyle)(LibLists.concat([LibLists.map(encodePosArg)(pos), LibLists.map(encodeKwargOrStarred)(ks), LibLists.map(encodeKwargOrDoubleStarred)(kss)]));
})();
}

export function encodeAssignment(a: PythonSyntax.Assignment): Ast.Expr {
  return (() => {
  const _m = a;
  switch (_m.tag) {
    case "typed": return ((t: PythonSyntax.TypedAssignment) => encodeTypedAssignment(t))((_m as any).value);
    case "untyped": return ((u: PythonSyntax.UntypedAssignment) => encodeUntypedAssignment(u))((_m as any).value);
    case "aug": return ((_: PythonSyntax.AugAssignment) => Serialization.cst("... += ..."))((_m as any).value);
  }
})();
}

export function encodeAssignmentExpression(ae: PythonSyntax.AssignmentExpression): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(ae);
  const expr = ((_x) => _x.expression)(ae);
  return Serialization.spaceSep([encodeName(name), Serialization.cst(":="), encodeExpression(expr)]);
})();
}

export function encodeAtom(atom: PythonSyntax.Atom): Ast.Expr {
  return (() => {
  const _m = atom;
  switch (_m.tag) {
    case "dict": return ((d: PythonSyntax.Dict) => encodeDict(d))((_m as any).value);
    case "dictcomp": return ((_: PythonSyntax.Dictcomp) => Serialization.cst("{...}"))((_m as any).value);
    case "ellipsis": return ((_: void) => Serialization.cst("..."))((_m as any).value);
    case "false": return ((_: void) => Serialization.cst("False"))((_m as any).value);
    case "genexp": return ((_: PythonSyntax.Genexp) => Serialization.cst("(...)"))((_m as any).value);
    case "group": return ((g: PythonSyntax.Group) => encodeGroup(g))((_m as any).value);
    case "list": return ((l: PythonSyntax.List) => encodeList(l))((_m as any).value);
    case "listcomp": return ((_: PythonSyntax.Listcomp) => Serialization.cst("[...]"))((_m as any).value);
    case "name": return ((n: PythonSyntax.Name) => encodeName(n))((_m as any).value);
    case "none": return ((_: void) => Serialization.cst("None"))((_m as any).value);
    case "number": return ((n: PythonSyntax.Number) => encodeNumber(n))((_m as any).value);
    case "set": return ((s: PythonSyntax.Set) => encodeSet(s))((_m as any).value);
    case "setcomp": return ((_: PythonSyntax.Setcomp) => Serialization.cst("{...}"))((_m as any).value);
    case "string": return ((s: PythonSyntax.String) => encodeString(s))((_m as any).value);
    case "true": return ((_: void) => Serialization.cst("True"))((_m as any).value);
    case "tuple": return ((t: PythonSyntax.Tuple) => encodeTuple(t))((_m as any).value);
  }
})();
}

export function encodeAttribute(attr: PythonSyntax.Attribute): Ast.Expr {
  return Serialization.dotSep(LibLists.map(encodeName)(((_x) => _x)(attr)));
}

export function encodeAwaitPrimary(ap: PythonSyntax.AwaitPrimary): Ast.Expr {
  return (() => {
  const await_ = ((_x) => _x.await)(ap);
  const primary = ((_x) => _x.primary)(ap);
  return LibLogic.ifElse(await_)(Serialization.spaceSep([Serialization.cst("await"), encodePrimary(primary)]))(encodePrimary(primary));
})();
}

export function encodeBitwiseAnd(band: PythonSyntax.BitwiseAnd): Ast.Expr {
  return (() => {
  const lhs = ((_x) => _x.lhs)(band);
  const rhs = ((_x) => _x.rhs)(band);
  return Serialization.spaceSep(LibMaybes.cat([LibMaybes.map(((l: PythonSyntax.BitwiseAnd) => Serialization.spaceSep([encodeBitwiseAnd(l), Serialization.cst("&")])))(lhs), encodeShiftExpression(rhs)]));
})();
}

export function encodeBitwiseOr(bor: PythonSyntax.BitwiseOr): Ast.Expr {
  return (() => {
  const lhs = ((_x) => _x.lhs)(bor);
  const rhs = ((_x) => _x.rhs)(bor);
  return Serialization.spaceSep(LibMaybes.cat([LibMaybes.map(((l: PythonSyntax.BitwiseOr) => Serialization.spaceSep([encodeBitwiseOr(l), Serialization.cst("|")])))(lhs), encodeBitwiseXor(rhs)]));
})();
}

export function encodeBitwiseXor(bxor: PythonSyntax.BitwiseXor): Ast.Expr {
  return (() => {
  const lhs = ((_x) => _x.lhs)(bxor);
  const rhs = ((_x) => _x.rhs)(bxor);
  return Serialization.spaceSep(LibMaybes.cat([LibMaybes.map(((l: PythonSyntax.BitwiseXor) => Serialization.spaceSep([encodeBitwiseXor(l), Serialization.cst("^")])))(lhs), encodeBitwiseAnd(rhs)]));
})();
}

export function encodeBlock(b: PythonSyntax.Block): Ast.Expr {
  return (() => {
  const _m = b;
  switch (_m.tag) {
    case "indented": return ((groups: ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>) => Serialization.tabIndentDoubleSpace(LibLists.map(((stmts: ReadonlyArray<PythonSyntax.Statement>) => Serialization.newlineSep(LibLists.map(encodeStatement)(stmts))))(groups)))((_m as any).value);
    case "simple": return ((ss: ReadonlyArray<PythonSyntax.SimpleStatement>) => Serialization.semicolonSep(LibLists.map(encodeSimpleStatement)(ss)))((_m as any).value);
  }
})();
}

export function encodeCapturePattern(cp: PythonSyntax.CapturePattern): Ast.Expr {
  return encodePatternCaptureTarget(((_x) => _x)(cp));
}

export function encodeCaseBlock(cb: PythonSyntax.CaseBlock): Ast.Expr {
  return (() => {
  const patterns = ((_x) => _x.patterns)(cb);
  const guard = ((_x) => _x.guard)(cb);
  const body = ((_x) => _x.body)(cb);
  return Serialization.newlineSep([Serialization.noSep([Serialization.spaceSep(LibMaybes.cat([Serialization.cst("case"), encodePatterns(patterns), LibMaybes.map(encodeGuard)(guard)])), Serialization.cst(":")]), encodeBlock(body)]);
})();
}

export function encodeClassDefinition(cd: PythonSyntax.ClassDefinition): Ast.Expr {
  return (() => {
  const decs = ((_x) => _x.decorators)(cd);
  const name = ((_x) => _x.name)(cd);
  const args = ((_x) => _x.arguments)(cd);
  const body = ((_x) => _x.body)(cd);
  const argPart = LibMaybes.map(((a: PythonSyntax.Args) => Serialization.noSep([Serialization.cst("("), encodeArgs(a), Serialization.cst(")")])))(args);
  return Serialization.newlineSep(LibMaybes.cat([LibMaybes.map(encodeDecorators)(decs), Serialization.noSep(LibMaybes.cat([Serialization.spaceSep([Serialization.cst("class"), encodeName(name)]), argPart, Serialization.cst(":")])), encodeBlock(body)]));
})();
}

export function encodeClassPattern(cp: PythonSyntax.ClassPattern): Ast.Expr {
  return (() => {
  const noa = ((_x) => _x.nameOrAttribute)(cp);
  const pos = ((_x) => _x.positionalPatterns)(cp);
  const kw = ((_x) => _x.keywordPatterns)(cp);
  return Serialization.noSep(LibMaybes.cat([encodeNameOrAttribute(noa), Serialization.cst("("), LibMaybes.map(encodePositionalPatterns)(pos), LibMaybes.map(encodeKeywordPatterns)(kw), Serialization.cst(")")]));
})();
}

export function encodeClosedPattern(cp: PythonSyntax.ClosedPattern): Ast.Expr {
  return (() => {
  const _m = cp;
  switch (_m.tag) {
    case "literal": return ((_: PythonSyntax.LiteralExpression) => Serialization.cst("..."))((_m as any).value);
    case "capture": return ((c: PythonSyntax.CapturePattern) => encodeCapturePattern(c))((_m as any).value);
    case "wildcard": return ((_: void) => Serialization.cst("_"))((_m as any).value);
    case "value": return ((v: PythonSyntax.ValuePattern) => encodeValuePattern(v))((_m as any).value);
    case "group": return ((_: PythonSyntax.GroupPattern) => Serialization.cst("(...)"))((_m as any).value);
    case "sequence": return ((_: PythonSyntax.SequencePattern) => Serialization.cst("[...]"))((_m as any).value);
    case "mapping": return ((_: PythonSyntax.MappingPattern) => Serialization.cst("{...}"))((_m as any).value);
    case "class": return ((c: PythonSyntax.ClassPattern) => encodeClassPattern(c))((_m as any).value);
  }
})();
}

export function encodeComparison(cmp: PythonSyntax.Comparison): Ast.Expr {
  return encodeBitwiseOr(((_x) => _x.lhs)(cmp));
}

export function encodeCompoundStatement(cs: PythonSyntax.CompoundStatement): Ast.Expr {
  return (() => {
  const _m = cs;
  switch (_m.tag) {
    case "function": return ((f: PythonSyntax.FunctionDefinition) => encodeFunctionDefinition(f))((_m as any).value);
    case "if": return ((_: PythonSyntax.IfStatement) => Serialization.cst("if ..."))((_m as any).value);
    case "classDef": return ((c: PythonSyntax.ClassDefinition) => encodeClassDefinition(c))((_m as any).value);
    case "with": return ((_: PythonSyntax.WithStatement) => Serialization.cst("with ..."))((_m as any).value);
    case "for": return ((_: PythonSyntax.ForStatement) => Serialization.cst("for ..."))((_m as any).value);
    case "try": return ((_: PythonSyntax.TryStatement) => Serialization.cst("try ..."))((_m as any).value);
    case "while": return ((w: PythonSyntax.WhileStatement) => encodeWhileStatement(w))((_m as any).value);
    case "match": return ((m: PythonSyntax.MatchStatement) => encodeMatchStatement(m))((_m as any).value);
  }
})();
}

export function encodeConditional(c: PythonSyntax.Conditional): Ast.Expr {
  return (() => {
  const body = ((_x) => _x.body)(c);
  const cond = ((_x) => _x.if)(c);
  const elseExpr = ((_x) => _x.else)(c);
  return Serialization.spaceSep([encodeDisjunction(body), Serialization.cst("if"), encodeDisjunction(cond), Serialization.cst("else"), encodeExpression(elseExpr)]);
})();
}

export function encodeConjunction(c: PythonSyntax.Conjunction): Ast.Expr {
  return Serialization.symbolSep("and")(Serialization.inlineStyle)(LibLists.map(encodeInversion)(((_x) => _x)(c)));
}

export function encodeDecorators(decs: PythonSyntax.Decorators): Ast.Expr {
  return Serialization.newlineSep(LibLists.map(((ne: PythonSyntax.NamedExpression) => Serialization.noSep([Serialization.cst("@"), encodeNamedExpression(ne)])))(((_x) => _x)(decs)));
}

export function encodeDict(d: PythonSyntax.Dict): Ast.Expr {
  return Serialization.curlyBracesList(null)(Serialization.halfBlockStyle)(LibLists.map(encodeDoubleStarredKvpair)(((_x) => _x)(d)));
}

export function encodeDisjunction(d: PythonSyntax.Disjunction): Ast.Expr {
  return Serialization.symbolSep("or")(Serialization.inlineStyle)(LibLists.map(encodeConjunction)(((_x) => _x)(d)));
}

export function encodeDottedAsName(dan: PythonSyntax.DottedAsName): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(dan);
  const alias = ((_x) => _x.as)(dan);
  return Serialization.spaceSep(LibMaybes.cat([encodeDottedName(name), LibMaybes.map(((a: PythonSyntax.Name) => Serialization.spaceSep([Serialization.cst("as"), encodeName(a)])))(alias)]));
})();
}

export function encodeDottedName(dn: PythonSyntax.DottedName): Ast.Expr {
  return Serialization.cst(LibStrings.intercalate(".")(LibLists.map(((n: PythonSyntax.Name) => ((_x) => _x)(n)))(((_x) => _x)(dn))));
}

export function encodeDoubleStarredKvpair(dskv: PythonSyntax.DoubleStarredKvpair): Ast.Expr {
  return (() => {
  const _m = dskv;
  switch (_m.tag) {
    case "pair": return ((p: PythonSyntax.Kvpair) => encodeKvpair(p))((_m as any).value);
    case "starred": return ((e: PythonSyntax.BitwiseOr) => Serialization.noSep([Serialization.cst("**"), encodeBitwiseOr(e)]))((_m as any).value);
  }
})();
}

export function encodeExpression(expr: PythonSyntax.Expression): Ast.Expr {
  return (() => {
  const _m = expr;
  switch (_m.tag) {
    case "simple": return ((d: PythonSyntax.Disjunction) => encodeDisjunction(d))((_m as any).value);
    case "conditional": return ((c: PythonSyntax.Conditional) => encodeConditional(c))((_m as any).value);
    case "lambda": return ((l: PythonSyntax.Lambda) => encodeLambda(l))((_m as any).value);
  }
})();
}

export function encodeFactor(f: PythonSyntax.Factor): Ast.Expr {
  return (() => {
  const _m = f;
  switch (_m.tag) {
    case "positive": return ((inner: PythonSyntax.Factor) => Serialization.noSep([Serialization.cst("+"), encodeFactor(inner)]))((_m as any).value);
    case "negative": return ((inner: PythonSyntax.Factor) => Serialization.noSep([Serialization.cst("-"), encodeFactor(inner)]))((_m as any).value);
    case "complement": return ((inner: PythonSyntax.Factor) => Serialization.noSep([Serialization.cst("~"), encodeFactor(inner)]))((_m as any).value);
    case "simple": return ((p: PythonSyntax.Power) => encodePower(p))((_m as any).value);
  }
})();
}

export function encodeFunctionDefRaw(fdr: PythonSyntax.FunctionDefRaw): Ast.Expr {
  return (() => {
  const async_ = ((_x) => _x.async)(fdr);
  const name = ((_x) => _x.name)(fdr);
  const tparams = ((_x) => _x.typeParams)(fdr);
  const params = ((_x) => _x.params)(fdr);
  const retType = ((_x) => _x.returnType)(fdr);
  const block = ((_x) => _x.block)(fdr);
  const asyncKw = LibLogic.ifElse(async_)(Serialization.cst("async"))(null);
  const tparamPart = LibLogic.ifElse(LibLists.null_(tparams))(null)(Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(encodeTypeParameter)(tparams)));
  const paramPart = LibMaybes.map(encodeParameters)(params);
  const retPart = LibMaybes.map(((t: PythonSyntax.Expression) => Serialization.spaceSep([Serialization.cst("->"), encodeExpression(t)])))(retType);
  return Serialization.newlineSep([Serialization.noSep([Serialization.spaceSep(LibMaybes.cat([asyncKw, Serialization.cst("def"), Serialization.noSep(LibMaybes.cat([encodeName(name), tparamPart, Serialization.cst("("), paramPart, Serialization.cst(")")])), retPart])), Serialization.cst(":")]), encodeBlock(block)]);
})();
}

export function encodeFunctionDefinition(fd: PythonSyntax.FunctionDefinition): Ast.Expr {
  return (() => {
  const decs = ((_x) => _x.decorators)(fd);
  const raw = ((_x) => _x.raw)(fd);
  return Serialization.newlineSep(LibMaybes.cat([LibMaybes.map(encodeDecorators)(decs), encodeFunctionDefRaw(raw)]));
})();
}

export function encodeGroup(g: PythonSyntax.Group): Ast.Expr {
  return (() => {
  const _m = g;
  switch (_m.tag) {
    case "expression": return ((ne: PythonSyntax.NamedExpression) => encodeNamedExpression(ne))((_m as any).value);
    case "yield": return ((_: PythonSyntax.YieldExpression) => Serialization.cst("(yield ...)"))((_m as any).value);
  }
})();
}

export function encodeGuard(g: PythonSyntax.Guard): Ast.Expr {
  return Serialization.spaceSep([Serialization.cst("if"), encodeNamedExpression(((_x) => _x)(g))]);
}

export function encodeImportFrom(if_: PythonSyntax.ImportFrom): Ast.Expr {
  return (() => {
  const prefixes = ((_x) => _x.prefixes)(if_);
  const name = ((_x) => _x.dottedName)(if_);
  const targets = ((_x) => _x.targets)(if_);
  const lhs = Serialization.noSep(LibMaybes.cat(LibLists.concat([LibLists.map(((p: PythonSyntax.RelativeImportPrefix) => encodeRelativeImportPrefix(p)))(prefixes), [LibMaybes.map(encodeDottedName)(name)]])));
  return Serialization.spaceSep([Serialization.cst("from"), lhs, Serialization.cst("import"), encodeImportFromTargets(targets)]);
})();
}

export function encodeImportFromAsName(ifan: PythonSyntax.ImportFromAsName): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(ifan);
  const alias = ((_x) => _x.as)(ifan);
  return LibMaybes.maybe(encodeName(name))(((a: PythonSyntax.Name) => Serialization.spaceSep([encodeName(name), Serialization.cst("as"), encodeName(a)])))(alias);
})();
}

export function encodeImportFromTargets(t: PythonSyntax.ImportFromTargets): Ast.Expr {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "simple": return ((names: ReadonlyArray<PythonSyntax.ImportFromAsName>) => Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeImportFromAsName)(names)))((_m as any).value);
    case "parens": return ((names: ReadonlyArray<PythonSyntax.ImportFromAsName>) => Serialization.noSep([Serialization.cst("("), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeImportFromAsName)(names)), Serialization.cst(")")]))((_m as any).value);
    case "star": return ((_: void) => Serialization.cst("*"))((_m as any).value);
  }
})();
}

export function encodeImportName(in_: PythonSyntax.ImportName): Ast.Expr {
  return Serialization.spaceSep([Serialization.cst("import"), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeDottedAsName)(((_x) => _x)(in_)))]);
}

export function encodeImportStatement(is_: PythonSyntax.ImportStatement): Ast.Expr {
  return (() => {
  const _m = is_;
  switch (_m.tag) {
    case "name": return ((n: PythonSyntax.ImportName) => encodeImportName(n))((_m as any).value);
    case "from": return ((f: PythonSyntax.ImportFrom) => encodeImportFrom(f))((_m as any).value);
  }
})();
}

export function encodeInversion(i: PythonSyntax.Inversion): Ast.Expr {
  return (() => {
  const _m = i;
  switch (_m.tag) {
    case "not": return ((other: PythonSyntax.Inversion) => Serialization.spaceSep([Serialization.cst("not"), encodeInversion(other)]))((_m as any).value);
    case "simple": return ((c: PythonSyntax.Comparison) => encodeComparison(c))((_m as any).value);
  }
})();
}

export function encodeKeywordPattern(kp: PythonSyntax.KeywordPattern): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(kp);
  const pat = ((_x) => _x.pattern)(kp);
  return Serialization.noSep([encodeName(name), Serialization.cst("="), encodePattern(pat)]);
})();
}

export function encodeKeywordPatterns(kp: PythonSyntax.KeywordPatterns): Ast.Expr {
  return Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeKeywordPattern)(((_x) => _x)(kp)));
}

export function encodeKvpair(kv: PythonSyntax.Kvpair): Ast.Expr {
  return (() => {
  const k = ((_x) => _x.key)(kv);
  const v = ((_x) => _x.value)(kv);
  return Serialization.spaceSep([Serialization.noSep([encodeExpression(k), Serialization.cst(":")]), encodeExpression(v)]);
})();
}

export function encodeKwarg(k: PythonSyntax.Kwarg): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(k);
  const expr = ((_x) => _x.value)(k);
  return Serialization.noSep([encodeName(name), Serialization.cst("="), encodeExpression(expr)]);
})();
}

export function encodeKwargOrDoubleStarred(kds: PythonSyntax.KwargOrDoubleStarred): Ast.Expr {
  return (() => {
  const _m = kds;
  switch (_m.tag) {
    case "kwarg": return ((k: PythonSyntax.Kwarg) => encodeKwarg(k))((_m as any).value);
    case "doubleStarred": return ((e: PythonSyntax.Expression) => Serialization.noSep([Serialization.cst("**"), encodeExpression(e)]))((_m as any).value);
  }
})();
}

export function encodeKwargOrStarred(ks: PythonSyntax.KwargOrStarred): Ast.Expr {
  return (() => {
  const _m = ks;
  switch (_m.tag) {
    case "kwarg": return ((k: PythonSyntax.Kwarg) => encodeKwarg(k))((_m as any).value);
    case "starred": return ((se: PythonSyntax.StarredExpression) => encodeStarredExpression(se))((_m as any).value);
  }
})();
}

export function encodeLambda(l: PythonSyntax.Lambda): Ast.Expr {
  return (() => {
  const params = ((_x) => _x.params)(l);
  const body = ((_x) => _x.body)(l);
  return Serialization.parens(Serialization.spaceSep([Serialization.cst("lambda"), Serialization.noSep([encodeLambdaParameters(params), Serialization.cst(":")]), encodeExpression(body)]));
})();
}

export function encodeLambdaParamNoDefault(p: PythonSyntax.LambdaParamNoDefault): Ast.Expr {
  return encodeName(((_x) => _x)(p));
}

export function encodeLambdaParameters(lp: PythonSyntax.LambdaParameters): Ast.Expr {
  return (() => {
  const nodef = ((_x) => _x.paramNoDefault)(lp);
  return Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeLambdaParamNoDefault)(nodef));
})();
}

export function encodeLambdaStarEtc(lse: PythonSyntax.LambdaStarEtc): Ast.Expr {
  return (() => {
  const _m = lse;
  switch (_m.tag) {
    case "paramNoDefault": return ((p: PythonSyntax.LambdaParamNoDefault) => encodeLambdaParamNoDefault(p))((_m as any).value);
    case "star": return ((_: boolean) => Serialization.cst("*..."))((_m as any).value);
    case "paramMaybeDefault": return ((_: ReadonlyArray<PythonSyntax.LambdaParamMaybeDefault>) => Serialization.cst("..."))((_m as any).value);
    case "kwds": return ((_: PythonSyntax.LambdaKwds) => Serialization.cst("**..."))((_m as any).value);
  }
})();
}

export function encodeList(l: PythonSyntax.List): Ast.Expr {
  return Serialization.bracketListAdaptive(LibLists.map(encodeStarNamedExpression)(((_x) => _x)(l)));
}

export function encodeMatchStatement(ms: PythonSyntax.MatchStatement): Ast.Expr {
  return (() => {
  const subj = ((_x) => _x.subject)(ms);
  const cases = ((_x) => _x.cases)(ms);
  return Serialization.newlineSep([Serialization.spaceSep([Serialization.cst("match"), Serialization.noSep([encodeSubjectExpression(subj), Serialization.cst(":")])]), Serialization.tabIndentDoubleSpace(LibLists.map(encodeCaseBlock)(cases))]);
})();
}

export function encodeModule(mod: PythonSyntax.Module): Ast.Expr {
  return (() => {
  const warning = Serialization.cst(toPythonComments(Constants.warningAutoGeneratedFile));
  const groups = LibLists.map(((group: ReadonlyArray<PythonSyntax.Statement>) => Serialization.newlineSep(LibLists.map(encodeStatement)(group))))(((_x) => _x)(mod));
  return Serialization.doubleNewlineSep(LibLists.cons(warning)(groups));
})();
}

export function encodeName(n: PythonSyntax.Name): Ast.Expr {
  return Serialization.cst(((_x) => _x)(n));
}

export function encodeNameOrAttribute(noa: PythonSyntax.NameOrAttribute): Ast.Expr {
  return Serialization.dotSep(LibLists.map(encodeName)(((_x) => _x)(noa)));
}

export function encodeNamedExpression(ne: PythonSyntax.NamedExpression): Ast.Expr {
  return (() => {
  const _m = ne;
  switch (_m.tag) {
    case "simple": return ((e: PythonSyntax.Expression) => encodeExpression(e))((_m as any).value);
    case "assignment": return ((ae: PythonSyntax.AssignmentExpression) => encodeAssignmentExpression(ae))((_m as any).value);
  }
})();
}

export function encodeNumber(num: PythonSyntax.Number): Ast.Expr {
  return (() => {
  const _m = num;
  switch (_m.tag) {
    case "float": return ((f: number) => Serialization.cst(pythonFloatLiteralText(LibLiterals.showBigfloat(f))))((_m as any).value);
    case "integer": return ((i: bigint) => Serialization.cst(LibLiterals.showBigint(i)))((_m as any).value);
  }
})();
}

export function encodeOrPattern(op: PythonSyntax.OrPattern): Ast.Expr {
  return Serialization.symbolSep("|")(Serialization.inlineStyle)(LibLists.map(encodeClosedPattern)(((_x) => _x)(op)));
}

export function encodeParam(p: PythonSyntax.Param): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(p);
  const ann = ((_x) => _x.annotation)(p);
  return Serialization.noSep(LibMaybes.cat([encodeName(name), LibMaybes.map(encodeAnnotation)(ann)]));
})();
}

export function encodeParamNoDefault(pnd: PythonSyntax.ParamNoDefault): Ast.Expr {
  return encodeParam(((_x) => _x.param)(pnd));
}

export function encodeParamNoDefaultParameters(pndp: PythonSyntax.ParamNoDefaultParameters): Ast.Expr {
  return (() => {
  const nodef = ((_x) => _x.paramNoDefault)(pndp);
  return Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeParamNoDefault)(nodef));
})();
}

export function encodeParameters(p: PythonSyntax.Parameters): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "paramNoDefault": return ((pnd: PythonSyntax.ParamNoDefaultParameters) => encodeParamNoDefaultParameters(pnd))((_m as any).value);
    case "slashNoDefault": return ((_: PythonSyntax.SlashNoDefaultParameters) => Serialization.cst("..."))((_m as any).value);
    case "slashWithDefault": return ((_: PythonSyntax.SlashWithDefaultParameters) => Serialization.cst("..."))((_m as any).value);
  }
})();
}

export function encodePattern(p: PythonSyntax.Pattern): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "or": return ((op: PythonSyntax.OrPattern) => encodeOrPattern(op))((_m as any).value);
    case "as": return ((_: PythonSyntax.AsPattern) => Serialization.cst("... as ..."))((_m as any).value);
  }
})();
}

export function encodePatternCaptureTarget(pct: PythonSyntax.PatternCaptureTarget): Ast.Expr {
  return encodeName(((_x) => _x)(pct));
}

export function encodePatterns(ps: PythonSyntax.Patterns): Ast.Expr {
  return (() => {
  const _m = ps;
  switch (_m.tag) {
    case "pattern": return ((p: PythonSyntax.Pattern) => encodePattern(p))((_m as any).value);
    case "sequence": return ((_: PythonSyntax.OpenSequencePattern) => Serialization.cst("..."))((_m as any).value);
  }
})();
}

export function encodePosArg(pa: PythonSyntax.PosArg): Ast.Expr {
  return (() => {
  const _m = pa;
  switch (_m.tag) {
    case "starred": return ((se: PythonSyntax.StarredExpression) => encodeStarredExpression(se))((_m as any).value);
    case "assignment": return ((ae: PythonSyntax.AssignmentExpression) => encodeAssignmentExpression(ae))((_m as any).value);
    case "expression": return ((e: PythonSyntax.Expression) => encodeExpression(e))((_m as any).value);
  }
})();
}

export function encodePositionalPatterns(pp: PythonSyntax.PositionalPatterns): Ast.Expr {
  return Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodePattern)(((_x) => _x)(pp)));
}

export function encodePower(p: PythonSyntax.Power): Ast.Expr {
  return (() => {
  const lhs = ((_x) => _x.lhs)(p);
  const rhs = ((_x) => _x.rhs)(p);
  return Serialization.spaceSep(LibMaybes.cat([encodeAwaitPrimary(lhs), LibMaybes.map(((r: PythonSyntax.Factor) => Serialization.spaceSep([Serialization.cst("**"), encodeFactor(r)])))(rhs)]));
})();
}

export function encodePrimary(p: PythonSyntax.Primary): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "simple": return ((a: PythonSyntax.Atom) => encodeAtom(a))((_m as any).value);
    case "compound": return ((pwr: PythonSyntax.PrimaryWithRhs) => encodePrimaryWithRhs(pwr))((_m as any).value);
  }
})();
}

export function encodePrimaryRhs(rhs: PythonSyntax.PrimaryRhs): Ast.Expr {
  return (() => {
  const _m = rhs;
  switch (_m.tag) {
    case "call": return ((args: PythonSyntax.Args) => Serialization.noSep([Serialization.cst("("), encodeArgs(args), Serialization.cst(")")]))((_m as any).value);
    case "project": return ((name: PythonSyntax.Name) => Serialization.noSep([Serialization.cst("."), encodeName(name)]))((_m as any).value);
    case "slices": return ((slices: PythonSyntax.Slices) => Serialization.noSep([Serialization.cst("["), encodeSlices(slices), Serialization.cst("]")]))((_m as any).value);
    case "genexp": return ((_: PythonSyntax.Genexp) => Serialization.cst("[...]"))((_m as any).value);
  }
})();
}

export function encodePrimaryWithRhs(pwr: PythonSyntax.PrimaryWithRhs): Ast.Expr {
  return (() => {
  const prim = ((_x) => _x.primary)(pwr);
  const rhs = ((_x) => _x.rhs)(pwr);
  return Serialization.noSep([encodePrimary(prim), encodePrimaryRhs(rhs)]);
})();
}

export function encodeRaiseExpression(re: PythonSyntax.RaiseExpression): Ast.Expr {
  return (() => {
  const expr = ((_x) => _x.expression)(re);
  const from_ = ((_x) => _x.from)(re);
  return Serialization.spaceSep(LibMaybes.cat([encodeExpression(expr), LibMaybes.map(((f: PythonSyntax.Expression) => Serialization.spaceSep([Serialization.cst("from"), encodeExpression(f)])))(from_)]));
})();
}

export function encodeRaiseStatement(rs: PythonSyntax.RaiseStatement): Ast.Expr {
  return Serialization.spaceSep(LibMaybes.cat([Serialization.cst("raise"), LibMaybes.map(encodeRaiseExpression)(((_x) => _x)(rs))]));
}

export function encodeRelativeImportPrefix(p: PythonSyntax.RelativeImportPrefix): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "dot": return ((_: void) => Serialization.cst("."))((_m as any).value);
    case "ellipsis": return ((_: void) => Serialization.cst("..."))((_m as any).value);
  }
})();
}

export function encodeReturnStatement(rs: PythonSyntax.ReturnStatement): Ast.Expr {
  return Serialization.spaceSep([Serialization.cst("return"), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(encodeStarExpression)(((_x) => _x)(rs)))]);
}

export function encodeSet(s: PythonSyntax.Set): Ast.Expr {
  return Serialization.bracesListAdaptive(LibLists.map(encodeStarNamedExpression)(((_x) => _x)(s)));
}

export function encodeShiftExpression(se: PythonSyntax.ShiftExpression): Ast.Expr {
  return encodeSum(((_x) => _x.rhs)(se));
}

export function encodeSimpleStatement(ss: PythonSyntax.SimpleStatement): Ast.Expr {
  return (() => {
  const _m = ss;
  switch (_m.tag) {
    case "assignment": return ((a: PythonSyntax.Assignment) => encodeAssignment(a))((_m as any).value);
    case "starExpressions": return ((es: ReadonlyArray<PythonSyntax.StarExpression>) => Serialization.newlineSep(LibLists.map(encodeStarExpression)(es)))((_m as any).value);
    case "return": return ((r: PythonSyntax.ReturnStatement) => encodeReturnStatement(r))((_m as any).value);
    case "raise": return ((r: PythonSyntax.RaiseStatement) => encodeRaiseStatement(r))((_m as any).value);
    case "pass": return ((_: void) => Serialization.cst("pass"))((_m as any).value);
    case "break": return ((_: void) => Serialization.cst("break"))((_m as any).value);
    case "continue": return ((_: void) => Serialization.cst("continue"))((_m as any).value);
    case "import": return ((i: PythonSyntax.ImportStatement) => encodeImportStatement(i))((_m as any).value);
    case "typeAlias": return ((t: PythonSyntax.TypeAlias) => encodeTypeAlias(t))((_m as any).value);
    case "assert": return ((_: PythonSyntax.AssertStatement) => Serialization.cst("assert ..."))((_m as any).value);
    case "global": return ((_: ReadonlyArray<PythonSyntax.Name>) => Serialization.cst("global ..."))((_m as any).value);
    case "nonlocal": return ((_: ReadonlyArray<PythonSyntax.Name>) => Serialization.cst("nonlocal ..."))((_m as any).value);
    case "del": return ((_: PythonSyntax.DelStatement) => Serialization.cst("del ..."))((_m as any).value);
  }
})();
}

export function encodeSimpleTypeParameter(stp: PythonSyntax.SimpleTypeParameter): Ast.Expr {
  return encodeName(((_x) => _x.name)(stp));
}

export function encodeSingleTarget(st: PythonSyntax.SingleTarget): Ast.Expr {
  return (() => {
  const _m = st;
  switch (_m.tag) {
    case "name": return ((n: PythonSyntax.Name) => encodeName(n))((_m as any).value);
    case "parens": return ((_: PythonSyntax.SingleTarget) => Serialization.cst("(...)"))((_m as any).value);
    case "subscriptAttributeTarget": return ((_: PythonSyntax.SingleSubscriptAttributeTarget) => Serialization.cst("..."))((_m as any).value);
  }
})();
}

export function encodeSlice(s: PythonSyntax.Slice): Ast.Expr {
  return (() => {
  const _m = s;
  switch (_m.tag) {
    case "named": return ((ne: PythonSyntax.NamedExpression) => encodeNamedExpression(ne))((_m as any).value);
    case "slice_": return ((_: PythonSyntax.SliceExpression) => Serialization.cst(":"))((_m as any).value);
  }
})();
}

export function encodeSliceOrStarredExpression(s: PythonSyntax.SliceOrStarredExpression): Ast.Expr {
  return (() => {
  const _m = s;
  switch (_m.tag) {
    case "slice": return ((sl: PythonSyntax.Slice) => encodeSlice(sl))((_m as any).value);
    case "starred": return ((se: PythonSyntax.StarredExpression) => encodeStarredExpression(se))((_m as any).value);
  }
})();
}

export function encodeSlices(s: PythonSyntax.Slices): Ast.Expr {
  return (() => {
  const hd = ((_x) => _x.head)(s);
  const tl = ((_x) => _x.tail)(s);
  return Serialization.commaSep(Serialization.inlineStyle)(LibLists.cons(encodeSlice(hd))(LibLists.map(encodeSliceOrStarredExpression)(tl)));
})();
}

export function encodeStarAtom(sa: PythonSyntax.StarAtom): Ast.Expr {
  return (() => {
  const _m = sa;
  switch (_m.tag) {
    case "name": return ((n: PythonSyntax.Name) => encodeName(n))((_m as any).value);
    case "targetWithStarAtom": return ((_: PythonSyntax.TargetWithStarAtom) => Serialization.cst("(...)"))((_m as any).value);
    case "starTargetsTupleSeq": return ((_: PythonSyntax.StarTargetsTupleSeq | null) => Serialization.cst("(...)"))((_m as any).value);
    case "starTargetsListSeq": return ((_: PythonSyntax.StarTargetsListSeq | null) => Serialization.cst("[...]"))((_m as any).value);
  }
})();
}

export function encodeStarExpression(se: PythonSyntax.StarExpression): Ast.Expr {
  return (() => {
  const _m = se;
  switch (_m.tag) {
    case "star": return ((bor: PythonSyntax.BitwiseOr) => Serialization.noSep([Serialization.cst("*"), encodeBitwiseOr(bor)]))((_m as any).value);
    case "simple": return ((e: PythonSyntax.Expression) => encodeExpression(e))((_m as any).value);
  }
})();
}

export function encodeStarNamedExpression(sne: PythonSyntax.StarNamedExpression): Ast.Expr {
  return (() => {
  const _m = sne;
  switch (_m.tag) {
    case "star": return ((bor: PythonSyntax.BitwiseOr) => Serialization.noSep([Serialization.cst("*"), encodeBitwiseOr(bor)]))((_m as any).value);
    case "simple": return ((ne: PythonSyntax.NamedExpression) => encodeNamedExpression(ne))((_m as any).value);
  }
})();
}

export function encodeStarTarget(st: PythonSyntax.StarTarget): Ast.Expr {
  return (() => {
  const _m = st;
  switch (_m.tag) {
    case "unstarred": return ((t: PythonSyntax.TargetWithStarAtom) => encodeTargetWithStarAtom(t))((_m as any).value);
    case "starred": return ((inner: PythonSyntax.StarTarget) => Serialization.noSep([Serialization.cst("*"), encodeStarTarget(inner)]))((_m as any).value);
  }
})();
}

export function encodeStarredExpression(se: PythonSyntax.StarredExpression): Ast.Expr {
  return Serialization.noSep([Serialization.cst("*"), encodeExpression(((_x) => _x)(se))]);
}

export function encodeStatement(stmt: PythonSyntax.Statement): Ast.Expr {
  return (() => {
  const _m = stmt;
  switch (_m.tag) {
    case "annotated": return ((a: PythonSyntax.AnnotatedStatement) => encodeAnnotatedStatement(a))((_m as any).value);
    case "simple": return ((ss: ReadonlyArray<PythonSyntax.SimpleStatement>) => Serialization.newlineSep(LibLists.map(encodeSimpleStatement)(ss)))((_m as any).value);
    case "compound": return ((c: PythonSyntax.CompoundStatement) => encodeCompoundStatement(c))((_m as any).value);
  }
})();
}

export function encodeString(s: PythonSyntax.String): Ast.Expr {
  return (() => {
  const content = ((_x) => _x.value)(s);
  const style = ((_x) => _x.quoteStyle)(s);
  return (() => {
  const _m = style;
  switch (_m.tag) {
    case "single": return ((_: void) => Serialization.cst(escapePythonString(false)(content)))((_m as any).value);
    case "double": return ((_: void) => Serialization.cst(escapePythonString(true)(content)))((_m as any).value);
    case "triple": return ((_: void) => Serialization.noSep([Serialization.cst("r\"\"\""), Serialization.cst(content), Serialization.cst("\"\"\"")]))((_m as any).value);
  }
})();
})();
}

export function encodeSubjectExpression(se: PythonSyntax.SubjectExpression): Ast.Expr {
  return (() => {
  const _m = se;
  switch (_m.tag) {
    case "simple": return ((ne: PythonSyntax.NamedExpression) => encodeNamedExpression(ne))((_m as any).value);
    case "tuple": return ((_: ReadonlyArray<PythonSyntax.StarNamedExpression>) => Serialization.cst("*..."))((_m as any).value);
  }
})();
}

export function encodeSum(s: PythonSyntax.Sum): Ast.Expr {
  return encodeTerm(((_x) => _x.rhs)(s));
}

export function encodeTPrimary(tp: PythonSyntax.TPrimary): Ast.Expr {
  return (() => {
  const _m = tp;
  switch (_m.tag) {
    case "atom": return ((a: PythonSyntax.Atom) => encodeAtom(a))((_m as any).value);
    case "primaryAndName": return ((pn: PythonSyntax.TPrimaryAndName) => encodeTPrimaryAndName(pn))((_m as any).value);
    case "primaryAndSlices": return ((_: PythonSyntax.TPrimaryAndSlices) => Serialization.cst("..."))((_m as any).value);
    case "primaryAndGenexp": return ((_: PythonSyntax.TPrimaryAndGenexp) => Serialization.cst("..."))((_m as any).value);
    case "primaryAndArguments": return ((_: PythonSyntax.TPrimaryAndArguments) => Serialization.cst("..."))((_m as any).value);
  }
})();
}

export function encodeTPrimaryAndName(pn: PythonSyntax.TPrimaryAndName): Ast.Expr {
  return (() => {
  const prim = ((_x) => _x.primary)(pn);
  const name_ = ((_x) => _x.name)(pn);
  return Serialization.noSep([encodeTPrimary(prim), Serialization.cst("."), encodeName(name_)]);
})();
}

export function encodeTargetWithStarAtom(t: PythonSyntax.TargetWithStarAtom): Ast.Expr {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "atom": return ((a: PythonSyntax.StarAtom) => encodeStarAtom(a))((_m as any).value);
    case "project": return ((pn: PythonSyntax.TPrimaryAndName) => encodeTPrimaryAndName(pn))((_m as any).value);
    case "slices": return ((_: PythonSyntax.TPrimaryAndSlices) => Serialization.cst("..."))((_m as any).value);
  }
})();
}

export function encodeTerm(t: PythonSyntax.Term): Ast.Expr {
  return encodeFactor(((_x) => _x.rhs)(t));
}

export function encodeTuple(t: PythonSyntax.Tuple): Ast.Expr {
  return (() => {
  const es = ((_x) => _x)(t);
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(es))(1))(Serialization.parens(Serialization.noSep([encodeStarNamedExpression(LibLists.head(es)), Serialization.cst(",")])))(Serialization.parenList(false)(LibLists.map(encodeStarNamedExpression)(es)));
})();
}

export function encodeTypeAlias(ta: PythonSyntax.TypeAlias): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(ta);
  const tparams = ((_x) => _x.typeParams)(ta);
  const expr = ((_x) => _x.expression)(ta);
  const alias = Serialization.noSep(LibMaybes.cat([encodeName(name), LibLogic.ifElse(LibLists.null_(tparams))(null)(Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(encodeTypeParameter)(tparams)))]));
  return Serialization.spaceSep([Serialization.cst("type"), alias, Serialization.cst("="), encodeExpression(expr)]);
})();
}

export function encodeTypeParameter(tp: PythonSyntax.TypeParameter): Ast.Expr {
  return (() => {
  const _m = tp;
  switch (_m.tag) {
    case "simple": return ((s: PythonSyntax.SimpleTypeParameter) => encodeSimpleTypeParameter(s))((_m as any).value);
    case "star": return ((_: PythonSyntax.StarTypeParameter) => Serialization.cst("*..."))((_m as any).value);
    case "doubleStar": return ((_: PythonSyntax.DoubleStarTypeParameter) => Serialization.cst("**..."))((_m as any).value);
  }
})();
}

export function encodeTypedAssignment(ta: PythonSyntax.TypedAssignment): Ast.Expr {
  return (() => {
  const lhs = ((_x) => _x.lhs)(ta);
  const typ = ((_x) => _x.type)(ta);
  const rhs = ((_x) => _x.rhs)(ta);
  return Serialization.spaceSep(LibMaybes.cat([Serialization.noSep([encodeSingleTarget(lhs), Serialization.cst(":")]), encodeExpression(typ), LibMaybes.map(encodeAnnotatedRhs)(rhs)]));
})();
}

export function encodeUntypedAssignment(ua: PythonSyntax.UntypedAssignment): Ast.Expr {
  return (() => {
  const targets = ((_x) => _x.targets)(ua);
  const rhs = ((_x) => _x.rhs)(ua);
  return Serialization.spaceSep(LibLists.concat([LibLists.map(encodeStarTarget)(targets), [encodeAnnotatedRhs(rhs)]]));
})();
}

export function encodeValuePattern(vp: PythonSyntax.ValuePattern): Ast.Expr {
  return encodeAttribute(((_x) => _x)(vp));
}

export function encodeWhileStatement(ws: PythonSyntax.WhileStatement): Ast.Expr {
  return (() => {
  const cond = ((_x) => _x.condition)(ws);
  const body = ((_x) => _x.body)(ws);
  const else_ = ((_x) => _x.else)(ws);
  return Serialization.newlineSep(LibMaybes.cat([Serialization.newlineSep([Serialization.spaceSep([Serialization.cst("while"), Serialization.noSep([encodeNamedExpression(cond), Serialization.cst(":")])]), encodeBlock(body)]), LibMaybes.map(((eb: PythonSyntax.Block) => Serialization.newlineSep([Serialization.cst("else:"), encodeBlock(eb)])))(else_)]));
})();
}

export function escapePythonString(doubleQuoted: boolean): ((x: string) => string) {
  return ((s: string) => (() => {
  const replace = ((old: string) => ((new_: string) => ((str: string) => LibStrings.intercalate(new_)(LibStrings.splitOn(old)(str)))));
  const s1 = replace("\\")("\\\\")(s);
  const s2 = replace(" ")("\\x00")(s1);
  const s3 = replace("\n")("\\n")(s2);
  const s4 = replace("\t")("\\t")(s3);
  const s5 = replace("\r")("\\r")(s4);
  const escaped = LibLogic.ifElse(doubleQuoted)(replace("\"")("\\\"")(s5))(replace("'")("\\'")(s5));
  const quote = LibLogic.ifElse(doubleQuoted)("\"")("'");
  return LibStrings.cat2(quote)(LibStrings.cat2(escaped)(quote));
})());
}

export function pythonFloatLiteralText(s: string): string {
  return LibLogic.ifElse(LibEquality.equal(s)("NaN"))("float('nan')")(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))("float('inf')")(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))("float('-inf')")(s)));
}

export function toPythonComments(doc_: string): string {
  return LibLogic.ifElse(LibEquality.equal(doc_)(""))("")(LibStrings.intercalate("\n")(LibLists.map(((line: string) => LibStrings.cat2("# ")(line)))(LibStrings.lines(doc_))));
}

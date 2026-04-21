// Note: this is an automatically generated file. Do not edit.

/**
 * Java serializer: converts Java AST to concrete syntax
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
import * as JavaSyntax from "./syntax.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMath from "../lib/math.js";
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

export function escapeJavaChar(c: number): string {
  return LibLogic.ifElse(LibEquality.equal(c)(34))("\\\"")(LibLogic.ifElse(LibEquality.equal(c)(92))("\\\\")(LibLogic.ifElse(LibEquality.equal(c)(10))("\\n")(LibLogic.ifElse(LibEquality.equal(c)(13))("\\r")(LibLogic.ifElse(LibEquality.equal(c)(9))("\\t")(LibLogic.ifElse(LibEquality.equal(c)(8))("\\b")(LibLogic.ifElse(LibEquality.equal(c)(12))("\\f")(LibLogic.ifElse(LibLogic.and(LibEquality.gte(c)(32))(LibEquality.lt(c)(127)))(LibStrings.fromList([c]))(javaUnicodeEscape(c)))))))));
}

export function escapeJavaString(s: string): string {
  return LibStrings.cat(LibLists.map(((c: number) => escapeJavaChar(c)))(LibStrings.toList(s)));
}

export function hexDigit(n: number): number {
  return LibLogic.ifElse(LibEquality.lt(n)(10))(LibMath.add(n)(48))(LibMath.add(LibMath.sub(n)(10))(65));
}

export function javaFloatLiteralText(s: string): string {
  return LibLogic.ifElse(LibEquality.equal(s)("NaN"))("Double.NaN")(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))("Double.POSITIVE_INFINITY")(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))("Double.NEGATIVE_INFINITY")(s)));
}

export function javaUnicodeEscape(n: number): string {
  return LibLogic.ifElse(LibEquality.gt(n)(65535))((() => {
  const n_ = LibMath.sub(n)(65536);
  return (() => {
  const hi = LibMath.add(55296)(LibMath.div(n_)(1024));
  return (() => {
  const lo = LibMath.add(56320)(LibMath.mod(n_)(1024));
  return LibStrings.cat2(LibStrings.cat2("\\u")(padHex4(hi)))(LibStrings.cat2("\\u")(padHex4(lo)));
})();
})();
})())(LibStrings.cat2("\\u")(padHex4(n)));
}

export function padHex4(n: number): string {
  return (() => {
  const d3 = LibMath.div(n)(4096);
  return (() => {
  const r3 = LibMath.mod(n)(4096);
  return (() => {
  const d2 = LibMath.div(r3)(256);
  return (() => {
  const r2 = LibMath.mod(r3)(256);
  return (() => {
  const d1 = LibMath.div(r2)(16);
  return (() => {
  const d0 = LibMath.mod(r2)(16);
  return LibStrings.fromList([hexDigit(d3), hexDigit(d2), hexDigit(d1), hexDigit(d0)]);
})();
})();
})();
})();
})();
})();
}

export function sanitizeJavaComment(s: string): string {
  return LibStrings.intercalate("&gt;")(LibStrings.splitOn(">")(LibStrings.intercalate("&lt;")(LibStrings.splitOn("<")(s))));
}

export function singleLineComment(c: string): Ast.Expr {
  return Serialization.cst(LibStrings.cat2("// ")(sanitizeJavaComment(c)));
}

export function withComments(mc: string | null): ((x: Ast.Expr) => Ast.Expr) {
  return ((expr: Ast.Expr) => LibMaybes.maybe(expr)(((c: string) => Serialization.newlineSep([Serialization.cst(LibStrings.cat2("/**\n")(LibStrings.cat2(LibStrings.intercalate("\n")(LibLists.map(((l: string) => LibStrings.cat2(" * ")(l)))(LibStrings.lines(sanitizeJavaComment(c)))))("\n */"))), expr])))(mc));
}

export function writeAdditionalBound(ab: JavaSyntax.AdditionalBound): Ast.Expr {
  return Serialization.spaceSep([Serialization.cst("&"), writeInterfaceType(((_x) => _x)(ab))]);
}

export function writeAdditiveExpression(e: JavaSyntax.AdditiveExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "unary": return ((m: JavaSyntax.MultiplicativeExpression) => writeMultiplicativeExpression(m))((_m as any).value);
    case "plus": return ((b: JavaSyntax.AdditiveExpression_Binary) => Serialization.infixWs("+")(writeAdditiveExpression(((_x) => _x.lhs)(b)))(writeMultiplicativeExpression(((_x) => _x.rhs)(b))))((_m as any).value);
    case "minus": return ((b: JavaSyntax.AdditiveExpression_Binary) => Serialization.infixWs("-")(writeAdditiveExpression(((_x) => _x.lhs)(b)))(writeMultiplicativeExpression(((_x) => _x.rhs)(b))))((_m as any).value);
  }
})();
}

export function writeAmbiguousName(an: JavaSyntax.AmbiguousName): Ast.Expr {
  return Serialization.dotSep(LibLists.map(writeIdentifier)(((_x) => _x)(an)));
}

export function writeAndExpression(ae: JavaSyntax.AndExpression): Ast.Expr {
  return Serialization.infixWsList("&")(LibLists.map(writeEqualityExpression)(((_x) => _x)(ae)));
}

export function writeAnnotatedIdentifier(ai: JavaSyntax.AnnotatedIdentifier): Ast.Expr {
  return writeIdentifier(((_x) => _x.identifier)(ai));
}

export function writeAnnotation(ann: JavaSyntax.Annotation): Ast.Expr {
  return (() => {
  const _m = ann;
  switch (_m.tag) {
    case "normal": return ((n: JavaSyntax.NormalAnnotation) => writeNormalAnnotation(n))((_m as any).value);
    case "marker": return ((m: JavaSyntax.MarkerAnnotation) => writeMarkerAnnotation(m))((_m as any).value);
    case "singleElement": return ((s: JavaSyntax.SingleElementAnnotation) => writeSingleElementAnnotation(s))((_m as any).value);
  }
})();
}

export function writeAnnotationTypeDeclaration<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:AnnotationTypeDeclaration");
}

export function writeArrayAccess<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ArrayAccess");
}

export function writeArrayCreationExpression(ace: JavaSyntax.ArrayCreationExpression): Ast.Expr {
  return (() => {
  const _m = ace;
  switch (_m.tag) {
    case "primitiveArray": return ((pa: JavaSyntax.ArrayCreationExpression_PrimitiveArray) => (() => {
  const pt = ((_x) => _x.type)(pa);
  const ai = ((_x) => _x.array)(pa);
  return Serialization.spaceSep([Serialization.cst("new"), Serialization.noSep([writePrimitiveTypeWithAnnotations(pt), Serialization.cst("[]")]), writeArrayInitializer(ai)]);
})())((_m as any).value);
    case "classOrInterfaceArray": return ((_: JavaSyntax.ArrayCreationExpression_ClassOrInterfaceArray) => Serialization.cst("STUB:ArrayCreationExpression"))((_m as any).value);
    case "primitive": return ((_: JavaSyntax.ArrayCreationExpression_Primitive) => Serialization.cst("STUB:ArrayCreationExpression"))((_m as any).value);
    case "classOrInterface": return ((_: JavaSyntax.ArrayCreationExpression_ClassOrInterface) => Serialization.cst("STUB:ArrayCreationExpression"))((_m as any).value);
  }
})();
}

export function writeArrayInitializer(ai: JavaSyntax.ArrayInitializer): Ast.Expr {
  return (() => {
  const groups = ((_x) => _x)(ai);
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(groups))(1))(Serialization.noSep([Serialization.cst("{"), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeVariableInitializer)(LibLists.head(groups))), Serialization.cst("}")]))(Serialization.cst("{}"));
})();
}

export function writeArrayType(at: JavaSyntax.ArrayType): Ast.Expr {
  return (() => {
  const dims = ((_x) => _x.dims)(at);
  const variant = ((_x) => _x.variant)(at);
  const varExpr = (() => {
  const _m = variant;
  switch (_m.tag) {
    case "primitive": return ((pt: JavaSyntax.PrimitiveTypeWithAnnotations) => writePrimitiveTypeWithAnnotations(pt))((_m as any).value);
    case "classOrInterface": return ((cit: JavaSyntax.ClassOrInterfaceType) => writeClassOrInterfaceType(cit))((_m as any).value);
    case "variable": return ((tv: JavaSyntax.TypeVariable) => writeTypeVariable(tv))((_m as any).value);
  }
})();
  return Serialization.noSep([varExpr, writeDims(dims)]);
})();
}

export function writeAssertStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:AssertStatement");
}

export function writeAssignment(a: JavaSyntax.Assignment): Ast.Expr {
  return (() => {
  const lhs = ((_x) => _x.lhs)(a);
  const op = ((_x) => _x.op)(a);
  const rhs = ((_x) => _x.expression)(a);
  const ctop = (() => {
  const _m = op;
  switch (_m.tag) {
    case "simple": return ((_: void) => "=")((_m as any).value);
    case "times": return ((_: void) => "*=")((_m as any).value);
    case "div": return ((_: void) => "/=")((_m as any).value);
    case "mod": return ((_: void) => "%=")((_m as any).value);
    case "plus": return ((_: void) => "+=")((_m as any).value);
    case "minus": return ((_: void) => "-=")((_m as any).value);
    case "shiftLeft": return ((_: void) => "<<=")((_m as any).value);
    case "shiftRight": return ((_: void) => ">>=")((_m as any).value);
    case "shiftRightZeroFill": return ((_: void) => ">>>=")((_m as any).value);
    case "and": return ((_: void) => "&=")((_m as any).value);
    case "xor": return ((_: void) => "^=")((_m as any).value);
    case "or": return ((_: void) => "|=")((_m as any).value);
  }
})();
  return Serialization.infixWs(ctop)(writeLeftHandSide(lhs))(writeExpression(rhs));
})();
}

export function writeAssignmentExpression(e: JavaSyntax.AssignmentExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "conditional": return ((c: JavaSyntax.ConditionalExpression) => writeConditionalExpression(c))((_m as any).value);
    case "assignment": return ((a: JavaSyntax.Assignment) => writeAssignment(a))((_m as any).value);
  }
})();
}

export function writeBlock(b: JavaSyntax.Block): Ast.Expr {
  return Serialization.curlyBlock(Serialization.fullBlockStyle)(Serialization.newlineSep(LibLists.map(writeBlockStatement)(((_x) => _x)(b))));
}

export function writeBlockStatement(s: JavaSyntax.BlockStatement): Ast.Expr {
  return (() => {
  const _m = s;
  switch (_m.tag) {
    case "localVariableDeclaration": return ((d: JavaSyntax.LocalVariableDeclarationStatement) => writeLocalVariableDeclarationStatement(d))((_m as any).value);
    case "class": return ((cd: JavaSyntax.ClassDeclaration) => writeClassDeclaration(cd))((_m as any).value);
    case "statement": return ((s2: JavaSyntax.Statement) => writeStatement(s2))((_m as any).value);
  }
})();
}

export function writeBreakStatement(bs: JavaSyntax.BreakStatement): Ast.Expr {
  return (() => {
  const mlabel = ((_x) => _x)(bs);
  return Serialization.withSemi(Serialization.spaceSep(LibMaybes.cat([Serialization.cst("break"), LibMaybes.map(writeIdentifier)(mlabel)])));
})();
}

export function writeCastExpression(e: JavaSyntax.CastExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "primitive": return ((p: JavaSyntax.CastExpression_Primitive) => writeCastExpression_Primitive(p))((_m as any).value);
    case "notPlusMinus": return ((npm: JavaSyntax.CastExpression_NotPlusMinus) => writeCastExpression_NotPlusMinus(npm))((_m as any).value);
    case "lambda": return ((l: JavaSyntax.CastExpression_Lambda) => writeCastExpression_Lambda(l))((_m as any).value);
  }
})();
}

export function writeCastExpression_Lambda<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:CastExpression_Lambda");
}

export function writeCastExpression_NotPlusMinus(npm: JavaSyntax.CastExpression_NotPlusMinus): Ast.Expr {
  return (() => {
  const rb = ((_x) => _x.refAndBounds)(npm);
  const ex = ((_x) => _x.expression)(npm);
  return Serialization.spaceSep([writeCastExpression_RefAndBounds(rb), writeUnaryExpression(ex)]);
})();
}

export function writeCastExpression_Primitive(cp: JavaSyntax.CastExpression_Primitive): Ast.Expr {
  return (() => {
  const pt = ((_x) => _x.type)(cp);
  const ex = ((_x) => _x.expression)(cp);
  return Serialization.spaceSep([Serialization.parenList(false)([writePrimitiveTypeWithAnnotations(pt)]), writeUnaryExpression(ex)]);
})();
}

export function writeCastExpression_RefAndBounds(rab: JavaSyntax.CastExpression_RefAndBounds): Ast.Expr {
  return (() => {
  const rt = ((_x) => _x.type)(rab);
  const adds = ((_x) => _x.bounds)(rab);
  return Serialization.parenList(false)([Serialization.spaceSep(LibMaybes.cat([writeReferenceType(rt), LibLogic.ifElse(LibLists.null_(adds))(null)(Serialization.spaceSep(LibLists.map(writeAdditionalBound)(adds)))]))]);
})();
}

export function writeClassBody(cb: JavaSyntax.ClassBody): Ast.Expr {
  return Serialization.curlyBlock(Serialization.fullBlockStyle)(Serialization.doubleNewlineSep(LibLists.map(writeClassBodyDeclarationWithComments)(((_x) => _x)(cb))));
}

export function writeClassBodyDeclaration(d: JavaSyntax.ClassBodyDeclaration): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "classMember": return ((d2: JavaSyntax.ClassMemberDeclaration) => writeClassMemberDeclaration(d2))((_m as any).value);
    case "instanceInitializer": return ((i: JavaSyntax.InstanceInitializer) => writeInstanceInitializer(i))((_m as any).value);
    case "staticInitializer": return ((i: JavaSyntax.StaticInitializer) => writeStaticInitializer(i))((_m as any).value);
    case "constructorDeclaration": return ((d2: JavaSyntax.ConstructorDeclaration) => writeConstructorDeclaration(d2))((_m as any).value);
  }
})();
}

export function writeClassBodyDeclarationWithComments(cbdwc: JavaSyntax.ClassBodyDeclarationWithComments): Ast.Expr {
  return (() => {
  const d = ((_x) => _x.value)(cbdwc);
  const mc = ((_x) => _x.comments)(cbdwc);
  return withComments(mc)(writeClassBodyDeclaration(d));
})();
}

export function writeClassDeclaration(d: JavaSyntax.ClassDeclaration): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "normal": return ((nd: JavaSyntax.NormalClassDeclaration) => writeNormalClassDeclaration(nd))((_m as any).value);
    case "enum": return ((ed: JavaSyntax.EnumDeclaration) => writeEnumDeclaration(ed))((_m as any).value);
  }
})();
}

export function writeClassInstanceCreationExpression(cice: JavaSyntax.ClassInstanceCreationExpression): Ast.Expr {
  return (() => {
  const mqual = ((_x) => _x.qualifier)(cice);
  const e = ((_x) => _x.expression)(cice);
  return LibMaybes.maybe(writeUnqualifiedClassInstanceCreationExpression(e))(((q: JavaSyntax.ClassInstanceCreationExpression_Qualifier) => Serialization.dotSep([writeClassInstanceCreationExpression_Qualifier(q), writeUnqualifiedClassInstanceCreationExpression(e)])))(mqual);
})();
}

export function writeClassInstanceCreationExpression_Qualifier(q: JavaSyntax.ClassInstanceCreationExpression_Qualifier): Ast.Expr {
  return (() => {
  const _m = q;
  switch (_m.tag) {
    case "expression": return ((en: JavaSyntax.ExpressionName) => writeExpressionName(en))((_m as any).value);
    case "primary": return ((p: JavaSyntax.Primary) => writePrimary(p))((_m as any).value);
  }
})();
}

export function writeClassLiteral<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ClassLiteral");
}

export function writeClassMemberDeclaration(d: JavaSyntax.ClassMemberDeclaration): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "field": return ((fd: JavaSyntax.FieldDeclaration) => writeFieldDeclaration(fd))((_m as any).value);
    case "method": return ((md: JavaSyntax.MethodDeclaration) => writeMethodDeclaration(md))((_m as any).value);
    case "class": return ((cd: JavaSyntax.ClassDeclaration) => writeClassDeclaration(cd))((_m as any).value);
    case "interface": return ((id: JavaSyntax.InterfaceDeclaration) => writeInterfaceDeclaration(id))((_m as any).value);
    case "none": return ((_: void) => Serialization.cst(";"))((_m as any).value);
  }
})();
}

export function writeClassModifier(m: JavaSyntax.ClassModifier): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "annotation": return ((ann: JavaSyntax.Annotation) => writeAnnotation(ann))((_m as any).value);
    case "public": return ((_: void) => Serialization.cst("public"))((_m as any).value);
    case "protected": return ((_: void) => Serialization.cst("protected"))((_m as any).value);
    case "private": return ((_: void) => Serialization.cst("private"))((_m as any).value);
    case "abstract": return ((_: void) => Serialization.cst("abstract"))((_m as any).value);
    case "static": return ((_: void) => Serialization.cst("static"))((_m as any).value);
    case "final": return ((_: void) => Serialization.cst("final"))((_m as any).value);
    case "strictfp": return ((_: void) => Serialization.cst("strictfp"))((_m as any).value);
  }
})();
}

export function writeClassOrInterfaceType(cit: JavaSyntax.ClassOrInterfaceType): Ast.Expr {
  return (() => {
  const _m = cit;
  switch (_m.tag) {
    case "class": return ((ct: JavaSyntax.ClassType) => writeClassType(ct))((_m as any).value);
    case "interface": return ((it: JavaSyntax.InterfaceType) => writeInterfaceType(it))((_m as any).value);
  }
})();
}

export function writeClassOrInterfaceTypeToInstantiate(coitti: JavaSyntax.ClassOrInterfaceTypeToInstantiate): Ast.Expr {
  return (() => {
  const ids = ((_x) => _x.identifiers)(coitti);
  const margs = ((_x) => _x.typeArguments)(coitti);
  return Serialization.noSep(LibMaybes.cat([Serialization.dotSep(LibLists.map(writeAnnotatedIdentifier)(ids)), LibMaybes.map(writeTypeArgumentsOrDiamond)(margs)]));
})();
}

export function writeClassType(ct: JavaSyntax.ClassType): Ast.Expr {
  return (() => {
  const anns = ((_x) => _x.annotations)(ct);
  const qual = ((_x) => _x.qualifier)(ct);
  const id = ((_x) => _x.identifier)(ct);
  const args = ((_x) => _x.arguments)(ct);
  const qualifiedId = (() => {
  const _m = qual;
  switch (_m.tag) {
    case "none": return ((_: void) => writeTypeIdentifier(id))((_m as any).value);
    case "package": return ((pkg: JavaSyntax.PackageName) => Serialization.dotSep([writePackageName(pkg), writeTypeIdentifier(id)]))((_m as any).value);
    case "parent": return ((cit: JavaSyntax.ClassOrInterfaceType) => Serialization.dotSep([writeClassOrInterfaceType(cit), writeTypeIdentifier(id)]))((_m as any).value);
  }
})();
  return Serialization.noSep(LibMaybes.cat([Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(anns))(null)(Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeAnnotation)(anns))), qualifiedId])), LibLogic.ifElse(LibLists.null_(args))(null)(Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeArgument)(args)))]));
})();
}

export function writeCompilationUnit(u: JavaSyntax.CompilationUnit): Ast.Expr {
  return (() => {
  const _m = u;
  switch (_m.tag) {
    case "ordinary": return ((ocu: JavaSyntax.OrdinaryCompilationUnit) => (() => {
  const mpkg = ((_x) => _x.package)(ocu);
  const imports = ((_x) => _x.imports)(ocu);
  const types = ((_x) => _x.types)(ocu);
  const warning = singleLineComment(Constants.warningAutoGeneratedFile);
  const pkgSec = LibMaybes.map(writePackageDeclaration)(mpkg);
  const importsSec = LibLogic.ifElse(LibLists.null_(imports))(null)(Serialization.newlineSep(LibLists.map(writeImportDeclaration)(imports)));
  const typesSec = LibLogic.ifElse(LibLists.null_(types))(null)(Serialization.doubleNewlineSep(LibLists.map(writeTypeDeclarationWithComments)(types)));
  return Serialization.doubleNewlineSep(LibMaybes.cat([warning, pkgSec, importsSec, typesSec]));
})())((_m as any).value);
  }
})();
}

export function writeConditionalAndExpression(cae: JavaSyntax.ConditionalAndExpression): Ast.Expr {
  return Serialization.infixWsList("&&")(LibLists.map(writeInclusiveOrExpression)(((_x) => _x)(cae)));
}

export function writeConditionalExpression(c: JavaSyntax.ConditionalExpression): Ast.Expr {
  return (() => {
  const _m = c;
  switch (_m.tag) {
    case "simple": return ((co: JavaSyntax.ConditionalOrExpression) => writeConditionalOrExpression(co))((_m as any).value);
    case "ternaryCond": return ((tc: JavaSyntax.ConditionalExpression_TernaryCond) => writeConditionalExpression_TernaryCond(tc))((_m as any).value);
    case "ternaryLambda": return ((tl: JavaSyntax.ConditionalExpression_TernaryLambda) => writeConditionalExpression_TernaryLambda(tl))((_m as any).value);
  }
})();
}

export function writeConditionalExpression_TernaryCond<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ConditionalExpression_TernaryCond");
}

export function writeConditionalExpression_TernaryLambda<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ConditionalExpression_TernaryLambda");
}

export function writeConditionalOrExpression(coe: JavaSyntax.ConditionalOrExpression): Ast.Expr {
  return Serialization.infixWsList("||")(LibLists.map(writeConditionalAndExpression)(((_x) => _x)(coe)));
}

export function writeConstantDeclaration(cd: JavaSyntax.ConstantDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(cd);
  const typ = ((_x) => _x.type)(cd);
  const vars = ((_x) => _x.variables)(cd);
  return Serialization.withSemi(Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeConstantModifier)(mods))), writeUnannType(typ), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeVariableDeclarator)(vars))])));
})();
}

export function writeConstantModifier<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ConstantModifier");
}

export function writeConstructorBody(cb: JavaSyntax.ConstructorBody): Ast.Expr {
  return (() => {
  const minvoc = ((_x) => _x.invocation)(cb);
  const stmts = ((_x) => _x.statements)(cb);
  return Serialization.curlyBlock(Serialization.fullBlockStyle)(Serialization.doubleNewlineSep(LibMaybes.cat([LibMaybes.map(writeExplicitConstructorInvocation)(minvoc), Serialization.newlineSep(LibLists.map(writeBlockStatement)(stmts))])));
})();
}

export function writeConstructorDeclaration(cd: JavaSyntax.ConstructorDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(cd);
  const cons = ((_x) => _x.constructor)(cd);
  const mthrows = ((_x) => _x.throws)(cd);
  const body = ((_x) => _x.body)(cd);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeConstructorModifier)(mods))), writeConstructorDeclarator(cons), LibMaybes.map(writeThrows)(mthrows), writeConstructorBody(body)]));
})();
}

export function writeConstructorDeclarator(cd: JavaSyntax.ConstructorDeclarator): Ast.Expr {
  return (() => {
  const tparams = ((_x) => _x.parameters)(cd);
  const name = ((_x) => _x.name)(cd);
  const fparams = ((_x) => _x.formalParameters)(cd);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(tparams))(null)(Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeParameter)(tparams))), writeSimpleTypeName(name), Serialization.parenList(false)(LibLists.map(writeFormalParameter)(fparams))]));
})();
}

export function writeConstructorModifier(m: JavaSyntax.ConstructorModifier): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "annotation": return ((ann: JavaSyntax.Annotation) => writeAnnotation(ann))((_m as any).value);
    case "public": return ((_: void) => Serialization.cst("public"))((_m as any).value);
    case "protected": return ((_: void) => Serialization.cst("protected"))((_m as any).value);
    case "private": return ((_: void) => Serialization.cst("private"))((_m as any).value);
  }
})();
}

export function writeContinueStatement(cs: JavaSyntax.ContinueStatement): Ast.Expr {
  return (() => {
  const mlabel = ((_x) => _x)(cs);
  return Serialization.withSemi(Serialization.spaceSep(LibMaybes.cat([Serialization.cst("continue"), LibMaybes.map(writeIdentifier)(mlabel)])));
})();
}

export function writeDims(d: JavaSyntax.Dims): Ast.Expr {
  return Serialization.noSep(LibLists.map(((_: ReadonlyArray<JavaSyntax.Annotation>) => Serialization.cst("[]")))(((_x) => _x)(d)));
}

export function writeDoStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:DoStatement");
}

export function writeElementValue(ev: JavaSyntax.ElementValue): Ast.Expr {
  return (() => {
  const _m = ev;
  switch (_m.tag) {
    case "conditionalExpression": return ((c: JavaSyntax.ConditionalExpression) => writeConditionalExpression(c))((_m as any).value);
    case "elementValueArrayInitializer": return ((evai: JavaSyntax.ElementValueArrayInitializer) => Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeElementValue)(((_x) => _x)(evai))))((_m as any).value);
    case "annotation": return ((ann: JavaSyntax.Annotation) => writeAnnotation(ann))((_m as any).value);
  }
})();
}

export function writeElementValuePair(evp: JavaSyntax.ElementValuePair): Ast.Expr {
  return (() => {
  const k = ((_x) => _x.key)(evp);
  const v = ((_x) => _x.value)(evp);
  return Serialization.infixWs("=")(writeIdentifier(k))(writeElementValue(v));
})();
}

export function writeEnumDeclaration<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:EnumDeclaration");
}

export function writeEqualityExpression(e: JavaSyntax.EqualityExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "unary": return ((r: JavaSyntax.RelationalExpression) => writeRelationalExpression(r))((_m as any).value);
    case "equal": return ((b: JavaSyntax.EqualityExpression_Binary) => Serialization.infixWs("==")(writeEqualityExpression(((_x) => _x.lhs)(b)))(writeRelationalExpression(((_x) => _x.rhs)(b))))((_m as any).value);
    case "notEqual": return ((b: JavaSyntax.EqualityExpression_Binary) => Serialization.infixWs("!=")(writeEqualityExpression(((_x) => _x.lhs)(b)))(writeRelationalExpression(((_x) => _x.rhs)(b))))((_m as any).value);
  }
})();
}

export function writeExclusiveOrExpression(eoe: JavaSyntax.ExclusiveOrExpression): Ast.Expr {
  return Serialization.infixWsList("^")(LibLists.map(writeAndExpression)(((_x) => _x)(eoe)));
}

export function writeExplicitConstructorInvocation<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ExplicitConstructorInvocation");
}

export function writeExpression(e: JavaSyntax.Expression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "lambda": return ((l: JavaSyntax.LambdaExpression) => writeLambdaExpression(l))((_m as any).value);
    case "assignment": return ((a: JavaSyntax.AssignmentExpression) => writeAssignmentExpression(a))((_m as any).value);
  }
})();
}

export function writeExpressionName(en: JavaSyntax.ExpressionName): Ast.Expr {
  return (() => {
  const mqual = ((_x) => _x.qualifier)(en);
  const id = ((_x) => _x.identifier)(en);
  return Serialization.dotSep(LibMaybes.cat([LibMaybes.map(writeAmbiguousName)(mqual), writeIdentifier(id)]));
})();
}

export function writeExpressionStatement(es: JavaSyntax.ExpressionStatement): Ast.Expr {
  return Serialization.withSemi(writeStatementExpression(((_x) => _x)(es)));
}

export function writeFieldAccess(fa: JavaSyntax.FieldAccess): Ast.Expr {
  return (() => {
  const qual = ((_x) => _x.qualifier)(fa);
  const id = ((_x) => _x.identifier)(fa);
  return (() => {
  const _m = qual;
  switch (_m.tag) {
    case "primary": return ((p: JavaSyntax.Primary) => Serialization.dotSep([writePrimary(p), writeIdentifier(id)]))((_m as any).value);
    case "super": return ((_: void) => Serialization.dotSep([Serialization.cst("super"), writeIdentifier(id)]))((_m as any).value);
    case "typed": return ((tn: JavaSyntax.TypeName) => Serialization.dotSep([writeTypeName(tn), Serialization.cst("super"), writeIdentifier(id)]))((_m as any).value);
  }
})();
})();
}

export function writeFieldDeclaration(fd: JavaSyntax.FieldDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(fd);
  const typ = ((_x) => _x.unannType)(fd);
  const vars = ((_x) => _x.variableDeclarators)(fd);
  return Serialization.withSemi(Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeFieldModifier)(mods))), writeUnannType(typ), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeVariableDeclarator)(vars))])));
})();
}

export function writeFieldModifier(m: JavaSyntax.FieldModifier): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "annotation": return ((ann: JavaSyntax.Annotation) => writeAnnotation(ann))((_m as any).value);
    case "public": return ((_: void) => Serialization.cst("public"))((_m as any).value);
    case "protected": return ((_: void) => Serialization.cst("protected"))((_m as any).value);
    case "private": return ((_: void) => Serialization.cst("private"))((_m as any).value);
    case "static": return ((_: void) => Serialization.cst("static"))((_m as any).value);
    case "final": return ((_: void) => Serialization.cst("final"))((_m as any).value);
    case "transient": return ((_: void) => Serialization.cst("transient"))((_m as any).value);
    case "volatile": return ((_: void) => Serialization.cst("volatile"))((_m as any).value);
  }
})();
}

export function writeFloatingPointLiteral(fl: JavaSyntax.FloatingPointLiteral): Ast.Expr {
  return Serialization.cst(javaFloatLiteralText(LibLiterals.showBigfloat(((_x) => _x)(fl))));
}

export function writeFloatingPointType(ft: JavaSyntax.FloatingPointType): Ast.Expr {
  return (() => {
  const _m = ft;
  switch (_m.tag) {
    case "float": return ((_: void) => Serialization.cst("float"))((_m as any).value);
    case "double": return ((_: void) => Serialization.cst("double"))((_m as any).value);
  }
})();
}

export function writeForStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ForStatement");
}

export function writeFormalParameter(p: JavaSyntax.FormalParameter): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "simple": return ((s: JavaSyntax.FormalParameter_Simple) => writeFormalParameter_Simple(s))((_m as any).value);
    case "variableArity": return ((v: JavaSyntax.VariableArityParameter) => writeVariableArityParameter(v))((_m as any).value);
  }
})();
}

export function writeFormalParameter_Simple(fps: JavaSyntax.FormalParameter_Simple): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(fps);
  const typ = ((_x) => _x.type)(fps);
  const id = ((_x) => _x.id)(fps);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeVariableModifier)(mods))), writeUnannType(typ), writeVariableDeclaratorId(id)]));
})();
}

export function writeIdentifier(id: JavaSyntax.Identifier): Ast.Expr {
  return Serialization.cst(((_x) => _x)(id));
}

export function writeIfThenElseStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:IfThenElseStatement");
}

export function writeIfThenStatement(its: JavaSyntax.IfThenStatement): Ast.Expr {
  return (() => {
  const cond = ((_x) => _x.expression)(its);
  const thn = ((_x) => _x.statement)(its);
  return Serialization.spaceSep([Serialization.cst("if"), Serialization.parenList(false)([writeExpression(cond)]), Serialization.curlyBlock(Serialization.fullBlockStyle)(writeStatement(thn))]);
})();
}

export function writeImportDeclaration(imp: JavaSyntax.ImportDeclaration): Ast.Expr {
  return (() => {
  const _m = imp;
  switch (_m.tag) {
    case "singleType": return ((st: JavaSyntax.SingleTypeImportDeclaration) => Serialization.withSemi(Serialization.spaceSep([Serialization.cst("import"), writeTypeName(((_x) => _x)(st))])))((_m as any).value);
    case "typeImportOnDemand": return ((_: JavaSyntax.TypeImportOnDemandDeclaration) => Serialization.cst("STUB:ImportDeclarationTypeImportOnDemand"))((_m as any).value);
    case "singleStaticImport": return ((_: JavaSyntax.SingleStaticImportDeclaration) => Serialization.cst("STUB:ImportDeclarationSingleStaticImport"))((_m as any).value);
    case "staticImportOnDemand": return ((_: JavaSyntax.StaticImportOnDemandDeclaration) => Serialization.cst("STUB:ImportDeclarationStaticImportOnDemand"))((_m as any).value);
  }
})();
}

export function writeInclusiveOrExpression(ioe: JavaSyntax.InclusiveOrExpression): Ast.Expr {
  return Serialization.infixWsList("|")(LibLists.map(writeExclusiveOrExpression)(((_x) => _x)(ioe)));
}

export function writeInstanceInitializer<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:InstanceInitializer");
}

export function writeIntegerLiteral(il: JavaSyntax.IntegerLiteral): Ast.Expr {
  return (() => {
  const i = ((_x) => _x)(il);
  const suffix = LibLogic.ifElse(LibLogic.or(LibEquality.gt(i)(2147483647n))(LibEquality.lt(i)(-2147483648n)))("L")("");
  return Serialization.cst(LibStrings.cat2(LibLiterals.showBigint(i))(suffix));
})();
}

export function writeIntegralType(t: JavaSyntax.IntegralType): Ast.Expr {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "byte": return ((_: void) => Serialization.cst("byte"))((_m as any).value);
    case "short": return ((_: void) => Serialization.cst("short"))((_m as any).value);
    case "int": return ((_: void) => Serialization.cst("int"))((_m as any).value);
    case "long": return ((_: void) => Serialization.cst("long"))((_m as any).value);
    case "char": return ((_: void) => Serialization.cst("char"))((_m as any).value);
  }
})();
}

export function writeInterfaceBody(ib: JavaSyntax.InterfaceBody): Ast.Expr {
  return Serialization.curlyBlock(Serialization.fullBlockStyle)(Serialization.doubleNewlineSep(LibLists.map(writeInterfaceMemberDeclaration)(((_x) => _x)(ib))));
}

export function writeInterfaceDeclaration(d: JavaSyntax.InterfaceDeclaration): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "normalInterface": return ((n: JavaSyntax.NormalInterfaceDeclaration) => writeNormalInterfaceDeclaration(n))((_m as any).value);
    case "annotationType": return ((a: JavaSyntax.AnnotationTypeDeclaration) => writeAnnotationTypeDeclaration(a))((_m as any).value);
  }
})();
}

export function writeInterfaceMemberDeclaration(d: JavaSyntax.InterfaceMemberDeclaration): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "constant": return ((c: JavaSyntax.ConstantDeclaration) => writeConstantDeclaration(c))((_m as any).value);
    case "interfaceMethod": return ((im: JavaSyntax.InterfaceMethodDeclaration) => writeInterfaceMethodDeclaration(im))((_m as any).value);
    case "class": return ((cd: JavaSyntax.ClassDeclaration) => writeClassDeclaration(cd))((_m as any).value);
    case "interface": return ((id: JavaSyntax.InterfaceDeclaration) => writeInterfaceDeclaration(id))((_m as any).value);
  }
})();
}

export function writeInterfaceMethodDeclaration(imd: JavaSyntax.InterfaceMethodDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(imd);
  const header = ((_x) => _x.header)(imd);
  const body = ((_x) => _x.body)(imd);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeInterfaceMethodModifier)(mods))), writeMethodHeader(header), writeMethodBody(body)]));
})();
}

export function writeInterfaceMethodModifier(m: JavaSyntax.InterfaceMethodModifier): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "annotation": return ((a: JavaSyntax.Annotation) => writeAnnotation(a))((_m as any).value);
    case "public": return ((_: void) => Serialization.cst("public"))((_m as any).value);
    case "private": return ((_: void) => Serialization.cst("private"))((_m as any).value);
    case "abstract": return ((_: void) => Serialization.cst("abstract"))((_m as any).value);
    case "default": return ((_: void) => Serialization.cst("default"))((_m as any).value);
    case "static": return ((_: void) => Serialization.cst("static"))((_m as any).value);
    case "strictfp": return ((_: void) => Serialization.cst("strictfp"))((_m as any).value);
  }
})();
}

export function writeInterfaceModifier(m: JavaSyntax.InterfaceModifier): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "annotation": return ((a: JavaSyntax.Annotation) => writeAnnotation(a))((_m as any).value);
    case "public": return ((_: void) => Serialization.cst("public"))((_m as any).value);
    case "protected": return ((_: void) => Serialization.cst("protected"))((_m as any).value);
    case "private": return ((_: void) => Serialization.cst("private"))((_m as any).value);
    case "abstract": return ((_: void) => Serialization.cst("abstract"))((_m as any).value);
    case "static": return ((_: void) => Serialization.cst("static"))((_m as any).value);
    case "strictfb": return ((_: void) => Serialization.cst("strictfb"))((_m as any).value);
  }
})();
}

export function writeInterfaceType(it: JavaSyntax.InterfaceType): Ast.Expr {
  return writeClassType(((_x) => _x)(it));
}

export function writeLabeledStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:LabeledStatement");
}

export function writeLambdaBody(b: JavaSyntax.LambdaBody): Ast.Expr {
  return (() => {
  const _m = b;
  switch (_m.tag) {
    case "expression": return ((e: JavaSyntax.Expression) => writeExpression(e))((_m as any).value);
    case "block": return ((b2: JavaSyntax.Block) => writeBlock(b2))((_m as any).value);
  }
})();
}

export function writeLambdaExpression(le: JavaSyntax.LambdaExpression): Ast.Expr {
  return (() => {
  const params = ((_x) => _x.parameters)(le);
  const body = ((_x) => _x.body)(le);
  return Serialization.infixWs("->")(writeLambdaParameters(params))(writeLambdaBody(body));
})();
}

export function writeLambdaParameters(p: JavaSyntax.LambdaParameters): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "tuple": return ((l: ReadonlyArray<JavaSyntax.LambdaParameters>) => Serialization.parenList(false)(LibLists.map(writeLambdaParameters)(l)))((_m as any).value);
    case "single": return ((id: JavaSyntax.Identifier) => writeIdentifier(id))((_m as any).value);
  }
})();
}

export function writeLeftHandSide(lhs: JavaSyntax.LeftHandSide): Ast.Expr {
  return (() => {
  const _m = lhs;
  switch (_m.tag) {
    case "expressionName": return ((en: JavaSyntax.ExpressionName) => writeExpressionName(en))((_m as any).value);
    case "fieldAccess": return ((fa: JavaSyntax.FieldAccess) => writeFieldAccess(fa))((_m as any).value);
    case "arrayAccess": return ((aa: JavaSyntax.ArrayAccess) => writeArrayAccess(aa))((_m as any).value);
  }
})();
}

export function writeLiteral(l: JavaSyntax.Literal): Ast.Expr {
  return (() => {
  const _m = l;
  switch (_m.tag) {
    case "null": return ((_: void) => Serialization.cst("null"))((_m as any).value);
    case "integer": return ((il: JavaSyntax.IntegerLiteral) => writeIntegerLiteral(il))((_m as any).value);
    case "floatingPoint": return ((fl: JavaSyntax.FloatingPointLiteral) => writeFloatingPointLiteral(fl))((_m as any).value);
    case "boolean": return ((b: boolean) => Serialization.cst(LibLogic.ifElse(b)("true")("false")))((_m as any).value);
    case "character": return ((c: number) => (() => {
  const ci = LibLiterals.bigintToInt32(LibLiterals.uint16ToBigint(c));
  return Serialization.cst(LibStrings.cat2("'")(LibStrings.cat2(LibLogic.ifElse(LibEquality.equal(ci)(39))("\\'")(LibLogic.ifElse(LibEquality.equal(ci)(92))("\\\\")(LibLogic.ifElse(LibEquality.equal(ci)(10))("\\n")(LibLogic.ifElse(LibEquality.equal(ci)(13))("\\r")(LibLogic.ifElse(LibEquality.equal(ci)(9))("\\t")(LibLogic.ifElse(LibLogic.and(LibEquality.gte(ci)(32))(LibEquality.lt(ci)(127)))(LibStrings.fromList([ci]))(javaUnicodeEscape(ci))))))))("'")));
})())((_m as any).value);
    case "string": return ((sl: JavaSyntax.StringLiteral) => writeStringLiteral(sl))((_m as any).value);
  }
})();
}

export function writeLocalName(t: JavaSyntax.LocalVariableType): Ast.Expr {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "type": return ((ut: JavaSyntax.UnannType) => writeUnannType(ut))((_m as any).value);
    case "var": return ((_: void) => Serialization.cst("var"))((_m as any).value);
  }
})();
}

export function writeLocalVariableDeclaration(lvd: JavaSyntax.LocalVariableDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(lvd);
  const t = ((_x) => _x.type)(lvd);
  const decls = ((_x) => _x.declarators)(lvd);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeVariableModifier)(mods))), writeLocalName(t), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeVariableDeclarator)(decls))]));
})();
}

export function writeLocalVariableDeclarationStatement(lvds: JavaSyntax.LocalVariableDeclarationStatement): Ast.Expr {
  return Serialization.withSemi(writeLocalVariableDeclaration(((_x) => _x)(lvds)));
}

export function writeMarkerAnnotation(ma: JavaSyntax.MarkerAnnotation): Ast.Expr {
  return Serialization.prefix("@")(writeTypeName(((_x) => _x)(ma)));
}

export function writeMethodBody(b: JavaSyntax.MethodBody): Ast.Expr {
  return (() => {
  const _m = b;
  switch (_m.tag) {
    case "block": return ((block: JavaSyntax.Block) => writeBlock(block))((_m as any).value);
    case "none": return ((_: void) => Serialization.cst(";"))((_m as any).value);
  }
})();
}

export function writeMethodDeclaration(md: JavaSyntax.MethodDeclaration): Ast.Expr {
  return (() => {
  const anns = ((_x) => _x.annotations)(md);
  const mods = ((_x) => _x.modifiers)(md);
  const header = ((_x) => _x.header)(md);
  const body = ((_x) => _x.body)(md);
  const headerAndBody = Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeMethodModifier)(mods))), writeMethodHeader(header), writeMethodBody(body)]));
  return Serialization.newlineSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(anns))(null)(Serialization.newlineSep(LibLists.map(writeAnnotation)(anns))), headerAndBody]));
})();
}

export function writeMethodDeclarator(md: JavaSyntax.MethodDeclarator): Ast.Expr {
  return (() => {
  const id = ((_x) => _x.identifier)(md);
  const params = ((_x) => _x.formalParameters)(md);
  return Serialization.noSep([writeIdentifier(id), Serialization.parenList(false)(LibLists.map(writeFormalParameter)(params))]);
})();
}

export function writeMethodHeader(mh: JavaSyntax.MethodHeader): Ast.Expr {
  return (() => {
  const params = ((_x) => _x.parameters)(mh);
  const result = ((_x) => _x.result)(mh);
  const decl = ((_x) => _x.declarator)(mh);
  const mthrows = ((_x) => _x.throws)(mh);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(params))(null)(Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeParameter)(params))), writeResult(result), writeMethodDeclarator(decl), LibMaybes.map(writeThrows)(mthrows)]));
})();
}

export function writeMethodInvocation(mi: JavaSyntax.MethodInvocation): Ast.Expr {
  return (() => {
  const header = ((_x) => _x.header)(mi);
  const args = ((_x) => _x.arguments)(mi);
  const argSec = Serialization.parenList(true)(LibLists.map(writeExpression)(args));
  const headerSec = (() => {
  const _m = header;
  switch (_m.tag) {
    case "simple": return ((mname: JavaSyntax.MethodName) => writeMethodName(mname))((_m as any).value);
    case "complex": return ((cx: JavaSyntax.MethodInvocation_Complex) => (() => {
  const cvar = ((_x) => _x.variant)(cx);
  const targs = ((_x) => _x.typeArguments)(cx);
  const cid = ((_x) => _x.identifier)(cx);
  const idSec = Serialization.noSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(targs))(null)(Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeArgument)(targs))), writeIdentifier(cid)]));
  return (() => {
  const _m = cvar;
  switch (_m.tag) {
    case "type": return ((tname: JavaSyntax.TypeName) => Serialization.dotSep([writeTypeName(tname), idSec]))((_m as any).value);
    case "expression": return ((en: JavaSyntax.ExpressionName) => Serialization.dotSep([writeExpressionName(en), idSec]))((_m as any).value);
    case "primary": return ((p: JavaSyntax.Primary) => Serialization.dotSep([writePrimary(p), idSec]))((_m as any).value);
    case "super": return ((_: void) => Serialization.dotSep([Serialization.cst("super"), idSec]))((_m as any).value);
    case "typeSuper": return ((tname: JavaSyntax.TypeName) => Serialization.dotSep([writeTypeName(tname), Serialization.cst("super"), idSec]))((_m as any).value);
  }
})();
})())((_m as any).value);
  }
})();
  return Serialization.noSep([headerSec, argSec]);
})();
}

export function writeMethodModifier(m: JavaSyntax.MethodModifier): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "annotation": return ((ann: JavaSyntax.Annotation) => writeAnnotation(ann))((_m as any).value);
    case "public": return ((_: void) => Serialization.cst("public"))((_m as any).value);
    case "protected": return ((_: void) => Serialization.cst("protected"))((_m as any).value);
    case "private": return ((_: void) => Serialization.cst("private"))((_m as any).value);
    case "abstract": return ((_: void) => Serialization.cst("abstract"))((_m as any).value);
    case "final": return ((_: void) => Serialization.cst("final"))((_m as any).value);
    case "synchronized": return ((_: void) => Serialization.cst("synchronized"))((_m as any).value);
    case "native": return ((_: void) => Serialization.cst("native"))((_m as any).value);
    case "strictfb": return ((_: void) => Serialization.cst("strictfb"))((_m as any).value);
  }
})();
}

export function writeMethodName(mn: JavaSyntax.MethodName): Ast.Expr {
  return writeIdentifier(((_x) => _x)(mn));
}

export function writeMethodReference<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:MethodReference");
}

export function writeMultiplicativeExpression(e: JavaSyntax.MultiplicativeExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "unary": return ((u: JavaSyntax.UnaryExpression) => writeUnaryExpression(u))((_m as any).value);
    case "times": return ((b: JavaSyntax.MultiplicativeExpression_Binary) => Serialization.infixWs("*")(writeMultiplicativeExpression(((_x) => _x.lhs)(b)))(writeUnaryExpression(((_x) => _x.rhs)(b))))((_m as any).value);
    case "divide": return ((b: JavaSyntax.MultiplicativeExpression_Binary) => Serialization.infixWs("/")(writeMultiplicativeExpression(((_x) => _x.lhs)(b)))(writeUnaryExpression(((_x) => _x.rhs)(b))))((_m as any).value);
    case "mod": return ((b: JavaSyntax.MultiplicativeExpression_Binary) => Serialization.infixWs("%")(writeMultiplicativeExpression(((_x) => _x.lhs)(b)))(writeUnaryExpression(((_x) => _x.rhs)(b))))((_m as any).value);
  }
})();
}

export function writeNormalAnnotation(na: JavaSyntax.NormalAnnotation): Ast.Expr {
  return (() => {
  const tname = ((_x) => _x.typeName)(na);
  const pairs = ((_x) => _x.pairs)(na);
  return Serialization.prefix("@")(Serialization.noSep([writeTypeName(tname), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeElementValuePair)(pairs))]));
})();
}

export function writeNormalClassDeclaration(ncd: JavaSyntax.NormalClassDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(ncd);
  const id = ((_x) => _x.identifier)(ncd);
  const tparams = ((_x) => _x.parameters)(ncd);
  const msuperc = ((_x) => _x.extends)(ncd);
  const superi = ((_x) => _x.implements)(ncd);
  const body = ((_x) => _x.body)(ncd);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeClassModifier)(mods))), Serialization.cst("class"), Serialization.noSep(LibMaybes.cat([writeTypeIdentifier(id), LibLogic.ifElse(LibLists.null_(tparams))(null)(Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeParameter)(tparams)))])), LibMaybes.map(((c: JavaSyntax.ClassType) => Serialization.spaceSep([Serialization.cst("extends"), writeClassType(c)])))(msuperc), LibLogic.ifElse(LibLists.null_(superi))(null)(Serialization.spaceSep([Serialization.cst("implements"), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeInterfaceType)(superi))])), writeClassBody(body)]));
})();
}

export function writeNormalInterfaceDeclaration(nid: JavaSyntax.NormalInterfaceDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(nid);
  const id = ((_x) => _x.identifier)(nid);
  const tparams = ((_x) => _x.parameters)(nid);
  const extends_ = ((_x) => _x.extends)(nid);
  const body = ((_x) => _x.body)(nid);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeInterfaceModifier)(mods))), Serialization.cst("interface"), Serialization.noSep(LibMaybes.cat([writeTypeIdentifier(id), LibLogic.ifElse(LibLists.null_(tparams))(null)(Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeParameter)(tparams)))])), LibLogic.ifElse(LibLists.null_(extends_))(null)(Serialization.spaceSep([Serialization.cst("extends"), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeInterfaceType)(extends_))])), writeInterfaceBody(body)]));
})();
}

export function writeNumericType(nt: JavaSyntax.NumericType): Ast.Expr {
  return (() => {
  const _m = nt;
  switch (_m.tag) {
    case "integral": return ((it: JavaSyntax.IntegralType) => writeIntegralType(it))((_m as any).value);
    case "floatingPoint": return ((ft: JavaSyntax.FloatingPointType) => writeFloatingPointType(ft))((_m as any).value);
  }
})();
}

export function writePackageDeclaration(pd: JavaSyntax.PackageDeclaration): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(pd);
  const ids = ((_x) => _x.identifiers)(pd);
  return Serialization.withSemi(Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writePackageModifier)(mods))), Serialization.spaceSep([Serialization.cst("package"), Serialization.cst(LibStrings.intercalate(".")(LibLists.map(((id: JavaSyntax.Identifier) => ((_x) => _x)(id)))(ids)))])])));
})();
}

export function writePackageModifier(pm: JavaSyntax.PackageModifier): Ast.Expr {
  return writeAnnotation(((_x) => _x)(pm));
}

export function writePackageName(pn: JavaSyntax.PackageName): Ast.Expr {
  return Serialization.dotSep(LibLists.map(writeIdentifier)(((_x) => _x)(pn)));
}

export function writePackageOrTypeName(potn: JavaSyntax.PackageOrTypeName): Ast.Expr {
  return Serialization.dotSep(LibLists.map(writeIdentifier)(((_x) => _x)(potn)));
}

export function writePostDecrementExpression<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:PostDecrementExpression");
}

export function writePostIncrementExpression<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:PostIncrementExpression");
}

export function writePostfixExpression(e: JavaSyntax.PostfixExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "primary": return ((p: JavaSyntax.Primary) => writePrimary(p))((_m as any).value);
    case "name": return ((en: JavaSyntax.ExpressionName) => writeExpressionName(en))((_m as any).value);
    case "postIncrement": return ((pi: JavaSyntax.PostIncrementExpression) => writePostIncrementExpression(pi))((_m as any).value);
    case "postDecrement": return ((pd: JavaSyntax.PostDecrementExpression) => writePostDecrementExpression(pd))((_m as any).value);
  }
})();
}

export function writePreDecrementExpression<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:PreDecrementExpression");
}

export function writePreIncrementExpression<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:PreIncrementExpression");
}

export function writePrimary(p: JavaSyntax.Primary): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "noNewArray": return ((n: JavaSyntax.PrimaryNoNewArrayExpression) => writePrimaryNoNewArrayExpressionExpression(n))((_m as any).value);
    case "arrayCreation": return ((a: JavaSyntax.ArrayCreationExpression) => writeArrayCreationExpression(a))((_m as any).value);
  }
})();
}

export function writePrimaryNoNewArrayExpressionExpression(p: JavaSyntax.PrimaryNoNewArrayExpression): Ast.Expr {
  return (() => {
  const _m = p;
  switch (_m.tag) {
    case "literal": return ((l: JavaSyntax.Literal) => writeLiteral(l))((_m as any).value);
    case "classLiteral": return ((cl: JavaSyntax.ClassLiteral) => writeClassLiteral(cl))((_m as any).value);
    case "this": return ((_: void) => Serialization.cst("this"))((_m as any).value);
    case "dotThis": return ((n: JavaSyntax.TypeName) => Serialization.dotSep([writeTypeName(n), Serialization.cst("this")]))((_m as any).value);
    case "parens": return ((e: JavaSyntax.Expression) => Serialization.parenList(false)([writeExpression(e)]))((_m as any).value);
    case "classInstance": return ((ci: JavaSyntax.ClassInstanceCreationExpression) => writeClassInstanceCreationExpression(ci))((_m as any).value);
    case "fieldAccess": return ((fa: JavaSyntax.FieldAccess) => writeFieldAccess(fa))((_m as any).value);
    case "arrayAccess": return ((aa: JavaSyntax.ArrayAccess) => writeArrayAccess(aa))((_m as any).value);
    case "methodInvocation": return ((mi: JavaSyntax.MethodInvocation) => writeMethodInvocation(mi))((_m as any).value);
    case "methodReference": return ((mr: JavaSyntax.MethodReference) => writeMethodReference(mr))((_m as any).value);
  }
})();
}

export function writePrimitiveType(pt: JavaSyntax.PrimitiveType): Ast.Expr {
  return (() => {
  const _m = pt;
  switch (_m.tag) {
    case "numeric": return ((nt: JavaSyntax.NumericType) => writeNumericType(nt))((_m as any).value);
    case "boolean": return ((_: void) => Serialization.cst("boolean"))((_m as any).value);
  }
})();
}

export function writePrimitiveTypeWithAnnotations(ptwa: JavaSyntax.PrimitiveTypeWithAnnotations): Ast.Expr {
  return (() => {
  const pt = ((_x) => _x.type)(ptwa);
  const anns = ((_x) => _x.annotations)(ptwa);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(anns))(null)(Serialization.spaceSep(LibLists.map(writeAnnotation)(anns))), writePrimitiveType(pt)]));
})();
}

export function writeReceiverParameter<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:ReceiverParameter");
}

export function writeReferenceType(rt: JavaSyntax.ReferenceType): Ast.Expr {
  return (() => {
  const _m = rt;
  switch (_m.tag) {
    case "classOrInterface": return ((cit: JavaSyntax.ClassOrInterfaceType) => writeClassOrInterfaceType(cit))((_m as any).value);
    case "variable": return ((v: JavaSyntax.TypeVariable) => writeTypeVariable(v))((_m as any).value);
    case "array": return ((at: JavaSyntax.ArrayType) => writeArrayType(at))((_m as any).value);
  }
})();
}

export function writeRelationalExpression(e: JavaSyntax.RelationalExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "simple": return ((s: JavaSyntax.ShiftExpression) => writeShiftExpression(s))((_m as any).value);
    case "lessThan": return ((lt: JavaSyntax.RelationalExpression_LessThan) => writeRelationalExpression_LessThan(lt))((_m as any).value);
    case "greaterThan": return ((gt: JavaSyntax.RelationalExpression_GreaterThan) => writeRelationalExpression_GreaterThan(gt))((_m as any).value);
    case "lessThanEqual": return ((lte: JavaSyntax.RelationalExpression_LessThanEqual) => writeRelationalExpression_LessThanEqual(lte))((_m as any).value);
    case "greaterThanEqual": return ((gte: JavaSyntax.RelationalExpression_GreaterThanEqual) => writeRelationalExpression_GreaterThanEqual(gte))((_m as any).value);
    case "instanceof": return ((i: JavaSyntax.RelationalExpression_InstanceOf) => writeRelationalExpression_InstanceOf(i))((_m as any).value);
  }
})();
}

export function writeRelationalExpression_GreaterThan(gt: JavaSyntax.RelationalExpression_GreaterThan): Ast.Expr {
  return Serialization.infixWs(">")(writeRelationalExpression(((_x) => _x.lhs)(gt)))(writeShiftExpression(((_x) => _x.rhs)(gt)));
}

export function writeRelationalExpression_GreaterThanEqual(gte: JavaSyntax.RelationalExpression_GreaterThanEqual): Ast.Expr {
  return Serialization.infixWs(">=")(writeRelationalExpression(((_x) => _x.lhs)(gte)))(writeShiftExpression(((_x) => _x.rhs)(gte)));
}

export function writeRelationalExpression_InstanceOf(io: JavaSyntax.RelationalExpression_InstanceOf): Ast.Expr {
  return Serialization.infixWs("instanceof")(writeRelationalExpression(((_x) => _x.lhs)(io)))(writeReferenceType(((_x) => _x.rhs)(io)));
}

export function writeRelationalExpression_LessThan(lt: JavaSyntax.RelationalExpression_LessThan): Ast.Expr {
  return Serialization.infixWs("<")(writeRelationalExpression(((_x) => _x.lhs)(lt)))(writeShiftExpression(((_x) => _x.rhs)(lt)));
}

export function writeRelationalExpression_LessThanEqual(lte: JavaSyntax.RelationalExpression_LessThanEqual): Ast.Expr {
  return Serialization.infixWs("<=")(writeRelationalExpression(((_x) => _x.lhs)(lte)))(writeShiftExpression(((_x) => _x.rhs)(lte)));
}

export function writeResult(r: JavaSyntax.Result): Ast.Expr {
  return (() => {
  const _m = r;
  switch (_m.tag) {
    case "type": return ((t: JavaSyntax.UnannType) => writeUnannType(t))((_m as any).value);
    case "void": return ((_: void) => Serialization.cst("void"))((_m as any).value);
  }
})();
}

export function writeReturnStatement(rs: JavaSyntax.ReturnStatement): Ast.Expr {
  return (() => {
  const mex = ((_x) => _x)(rs);
  return Serialization.withSemi(Serialization.spaceSep(LibMaybes.cat([Serialization.cst("return"), LibMaybes.map(writeExpression)(mex)])));
})();
}

export function writeShiftExpression(e: JavaSyntax.ShiftExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "unary": return ((a: JavaSyntax.AdditiveExpression) => writeAdditiveExpression(a))((_m as any).value);
    case "shiftLeft": return ((b: JavaSyntax.ShiftExpression_Binary) => Serialization.infixWs("<<")(writeShiftExpression(((_x) => _x.lhs)(b)))(writeAdditiveExpression(((_x) => _x.rhs)(b))))((_m as any).value);
    case "shiftRight": return ((b: JavaSyntax.ShiftExpression_Binary) => Serialization.infixWs(">>")(writeShiftExpression(((_x) => _x.lhs)(b)))(writeAdditiveExpression(((_x) => _x.rhs)(b))))((_m as any).value);
    case "shiftRightZeroFill": return ((b: JavaSyntax.ShiftExpression_Binary) => Serialization.infixWs(">>>")(writeShiftExpression(((_x) => _x.lhs)(b)))(writeAdditiveExpression(((_x) => _x.rhs)(b))))((_m as any).value);
  }
})();
}

export function writeSimpleTypeName(stn: JavaSyntax.SimpleTypeName): Ast.Expr {
  return writeTypeIdentifier(((_x) => _x)(stn));
}

export function writeSingleElementAnnotation(sea: JavaSyntax.SingleElementAnnotation): Ast.Expr {
  return (() => {
  const tname = ((_x) => _x.name)(sea);
  const mv = ((_x) => _x.value)(sea);
  return LibMaybes.maybe(writeMarkerAnnotation(tname))(((v: JavaSyntax.ElementValue) => Serialization.prefix("@")(Serialization.noSep([writeTypeName(tname), Serialization.parenList(false)([writeElementValue(v)])]))))(mv);
})();
}

export function writeStatement(s: JavaSyntax.Statement): Ast.Expr {
  return (() => {
  const _m = s;
  switch (_m.tag) {
    case "withoutTrailing": return ((s2: JavaSyntax.StatementWithoutTrailingSubstatement) => writeStatementWithoutTrailingSubstatement(s2))((_m as any).value);
    case "labeled": return ((l: JavaSyntax.LabeledStatement) => writeLabeledStatement(l))((_m as any).value);
    case "ifThen": return ((it: JavaSyntax.IfThenStatement) => writeIfThenStatement(it))((_m as any).value);
    case "ifThenElse": return ((ite: JavaSyntax.IfThenElseStatement) => writeIfThenElseStatement(ite))((_m as any).value);
    case "while": return ((w: JavaSyntax.WhileStatement) => writeWhileStatement(w))((_m as any).value);
    case "for": return ((f: JavaSyntax.ForStatement) => writeForStatement(f))((_m as any).value);
  }
})();
}

export function writeStatementExpression(e: JavaSyntax.StatementExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "assignment": return ((a: JavaSyntax.Assignment) => writeAssignment(a))((_m as any).value);
    case "preIncrement": return ((pi: JavaSyntax.PreIncrementExpression) => writePreIncrementExpression(pi))((_m as any).value);
    case "preDecrement": return ((pd: JavaSyntax.PreDecrementExpression) => writePreDecrementExpression(pd))((_m as any).value);
    case "postIncrement": return ((pi: JavaSyntax.PostIncrementExpression) => writePostIncrementExpression(pi))((_m as any).value);
    case "postDecrement": return ((pd: JavaSyntax.PostDecrementExpression) => writePostDecrementExpression(pd))((_m as any).value);
    case "methodInvocation": return ((m: JavaSyntax.MethodInvocation) => writeMethodInvocation(m))((_m as any).value);
    case "classInstanceCreation": return ((cic: JavaSyntax.ClassInstanceCreationExpression) => writeClassInstanceCreationExpression(cic))((_m as any).value);
  }
})();
}

export function writeStatementWithoutTrailingSubstatement(s: JavaSyntax.StatementWithoutTrailingSubstatement): Ast.Expr {
  return (() => {
  const _m = s;
  switch (_m.tag) {
    case "block": return ((b: JavaSyntax.Block) => writeBlock(b))((_m as any).value);
    case "empty": return ((_: void) => Serialization.cst(";"))((_m as any).value);
    case "expression": return ((e: JavaSyntax.ExpressionStatement) => writeExpressionStatement(e))((_m as any).value);
    case "assert": return ((a: JavaSyntax.AssertStatement) => writeAssertStatement(a))((_m as any).value);
    case "switch": return ((s2: JavaSyntax.SwitchStatement) => writeSwitchStatement(s2))((_m as any).value);
    case "do": return ((d: JavaSyntax.DoStatement) => writeDoStatement(d))((_m as any).value);
    case "break": return ((b: JavaSyntax.BreakStatement) => writeBreakStatement(b))((_m as any).value);
    case "continue": return ((c: JavaSyntax.ContinueStatement) => writeContinueStatement(c))((_m as any).value);
    case "return": return ((r: JavaSyntax.ReturnStatement) => writeReturnStatement(r))((_m as any).value);
    case "synchronized": return ((s2: JavaSyntax.SynchronizedStatement) => writeSynchronizedStatement(s2))((_m as any).value);
    case "throw": return ((t: JavaSyntax.ThrowStatement) => writeThrowStatement(t))((_m as any).value);
    case "try": return ((t: JavaSyntax.TryStatement) => writeTryStatement(t))((_m as any).value);
  }
})();
}

export function writeStaticInitializer<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:StaticInitializer");
}

export function writeStringLiteral(sl: JavaSyntax.StringLiteral): Ast.Expr {
  return (() => {
  const s = ((_x) => _x)(sl);
  return Serialization.cst(LibStrings.cat2("\"")(LibStrings.cat2(escapeJavaString(s))("\"")));
})();
}

export function writeSwitchStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:SwitchStatement");
}

export function writeSynchronizedStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:SynchronizedStatement");
}

export function writeThrowStatement(ts: JavaSyntax.ThrowStatement): Ast.Expr {
  return Serialization.withSemi(Serialization.spaceSep([Serialization.cst("throw"), writeExpression(((_x) => _x)(ts))]));
}

export function writeThrows<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:Throws");
}

export function writeTryStatement<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:TryStatement");
}

export function writeType(t: JavaSyntax.Type): Ast.Expr {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "primitive": return ((pt: JavaSyntax.PrimitiveTypeWithAnnotations) => writePrimitiveTypeWithAnnotations(pt))((_m as any).value);
    case "reference": return ((rt: JavaSyntax.ReferenceType) => writeReferenceType(rt))((_m as any).value);
  }
})();
}

export function writeTypeArgument(a: JavaSyntax.TypeArgument): Ast.Expr {
  return (() => {
  const _m = a;
  switch (_m.tag) {
    case "reference": return ((rt: JavaSyntax.ReferenceType) => writeReferenceType(rt))((_m as any).value);
    case "wildcard": return ((w: JavaSyntax.Wildcard) => writeWildcard(w))((_m as any).value);
  }
})();
}

export function writeTypeArgumentsOrDiamond(targs: JavaSyntax.TypeArgumentsOrDiamond): Ast.Expr {
  return (() => {
  const _m = targs;
  switch (_m.tag) {
    case "arguments": return ((args: ReadonlyArray<JavaSyntax.TypeArgument>) => Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeArgument)(args)))((_m as any).value);
    case "diamond": return ((_: void) => Serialization.cst("<>"))((_m as any).value);
  }
})();
}

export function writeTypeBound(b: JavaSyntax.TypeBound): Ast.Expr {
  return (() => {
  const _m = b;
  switch (_m.tag) {
    case "variable": return ((tv: JavaSyntax.TypeVariable) => writeTypeVariable(tv))((_m as any).value);
    case "classOrInterface": return ((ci: JavaSyntax.TypeBound_ClassOrInterface) => (() => {
  const cit = ((_x) => _x.type)(ci);
  const additional = ((_x) => _x.additional)(ci);
  return LibLogic.ifElse(LibLists.null_(additional))(writeClassOrInterfaceType(cit))(Serialization.spaceSep(LibLists.cons(writeClassOrInterfaceType(cit))(LibLists.map(writeAdditionalBound)(additional))));
})())((_m as any).value);
  }
})();
}

export function writeTypeDeclaration(d: JavaSyntax.TypeDeclaration): Ast.Expr {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "class": return ((d2: JavaSyntax.ClassDeclaration) => writeClassDeclaration(d2))((_m as any).value);
    case "interface": return ((d2: JavaSyntax.InterfaceDeclaration) => writeInterfaceDeclaration(d2))((_m as any).value);
    case "none": return ((_: void) => Serialization.cst(";"))((_m as any).value);
  }
})();
}

export function writeTypeDeclarationWithComments(tdwc: JavaSyntax.TypeDeclarationWithComments): Ast.Expr {
  return (() => {
  const d = ((_x) => _x.value)(tdwc);
  const mc = ((_x) => _x.comments)(tdwc);
  return withComments(mc)(writeTypeDeclaration(d));
})();
}

export function writeTypeIdentifier(tid: JavaSyntax.TypeIdentifier): Ast.Expr {
  return writeIdentifier(((_x) => _x)(tid));
}

export function writeTypeName(tn: JavaSyntax.TypeName): Ast.Expr {
  return (() => {
  const id = ((_x) => _x.identifier)(tn);
  const mqual = ((_x) => _x.qualifier)(tn);
  return Serialization.dotSep(LibMaybes.cat([LibMaybes.map(writePackageOrTypeName)(mqual), writeTypeIdentifier(id)]));
})();
}

export function writeTypeParameter(tp: JavaSyntax.TypeParameter): Ast.Expr {
  return (() => {
  const mods = ((_x) => _x.modifiers)(tp);
  const id = ((_x) => _x.identifier)(tp);
  const bound = ((_x) => _x.bound)(tp);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(mods))(null)(Serialization.spaceSep(LibLists.map(writeTypeParameterModifier)(mods))), writeTypeIdentifier(id), LibMaybes.map(((b: JavaSyntax.TypeBound) => Serialization.spaceSep([Serialization.cst("extends"), writeTypeBound(b)])))(bound)]));
})();
}

export function writeTypeParameterModifier(tpm: JavaSyntax.TypeParameterModifier): Ast.Expr {
  return writeAnnotation(((_x) => _x)(tpm));
}

export function writeTypeVariable(tv: JavaSyntax.TypeVariable): Ast.Expr {
  return (() => {
  const anns = ((_x) => _x.annotations)(tv);
  const id = ((_x) => _x.identifier)(tv);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(anns))(null)(Serialization.spaceSep(LibLists.map(writeAnnotation)(anns))), writeTypeIdentifier(id)]));
})();
}

export function writeUnannType(ut: JavaSyntax.UnannType): Ast.Expr {
  return writeType(((_x) => _x)(ut));
}

export function writeUnaryExpression(e: JavaSyntax.UnaryExpression): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "preIncrement": return ((pi: JavaSyntax.PreIncrementExpression) => writePreIncrementExpression(pi))((_m as any).value);
    case "preDecrement": return ((pd: JavaSyntax.PreDecrementExpression) => writePreDecrementExpression(pd))((_m as any).value);
    case "plus": return ((p: JavaSyntax.UnaryExpression) => Serialization.spaceSep([Serialization.cst("+"), writeUnaryExpression(p)]))((_m as any).value);
    case "minus": return ((m: JavaSyntax.UnaryExpression) => Serialization.spaceSep([Serialization.cst("-"), writeUnaryExpression(m)]))((_m as any).value);
    case "other": return ((o: JavaSyntax.UnaryExpressionNotPlusMinus) => writeUnaryExpressionNotPlusMinus(o))((_m as any).value);
  }
})();
}

export function writeUnaryExpressionNotPlusMinus(e: JavaSyntax.UnaryExpressionNotPlusMinus): Ast.Expr {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "postfix": return ((p: JavaSyntax.PostfixExpression) => writePostfixExpression(p))((_m as any).value);
    case "tilde": return ((u: JavaSyntax.UnaryExpression) => Serialization.spaceSep([Serialization.cst("~"), writeUnaryExpression(u)]))((_m as any).value);
    case "not": return ((u: JavaSyntax.UnaryExpression) => Serialization.noSep([Serialization.cst("!"), writeUnaryExpression(u)]))((_m as any).value);
    case "cast": return ((c: JavaSyntax.CastExpression) => writeCastExpression(c))((_m as any).value);
  }
})();
}

export function writeUnqualifiedClassInstanceCreationExpression(ucice: JavaSyntax.UnqualifiedClassInstanceCreationExpression): Ast.Expr {
  return (() => {
  const targs = ((_x) => _x.typeArguments)(ucice);
  const cit = ((_x) => _x.classOrInterface)(ucice);
  const args = ((_x) => _x.arguments)(ucice);
  const mbody = ((_x) => _x.body)(ucice);
  return Serialization.spaceSep(LibMaybes.cat([Serialization.cst("new"), LibLogic.ifElse(LibLists.null_(targs))(null)(Serialization.angleBracesList(Serialization.inlineStyle)(LibLists.map(writeTypeArgument)(targs))), Serialization.noSep([writeClassOrInterfaceTypeToInstantiate(cit), Serialization.parenList(false)(LibLists.map(writeExpression)(args))]), LibMaybes.map(writeClassBody)(mbody)]));
})();
}

export function writeVariableArityParameter<t0>(_: t0): Ast.Expr {
  return Serialization.cst("STUB:VariableArityParameter");
}

export function writeVariableDeclarator(vd: JavaSyntax.VariableDeclarator): Ast.Expr {
  return (() => {
  const id = ((_x) => _x.id)(vd);
  const minit = ((_x) => _x.initializer)(vd);
  const idSec = writeVariableDeclaratorId(id);
  return LibMaybes.maybe(idSec)(((init: JavaSyntax.VariableInitializer) => Serialization.infixWs("=")(idSec)(writeVariableInitializer(init))))(minit);
})();
}

export function writeVariableDeclaratorId(vdi: JavaSyntax.VariableDeclaratorId): Ast.Expr {
  return (() => {
  const id = ((_x) => _x.identifier)(vdi);
  const mdims = ((_x) => _x.dims)(vdi);
  return Serialization.noSep(LibMaybes.cat([writeIdentifier(id), LibMaybes.map(writeDims)(mdims)]));
})();
}

export function writeVariableInitializer(i: JavaSyntax.VariableInitializer): Ast.Expr {
  return (() => {
  const _m = i;
  switch (_m.tag) {
    case "expression": return ((e: JavaSyntax.Expression) => writeExpression(e))((_m as any).value);
    case "arrayInitializer": return ((ai: JavaSyntax.ArrayInitializer) => writeArrayInitializer(ai))((_m as any).value);
  }
})();
}

export function writeVariableModifier(m: JavaSyntax.VariableModifier): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "annotation": return ((ann: JavaSyntax.Annotation) => writeAnnotation(ann))((_m as any).value);
    case "final": return ((_: void) => Serialization.cst("final"))((_m as any).value);
  }
})();
}

export function writeWhileStatement(ws: JavaSyntax.WhileStatement): Ast.Expr {
  return (() => {
  const mcond = ((_x) => _x.cond)(ws);
  const body = ((_x) => _x.body)(ws);
  const condSer = LibMaybes.maybe(Serialization.cst("true"))(((c: JavaSyntax.Expression) => writeExpression(c)))(mcond);
  return Serialization.spaceSep([Serialization.cst("while"), Serialization.parenList(false)([condSer]), Serialization.curlyBlock(Serialization.fullBlockStyle)(writeStatement(body))]);
})();
}

export function writeWildcard(w: JavaSyntax.Wildcard): Ast.Expr {
  return (() => {
  const anns = ((_x) => _x.annotations)(w);
  const mbounds = ((_x) => _x.wildcard)(w);
  return Serialization.spaceSep(LibMaybes.cat([LibLogic.ifElse(LibLists.null_(anns))(null)(Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeAnnotation)(anns))), Serialization.cst("*"), LibMaybes.map(writeWildcardBounds)(mbounds)]));
})();
}

export function writeWildcardBounds(b: JavaSyntax.WildcardBounds): Ast.Expr {
  return (() => {
  const _m = b;
  switch (_m.tag) {
    case "extends": return ((rt: JavaSyntax.ReferenceType) => Serialization.spaceSep([Serialization.cst("extends"), writeReferenceType(rt)]))((_m as any).value);
    case "super": return ((rt: JavaSyntax.ReferenceType) => Serialization.spaceSep([Serialization.cst("super"), writeReferenceType(rt)]))((_m as any).value);
  }
})();
}

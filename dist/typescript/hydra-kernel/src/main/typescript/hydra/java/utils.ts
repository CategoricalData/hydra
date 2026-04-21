// Note: this is an automatically generated file. Do not edit.

/**
 * Java utilities for constructing Java syntax trees
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
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JavaEnvironment from "./environment.js";
import * as JavaLanguage from "./language.js";
import * as JavaNames from "./names.js";
import * as JavaSerde from "./serde.js";
import * as JavaSyntax from "./syntax.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMath from "../lib/math.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Names from "../names.js";
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

export function addExpressions(exprs: ReadonlyArray<JavaSyntax.MultiplicativeExpression>): JavaSyntax.AdditiveExpression {
  return (() => {
  const first = ({ tag: "unary", value: LibLists.head(exprs) });
  const rest = LibLists.tail(exprs);
  return LibLists.foldl(((ae: JavaSyntax.AdditiveExpression) => ((me: JavaSyntax.MultiplicativeExpression) => ({ tag: "plus", value: ({
    lhs: ae,
    rhs: me
  }) }))))(first)(rest);
})();
}

export function addInScopeVar(name: Core.Name): ((x: JavaEnvironment.Aliases) => JavaEnvironment.Aliases) {
  return ((aliases: JavaEnvironment.Aliases) => ({
    currentNamespace: ((_x) => _x.currentNamespace)(aliases),
    packages: ((_x) => _x.packages)(aliases),
    branchVars: ((_x) => _x.branchVars)(aliases),
    recursiveVars: ((_x) => _x.recursiveVars)(aliases),
    inScopeTypeParams: ((_x) => _x.inScopeTypeParams)(aliases),
    polymorphicLocals: ((_x) => _x.polymorphicLocals)(aliases),
    inScopeJavaVars: LibSets.insert(name)(((_x) => _x.inScopeJavaVars)(aliases)),
    varRenames: ((_x) => _x.varRenames)(aliases),
    lambdaVars: ((_x) => _x.lambdaVars)(aliases),
    typeVarSubst: ((_x) => _x.typeVarSubst)(aliases),
    trustedTypeVars: ((_x) => _x.trustedTypeVars)(aliases),
    methodCodomain: ((_x) => _x.methodCodomain)(aliases),
    thunkedVars: ((_x) => _x.thunkedVars)(aliases)
  }));
}

export function addInScopeVars(names: ReadonlyArray<Core.Name>): ((x: JavaEnvironment.Aliases) => JavaEnvironment.Aliases) {
  return ((aliases: JavaEnvironment.Aliases) => LibLists.foldl(((a: JavaEnvironment.Aliases) => ((n: Core.Name) => addInScopeVar(n)(a))))(aliases)(names));
}

export function addJavaTypeParameter<t0>(rt: JavaSyntax.ReferenceType): ((x: JavaSyntax.Type) => ((x: t0) => Errors.Error | JavaSyntax.Type)) {
  return ((t: JavaSyntax.Type) => ((cx: t0) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "reference": return ((rt1: JavaSyntax.ReferenceType) => (() => {
  const _m = rt1;
  switch (_m.tag) {
    case "classOrInterface": return ((cit: JavaSyntax.ClassOrInterfaceType) => (() => {
  const _m = cit;
  switch (_m.tag) {
    case "class": return ((ct: JavaSyntax.ClassType) => (() => {
  const anns = ((_x) => _x.annotations)(ct);
  const qual = ((_x) => _x.qualifier)(ct);
  const id = ((_x) => _x.identifier)(ct);
  const args = ((_x) => _x.arguments)(ct);
  return ({ tag: "right", value: ({ tag: "reference", value: ({ tag: "classOrInterface", value: ({ tag: "class", value: ({
    annotations: anns,
    qualifier: qual,
    identifier: id,
    arguments: LibLists.concat2(args)([({ tag: "reference", value: rt })])
  }) }) }) }) });
})())((_m as any).value);
    case "interface": return ((_: JavaSyntax.InterfaceType) => ({ tag: "left", value: ({ tag: "other", value: "expected a Java class type" }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "variable": return ((tv: JavaSyntax.TypeVariable) => ({ tag: "right", value: javaTypeVariableToType(tv) }))((_m as any).value);
    case "array": return ((_: JavaSyntax.ArrayType) => ({ tag: "left", value: ({ tag: "other", value: "expected a Java class or interface type, or a variable" }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "primitive": return ((_: JavaSyntax.PrimitiveTypeWithAnnotations) => ({ tag: "left", value: ({ tag: "other", value: "expected a reference type" }) }))((_m as any).value);
  }
})()));
}

export function addVarRename(original: Core.Name): ((x: Core.Name) => ((x: JavaEnvironment.Aliases) => JavaEnvironment.Aliases)) {
  return ((renamed: Core.Name) => ((aliases: JavaEnvironment.Aliases) => ({
    currentNamespace: ((_x) => _x.currentNamespace)(aliases),
    packages: ((_x) => _x.packages)(aliases),
    branchVars: ((_x) => _x.branchVars)(aliases),
    recursiveVars: ((_x) => _x.recursiveVars)(aliases),
    inScopeTypeParams: ((_x) => _x.inScopeTypeParams)(aliases),
    polymorphicLocals: ((_x) => _x.polymorphicLocals)(aliases),
    inScopeJavaVars: ((_x) => _x.inScopeJavaVars)(aliases),
    varRenames: LibMaps.insert(original)(renamed)(((_x) => _x.varRenames)(aliases)),
    lambdaVars: ((_x) => _x.lambdaVars)(aliases),
    typeVarSubst: ((_x) => _x.typeVarSubst)(aliases),
    trustedTypeVars: ((_x) => _x.trustedTypeVars)(aliases),
    methodCodomain: ((_x) => _x.methodCodomain)(aliases),
    thunkedVars: ((_x) => _x.thunkedVars)(aliases)
  })));
}

export function fieldExpression(varId: JavaSyntax.Identifier): ((x: JavaSyntax.Identifier) => JavaSyntax.ExpressionName) {
  return ((fieldId: JavaSyntax.Identifier) => ({
    qualifier: [varId],
    identifier: fieldId
  }));
}

export function fieldNameToJavaExpression(fname: Core.Name): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "name", value: javaIdentifierToJavaExpressionName(fieldNameToJavaIdentifier(fname)) }) }) }) }) }) }) }) })]]]]] }) }) });
}

export function fieldNameToJavaIdentifier(fname: Core.Name): JavaSyntax.Identifier {
  return javaIdentifier(((_x) => _x)(fname));
}

export function fieldNameToJavaVariableDeclarator(fname: Core.Name): JavaSyntax.VariableDeclarator {
  return javaVariableDeclarator(javaIdentifier(((_x) => _x)(fname)))(null);
}

export function fieldNameToJavaVariableDeclaratorId(fname: Core.Name): JavaSyntax.VariableDeclaratorId {
  return javaVariableDeclaratorId(javaIdentifier(((_x) => _x)(fname)));
}

export function finalVarDeclarationStatement(id: JavaSyntax.Identifier): ((x: JavaSyntax.Expression) => JavaSyntax.BlockStatement) {
  return ((rhs: JavaSyntax.Expression) => ({ tag: "localVariableDeclaration", value: ({
    modifiers: [({ tag: "final" })],
    type: ({ tag: "var" }),
    declarators: [javaVariableDeclarator(id)(({ tag: "expression", value: rhs }))]
  }) }));
}

export function importAliasesForModule(mod: Packaging.Module): JavaEnvironment.Aliases {
  return ({
    currentNamespace: ((_x) => _x.namespace)(mod),
    packages: LibMaps.empty,
    branchVars: LibSets.empty,
    recursiveVars: LibSets.empty,
    inScopeTypeParams: LibSets.empty,
    polymorphicLocals: LibSets.empty,
    inScopeJavaVars: LibSets.empty,
    varRenames: LibMaps.empty,
    lambdaVars: LibSets.empty,
    typeVarSubst: LibMaps.empty,
    trustedTypeVars: LibSets.empty,
    methodCodomain: null,
    thunkedVars: LibSets.empty
  });
}

export function interfaceMethodDeclaration(mods: ReadonlyArray<JavaSyntax.InterfaceMethodModifier>): ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: string) => ((x: ReadonlyArray<JavaSyntax.FormalParameter>) => ((x: JavaSyntax.Result) => ((x: ReadonlyArray<JavaSyntax.BlockStatement> | null) => JavaSyntax.InterfaceMemberDeclaration))))) {
  return ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((methodName: string) => ((params: ReadonlyArray<JavaSyntax.FormalParameter>) => ((result: JavaSyntax.Result) => ((stmts: ReadonlyArray<JavaSyntax.BlockStatement> | null) => ({ tag: "interfaceMethod", value: ({
    modifiers: mods,
    header: javaMethodHeader(tparams)(methodName)(params)(result),
    body: javaMethodBody(stmts)
  }) }))))));
}

export function isEscaped(s: string): boolean {
  return LibEquality.equal(LibStrings.charAt(0)(s))(36);
}

export function javaAdditiveExpressionToJavaExpression(ae: JavaSyntax.AdditiveExpression): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ae }) }) })]]]]] }) }) });
}

export function javaArrayCreation(primType: JavaSyntax.PrimitiveTypeWithAnnotations): ((x: JavaSyntax.ArrayInitializer | null) => JavaSyntax.Expression) {
  return ((minit: JavaSyntax.ArrayInitializer | null) => (() => {
  const init_ = LibMaybes.cases(minit)([])(((i: JavaSyntax.ArrayInitializer) => i));
  return javaPrimaryToJavaExpression(({ tag: "arrayCreation", value: ({ tag: "primitiveArray", value: ({
    type: primType,
    dims: [],
    array: init_
  }) }) }));
})());
}

export function javaArrayInitializer(exprs: ReadonlyArray<JavaSyntax.Expression>): JavaSyntax.ArrayInitializer {
  return [LibLists.map(((e: JavaSyntax.Expression) => ({ tag: "expression", value: e })))(exprs)];
}

export function javaAssignmentStatement(lhs: JavaSyntax.LeftHandSide): ((x: JavaSyntax.Expression) => JavaSyntax.Statement) {
  return ((rhs: JavaSyntax.Expression) => ({ tag: "withoutTrailing", value: ({ tag: "expression", value: ({ tag: "assignment", value: ({
    lhs: lhs,
    op: ({ tag: "simple" }),
    expression: rhs
  }) }) }) }));
}

export function javaBoolean(b: boolean): JavaSyntax.Literal {
  return ({ tag: "boolean", value: b });
}

export function javaBooleanExpression(b: boolean): JavaSyntax.Expression {
  return javaPrimaryToJavaExpression(javaLiteralToJavaPrimary(javaBoolean(b)));
}

export const javaBooleanType: JavaSyntax.Type = javaPrimitiveTypeToJavaType(({ tag: "boolean" }));

export const javaBytePrimitiveType: JavaSyntax.PrimitiveTypeWithAnnotations = ({
    type: ({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "byte" }) }) }),
    annotations: []
  });

export function javaCastExpression(rt: JavaSyntax.ReferenceType): ((x: JavaSyntax.UnaryExpression) => JavaSyntax.CastExpression) {
  return ((expr: JavaSyntax.UnaryExpression) => ({ tag: "notPlusMinus", value: ({
    refAndBounds: ({
    type: rt,
    bounds: []
  }),
    expression: expr
  }) }));
}

export function javaCastExpressionToJavaExpression(ce: JavaSyntax.CastExpression): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "cast", value: ce }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaCastPrimitive(pt: JavaSyntax.PrimitiveType): ((x: JavaSyntax.UnaryExpression) => JavaSyntax.CastExpression) {
  return ((expr: JavaSyntax.UnaryExpression) => ({ tag: "primitive", value: ({
    type: ({
    type: pt,
    annotations: []
  }),
    expression: expr
  }) }));
}

export function javaClassDeclaration(aliases: JavaEnvironment.Aliases): ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: Core.Name) => ((x: ReadonlyArray<JavaSyntax.ClassModifier>) => ((x: Core.Name | null) => ((x: ReadonlyArray<JavaSyntax.InterfaceType>) => ((x: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => JavaSyntax.ClassDeclaration)))))) {
  return ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((elName: Core.Name) => ((mods: ReadonlyArray<JavaSyntax.ClassModifier>) => ((supname: Core.Name | null) => ((impls: ReadonlyArray<JavaSyntax.InterfaceType>) => ((bodyDecls: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => (() => {
  const extends_ = LibMaybes.map(((n: Core.Name) => nameToJavaClassType(aliases)(true)([])(n)(null)))(supname);
  return ({ tag: "normal", value: ({
    modifiers: mods,
    identifier: javaDeclName(elName),
    parameters: tparams,
    extends: extends_,
    implements: impls,
    body: bodyDecls
  }) });
})()))))));
}

export function javaClassType(args: ReadonlyArray<JavaSyntax.ReferenceType>): ((x: JavaSyntax.PackageName | null) => ((x: string) => JavaSyntax.ClassType)) {
  return ((pkg: JavaSyntax.PackageName | null) => ((id: string) => (() => {
  const qual = LibMaybes.cases(pkg)(({ tag: "none" }))(((p: JavaSyntax.PackageName) => ({ tag: "package", value: p })));
  const targs = LibLists.map(((rt: JavaSyntax.ReferenceType) => ({ tag: "reference", value: rt })))(args);
  return ({
    annotations: [],
    qualifier: qual,
    identifier: javaTypeIdentifier(id),
    arguments: targs
  });
})()));
}

export function javaClassTypeToJavaType(ct: JavaSyntax.ClassType): JavaSyntax.Type {
  return ({ tag: "reference", value: ({ tag: "classOrInterface", value: ({ tag: "class", value: ct }) }) });
}

export function javaConditionalAndExpressionToJavaExpression(cae: JavaSyntax.ConditionalAndExpression): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [cae] }) }) });
}

export function javaConstructorCall(ci: JavaSyntax.ClassOrInterfaceTypeToInstantiate): ((x: ReadonlyArray<JavaSyntax.Expression>) => ((x: JavaSyntax.ClassBody | null) => JavaSyntax.Expression)) {
  return ((args: ReadonlyArray<JavaSyntax.Expression>) => ((mbody: JavaSyntax.ClassBody | null) => ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "classInstance", value: ({
    qualifier: null,
    expression: ({
    typeArguments: [],
    classOrInterface: ci,
    arguments: args,
    body: mbody
  })
  }) }) }) }) }) }) }) }) }) }) })]]]]] }) }) })));
}

export function javaConstructorName(id: JavaSyntax.Identifier): ((x: JavaSyntax.TypeArgumentsOrDiamond | null) => JavaSyntax.ClassOrInterfaceTypeToInstantiate) {
  return ((targs: JavaSyntax.TypeArgumentsOrDiamond | null) => ({
    identifiers: [({
    annotations: [],
    identifier: id
  })],
    typeArguments: targs
  }));
}

export function javaDeclName(name: Core.Name): JavaSyntax.TypeIdentifier {
  return javaVariableName(name);
}

export function javaDoubleCastExpression(rawRt: JavaSyntax.ReferenceType): ((x: JavaSyntax.ReferenceType) => ((x: JavaSyntax.UnaryExpression) => JavaSyntax.CastExpression)) {
  return ((targetRt: JavaSyntax.ReferenceType) => ((expr: JavaSyntax.UnaryExpression) => (() => {
  const firstCast = javaCastExpressionToJavaExpression(javaCastExpression(rawRt)(expr));
  return javaCastExpression(targetRt)(javaExpressionToJavaUnaryExpression(firstCast));
})()));
}

export function javaDoubleCastExpressionToJavaExpression(rawRt: JavaSyntax.ReferenceType): ((x: JavaSyntax.ReferenceType) => ((x: JavaSyntax.UnaryExpression) => JavaSyntax.Expression)) {
  return ((targetRt: JavaSyntax.ReferenceType) => ((expr: JavaSyntax.UnaryExpression) => javaCastExpressionToJavaExpression(javaDoubleCastExpression(rawRt)(targetRt)(expr))));
}

export const javaEmptyStatement: JavaSyntax.Statement = ({ tag: "withoutTrailing", value: ({ tag: "empty" }) });

export function javaEqualityExpressionToJavaExpression(ee: JavaSyntax.EqualityExpression): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[ee]]]]] }) }) });
}

export function javaEqualityExpressionToJavaInclusiveOrExpression(ee: JavaSyntax.EqualityExpression): JavaSyntax.InclusiveOrExpression {
  return [[[ee]]];
}

export function javaEquals(lhs: JavaSyntax.EqualityExpression): ((x: JavaSyntax.RelationalExpression) => JavaSyntax.EqualityExpression) {
  return ((rhs: JavaSyntax.RelationalExpression) => ({ tag: "equal", value: ({
    lhs: lhs,
    rhs: rhs
  }) }));
}

export function javaEqualsNull(lhs: JavaSyntax.EqualityExpression): JavaSyntax.EqualityExpression {
  return javaEquals(lhs)(javaLiteralToJavaRelationalExpression(({ tag: "null" })));
}

export function javaExpressionNameToJavaExpression(en: JavaSyntax.ExpressionName): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "name", value: en }) }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaExpressionToJavaPrimary(e: JavaSyntax.Expression): JavaSyntax.Primary {
  return (() => {
  const fallback = ({ tag: "noNewArray", value: ({ tag: "parens", value: e }) });
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "assignment": return ((ae: JavaSyntax.AssignmentExpression) => (() => {
  const _m = ae;
  switch (_m.tag) {
    case "conditional": return ((ce: JavaSyntax.ConditionalExpression) => (() => {
  const _m = ce;
  switch (_m.tag) {
    case "simple": return ((cor: JavaSyntax.ConditionalOrExpression) => (() => {
  const cands = ((_x) => _x)(cor);
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(cands))(1))((() => {
  const iors = ((_x) => _x)(LibLists.head(cands));
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(iors))(1))((() => {
  const xors = ((_x) => _x)(LibLists.head(iors));
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(xors))(1))((() => {
  const ands = ((_x) => _x)(LibLists.head(xors));
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(ands))(1))((() => {
  const eqs = ((_x) => _x)(LibLists.head(ands));
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(eqs))(1))((() => {
  const _m = LibLists.head(eqs);
  switch (_m.tag) {
    case "unary": return ((rel: JavaSyntax.RelationalExpression) => (() => {
  const _m = rel;
  switch (_m.tag) {
    case "simple": return ((shift: JavaSyntax.ShiftExpression) => (() => {
  const _m = shift;
  switch (_m.tag) {
    case "unary": return ((add: JavaSyntax.AdditiveExpression) => (() => {
  const _m = add;
  switch (_m.tag) {
    case "unary": return ((mul: JavaSyntax.MultiplicativeExpression) => (() => {
  const _m = mul;
  switch (_m.tag) {
    case "unary": return ((unary: JavaSyntax.UnaryExpression) => (() => {
  const _m = unary;
  switch (_m.tag) {
    case "other": return ((npm: JavaSyntax.UnaryExpressionNotPlusMinus) => (() => {
  const _m = npm;
  switch (_m.tag) {
    case "postfix": return ((pf: JavaSyntax.PostfixExpression) => (() => {
  const _m = pf;
  switch (_m.tag) {
    case "primary": return ((p: JavaSyntax.Primary) => p)((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())(fallback);
})())(fallback);
})())(fallback);
})())(fallback);
})())(fallback);
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})())((_m as any).value);
    default: return fallback(_m);
  }
})();
})();
}

export function javaExpressionToJavaUnaryExpression(e: JavaSyntax.Expression): JavaSyntax.UnaryExpression {
  return ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "parens", value: e }) }) }) }) });
}

export function javaFieldAccessToJavaExpression(fa: JavaSyntax.FieldAccess): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "fieldAccess", value: fa }) }) }) }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaIdentifier(s: string): JavaSyntax.Identifier {
  return sanitizeJavaName(s);
}

export function javaIdentifierToJavaExpression(id: JavaSyntax.Identifier): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "name", value: ({
    qualifier: null,
    identifier: id
  }) }) }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaIdentifierToJavaExpressionName(id: JavaSyntax.Identifier): JavaSyntax.ExpressionName {
  return ({
    qualifier: null,
    identifier: id
  });
}

export function javaIdentifierToJavaRelationalExpression(id: JavaSyntax.Identifier): JavaSyntax.RelationalExpression {
  return ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "name", value: ({
    qualifier: null,
    identifier: id
  }) }) }) }) }) }) }) });
}

export function javaIdentifierToJavaUnaryExpression(id: JavaSyntax.Identifier): JavaSyntax.UnaryExpression {
  return ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "name", value: ({
    qualifier: null,
    identifier: id
  }) }) }) });
}

export function javaInstanceOf(lhs: JavaSyntax.RelationalExpression): ((x: JavaSyntax.ReferenceType) => JavaSyntax.RelationalExpression) {
  return ((rhs: JavaSyntax.ReferenceType) => ({ tag: "instanceof", value: ({
    lhs: lhs,
    rhs: rhs
  }) }));
}

export function javaInt(i: bigint): JavaSyntax.Literal {
  return ({ tag: "integer", value: i });
}

export function javaIntExpression(i: bigint): JavaSyntax.Expression {
  return javaPrimaryToJavaExpression(javaLiteralToJavaPrimary(javaInt(i)));
}

export const javaIntType: JavaSyntax.Type = javaPrimitiveTypeToJavaType(({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "int" }) }) }));

export function javaInterfaceDeclarationToJavaClassBodyDeclaration(nid: JavaSyntax.NormalInterfaceDeclaration): JavaSyntax.ClassBodyDeclaration {
  return ({ tag: "classMember", value: ({ tag: "interface", value: ({ tag: "normalInterface", value: nid }) }) });
}

export function javaLambda(v: Core.Name): ((x: JavaSyntax.Expression) => JavaSyntax.Expression) {
  return ((body: JavaSyntax.Expression) => ({ tag: "lambda", value: ({
    parameters: ({ tag: "single", value: variableToJavaIdentifier(v) }),
    body: ({ tag: "expression", value: body })
  }) }));
}

export function javaLambdaFromBlock(v: Core.Name): ((x: JavaSyntax.Block) => JavaSyntax.Expression) {
  return ((block: JavaSyntax.Block) => ({ tag: "lambda", value: ({
    parameters: ({ tag: "single", value: variableToJavaIdentifier(v) }),
    body: ({ tag: "block", value: block })
  }) }));
}

export function javaLiteralToJavaExpression(lit: JavaSyntax.Literal): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "literal", value: lit }) }) }) }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaLiteralToJavaMultiplicativeExpression(lit: JavaSyntax.Literal): JavaSyntax.MultiplicativeExpression {
  return ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "literal", value: lit }) }) }) }) }) });
}

export function javaLiteralToJavaPrimary(lit: JavaSyntax.Literal): JavaSyntax.Primary {
  return ({ tag: "noNewArray", value: ({ tag: "literal", value: lit }) });
}

export function javaLiteralToJavaRelationalExpression(lit: JavaSyntax.Literal): JavaSyntax.RelationalExpression {
  return ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "literal", value: lit }) }) }) }) }) }) }) }) });
}

export function javaMemberField(mods: ReadonlyArray<JavaSyntax.FieldModifier>): ((x: JavaSyntax.Type) => ((x: JavaSyntax.VariableDeclarator) => JavaSyntax.ClassBodyDeclaration)) {
  return ((jt: JavaSyntax.Type) => ((v: JavaSyntax.VariableDeclarator) => ({ tag: "classMember", value: ({ tag: "field", value: ({
    modifiers: mods,
    unannType: jt,
    variableDeclarators: [v]
  }) }) })));
}

export function javaMethodBody(mstmts: ReadonlyArray<JavaSyntax.BlockStatement> | null): JavaSyntax.MethodBody {
  return LibMaybes.cases(mstmts)(({ tag: "none" }))(((stmts: ReadonlyArray<JavaSyntax.BlockStatement>) => ({ tag: "block", value: stmts })));
}

export function javaMethodDeclarationToJavaClassBodyDeclaration(md: JavaSyntax.MethodDeclaration): JavaSyntax.ClassBodyDeclaration {
  return ({ tag: "classMember", value: ({ tag: "method", value: md }) });
}

export function javaMethodHeader(tparams: ReadonlyArray<JavaSyntax.TypeParameter>): ((x: string) => ((x: ReadonlyArray<JavaSyntax.FormalParameter>) => ((x: JavaSyntax.Result) => JavaSyntax.MethodHeader))) {
  return ((methodName: string) => ((params: ReadonlyArray<JavaSyntax.FormalParameter>) => ((result: JavaSyntax.Result) => ({
    parameters: tparams,
    result: result,
    declarator: ({
    identifier: methodName,
    receiverParameter: null,
    formalParameters: params
  }),
    throws: null
  }))));
}

export function javaMethodInvocationToJavaExpression(mi: JavaSyntax.MethodInvocation): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "methodInvocation", value: mi }) }) }) }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaMethodInvocationToJavaPostfixExpression(mi: JavaSyntax.MethodInvocation): JavaSyntax.PostfixExpression {
  return ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "methodInvocation", value: mi }) }) });
}

export function javaMethodInvocationToJavaPrimary(mi: JavaSyntax.MethodInvocation): JavaSyntax.Primary {
  return ({ tag: "noNewArray", value: ({ tag: "methodInvocation", value: mi }) });
}

export function javaMethodInvocationToJavaStatement(mi: JavaSyntax.MethodInvocation): JavaSyntax.Statement {
  return ({ tag: "withoutTrailing", value: ({ tag: "expression", value: ({ tag: "methodInvocation", value: mi }) }) });
}

export function javaMultiplicativeExpressionToJavaRelationalExpression(me: JavaSyntax.MultiplicativeExpression): JavaSyntax.RelationalExpression {
  return ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: me }) }) });
}

export function javaPackageDeclaration(ns: Packaging.Namespace): JavaSyntax.PackageDeclaration {
  return ({
    modifiers: [],
    identifiers: LibLists.map(((s: string) => s))(LibStrings.splitOn(".")(((_x) => _x)(ns)))
  });
}

export function javaPostfixExpressionToJavaEqualityExpression(pe: JavaSyntax.PostfixExpression): JavaSyntax.EqualityExpression {
  return ({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: pe }) }) }) }) }) }) });
}

export function javaPostfixExpressionToJavaExpression(pe: JavaSyntax.PostfixExpression): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: pe }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaPostfixExpressionToJavaInclusiveOrExpression(pe: JavaSyntax.PostfixExpression): JavaSyntax.InclusiveOrExpression {
  return [[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: pe }) }) }) }) }) }) })]]];
}

export function javaPostfixExpressionToJavaRelationalExpression(pe: JavaSyntax.PostfixExpression): JavaSyntax.RelationalExpression {
  return ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: pe }) }) }) }) }) });
}

export function javaPostfixExpressionToJavaUnaryExpression(pe: JavaSyntax.PostfixExpression): JavaSyntax.UnaryExpression {
  return ({ tag: "other", value: ({ tag: "postfix", value: pe }) });
}

export function javaPrimaryToJavaExpression(p: JavaSyntax.Primary): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: p }) }) }) }) }) }) }) })]]]]] }) }) });
}

export function javaPrimaryToJavaUnaryExpression(p: JavaSyntax.Primary): JavaSyntax.UnaryExpression {
  return ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: p }) }) });
}

export function javaPrimitiveTypeToJavaType(pt: JavaSyntax.PrimitiveType): JavaSyntax.Type {
  return ({ tag: "primitive", value: ({
    type: pt,
    annotations: []
  }) });
}

export function javaRefType(args: ReadonlyArray<JavaSyntax.ReferenceType>): ((x: JavaSyntax.PackageName | null) => ((x: string) => JavaSyntax.Type)) {
  return ((pkg: JavaSyntax.PackageName | null) => ((id: string) => ({ tag: "reference", value: ({ tag: "classOrInterface", value: ({ tag: "class", value: javaClassType(args)(pkg)(id) }) }) })));
}

export function javaReferenceTypeToRawType(rt: JavaSyntax.ReferenceType): JavaSyntax.ReferenceType {
  return (() => {
  const _m = rt;
  switch (_m.tag) {
    case "classOrInterface": return ((cit: JavaSyntax.ClassOrInterfaceType) => (() => {
  const _m = cit;
  switch (_m.tag) {
    case "class": return ((ct: JavaSyntax.ClassType) => (() => {
  const anns = ((_x) => _x.annotations)(ct);
  const qual = ((_x) => _x.qualifier)(ct);
  const id = ((_x) => _x.identifier)(ct);
  return ({ tag: "classOrInterface", value: ({ tag: "class", value: ({
    annotations: anns,
    qualifier: qual,
    identifier: id,
    arguments: []
  }) }) });
})())((_m as any).value);
    case "interface": return ((it: JavaSyntax.InterfaceType) => (() => {
  const ct = ((_x) => _x)(it);
  const anns = ((_x) => _x.annotations)(ct);
  const qual = ((_x) => _x.qualifier)(ct);
  const id = ((_x) => _x.identifier)(ct);
  return ({ tag: "classOrInterface", value: ({ tag: "interface", value: ({
    annotations: anns,
    qualifier: qual,
    identifier: id,
    arguments: []
  }) }) });
})())((_m as any).value);
  }
})())((_m as any).value);
    default: return rt(_m);
  }
})();
}

export function javaRelationalExpressionToJavaEqualityExpression(re: JavaSyntax.RelationalExpression): JavaSyntax.EqualityExpression {
  return ({ tag: "unary", value: re });
}

export function javaRelationalExpressionToJavaExpression(re: JavaSyntax.RelationalExpression): JavaSyntax.Expression {
  return javaEqualityExpressionToJavaExpression(({ tag: "unary", value: re }));
}

export function javaRelationalExpressionToJavaUnaryExpression(re: JavaSyntax.RelationalExpression): JavaSyntax.UnaryExpression {
  return ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "parens", value: ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: re })]]]]] }) }) }) }) }) }) }) });
}

export function javaReturnStatement(mex: JavaSyntax.Expression | null): JavaSyntax.Statement {
  return ({ tag: "withoutTrailing", value: ({ tag: "return", value: mex }) });
}

export function javaStatementsToBlock(stmts: ReadonlyArray<JavaSyntax.Statement>): JavaSyntax.Block {
  return LibLists.map(((s: JavaSyntax.Statement) => ({ tag: "statement", value: s })))(stmts);
}

export function javaString(s: string): JavaSyntax.Literal {
  return ({ tag: "string", value: s });
}

export function javaStringMultiplicativeExpression(s: string): JavaSyntax.MultiplicativeExpression {
  return javaLiteralToJavaMultiplicativeExpression(javaString(s));
}

export const javaThis: JavaSyntax.Expression = ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "other", value: ({ tag: "postfix", value: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "this" }) }) }) }) }) }) }) }) }) })]]]]] }) }) });

export function javaThrowIllegalArgumentException(args: ReadonlyArray<JavaSyntax.Expression>): JavaSyntax.Statement {
  return javaThrowStatement(javaConstructorCall(javaConstructorName("IllegalArgumentException")(null))(args)(null));
}

export function javaThrowIllegalStateException(args: ReadonlyArray<JavaSyntax.Expression>): JavaSyntax.Statement {
  return javaThrowStatement(javaConstructorCall(javaConstructorName("IllegalStateException")(null))(args)(null));
}

export function javaThrowStatement(e: JavaSyntax.Expression): JavaSyntax.Statement {
  return ({ tag: "withoutTrailing", value: ({ tag: "throw", value: e }) });
}

export function javaTypeFromTypeName(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => JavaSyntax.Type) {
  return ((elName: Core.Name) => javaTypeVariableToType(({
    annotations: [],
    identifier: nameToJavaTypeIdentifier(aliases)(false)(elName)
  })));
}

export function javaTypeIdentifier(s: string): JavaSyntax.TypeIdentifier {
  return s;
}

export function javaTypeIdentifierToJavaTypeArgument(id: JavaSyntax.TypeIdentifier): JavaSyntax.TypeArgument {
  return ({ tag: "reference", value: ({ tag: "variable", value: ({
    annotations: [],
    identifier: id
  }) }) });
}

export function javaTypeName(id: JavaSyntax.Identifier): JavaSyntax.TypeName {
  return ({
    identifier: id,
    qualifier: null
  });
}

export function javaTypeParameter(v: string): JavaSyntax.TypeParameter {
  return ({
    modifiers: [],
    identifier: javaTypeIdentifier(v),
    bound: null
  });
}

export function javaTypeToJavaFormalParameter(jt: JavaSyntax.Type): ((x: Core.Name) => JavaSyntax.FormalParameter) {
  return ((fname: Core.Name) => ({ tag: "simple", value: ({
    modifiers: [],
    type: jt,
    id: fieldNameToJavaVariableDeclaratorId(fname)
  }) }));
}

export function javaTypeToJavaReferenceType<t0>(t: JavaSyntax.Type): ((x: t0) => Errors.Error | JavaSyntax.ReferenceType) {
  return ((cx: t0) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "reference": return ((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: rt }))((_m as any).value);
    case "primitive": return ((_: JavaSyntax.PrimitiveTypeWithAnnotations) => ({ tag: "left", value: ({ tag: "other", value: "expected a Java reference type" }) }))((_m as any).value);
  }
})());
}

export function javaTypeToJavaResult(jt: JavaSyntax.Type): JavaSyntax.Result {
  return ({ tag: "type", value: jt });
}

export function javaTypeToJavaTypeArgument(t: JavaSyntax.Type): JavaSyntax.TypeArgument {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "reference": return ((rt: JavaSyntax.ReferenceType) => ({ tag: "reference", value: rt }))((_m as any).value);
    case "primitive": return ((_: JavaSyntax.PrimitiveTypeWithAnnotations) => ({ tag: "wildcard", value: ({
    annotations: [],
    wildcard: null
  }) }))((_m as any).value);
  }
})();
}

export function javaTypeVariable(v: string): JavaSyntax.ReferenceType {
  return ({ tag: "variable", value: ({
    annotations: [],
    identifier: javaTypeIdentifier(Formatting.capitalize(v))
  }) });
}

export function javaTypeVariableToType(tv: JavaSyntax.TypeVariable): JavaSyntax.Type {
  return ({ tag: "reference", value: ({ tag: "variable", value: tv }) });
}

export function javaUnaryExpressionToJavaExpression(ue: JavaSyntax.UnaryExpression): JavaSyntax.Expression {
  return ({ tag: "assignment", value: ({ tag: "conditional", value: ({ tag: "simple", value: [[[[[({ tag: "unary", value: ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ue }) }) }) }) })]]]]] }) }) });
}

export function javaUnaryExpressionToJavaRelationalExpression(ue: JavaSyntax.UnaryExpression): JavaSyntax.RelationalExpression {
  return ({ tag: "simple", value: ({ tag: "unary", value: ({ tag: "unary", value: ({ tag: "unary", value: ue }) }) }) });
}

export function javaVariableDeclarator(id: JavaSyntax.Identifier): ((x: JavaSyntax.VariableInitializer | null) => JavaSyntax.VariableDeclarator) {
  return ((minit: JavaSyntax.VariableInitializer | null) => ({
    id: javaVariableDeclaratorId(id),
    initializer: minit
  }));
}

export function javaVariableDeclaratorId(id: JavaSyntax.Identifier): JavaSyntax.VariableDeclaratorId {
  return ({
    identifier: id,
    dims: null
  });
}

export function javaVariableName(name: Core.Name): JavaSyntax.Identifier {
  return javaIdentifier(Names.localNameOf(name));
}

export function lookupJavaVarName(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => Core.Name) {
  return ((name: Core.Name) => LibMaybes.cases(LibMaps.lookup(name)(((_x) => _x.varRenames)(aliases)))(name)(((renamed: Core.Name) => renamed)));
}

export function makeConstructor(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => ((x: boolean) => ((x: ReadonlyArray<JavaSyntax.FormalParameter>) => ((x: ReadonlyArray<JavaSyntax.BlockStatement>) => JavaSyntax.ClassBodyDeclaration)))) {
  return ((elName: Core.Name) => ((private_: boolean) => ((params: ReadonlyArray<JavaSyntax.FormalParameter>) => ((stmts: ReadonlyArray<JavaSyntax.BlockStatement>) => (() => {
  const nm = nameToJavaTypeIdentifier(aliases)(false)(elName);
  const cons = ({
    parameters: [],
    name: nm,
    receiverParameter: null,
    formalParameters: params
  });
  const mods = [LibLogic.ifElse(private_)(({ tag: "private" }))(({ tag: "public" }))];
  const body = ({
    invocation: null,
    statements: stmts
  });
  return ({ tag: "constructorDeclaration", value: ({
    modifiers: mods,
    constructor: cons,
    throws: null,
    body: body
  }) });
})()))));
}

export function methodDeclaration(mods: ReadonlyArray<JavaSyntax.MethodModifier>): ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: ReadonlyArray<JavaSyntax.Annotation>) => ((x: string) => ((x: ReadonlyArray<JavaSyntax.FormalParameter>) => ((x: JavaSyntax.Result) => ((x: ReadonlyArray<JavaSyntax.BlockStatement> | null) => JavaSyntax.ClassBodyDeclaration)))))) {
  return ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((anns: ReadonlyArray<JavaSyntax.Annotation>) => ((methodName: string) => ((params: ReadonlyArray<JavaSyntax.FormalParameter>) => ((result: JavaSyntax.Result) => ((stmts: ReadonlyArray<JavaSyntax.BlockStatement> | null) => javaMethodDeclarationToJavaClassBodyDeclaration(({
    annotations: anns,
    modifiers: mods,
    header: javaMethodHeader(tparams)(methodName)(params)(result),
    body: javaMethodBody(stmts)
  }))))))));
}

export function methodInvocation(lhs: JavaSyntax.ExpressionName | JavaSyntax.Primary | null): ((x: JavaSyntax.Identifier) => ((x: ReadonlyArray<JavaSyntax.Expression>) => JavaSyntax.MethodInvocation)) {
  return ((methodName: JavaSyntax.Identifier) => ((args: ReadonlyArray<JavaSyntax.Expression>) => (() => {
  const header = LibMaybes.cases(lhs)(({ tag: "simple", value: methodName }))(((either: JavaSyntax.ExpressionName | JavaSyntax.Primary) => ({ tag: "complex", value: ({
    variant: LibEithers.either(((en: JavaSyntax.ExpressionName) => ({ tag: "expression", value: en })))(((p: JavaSyntax.Primary) => ({ tag: "primary", value: p })))(either),
    typeArguments: [],
    identifier: methodName
  }) })));
  return ({
    header: header,
    arguments: args
  });
})()));
}

export function methodInvocationStatic(self: JavaSyntax.Identifier): ((x: JavaSyntax.Identifier) => ((x: ReadonlyArray<JavaSyntax.Expression>) => JavaSyntax.MethodInvocation)) {
  return ((methodName: JavaSyntax.Identifier) => ((args: ReadonlyArray<JavaSyntax.Expression>) => methodInvocation(({ tag: "left", value: javaIdentifierToJavaExpressionName(self) }))(methodName)(args)));
}

export function methodInvocationStaticWithTypeArgs(self: JavaSyntax.Identifier): ((x: JavaSyntax.Identifier) => ((x: ReadonlyArray<JavaSyntax.TypeArgument>) => ((x: ReadonlyArray<JavaSyntax.Expression>) => JavaSyntax.MethodInvocation))) {
  return ((methodName: JavaSyntax.Identifier) => ((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => ((args: ReadonlyArray<JavaSyntax.Expression>) => (() => {
  const header = ({ tag: "complex", value: ({
    variant: ({ tag: "expression", value: javaIdentifierToJavaExpressionName(self) }),
    typeArguments: targs,
    identifier: methodName
  }) });
  return ({
    header: header,
    arguments: args
  });
})())));
}

export function nameToJavaClassType(aliases: JavaEnvironment.Aliases): ((x: boolean) => ((x: ReadonlyArray<JavaSyntax.TypeArgument>) => ((x: Core.Name) => ((x: string | null) => JavaSyntax.ClassType)))) {
  return ((qualify: boolean) => ((args: ReadonlyArray<JavaSyntax.TypeArgument>) => ((name: Core.Name) => ((mlocal: string | null) => (() => {
  const result = nameToQualifiedJavaName(aliases)(qualify)(name)(mlocal);
  const id = LibPairs.first(result);
  const pkg = LibPairs.second(result);
  return ({
    annotations: [],
    qualifier: pkg,
    identifier: id,
    arguments: args
  });
})()))));
}

export function nameToJavaName(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => JavaSyntax.Identifier) {
  return ((name: Core.Name) => (() => {
  const qn = Names.qualifyName(name);
  const ns_ = ((_x) => _x.namespace)(qn);
  const local = ((_x) => _x.local)(qn);
  return LibLogic.ifElse(isEscaped(((_x) => _x)(name)))(sanitizeJavaName(local))(LibMaybes.cases(ns_)(local)(((gname: Packaging.Namespace) => (() => {
  const parts = LibMaybes.cases(LibMaps.lookup(gname)(((_x) => _x.packages)(aliases)))(LibStrings.splitOn(".")(((_x) => _x)(gname)))(((pkgName: JavaSyntax.PackageName) => LibLists.map(((i: JavaSyntax.Identifier) => ((_x) => _x)(i)))(((_x) => _x)(pkgName))));
  const allParts = LibLists.concat2(parts)([sanitizeJavaName(local)]);
  return LibStrings.intercalate(".")(allParts);
})())));
})());
}

export function nameToJavaReferenceType(aliases: JavaEnvironment.Aliases): ((x: boolean) => ((x: ReadonlyArray<JavaSyntax.TypeArgument>) => ((x: Core.Name) => ((x: string | null) => JavaSyntax.ReferenceType)))) {
  return ((qualify: boolean) => ((args: ReadonlyArray<JavaSyntax.TypeArgument>) => ((name: Core.Name) => ((mlocal: string | null) => ({ tag: "classOrInterface", value: ({ tag: "class", value: nameToJavaClassType(aliases)(qualify)(args)(name)(mlocal) }) })))));
}

export function nameToJavaTypeIdentifier(aliases: JavaEnvironment.Aliases): ((x: boolean) => ((x: Core.Name) => JavaSyntax.TypeIdentifier)) {
  return ((qualify: boolean) => ((name: Core.Name) => LibPairs.first(nameToQualifiedJavaName(aliases)(qualify)(name)(null))));
}

export function nameToQualifiedJavaName(aliases: JavaEnvironment.Aliases): ((x: boolean) => ((x: Core.Name) => ((x: string | null) => readonly [JavaSyntax.TypeIdentifier, JavaSyntax.ClassTypeQualifier]))) {
  return ((qualify: boolean) => ((name: Core.Name) => ((mlocal: string | null) => (() => {
  const qn = Names.qualifyName(name);
  const ns_ = ((_x) => _x.namespace)(qn);
  const local = ((_x) => _x.local)(qn);
  const alias = LibMaybes.cases(ns_)(null)(((n: Packaging.Namespace) => LibMaybes.cases(LibMaps.lookup(n)(((_x) => _x.packages)(aliases)))(JavaNames.javaPackageName(LibStrings.splitOn(".")(((_x) => _x)(n))))(((id: JavaSyntax.PackageName) => id))));
  const pkg = LibLogic.ifElse(qualify)(LibMaybes.cases(alias)(({ tag: "none" }))(((p: JavaSyntax.PackageName) => ({ tag: "package", value: p }))))(({ tag: "none" }));
  const jid = javaTypeIdentifier(LibMaybes.cases(mlocal)(sanitizeJavaName(local))(((l: string) => LibStrings.cat2(LibStrings.cat2(sanitizeJavaName(local))("."))(sanitizeJavaName(l)))));
  return [jid, pkg];
})())));
}

export const overrideAnnotation: JavaSyntax.Annotation = ({ tag: "marker", value: javaTypeName("Override") });

export function referenceTypeToResult(rt: JavaSyntax.ReferenceType): JavaSyntax.Result {
  return javaTypeToJavaResult(({ tag: "reference", value: rt }));
}

export function sanitizeJavaName(name: string): string {
  return LibLogic.ifElse(isEscaped(name))(unescape(name))(LibLogic.ifElse(LibEquality.equal(name)("_"))("ignored")(Formatting.sanitizeWithUnderscores(JavaLanguage.reservedWords)(name)));
}

export const suppressWarningsUncheckedAnnotation: JavaSyntax.Annotation = ({ tag: "singleElement", value: ({
    name: javaTypeName("SuppressWarnings"),
    value: ({ tag: "conditionalExpression", value: ({ tag: "simple", value: [[javaPostfixExpressionToJavaInclusiveOrExpression(({ tag: "primary", value: javaLiteralToJavaPrimary(javaString("unchecked")) }))]] }) })
  }) });

export function toAcceptMethod(abstract_: boolean): ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => JavaSyntax.ClassBodyDeclaration) {
  return ((vtparams: ReadonlyArray<JavaSyntax.TypeParameter>) => (() => {
  const mods = LibLogic.ifElse(abstract_)([({ tag: "public" }), ({ tag: "abstract" })])([({ tag: "public" })]);
  const tparams = [javaTypeParameter(JavaNames.visitorReturnParameter)];
  const anns = LibLogic.ifElse(abstract_)([])([overrideAnnotation]);
  const typeArgs = LibLists.map(((tp: JavaSyntax.TypeParameter) => ({ tag: "reference", value: typeParameterToReferenceType(tp) })))(vtparams);
  const ref = javaClassTypeToJavaType(({
    annotations: [],
    qualifier: ({ tag: "none" }),
    identifier: javaTypeIdentifier(JavaNames.visitorName),
    arguments: LibLists.concat2(typeArgs)([({ tag: "reference", value: visitorTypeVariable })])
  }));
  const param = javaTypeToJavaFormalParameter(ref)("visitor");
  const result = javaTypeToJavaResult(({ tag: "reference", value: visitorTypeVariable }));
  const returnExpr = javaMethodInvocationToJavaExpression(methodInvocationStatic("visitor")(JavaNames.visitMethodName)([javaThis]));
  const body = LibLogic.ifElse(abstract_)(null)([({ tag: "statement", value: javaReturnStatement(returnExpr) })]);
  return methodDeclaration(mods)(tparams)(anns)(JavaNames.acceptMethodName)([param])(result)(body);
})());
}

export function toAssignStmt(fname: Core.Name): JavaSyntax.Statement {
  return (() => {
  const id = fieldNameToJavaIdentifier(fname);
  const lhs = ({ tag: "fieldAccess", value: ({
    qualifier: ({ tag: "primary", value: ({ tag: "noNewArray", value: ({ tag: "this" }) }) }),
    identifier: id
  }) });
  const rhs = fieldNameToJavaExpression(fname);
  return javaAssignmentStatement(lhs)(rhs);
})();
}

export function toJavaArrayType<t0>(t: JavaSyntax.Type): ((x: t0) => Errors.Error | JavaSyntax.Type) {
  return ((cx: t0) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "reference": return ((rt: JavaSyntax.ReferenceType) => (() => {
  const _m = rt;
  switch (_m.tag) {
    case "classOrInterface": return ((cit: JavaSyntax.ClassOrInterfaceType) => ({ tag: "right", value: ({ tag: "reference", value: ({ tag: "array", value: ({
    dims: [[]],
    variant: ({ tag: "classOrInterface", value: cit })
  }) }) }) }))((_m as any).value);
    case "array": return ((at: JavaSyntax.ArrayType) => (() => {
  const oldDims = ((_x) => _x)(((_x) => _x.dims)(at));
  const newDims = LibLists.concat2(oldDims)([[]]);
  const variant = ((_x) => _x.variant)(at);
  return ({ tag: "right", value: ({ tag: "reference", value: ({ tag: "array", value: ({
    dims: newDims,
    variant: variant
  }) }) }) });
})())((_m as any).value);
    case "variable": return ((_: JavaSyntax.TypeVariable) => ({ tag: "left", value: ({ tag: "other", value: "don't know how to make Java reference type into array type" }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "primitive": return ((_: JavaSyntax.PrimitiveTypeWithAnnotations) => ({ tag: "left", value: ({ tag: "other", value: "don't know how to make Java type into array type" }) }))((_m as any).value);
  }
})());
}

export function typeParameterToReferenceType(tp: JavaSyntax.TypeParameter): JavaSyntax.ReferenceType {
  return javaTypeVariable(((_x) => _x)(((_x) => _x)(((_x) => _x.identifier)(tp))));
}

export function typeParameterToTypeArgument(tp: JavaSyntax.TypeParameter): JavaSyntax.TypeArgument {
  return javaTypeIdentifierToJavaTypeArgument(((_x) => _x.identifier)(tp));
}

export function unTypeParameter(tp: JavaSyntax.TypeParameter): string {
  return ((_x) => _x)(((_x) => _x)(((_x) => _x.identifier)(tp)));
}

export function unescape(s: string): string {
  return LibStrings.fromList(LibLists.tail(LibStrings.toList(s)));
}

export function uniqueVarName(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => Core.Name) {
  return ((name: Core.Name) => LibLogic.ifElse(LibSets.member(name)(((_x) => _x.inScopeJavaVars)(aliases)))(uniqueVarName_go(aliases)(((_x) => _x)(name))(2))(name));
}

export function uniqueVarName_go(aliases: JavaEnvironment.Aliases): ((x: string) => ((x: number) => Core.Name)) {
  return ((base: string) => ((n: number) => (() => {
  const candidate = LibStrings.cat2(base)(LibLiterals.showInt32(n));
  return LibLogic.ifElse(LibSets.member(candidate)(((_x) => _x.inScopeJavaVars)(aliases)))(uniqueVarName_go(aliases)(base)(LibMath.add(n)(1)))(candidate);
})()));
}

export function varDeclarationStatement(id: JavaSyntax.Identifier): ((x: JavaSyntax.Expression) => JavaSyntax.BlockStatement) {
  return ((rhs: JavaSyntax.Expression) => ({ tag: "localVariableDeclaration", value: ({
    modifiers: [],
    type: ({ tag: "var" }),
    declarators: [javaVariableDeclarator(id)(({ tag: "expression", value: rhs }))]
  }) }));
}

export function variableDeclarationStatement<t0>(aliases: t0): ((x: JavaSyntax.Type) => ((x: JavaSyntax.Identifier) => ((x: JavaSyntax.Expression) => JavaSyntax.BlockStatement))) {
  return ((jtype: JavaSyntax.Type) => ((id: JavaSyntax.Identifier) => ((rhs: JavaSyntax.Expression) => (() => {
  const init_ = ({ tag: "expression", value: rhs });
  const vdec = javaVariableDeclarator(id)(init_);
  return ({ tag: "localVariableDeclaration", value: ({
    modifiers: [],
    type: ({ tag: "type", value: jtype }),
    declarators: [vdec]
  }) });
})())));
}

export function variableToJavaIdentifier(name: Core.Name): JavaSyntax.Identifier {
  return (() => {
  const v = ((_x) => _x)(name);
  return LibLogic.ifElse(LibEquality.equal(v)("_"))("ignored")(sanitizeJavaName(v));
})();
}

export function variantClassName(qualify: boolean): ((x: Core.Name) => ((x: Core.Name) => Core.Name)) {
  return ((elName: Core.Name) => ((fname: Core.Name) => (() => {
  const qn = Names.qualifyName(elName);
  const ns_ = ((_x) => _x.namespace)(qn);
  const local = ((_x) => _x.local)(qn);
  const flocal = Formatting.capitalize(((_x) => _x)(fname));
  const local1 = LibLogic.ifElse(qualify)(LibStrings.cat2(LibStrings.cat2(local)("."))(flocal))(LibLogic.ifElse(LibEquality.equal(flocal)(local))(LibStrings.cat2(flocal)("_"))(flocal));
  return Names.unqualifyName(({
    namespace: ns_,
    local: local1
  }));
})()));
}

export const visitorTypeVariable: JavaSyntax.ReferenceType = javaTypeVariable("r");
